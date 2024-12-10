####################################################################################################################################
# Get census data
####################################################################################################################################

#--------------------------
# Set up spark session
#--------------------------

import pyspark.sql.functions as f
import pyspark.sql.functions as F
import os 
import de_utils
from functools import reduce
import sys

de_utils.rm_pre_wrap()

from pyspark.sql import SparkSession
from pyspark.sql.types import *

spark = (
    SparkSession.builder
    .appName("endo")
    .getOrCreate()
)

#--------------------------
# Read in functions
#--------------------------

sys.path.insert(1, f'...')
import pysparkFunctions as pf

sys.path.insert(1, f'...')
import parameters

#--------------------------
# Define additional functions
#--------------------------

def add_data_source_suffix(dataframe, source):
  """ 
  For a dataframe with will convert all column names to contain a 
  suffix that should be related to the source of the data
  
  Args:
    dataframe: PySpark dataframe
    source (String): Data source does the data come from
  
  Returns:
    dataframe : PySpark dataframe  
  """
  for col in dataframe.columns:
    suffix = f'_{source}'
    if suffix not in col.lower() and source != 'vacc':
      dataframe = dataframe.withColumnRenamed(col, col + suffix)
    elif source == 'vacc': 
      dataframe = dataframe.withColumnRenamed(col, col + suffix)

  return dataframe


def derive_variables(dataframe):
  """
  This function is intended to create the variables that are derived 
  in the death data.
  
  Args:
    dataframe: PySpark dataframe
  
  Returns:
    dataframe: PySpark dataframe
  """
  double_leading_zero_cols = [
    'DODMT_deaths',
    'DODDY_deaths',
    'dobdy_deaths',
    'dobmt_deaths',
    'dob_day_census',
    'dob_month_census']
    
  # Formatting the DOD variables to have a zero in front of the single digits
  for col in double_leading_zero_cols: # Adding leading zeros to month and day
    dataframe = dataframe.withColumn(
      col, 
      f.lpad(dataframe[col], 2, '0')
    )
  
  # Creating a deaths DOB
  dataframe = dataframe.withColumn(
    'dob_deaths',
    f.concat(
      dataframe['dobyr_deaths'],
      dataframe['dobmt_deaths'],
      dataframe['dobdy_deaths'])
  )
  
  # Creating a census DOB
  dataframe = dataframe.withColumn(
    'dob_census',
    f.concat(dataframe['dob_day_census'],
             dataframe['dob_month_census'],
             dataframe['dob_year_census'])
  )
    
  # Converting DOB to a date 
  dataframe = dataframe.withColumn(
    "dob_census",
    f.to_date(
      f.col("dob_census"),
      "ddMMyyyy")
  )

  
  dataframe = dataframe.withColumn(
    'diff_dobyr',
    f.when(
      dataframe['DOBYR_deaths'] != dataframe['dob_year_census'],1
    ).otherwise(0)
  )
    
  # Creating a DOD variable based on the three DOD variables
  dataframe = dataframe.withColumn(
    'dod_deaths',
    f.concat(
      dataframe['DODYR_deaths'],
      dataframe['DODMT_deaths'],
      dataframe['DODDY_deaths'])
  )

  # Creating a DORYR variable based on the year in which the death occurred
  dataframe = dataframe.withColumn(
    'DORYR_deaths',
    dataframe['DOR_deaths'].substr(1, 4)
  ) 
  
  dataframe = dataframe.withColumn(
    'sample',
    f.when(
      f.col('DOR_deaths').isNull(),
      'Alive'
    ).otherwise('Died')
  )
  
  dataframe = dataframe.withColumn(
    'sampling_weight',
    f.when(
      f.col('sample') == 'Alive', 20
    ).otherwise(1)
  )
    
  # Deriving a grouped version of the ethnicity variable - 'Ethnicity'
  dataframe = dataframe.withColumn(
    'ethnicity',
    f.when(dataframe['ethpuk11_census'] == "01", 'White')
    .when(dataframe['ethpuk11_census'] == "02", 'White')
    .when(dataframe['ethpuk11_census'] == "03", 'White')
    .when(dataframe['ethpuk11_census'] == "04", 'White')
    .when(dataframe['ethpuk11_census'] == "05", 'Mixed')
    .when(dataframe['ethpuk11_census'] == "06", 'Mixed')
    .when(dataframe['ethpuk11_census'] == "07", 'Mixed')
    .when(dataframe['ethpuk11_census'] == "08", 'Mixed')
    .when(dataframe['ethpuk11_census'] == "09", 'Indian')
    .when(dataframe['ethpuk11_census'] == "10", 'Bangladeshi and Pakistani')
    .when(dataframe['ethpuk11_census'] == "11", 'Bangladeshi and Pakistani')
    .when(dataframe['ethpuk11_census'] == "12", 'Chinese')
    .when(dataframe['ethpuk11_census'] == "13", 'Other')
    .when(dataframe['ethpuk11_census'] == "14", 'Black')
    .when(dataframe['ethpuk11_census'] == "15", 'Black')
    .when(dataframe['ethpuk11_census'] == "16", 'Black')
    .when(dataframe['ethpuk11_census'] == "17", 'Other')
    .when(dataframe['ethpuk11_census'] == "18", 'Other')
    .when(dataframe['ethpuk11_census'] == "19", 'Other')
    .otherwise("Unknown")
  )
                                     
  return dataframe


def create_present_in_dataset_flag(
  dataframe,
  datasource_name,
  datasource_identifier):
  """
  Create a new variable specifying if a person is present in a particular 
  data source. By using a variable to check for null and non-null values 
  this will create a Boolean variable.
  
  The intention for this variable is to make filtering the dataset more 
  easy for analysts.
  
  The produced variable will be called: "present_in_<datasource_name>"
  
  Args:
    dataframe: PySpark dataframe
    datasource_name (String): name of the data source'
    datasource_identifier (String): name of the variable to check on
    
  Returns:
    dataframe: PySpark dataframe
  """
  dataframe = dataframe.withColumn(
    f'present_in_{datasource_name}',
    f.when(
      f.col(datasource_identifier).isNotNull(), 1
    ).otherwise(0)
  )
  return dataframe


def format_specific_variables_for_analysis(dataframe):
  """
  Some columns are being requested for certain foramtting.
  
  This is because R scripts handle variables more efficiently and with more 
  ease in certain formats. For example, using date columns as strings 
  containing a '-' in between YYYY MM and DD ... YYYY-MM-DD
  
  Parameters
  -----------
  dataframe : PySpark
  
  Returns
  -----------
  dataframe : PySpark
  """
  format_date_cols_stringtype_list = ['dod_deaths','dor_deaths',
                                      'dob_deaths','dobs_deaths','dobt_deaths']
  
  dataframe = (reduce(
    lambda dataframe, col: dataframe.withColumn(
      col, f.regexp_replace(f.col(col),'-','')),
    format_date_cols_stringtype_list,
  dataframe
  ))
  
  dataframe = (reduce(
    lambda dataframe, col: dataframe.withColumn(
      col, f.concat(f.col(col).substr(1, 4),
                    f.lit('-'),
                    f.col(col).substr(5, 2),
                    f.lit('-'),
                    f.col(col).substr(7, 2),
                    )),
    format_date_cols_stringtype_list,
  dataframe
  ))
  
  format_date_cols_datetype_list = ['dob_census']
    
  dataframe = (reduce(
    lambda dataframe, col: dataframe.withColumn(
      col, 
      f.from_unixtime(
        f.unix_timestamp(col,
                       'yyyy-mm-dd'))),
    format_date_cols_datetype_list,
  dataframe
  ))
    
  return dataframe


def get_linkage_rates(before_dataframe, after_dataframe, identifier):
  """
  Use the null count in columns and the non-null count in columns to
  assess the linkage between two datasets.

  Ideally this should be the join key used to ensure an accurate linkage rate.

  Args:
    before_dataframe: PySpark dataframe
    after_dataframe: PySpark dataframe
    identifier (string): the variable being used for assessment
    
  Returns:
    link_rate(int): Linkage rate as a percentage 
  """
  before_count = before_dataframe.where(
      before_dataframe[identifier].isNotNull()
  ).count()
  after_count = after_dataframe.where(after_dataframe[identifier].isNotNull()).count()
  link_rate = (after_count / before_count) * 100

  pf.display_header("Link rate = {}%".format(link_rate))
  
  return link_rate



#--------------------------
# Read in census 2011 data
#--------------------------

census_dataframe = pf.get_census2011_data(spark,cols='*').cache()

census_dataframe = add_data_source_suffix(census_dataframe, "census")

census_dataframe.count()

#--------------------------
# Read in cenmortlink data
#--------------------------
  
latest_link_file = pf.get_recent_cenmort_link(spark)

pf.display_header(f"Reading link file: {latest_link_file}")

link_file = pf.get_cenmortlink(
    spark, latest_link_file, "..."
)

link_file = link_file.select(
  "census_person_id",
  "hes_nhsno",
  "deaths_nhsno",
  "pr19_lsoa11",
  "region_pr19",
  "care_home_dr",
  "care_home_pr19",
  "final_nhs_number",
  "cen_pr_flag",
  "priority"
)

link_file.cache().count()

#--------------------------
# Read in deaths data
#--------------------------

# Using function from pysparkFunctions to read mortality data
deaths_dataframe = (
    pf.get_mortality_data(
        spark,
        death_start_date=parameters.mortality_parameters.death_start_date,
        death_end_date=parameters.mortality_parameters.death_end_date,
        latest_death_file=parameters.mortality_parameters.latest_death_file,
        death_type=parameters.mortality_parameters.death_type,
    )
    .repartition("NHSNO")
)

deaths_dataframe = add_data_source_suffix(deaths_dataframe, "deaths")

deaths_dataframe.cache().count()

#--------------------------
# Link data
#--------------------------
  
# Link the census to the link file
link_cen = census_dataframe.join(
    link_file,
    census_dataframe["il4_person_id_census"] == link_file["census_person_id"],
    how = "outer",
)

link_cen.cache().count()

# Link the deaths data to the census and link dataframe
link_cen_deaths = link_cen.join(
    deaths_dataframe,
    link_cen["final_nhs_number"] == deaths_dataframe["NHSNO_deaths"],
    how = "left",
)

link_cen_deaths.cache().count()

#--------------------------
# Manipulate linked data
#--------------------------

# Sorting the columns into alphabetic order
link_cen_deaths = link_cen_deaths.select(
    sorted(link_cen_deaths.columns)
)

# Derive the rest of the variables that haven't been derived
dataframe_derived = derive_variables(link_cen_deaths)

# Adding a variable to specify if a person is in a datasource
df_variable_dict = {
  'deaths':'nhsno_deaths',
  'cenmortlink':'census_person_id',
  'census':'il4_person_id_census',
}

# Create flag for nhs no present in linked files
for key, value in df_variable_dict.items():
  dataframe_derived = create_present_in_dataset_flag(
  dataframe_derived, key, value
  )

df = dataframe_derived.repartition(300)

#--------------------------
# Get linkage rates
#--------------------------

pf.display_header("Displaying deaths linkage rate")
get_linkage_rates(deaths_dataframe, df, "nhsno_deaths")

#--------------------------
# Save linked dataset
#--------------------------  

# Make all column names lower case
df = (reduce(
    lambda df, col_name: df.withColumnRenamed(
      col_name, col_name.lower()),
    df.columns,
  df
))
  
df = format_specific_variables_for_analysis(df)

# After removing vaccination data, collect all present in flags
present_in_flag_cols = [i for i in df.columns if 'present_in' in i]

# After removing vaccination data, ensure no extra people slip through
df = df.replace(to_replace=0, value=None, subset=present_in_flag_cols)

df = df.dropna(how='all', thresh=None, subset=present_in_flag_cols)

# Replace nulls with 0 for present in flags
df = df.fillna(0, subset=present_in_flag_cols)

df.cache().count()

df.write.mode("overwrite").saveAsTable("...")


#-------------#
# END OF FILE #
#-------------#


