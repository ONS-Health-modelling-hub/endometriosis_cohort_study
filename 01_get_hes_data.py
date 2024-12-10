####################################################################################################################################
# Get HES data
####################################################################################################################################

import pyspark.sql.functions as f
import pyspark.sql.functions as F
import os 
import de_utils
import sys

de_utils.rm_pre_wrap()


from pyspark.sql import SparkSession
from pyspark.sql.types import *

spark = (
    SparkSession.builder
    .appName("endo")
    .getOrCreate()
)

# load in hes
sys.path.insert(0, r'...')

#read in functions
sys.path.insert(1, f'...')
import pysparkFunctions as pf


sys.path.insert(1, f'...')
import hes_processing_v3 as hes

#--------------------------
# read in APC data
#--------------------------

apc = hes.loadHESAPC(
    spark,
    years=[
        "2009",
        "2010",
        "2011",
        "2012",
        "2013",
        "2014",
        "2015",
        "2016",
        "2017",
        "2018",
        "2019",
        "2020",
        "2021",
        "2022",
        "2023",
        "2024",
    ],
    make_cips_id=False,
    remove_episodes_with_blank_nhs_numbers=True,
)

#--------------------------
# read in lookup
#--------------------------

import pandas as pd

lookup = (
    r"..."
)

lookup = pd.read_csv(lookup)

#--------------------------
# create endo flags for both primary and secondary diagnoses (main analysis)
#--------------------------

# return diag flags, inc. early_return to not agg
apc_both, _ , _  = hes.personLevelNeverEverFlag(
    apc = apc, 
    icdLookup = lookup,
    icdLookupLevel = "Both",
    early_return = True
)


# consistently name columns across tables - make sure you run this post flag
all_hes = hes.HESDeriveCrossSettingVariables(
    spark,
    apc=apc_both,
    newColumns={
        "start_date": {
            "apc": F.col("EPISTART"),
            "op": F.col("APPTDATE"),
            "ae": F.col("ARRIVALDATE"),
        },
        "setting": {"apc": F.lit("APC"), "op": F.lit("OP"), "ae": F.lit("AE")},
        "episode_identifier": {
            "apc": F.col("EPIKEY"),
            "op": F.col("ATTENDKEY"),
            "ae": F.col("AEKEY"),
        },
        # No end_date column for AE,so leave empty
        # Start and end date are same for OP
        "end_date": {
            "apc": F.col("EPIEND"),
            "op": F.col("APPTDATE"),
            "ae": F.lit(""),
        },
    },
    return_type="combined",
)

all_hes.cache().count()


# save out
all_hes.write.mode("overwrite").saveAsTable("...")


#--------------------------
# create endo flags for primary diagnoses only (secondary analysis)
#--------------------------

# return diag flags, inc. early_return to not agg
apc_primary, _ , _  = hes.personLevelNeverEverFlag(
    apc = apc, 
    icdLookup = lookup,
    icdLookupLevel = "Primary",
    early_return = True
)

# Consistently name columns across tables - make sure you run this post flag
all_hes_primary = hes.HESDeriveCrossSettingVariables(
    spark,
    apc=apc_primary,
    newColumns={
        "start_date": {
            "apc": F.col("EPISTART"),
            "op": F.col("APPTDATE"),
            "ae": F.col("ARRIVALDATE"),
        },
        "setting": {"apc": F.lit("APC"), "op": F.lit("OP"), "ae": F.lit("AE")},
        "episode_identifier": {
            "apc": F.col("EPIKEY"),
            "op": F.col("ATTENDKEY"),
            "ae": F.col("AEKEY"),
        },
        # No end_date column for AE,so leave empty
        # Start and end date are same for OP
        "end_date": {
            "apc": F.col("EPIEND"),
            "op": F.col("APPTDATE"),
            "ae": F.lit(""),
        },
    },
    return_type="combined",
)


# save out
all_hes_primary.write.mode("overwrite").saveAsTable("...")



#-------------#
# END OF FILE #
#-------------#

