class mortality_parameters():
  """
  These parameters refer to the raw death data before being linked onto the asset.  
  Changes in this file will only relate to the extraction of the mortality data. 
  
  The script will use these parameters and select the correct tables from the database
  and evenutally join the deaths to the asset.
  
  Annual death data is received annually, and provisional death data is weekly.
  
  For deaths to join to the asset, these people must have been enumerated in the census
  Therefore, in the output you will not see:
    deaths prior to 27/03/2011 
    migrants to the UK after 27/03/2011 
    children born after 27/03/2011 
    and others based on linkage rates   
  """
  # What is the first date that you would like to see death data (inclusive)
  death_start_date = "20110101"
  
  # What is the last date that you would like to see death data (inclusive)
  death_end_date = "20231231"
  
  # What provisional death file are you looking to use? Stored in "provisional_deaths_weekly"
  latest_death_file = "deaths_20240926_std"
  
  # Are you looking at deaths according 'registration' or 'occurrence' date?
  death_type = "registration"
