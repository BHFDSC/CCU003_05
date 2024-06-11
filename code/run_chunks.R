# This script connects to DataBricks to generate characteristics of the 
# dars_nic_391419_j3w9t_collab.ccu003_05_out_cohort_covariates table - which must 
# be accessed in chunks. The data is summarized by various grouping variables, which 
# are separately saved to disk to be accessed again by the main analysis script  


library(Hmisc)
library(DBI)
#library(rjson)
library(tidyverse)
library(lubridate)
library(dbplyr)
#library(logger)

#Turn off pesky dplyr messages
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

setwd("D:/PhotonUser/My Files/Home Folder/collab/CCU003_05/final")

source("functions.R")

tables=create_table_connections()

chunk_num=1

for (chunk_num in c(1:10)){ #Data is in 10 chunks, which must be accessed separately (or we run out of memory)


  print(paste0("Running chunk: ",chunk_num))
  #Get datasets for this chunk and add on chronic info
  dat=combine_covariate_and_chronic(tables,chunk=chunk_num,test=FALSE) 
  #This keeps the dat dataset as 1 row = 1 individual
  
  #Create a secondary dataset that adds acute info
  dat_acute=combine_covariate_and_acute(tables,chunk=chunk_num,test=FALSE)
  #The dat_acute dataset is NOT 1 row = 1 individual
  
  print("Datasets combined")
  
  #Add in some columns and change some data 
  dat=reformat_data(dat) 
  dat_acute=reformat_data(dat_acute)
  
  create_table_S3(dat,dat_acute)
  
} #Temporary bracket so we don't need to run the whole code 
  
  create_supplementary_tables(dat_acute,chunk_num)
   
  #Create Table1:
  print("Creating table 1")
  create_table_one(dat,dat_acute,chunk_num)
  
  #Get age info:
  get_age_info_outcomes_acute(dat_acute)
  get_age_info_outcomes_chron(dat)
  
  #Get info on ages monthly for acute conditions 
  get_monthly_age_info_acute(dat_acute)
  
  #Get monthly age breakdowns
  get_monthly_grouped_age_info_acute(dat_acute)
  get_monthly_grouped_age_info_chron(dat)
    
  #Get age breakdowns to calculate averages later
  get_grouped_age_info(dat)
  #get_grouped_monthly_age_info(dat)
  
  #Get info on ages and save
  get_age_info(dat,
               c("age_year"),
               file.path(output_file_age,
                         paste0("table_1_dat_age_chunk_",
                                chunk_num,
                                ".csv")))
  
  #Get the full cohort descriptors - this can be done on only the chronic frame
  print("Calculating full cohort descriptors")
  get_fullcohort_grouping_stats(dat)
  
  #Calculate the monthly rates for both acute and chronic data
  #These are stratified by the groups defined in monthly_groups in functions.R
  
  #For some reason this variable resets itself in the loop somehow, so redefine it here   
  monthly_groups=c("age_group",
                   "SEX",
                   "ETHNIC_CAT",
                   "cci_index",
                   "region",
                   "IMD_2019_DECILES")
  
  print("Starting monthly group loop")
  for (monthly_group in monthly_groups){
    #if (monthly_group=="age_group"){next}
    print(monthly_group)
    get_monthly_chronic_counts(dat,monthly_group)
    get_monthly_acute_counts(dat_acute,monthly_group)
    #gc() #Collect garbage to free up memory in loop
  }
  
  
  print("Starting monthly group multi loop")

  #Same with these ones   
  monthly_groups_multi=c(";age_group&SEX;",
                         ";age_group&ETHNIC_CAT;",
                         ";age_group&cci_index;",
                         ";age_group&IMD_2019_DECILES;",
                         ";age_group&region;")
  
  #And these are for multiple strata, specified in functions.R
  for (monthly_group_string in monthly_groups_multi){
    #if (monthly_group=="age_group"){next}
    print(monthly_group_string)
    monthly_groups=groupvars_from_filename(monthly_group_string)
    get_monthly_chronic_counts(dat,monthly_groups)
    get_monthly_acute_counts(dat_acute,monthly_groups)
    #gc() #Collect garbage to free up memory in loop 
  }
    
} #End chunk loop


#Disconnect from data source 
DBI::dbDisconnect(con)

#Now we can conglomerate chunked-up data and calculate summary stats

#Generate table 1
reconstitute_fullcohort_data()

#Generate info on ages
reconstitute_age_data()
reconstitute_grouped_monthly_age_data()
reconstitute_grouped_age_data()
reconstitute_age_outcomes_chron()
reconstitute_age_outcomes_acute()

#Generate the monthly rates
create_monthly_rates_acute()
create_monthly_rates_chronic()

create_age_standardised_rates_acute()
create_age_standardised_rates_chronic()

#Last one:
create_monthly_rates_nostrat()

constitute_table_1()


