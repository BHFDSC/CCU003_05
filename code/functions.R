#library(rjson)
library(dbplyr)
library(odbc)


#Generate an access token using Databricks 
con <- dbConnect( odbc::odbc(), "Databricks")

#Convert from years to days: 
days_in_year=365.2425

#Set file locations:
output_file_age=file.path("outputs","data","age")
output_file_age_outcomes=file.path("outputs","data","age_outcomes")

output_file_fullcohort_groups=file.path("outputs","data","fullcohort_groups")
output_file_death=file.path("outputs","data","death")
output_file_monthly=file.path("outputs","data","monthly")

outfile_reconstituted_full_cohort=file.path("outputs","data","reconstituted")
outfile_reconstituted_ages=file.path("outputs","data","reconstituted")
outfile_monthly_rates=file.path("outputs","data","monthly_rates")

output_file_supplement=file.path("outputs","data","supplement")

#Define the group variables - the variables by which we wish to stratify the data
group_vars=c("SEX",
             "ETHNIC_CAT",
             "age_group",
             "IMD_2019_DECILES",
             "cci_index",
             "region",
             "prior_stroke_or_mi",
             "cov_hx_com_diabetes_flag",
             "cov_hx_com_hypercholesterolaemia_flag",
             "cov_hx_com_hf_flag",
             "cov_hx_com_pad_flag",
             "cov_hx_com_ckd_flag",
             "cov_hx_out_ami_flag",
             "cov_hx_out_stroke_flag"
)

sup_vars=c("SEX",
             "age_group",
             "cci_index",
             "region",
             "cov_hx_com_diabetes_flag",
             "cov_hx_com_hypercholesterolaemia_flag",
             "cov_hx_com_hf_flag",
             "cov_hx_com_pad_flag",
             "cov_hx_com_ckd_flag",
             "cov_hx_out_ami_flag",
             "cov_hx_out_stroke_flag")

chron_vars=c("name","event_dt")

#The monthly group variables are the groups by which we want monthly rates stratified 
monthly_groups=c("age_group",
                 "SEX",
                 "ETHNIC_CAT",
                 "cci_index",
                 "region",
                 "IMD_2019_DECILES")

#And these ones specify multiple strata
monthly_groups_multi=c(";age_group&SEX;",
                       ";age_group&ETHNIC_CAT;",
                       ";age_group&cci_index;",
                       ";age_group&IMD_2019_DECILES;",
                       ";age_group&region;")

#date variables for person time calculations:
dates <- c("2019-11-01", "2019-12-01", 
           "2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01",
           "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01",
           "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", "2021-06-01", 
           "2021-07-01", "2021-08-01", "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01",
           "2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01", "2022-05-01", "2022-06-01",
           "2022-07-01", "2022-08-01", "2022-09-01", "2022-10-01", "2022-11-01", "2022-12-01")


#Ensure TRE escrow rules
sanitize_output=function(vector_in){
  
  #Returns vector_in but with numbers <10 rounded to 10, and numbers >10 rounded to nearest 5
  #NAs are returned unchanged
  vector_in[!is.na(vector_in) & vector_in<10]=NA
  vector_in[!is.na(vector_in) & vector_in>10 ]=round(vector_in[!is.na(vector_in)& vector_in>10]/5)*5
  return(vector_in)
} 


create_table_connections=function(){
  
  #Read the tables of interest from databricks are return dbplyr objects for them

  #The original tables
  # cov_tbl <- tbl(con, in_schema("dars_nic_391419_j3w9t_collab", "ccu003_05_out_cohort_covariates"))
  # out_chronic_tbl <- tbl(con, in_schema("dars_nic_391419_j3w9t_collab", "ccu003_05_out_chronic_outcomes"))
  # out_acute_tbl <- tbl(con, in_schema("dars_nic_391419_j3w9t_collab", "ccu003_05_out_acute_outcomes"))
  
  #The updated (May 2023) tables
  cov_tbl <- tbl(con, in_schema("dars_nic_391419_j3w9t_collab", "ccu003_05_v2_out_cohort_covariates"))
  out_chronic_tbl <- tbl(con, in_schema("dars_nic_391419_j3w9t_collab", "ccu003_05_v2_out_chronic_outcomes"))
  out_acute_tbl <- tbl(con, in_schema("dars_nic_391419_j3w9t_collab", "ccu003_05_v2_out_acute_outcomes_all_sources_revised_washout"))  
  
  return(list("cov_tbl"=cov_tbl,
              "out_chron_tbl"=out_chronic_tbl,
              "out_acute_tbl"=out_acute_tbl))
}


combine_covariate_and_chronic = function(tables,chunk,test=FALSE){
  #Combine the covariate table with the hypertension/AF event tables 
  #This makes it possible to get individual descriptors for patients based on diagnoses
  
  
  if (test){
    #Shorten the datasets so we can work with them in testing
    tables$cov_tbl=tables$cov_tbl %>% filter(CHUNK==chunk) %>% head(10000)
    tables$out_chron_tbl=tables$out_chron_tbl %>% head(10000)
    tables$out_acute_tbl=tables$out_acute_tbl %>% head(10000)
  }
  
  #Extended table with hypertension info
  dat_ext_htn=tables$cov_tbl %>% 
    filter(CHUNK==chunk) %>% 
    left_join(tables$out_chron_tbl %>%
                filter(name=="hypertension") %>% 
                select(PERSON_ID,all_of(chron_vars)),
              by = "PERSON_ID") %>% 
    rename(event_dt_htn=event_dt) %>% 
    select(-name) 
  
  #Extended table with AF info
  dat_ext_af=tables$cov_tbl %>% 
    filter(CHUNK==chunk) %>% 
    left_join(tables$out_chron_tbl %>%
                filter(name=="AF") %>% 
                select(PERSON_ID,all_of(chron_vars)),
              by = "PERSON_ID") %>% 
    rename(event_dt_af=event_dt) %>% 
    select(-name) 

  dat=
    merge(dat_ext_htn,
          dat_ext_af,
          by=head(colnames(dat_ext_htn),-1)) %>% 
    filter(CHUNK==chunk) 

  
  if (test){
    dat=dat %>% 
      head(1000) %>% 
      collect()
  }else{
    dat=dat %>% collect()
  }
    
  
  #Consistency check, this new frame should be the same size as the original table
  if (test!=TRUE){
    stopifnot(nrow(dat)==tables$cov_tbl %>% filter(CHUNK==chunk) %>% tally() %>% pull(n))
  }
  
  #Check numbers look about right
  paste0("Read in: ", prettyNum(nrow(dat),big.mark=",")," records")
  return(dat)
  
}


combine_covariate_and_acute = function(tables,chunk,test=FALSE){
  #Combine the covariate table with the hypertension/AF event tables 
  #This makes it possible to get individual descriptors for patients based on diagnoses
  
  
  if (test){ 
    #Shorten the datasets so we can work with them in testing
    tables$cov_tbl=tables$cov_tbl %>% head(1000)
    tables$out_chron_tbl=tables$out_chron_tbl %>% head(1000)
    tables$out_acute_tbl=tables$out_acute_tbl %>% head(1000)
  }
  
  acute_vars=c("name","event_dt")
  
  #Extended table with stroke info
  dat_ext_str=tables$cov_tbl %>% 
    filter(CHUNK==chunk) %>% 
    left_join(tables$out_acute_tbl %>%
                filter(name=="stroke") %>% 
                select(PERSON_ID,all_of(chron_vars)),
              by = "PERSON_ID") %>% 
    rename(event_dt_str=event_dt) %>% 
    select(-name) 
  
  #Extended table with MI info
  dat_ext_ami=tables$cov_tbl %>% 
    filter(CHUNK==chunk) %>% 
    left_join(tables$out_acute_tbl %>%
                filter(name=="AMI") %>% 
                select(PERSON_ID,all_of(chron_vars)),
              by = "PERSON_ID") %>% 
    rename(event_dt_ami=event_dt) %>% 
    select(-name) 
  
  #Merge these two tables together, on all common columns 
  dat=
    merge(dat_ext_str,
          dat_ext_ami,
          by=head(colnames(dat_ext_str),-1)) %>% 
    filter(CHUNK==chunk) %>% 
    collect()
  
  
  #Unlike in the chronic table, the acute table is not one row per patient, so no consistency check is performed here
  
  #Check numbers look about right
  paste0("Read in: ",prettyNum(nrow(dat),big.mark=",")," records")
  return(dat)
}



get_age_info=function(dat,grouping_vars=c("age_year"),savefile){
  
  #Get summary information on the ages of the study individuals, and save to disk
  dat %>% 
    mutate(age_year=floor(study_start_age)) %>% 
    group_by(.dots=grouping_vars) %>% 
    dplyr::summarize(num_people=n(),
                     chunk=chunk_num) %>% 
    write_csv(file=savefile)
} 


get_age_info_outcomes_chron=function(dat,grouping_vars=c("age_year"),savefile=output_file_age_outcomes){
  
  #Get summary information on the ages of the study individuals, and save to disk
  dat %>% 
    mutate(age_year=floor(study_start_age)) %>% 
    group_by(age_year) %>% 
    dplyr::summarize(num_people_htn=sum(!is.na(event_dt_htn)),
                     num_people_af=sum(!is.na(event_dt_af)),
                     chunk=chunk_num) %>% 
    write_csv(file=file.path(savefile,paste0("chron_chunk",
                                             chunk_num,
                                             ".csv")))
} 


get_age_info_outcomes_acute=function(dat_acute,grouping_vars=c("age_year"),savefile=output_file_age_outcomes){
  
  #Get summary information on the ages of the study individuals, and save to disk
  dat_acute %>% 
    filter(!is.na(event_dt_ami) | !is.na(event_dt_str)) %>% 
    mutate(age_year=floor(study_start_age)) %>% 
    group_by(age_year,PERSON_ID) %>%
    summarize(event_dt_ami=first(event_dt_ami),
              event_dt_str=first(event_dt_str)) %>% 
    group_by(age_year) %>% 
    summarize(event_dt_ami=sum(!is.na(event_dt_ami)),
              event_dt_str=sum(!is.na(event_dt_str)),
              chunk=chunk_num) %>% 
    write_csv(file=file.path(savefile,paste0("acute_chunk",
                                             chunk_num,
                                             ".csv")))
}



get_grouped_age_info=function(dat){
  
  for (grouping_var in monthly_groups[2:length(monthly_groups)]){
    
    #Get summary information on the ages of the study individuals, and save to disk
    dat %>% 
      mutate(age_year=floor(study_start_age)) %>% 
      group_by(.dots=c("age_year",grouping_var)) %>% 
      dplyr::summarize(num_people=n(),
                       chunk=chunk_num) %>%
      write_csv(file=file.path(output_file_age,
                               paste0(grouping_var,
                                      "_chunk_",
                                      chunk_num,
                                      ".csv")))
  }
} 

get_monthly_age_info_acute=function(dat_acute){
  
  #Get summary info on monthly AMI events
  dat_ami=
    dat_acute %>% 
    filter(!is.na(event_dt_ami)) %>% 
    mutate(age_year=floor(study_start_age),
           yearmonth=ymd(paste0(as.character(year(ymd(event_dt_ami))),"-",as.character(month(ymd(event_dt_ami))),"-01"))) %>% 
    group_by(.dots=c("age_year","yearmonth")) %>% 
    dplyr::summarize(num_people_ami=length(unique(PERSON_ID)))
  
  #Get summary info on monthly stroke events
  dat_str=
    dat_acute %>% 
    filter(!is.na(event_dt_str)) %>% 
    mutate(age_year=floor(study_start_age),
           yearmonth=ymd(paste0(as.character(year(ymd(event_dt_str))),"-",as.character(month(ymd(event_dt_str))),"-01"))) %>% 
    group_by(.dots=c("age_year","yearmonth")) %>% 
    dplyr::summarize(num_people_str=length(unique(PERSON_ID)))
  
  #Merge and save
  dat_ami %>% 
    full_join(dat_str,
              by=c("age_year","yearmonth")) %>% 
    mutate(num_people_str=replace_na(num_people_str,0),
           num_people_ami=replace_na(num_people_ami,0),
           chunk=chunk_num) %>% 
    write_csv(file=file.path(output_file_age,
                             paste0("chunk_",
                                    chunk_num,
                                    "_nonmonthly_acute.csv")))
}


get_monthly_age_info_acute=function(dat_acute){
  
  #Get summary info on monthly AMI events
  dat_ami=
    dat_acute %>% 
    filter(!is.na(event_dt_ami)) %>% 
    mutate(age_year=floor(study_start_age),
           yearmonth=ymd(paste0(as.character(year(ymd(event_dt_ami))),"-",as.character(month(ymd(event_dt_ami))),"-01"))) %>% 
    group_by(.dots=c("age_year","yearmonth")) %>% 
    dplyr::summarize(num_people_ami=length(unique(PERSON_ID)))
  
  #Get summary info on monthly stroke events
  dat_str=
    dat_acute %>% 
    filter(!is.na(event_dt_str)) %>% 
    mutate(age_year=floor(study_start_age),
           yearmonth=ymd(paste0(as.character(year(ymd(event_dt_str))),"-",as.character(month(ymd(event_dt_str))),"-01"))) %>% 
    group_by(.dots=c("age_year","yearmonth")) %>% 
    dplyr::summarize(num_people_str=length(unique(PERSON_ID)))
  
  #Merge and save
  dat_ami %>% 
    full_join(dat_str,
              by=c("age_year","yearmonth")) %>% 
    mutate(num_people_str=replace_na(num_people_str,0),
           num_people_ami=replace_na(num_people_ami,0),
           chunk=chunk_num) %>% 
    write_csv(file=file.path(output_file_age,
                             paste0("chunk_",
                                    chunk_num,
                                    "_nonmonthly_acute.csv")))
}


get_monthly_grouped_age_info_acute=function(dat_acute){
  
  for (grouping_var in monthly_groups[2:length(monthly_groups)]){
    
    #Get summary info on monthly AMI events
    dat_ami=
      dat_acute %>% 
      filter(!is.na(event_dt_ami)) %>% 
      mutate(age_year=floor(study_start_age),
             yearmonth=ymd(paste0(as.character(year(ymd(event_dt_ami))),"-",as.character(month(ymd(event_dt_ami))),"-01"))) %>% 
      group_by(.dots=c("age_year","yearmonth",grouping_var)) %>% 
      dplyr::summarize(num_people_ami=length(unique(PERSON_ID)))
    
    #Get summary info on monthly stroke events
    dat_str=
      dat_acute %>% 
      filter(!is.na(event_dt_str)) %>% 
      mutate(age_year=floor(study_start_age),
             yearmonth=ymd(paste0(as.character(year(ymd(event_dt_str))),"-",as.character(month(ymd(event_dt_str))),"-01"))) %>% 
      group_by(.dots=c("age_year","yearmonth",grouping_var)) %>% 
      dplyr::summarize(num_people_str=length(unique(PERSON_ID)))
    
    #Merge and save
    dat_ami %>% 
      full_join(dat_str,
                by=c("age_year","yearmonth",grouping_var)) %>% 
      mutate(num_people_str=replace_na(num_people_str,0),
             num_people_ami=replace_na(num_people_ami,0),
             chunk=chunk_num) %>% 
      write_csv(file=file.path(output_file_age,
                               paste0(grouping_var,
                                      "_chunk_monthly_acute_",
                                      chunk_num,
                                      ".csv")))
  }
} 


get_monthly_grouped_age_info_chron=function(dat){
  
  for (grouping_var in monthly_groups[2:length(monthly_groups)]){
    
    #Get summary info on monthly htn events
    dat_htn=
      dat %>% 
      filter(!is.na(event_dt_htn)) %>% 
      mutate(age_year=floor(study_start_age),
             yearmonth=ymd(paste0(as.character(year(ymd(event_dt_htn))),"-",as.character(month(ymd(event_dt_htn))),"-01"))) %>% 
      group_by(.dots=c("age_year","yearmonth",grouping_var)) %>% 
      dplyr::summarize(num_people_htn=length(unique(PERSON_ID)))
    
    #Get summary info on monthly AF events
    dat_af=
      dat %>% 
      filter(!is.na(event_dt_af)) %>% 
      mutate(age_year=floor(study_start_age),
             yearmonth=ymd(paste0(as.character(year(ymd(event_dt_af))),"-",as.character(month(ymd(event_dt_af))),"-01"))) %>% 
      group_by(.dots=c("age_year","yearmonth",grouping_var)) %>% 
      dplyr::summarize(num_people_af=length(unique(PERSON_ID)))
    
    #Merge and save
    dat_htn %>% 
      full_join(dat_af,
                by=c("age_year","yearmonth",grouping_var)) %>% 
      mutate(num_people_af=replace_na(num_people_af,0),
             num_people_htn=replace_na(num_people_htn,0),
             chunk=chunk_num) %>% 
      write_csv(file=file.path(output_file_age,
                               paste0(grouping_var,
                                      "_chunk_monthly_chronic_",
                                      chunk_num,
                                      ".csv")))
  }
} 


reformat_data = function(dat){
  
  #Reformat data as it comes out of table object by: 
  # * Adding age group columns
  # * Reformat history columns
  # * Calculate hypertension and AF followups
  # * Reformat Charlson scores
  
  dat=
    dat %>%     #Bin the age groups
    mutate(age_group=case_when(0 < study_start_age  & study_start_age < 41 ~ "18-40",
                               41 <= study_start_age & study_start_age < 51 ~ "41-50", 
                               51 <= study_start_age & study_start_age< 61 ~ "51-60",  
                               61 <= study_start_age & study_start_age < 71 ~ "61-70", 
                               71 <= study_start_age & study_start_age< 81 ~ "71-80",  
                               81 <= study_start_age  & study_start_age~ ">81"),
           #Replace NAs with 0s in flag columns
           cov_hx_out_af_flag = ifelse(is.na(cov_hx_out_af_flag), 0, cov_hx_out_af_flag),
           cov_hx_out_hypertension_flag = ifelse(is.na(cov_hx_out_hypertension_flag), 0, cov_hx_out_hypertension_flag),
           cov_hx_com_ckd_flag = ifelse(is.na(cov_hx_com_ckd_flag), 0, cov_hx_com_ckd_flag),
           cov_hx_out_stroke_flag = ifelse(is.na(cov_hx_out_stroke_flag), 0, cov_hx_out_stroke_flag),
           cov_hx_com_hf_flag = ifelse(is.na(cov_hx_com_hf_flag), 0, cov_hx_com_hf_flag),
           cov_hx_com_pad_flag = ifelse(is.na(cov_hx_com_pad_flag), 0, cov_hx_com_pad_flag),
           cov_hx_out_ami_flag = ifelse(is.na(cov_hx_out_ami_flag), 0, cov_hx_out_ami_flag),
           cov_hx_out_stroke_flag = ifelse(is.na(cov_hx_out_stroke_flag), 0, cov_hx_out_stroke_flag),
           cov_hx_com_diabetes_flag = ifelse(is.na(cov_hx_com_diabetes_flag), 0, cov_hx_com_diabetes_flag),
           cov_hx_com_hypercholesterolaemia_flag = ifelse(is.na(cov_hx_com_hypercholesterolaemia_flag), 0, cov_hx_com_hypercholesterolaemia_flag))
  
  if ("event_dt_htn" %in% colnames(dat)){ #If this dataframe has hypertension info
    dat = 
      dat %>% 
      #Flag for prior stroke or MI
      #TODO: fix this so it's integer not true/false 
      mutate(prior_stroke_or_mi=cov_hx_out_stroke_flag==1 | cov_hx_out_ami_flag==1,
             #Calculate censoring dates for hypertension and MI - if there are no chronic events then use regular followup days
             follow_up_htn=case_when(!is.na(event_dt_htn)~as.integer(interval(ymd(study_start_date),ymd(event_dt_htn))/days(1)),
                                     TRUE ~ fu_days),
             follow_up_af=case_when(!is.na(event_dt_af)~as.integer(interval(ymd(study_start_date),ymd(event_dt_af))/days(1)),
                                    TRUE ~ fu_days))
  }
  
  #Rejig the Charlson indices slightly
  if (!length(is.na(dat$cci_index))==0){
    dat[is.na(dat$cci_index),]$cci_index = 0 #Set NaNs to 0
    dat[dat$cci_index >= 4,]$cci_index = 4 #Set anything 4+ to just 4 
  }
  return(dat)  
}


get_fullcohort_grouping_stats=function(dat){
  
  #Get the full cohort descriptors, grouping by the variables defined in group_vars
  # for (grouping_var in c(group_vars,monthly_groups_multi)){ #Group_vars is defined above
  for (grouping_var in c(monthly_groups,monthly_groups_multi)){
    
    grouping_vars=groupvars_from_filename(grouping_var)
    
    dat %>% 
      dplyr::group_by(.dots=grouping_vars) %>%
      dplyr::summarize(number_of_individuals=n(),
                       sum_followup=sum(fu_days,rm.na=TRUE),
                       sum_followup_htn=sum(follow_up_htn,rm.na=TRUE), #Censor to hypertension and AF events
                       sum_followup_af=sum(follow_up_af,rm.na=TRUE)) %>% 
      mutate(chunk=chunk_num) %>% 
      #Old code that added "descriptor:" text
      # mutate(descriptor = paste0(grouping_vars,": ",!!as.symbol(grouping_vars))) %>% 
      # select(-!!as.symbol(grouping_vars)) %>% 
      # mutate(chunk=chunk_num) 
      write_csv(file=file.path(output_file_fullcohort_groups,
                               paste0(filename_from_groupvars(grouping_vars),
                                      "_chunk",
                                      first(.$chunk),
                                      ".csv")))
  } 
}


get_monthly_chronic_counts = function(dat,groupvars){
  
  #Get monthly death data, stratified by groupvars,
  #and combine with hypertension and af counts  
  get_monthly_deaths(dat,groupvars) %>% 
    left_join(get_monthly_htn(dat,groupvars),
              by=c("yearmonth",groupvars),
              copy=TRUE) %>% 
    left_join(get_monthly_af(dat,groupvars),
              by=c("yearmonth",groupvars),
              copy=TRUE)%>% 
    write_csv(file=file.path(output_file_monthly,
                             "chronic",
                             paste0(
                               filename_from_groupvars(groupvars),
                               "_chunk",
                               first(.$chunk),
                               ".csv",collapse="")))            
}


get_monthly_acute_counts = function(dat,groupvars){
  
  #Get monthly death data, stratified by groupvars,
  #and combine with stroke and MI counts  
  get_monthly_deaths(dat,groupvars) %>% 
    left_join(get_monthly_str(dat,groupvars),
              by=c("yearmonth",groupvars),
              copy=TRUE) %>% 
    left_join(get_monthly_ami(dat,groupvars),
              by=c("yearmonth",groupvars),
              copy=TRUE) %>% 
    write_csv(file=file.path(output_file_monthly,
                             "acute",
                             paste0(
                               filename_from_groupvars(groupvars),
                               "_chunk",
                               first(.$chunk),
                               ".csv",collapse="")))
}


filename_from_groupvars=function(groupvars){
  #Codify the groupvariables into a string that can be inserted into a filename
  
  return(
    paste0(";",
           paste(groupvars,collapse="&"),
           ";",collapse="")
  )
}

groupvars_from_filename=function(filename){#
  #Read the codified grouping variables from a filename
  
  #find relevant parts of filename:
  reg=regexpr(";.*;",filename)
  if (reg!=-1){
    groups=substr(filename,reg,reg+attr(reg,"match.length")-1)
    groups=gsub(";","",groups)
    groups=c(str_split(groups,"&",simplify=TRUE))
    return(groups)
  }
  return(filename)
}


get_monthly_deaths=function(dat,groupvars){ 
  #Get the monthly deaths, stratifying using the specified variables,
  #and save to disk
  deaths=
    dat %>% 
    filter(!is.na(DOD)) %>% #Get only records that have dates of death
    mutate(yearmonth=ymd(paste0(as.character(year(DOD)),"-",as.character(month(DOD)),"-01")))%>% 
    filter(yearmonth<first(study_end_date)) %>%
    group_by(.dots=c("yearmonth",groupvars)) %>%
    summarise(number_of_deaths=n(),
              sum_followup_death=sum(interval(yearmonth,DOD)/days(1)))%>%
    arrange(yearmonth) %>%
    mutate(chunk=chunk_num,
           death_ratio=sum_followup_death/number_of_deaths) 
  
  deaths %>% 
    write_csv(file=file.path(output_file_death,
                             paste0("deaths",
                                    filename_from_groupvars(groupvars),
                                    "_chunk",
                                    first(.$chunk),
                                    ".csv")))
  return(deaths)
}


get_monthly_htn=function(dat,groupvars){ 
  #Monthly counts of hypertension events and their associated followup
  monthly_htn_pt <- NULL
  for (i in 1:length(dates)){
    temp_monthly_htn_pt <- dat %>%
      mutate(htn_end_date = fu_end_date,
             htn_end_date = as.Date(ifelse(event_dt_htn < fu_end_date & !is.na(event_dt_htn), event_dt_htn, htn_end_date),format = "%Y-%m-%d", origin = "1970-01-01"),
             start = as.Date(dates[i], format = "%Y-%m-%d", origin = "1970-01-01"),
             end = as.Date(start %m+% months(1),origin = "1970-01-01"),
             end = as.Date(ifelse(end > htn_end_date, htn_end_date, end),origin = "1970-01-01"),
             person_time = ifelse(interval(start,end)/days(1) > 0, interval(start,end)/days(1), 0) ,
             yearmonth = as.Date(dates[i], format = "%Y-%m-%d", origin = "1970-01-01")) %>%
      dplyr::group_by(.dots=c("yearmonth",groupvars)) %>%
      dplyr::summarize(pt_htn = sum(person_time)/days_in_year)
    monthly_htn_pt = rbind(monthly_htn_pt, temp_monthly_htn_pt)
  }
  
  out=dat %>% 
    filter(!is.na(event_dt_htn)) %>% 
    mutate(yearmonth=ymd(paste0(as.character(year(ymd(event_dt_htn))),"-",as.character(month(ymd(event_dt_htn))),"-01"))) %>% 
    dplyr::group_by(.dots=c("yearmonth",groupvars)) %>%
    dplyr::summarize(events_htn=n()) %>%
    left_join(monthly_htn_pt)
  return(out)
}


get_monthly_af=function(dat,groupvars){ 
  #Monthly counts of AF events and their associated followup
  monthly_af_pt <- NULL
  for (i in 1:length(dates)){
    temp_monthly_af_pt <- dat %>%
      mutate(af_end_date = fu_end_date,
             af_end_date = as.Date(ifelse(event_dt_af < fu_end_date & !is.na(event_dt_af), event_dt_af, af_end_date),format = "%Y-%m-%d", origin = "1970-01-01"),
             start = as.Date(dates[i], format = "%Y-%m-%d", origin = "1970-01-01"),
             end = as.Date(start %m+% months(1),origin = "1970-01-01"),
             end = as.Date(ifelse(end > af_end_date, af_end_date, end),origin = "1970-01-01"),
             person_time = ifelse(interval(start,end)/days(1) > 0, interval(start,end)/days(1), 0) ,
             yearmonth = as.Date(dates[i], format = "%Y-%m-%d", origin = "1970-01-01")) %>%
      dplyr::group_by(.dots=c("yearmonth",groupvars)) %>%
      dplyr::summarize(pt_af = sum(person_time)/days_in_year)
    monthly_af_pt = rbind(monthly_af_pt, temp_monthly_af_pt)
  }
  
  out=dat %>% 
    filter(!is.na(event_dt_af)) %>% 
    mutate(yearmonth=ymd(paste0(as.character(year(ymd(event_dt_af))),"-",as.character(month(ymd(event_dt_af))),"-01"))) %>% 
    dplyr::group_by(.dots=c("yearmonth",groupvars)) %>%
    dplyr::summarize(events_af=n()) %>% 
    #mean_age=round(weighted.mean(floor(study_start_age),num_people),2))%>%
    left_join(monthly_af_pt)
  
  return(out)}


get_monthly_str=function(dat,groupvars){ 
  #Monthly counts of AF events and their associated followup
  monthly_str_pt <- NULL
  for (i in 1:length(dates)){
    temp_monthly_str_pt <- dat %>%
      mutate(str_end_date = fu_end_date,
             str_end_date = as.Date(ifelse(event_dt_str < fu_end_date & !is.na(event_dt_str), event_dt_str, str_end_date),format = "%Y-%m-%d", origin = "1970-01-01"),
             start = as.Date(dates[i], format = "%Y-%m-%d", origin = "1970-01-01"),
             end = as.Date(start %m+% months(1),origin = "1970-01-01"),
             end = as.Date(ifelse(end > str_end_date, str_end_date, end),origin = "1970-01-01"),
             person_time = ifelse(interval(start,end)/days(1) > 0, interval(start,end)/days(1), 0) ,
             yearmonth = as.Date(dates[i], format = "%Y-%m-%d", origin = "1970-01-01")) %>%
      dplyr::group_by(.dots=c("yearmonth",groupvars)) %>%
      dplyr::summarize(pt_str = sum(person_time)/days_in_year)
    monthly_str_pt = rbind(monthly_str_pt, temp_monthly_str_pt)
  }
  
  out=dat %>% 
    filter(!is.na(event_dt_str)) %>% 
    mutate(yearmonth=ymd(paste0(as.character(year(ymd(event_dt_str))),"-",as.character(month(ymd(event_dt_str))),"-01"))) %>% 
    dplyr::group_by(.dots=c("yearmonth",groupvars)) %>%
    dplyr::summarize(events_str=n(),
                     individuals=length(unique(PERSON_ID))) %>% 
    left_join(monthly_str_pt)
  
  
  return(out)}



get_monthly_ami=function(dat,groupvars){ 
  #Monthly counts of AF events and their associated followup
  monthly_ami_pt <- NULL
  for (i in 1:length(dates)){
    temp_monthly_ami_pt <- dat %>%
      mutate(ami_end_date = fu_end_date,
             ami_end_date = as.Date(ifelse(event_dt_ami < fu_end_date & !is.na(event_dt_ami), event_dt_ami, ami_end_date),format = "%Y-%m-%d", origin = "1970-01-01"),
             start = as.Date(dates[i], format = "%Y-%m-%d", origin = "1970-01-01"),
             end = as.Date(start %m+% months(1),origin = "1970-01-01"),
             end = as.Date(ifelse(end > ami_end_date, ami_end_date, end),origin = "1970-01-01"),
             person_time = ifelse(interval(start,end)/days(1) > 0, interval(start,end)/days(1), 0) ,
             yearmonth = as.Date(dates[i], format = "%Y-%m-%d", origin = "1970-01-01")) %>%
      dplyr::group_by(.dots=c("yearmonth",groupvars)) %>%
      dplyr::summarize(pt_ami = sum(person_time)/days_in_year)
    monthly_ami_pt = rbind(monthly_ami_pt, temp_monthly_ami_pt)
  }
  
  out=dat %>% 
    filter(!is.na(event_dt_ami)) %>% 
    mutate(yearmonth=ymd(paste0(as.character(year(ymd(event_dt_ami))),"-",as.character(month(ymd(event_dt_ami))),"-01"))) %>% 
    dplyr::group_by(.dots=c("yearmonth",groupvars)) %>%
    dplyr::summarize(events_ami=n(), 
                     individuals=length(unique(PERSON_ID))) %>% 
    left_join(monthly_ami_pt)
  
  return(out)}


reconstitute_fullcohort_data=function(){
  #Put back together the chunked up full cohort data, and sum over the chunks
  #This is done for each descriptor
  
  #Define relevant files
  files_fullcohort=list.files(output_file_fullcohort_groups)
  #Associate them with their type:
  names(files_fullcohort)=lapply(files_fullcohort,groupvars_from_filename)
 
  for (grouping_var in c(monthly_groups,monthly_groups_multi)){
    
    
    if (!startsWith(grouping_var,";")){grouping_var=paste0(";",grouping_var,";")}
    
    #Find the relevant files:
    files_fullcohort_group=files_fullcohort[grepl(paste0("[^&]*",
                                                         grouping_var,"[^&]"),
                                                  files_fullcohort)]
    
    #Read in the saved data 
    groupdat=data.frame()  
    for (group_file in files_fullcohort_group){
      groupdat=rbind(groupdat,read.csv(file.path(output_file_fullcohort_groups,group_file)))
    }
    
    #Sum across chunks and add percentages 
    #groupdat =
    groupdat %>% 
      group_by(.dots=groupvars_from_filename(grouping_var)) %>% 
      dplyr::summarize(number_of_individuals=sanitize_output(sum(number_of_individuals)),
                       sum_followup=sum(sum_followup,na.rm=TRUE)/days_in_year,
                       sum_followup_htn=sum(sum_followup_htn,na.rm=TRUE)/days_in_year,
                       sum_followup_af=sum(sum_followup_af,na.rm=TRUE)/days_in_year) %>% 
      mutate(avg_followup=sum_followup/number_of_individuals,
             avg_followup_htn=sum_followup_htn/number_of_individuals,
             avg_followup_af=sum_followup_af/number_of_individuals,
             perc_number_of_individuals=round(number_of_individuals/sum(number_of_individuals)*100,2),
             perc_followup=round(sum_followup/sum(sum_followup)*100,2),
             perc_followup_htn=round(avg_followup_htn/sum(avg_followup_htn)*100,2),
             perc_followup_af=round(avg_followup_af/sum(avg_followup_af)*100,2)) %>% 
      write_csv(file=file.path(outfile_reconstituted_full_cohort,
                               paste0(filename_from_groupvars(grouping_var),
                                      ".csv")))
    #out=rbind(out,groupdat)
  }
}


reconstitute_grouped_age_data= function(){
  
  #Get the grouped ages data 
  age_files=list.files(output_file_age)
  age_files=age_files[!startsWith(age_files,"table_1")]
  
  #Exclude the monthly files
  age_files=age_files[!grepl("monthly",age_files)]
  
  #Associate with their types 
  names(age_files)=str_split(age_files,"_chunk",simplify=TRUE)[,1]
  
  
  for (type in unique(names(age_files))){
    
    #Get files for this type only
    files=age_files[names(age_files)==type]
    
    agedat=data.frame()  
    for (file in files) {
      agedat=rbind(agedat,read.csv(file.path(output_file_age,file)))
    }
    
    #Sum over chunks and save    
    agedat %>% 
      group_by(.dots=c("age_year",type)) %>% 
      summarize(num_people=sum(num_people)) %>% 
      group_by(.dots=c(type)) %>% 
      summarize(mean_age=round(weighted.mean(age_year,num_people),2),
                sd_age=round(sqrt(wtd.var(age_year,num_people)),2),
                median_age=median(rep(age_year,num_people)),
                IQR_ages=paste0(as.character(max(18,median_age-0.5*IQR(rep(age_year,num_people)))),
                                "-", 
                                as.character(median_age+0.5*IQR(rep(age_year,num_people))))) %>% 
      write_csv(file=file.path(outfile_reconstituted_ages,
                               paste0("grouped_age_",
                                      type,
                                      ".csv")))
  } 
}


reconstitute_monthly_age_data_acute=function(){
  #Get the monthly average age for acute conditions
  
  #Get the grouped ages data 
  age_files=list.files(output_file_age)
  age_files=age_files[grepl("_nonmonthly",age_files)]
  age_files=age_files[grepl("acute",age_files)]
  
  
  agedat=data.frame()  
  for (file in age_files) {
    agedat=rbind(agedat,read.csv(file.path(output_file_age,file)))
  }
  
  
  #Sum over chunks and save
  agedat %>% 
    group_by(yearmonth) %>% 
    summarize(mean_age_ami=round(weighted.mean(age_year,num_people_ami)),
              mean_age_str=round(weighted.mean(age_year,num_people_str)),
              median_age_ami=median(rep(age_year,num_people_ami)),
              median_age_str=median(rep(age_year,num_people_str)),
              IQR_ami=paste0(as.character(max(18,median_age_ami)-0.5*IQR(rep(age_year,num_people_ami))),
                             "-", 
                             as.character(median_age_ami+0.5*IQR(rep(age_year,num_people_ami)))),
              IQR_str=paste0(as.character(max(18,median_age_str)-0.5*IQR(rep(age_year,num_people_str))),
                             "-", 
                             as.character(median_age_str+0.5*IQR(rep(age_year,num_people_str))))) %>% 
    write_csv(file=file.path(outfile_reconstituted_ages,
                             paste0("nongrouped_age_monthly_acute.csv")))
              
}

reconstitute_grouped_monthly_age_data= function(){
  
  for (event_type in c("acute","chronic")){
    
    #Get the grouped ages data 
    age_files=list.files(output_file_age)
    age_files=age_files[grepl("_monthly",age_files)]
    age_files=age_files[grepl(event_type,age_files)]
    
    #Associate with their types 
    names(age_files)=str_split(age_files,"_chunk_monthly",simplify=TRUE)[,1]
    
    for (type in unique(names(age_files))){
      
      #Get files for this type only
      files=age_files[names(age_files)==type]
      
      agedat=data.frame()  
      for (file in files) {
        agedat=rbind(agedat,read.csv(file.path(output_file_age,file)))
      }
      
      
      count_cols=colnames(agedat)[startsWith(colnames(agedat),"num_people")]
      suffices=str_split(count_cols,"_",simplify=TRUE)[,3]
      
      #Sum over chunks and save   
      agedat %>% 
        group_by(.dots=c("yearmonth",type)) %>% 
        summarize(!!as.symbol(paste0("mean_age_",suffices[1])):=round(weighted.mean(age_year,
                                                                                    !!as.symbol(count_cols[1])),2),
                  !!as.symbol(paste0("mean_age_",suffices[2])):=round(weighted.mean(age_year,
                                                                                    !!as.symbol(count_cols[2])),2),
                  !!as.symbol(paste0("median_age_",suffices[1])):=median(rep(age_year,!!as.symbol(count_cols[1]))),
                  !!as.symbol(paste0("median_age_",suffices[2])):=median(rep(age_year,!!as.symbol(count_cols[2]))),
                  !!as.symbol(paste0("IQR_ages_",suffices[1])):=paste0(as.character(max(18,!!as.symbol(paste0("median_age_",suffices[1]))-
                                                                                          0.5*IQR(rep(age_year,!!as.symbol(count_cols[1]))))),
                                                                       "-", 
                                                                       as.character(!!as.symbol(paste0("median_age_",suffices[1]))+0.5*IQR(rep(age_year,!!as.symbol(count_cols[1]))))),
                  
                  !!as.symbol(paste0("IQR_ages_",suffices[2])):=paste0(as.character(max(18,!!as.symbol(paste0("median_age_",suffices[1]))-
                                                                                          0.5*IQR(rep(age_year,!!as.symbol(count_cols[2]))))),
                                                                       "-", 
                                                                       as.character(!!as.symbol(paste0("median_age_",suffices[1]))+0.5*IQR(rep(age_year,!!as.symbol(count_cols[2])))))) %>% 
        write_csv(file=file.path(outfile_reconstituted_ages,
                                 paste0("grouped_age_monthly_",
                                        type,
                                        "_",
                                        event_type,
                                        ".csv")))
    }
  } 
}


reconstitute_age_data = function(){
  #Now get the info on the ages
  age_files=list.files(output_file_age)
  age_files=age_files[startsWith(age_files,"table_1")]
  
  agedat=data.frame()  
  for (file in age_files) {
    agedat=rbind(agedat,read.csv(file.path(output_file_age,file)))
  }
  agedat_out=
    agedat %>% 
    group_by(age_year) %>% 
    summarise(num_people=sum(num_people)) %>% 
    mutate(age_group=case_when(0 < age_year  & age_year < 41 ~ "18-40",
                               41 <= age_year & age_year < 51 ~ "41-50", 
                               51 <= age_year & age_year< 61 ~ "51-60",  
                               61 <= age_year & age_year < 71 ~ "61-70", 
                               71 <= age_year & age_year< 81 ~ "71-80",  
                               81 <= age_year  & age_year~ ">81"))  
  
  #Get data on ages of full cohort at index start, split by age groups 
  ages=
    agedat_out %>% 
    dplyr::group_by(age_group) %>% 
    dplyr::summarize(num_people=sum(num_people))
  
  mean_age=weighted.mean(agedat_out$age_year,agedat_out$num_people)
  sd_age=sqrt(wtd.var(agedat_out$age_year,agedat_out$num_people))
  
  ages=rep(agedat_out$age_year,agedat_out$num_people)
  median_age=median(ages)
  IQR_ages=paste0(as.character(max(18,median_age-0.5*IQR(ages))),"-", as.character(median_age+0.5*IQR(ages)))
  
  agedata=data.frame(data="full cohort",
                     number_of_individuals=sanitize_output(length(ages)),
                     mean_age=mean_age,
                     sd_age=sd_age,
                     median_age=median_age,
                     IQR=IQR_ages)
  
  write_csv(agedata,file=file.path(outfile_reconstituted_ages,"ages.csv"))
}


create_monthly_rates_acute=function(){
  
 
  #Create the monthly rates from the chunked-up monthly data
  #The rates are split by the groups in monthly_vars and saved to 
  #outfile_monthly_rates directory 
  
  #Define full cohort data - calculated in chunk loop
  files_fullcohort=list.files(output_file_fullcohort_groups)
  
  #Associate them with their type:
  names(files_fullcohort)=lapply(files_fullcohort,groupvars_from_filename)
  
  type <- "acute" 
  #Define monthly files - calculated in chunk loop
  monthly_files=list.files(file.path(output_file_monthly,type))
  
  #Associate the groupvariables with the filenames
  names(monthly_files)=lapply(monthly_files,groupvars_from_filename)
  
  #For each monthly grouping variable
  for (monthly_var_group in unique(names(monthly_files))){
    #Get the files that hold monthly data - already calculated in chunk loop
    monthly_files_group=monthly_files[names(monthly_files)==monthly_var_group]
    # monthly_files_group=monthly_files[lapply(monthly_groups,groupvars_from_filename())]
    
    #Execute the string text of the variable name, so we can use the groups
    if(startsWith(monthly_var_group,"c(")){ #Then we are using multiple groups
      monthly_vars=eval(parse(text=monthly_var_group))
    }else{
      monthly_vars=monthly_var_group
    }
    
    #Sum data over chunks
    monthlydat=data.frame()  
    for (file in monthly_files_group){
      monthlydat=rbind(monthlydat,read.csv(file.path(output_file_monthly,type,file)))
    }
    
    #Get the relevant count columns:
    event_cols=colnames(monthlydat)[startsWith(colnames(monthlydat),"events")]
    event_cols_followup=paste0("pt",paste0("_",str_split(event_cols,"_",simplify=TRUE)[,2]))
    

    #Summarize over chunks
    monthlydat= 
      monthlydat %>% 
      dplyr::group_by(.dots=c("yearmonth",monthly_vars)) %>% 
      dplyr::summarize(number_of_deaths=replace_na(sum(number_of_deaths,na.rm=TRUE),0),
                       sum_followup_death=replace_na(sum(sum_followup_death,na.rm=TRUE)/days_in_year,0),
                       !!as.symbol(event_cols[1]):=replace_na(sum(!!as.symbol(event_cols[1]),na.rm=TRUE),0),
                       !!as.symbol(event_cols_followup[1]):=replace_na(sum(!!as.symbol(event_cols_followup[1]),
                                                                           na.rm=TRUE)/days_in_year,0),
                       !!as.symbol(event_cols[2]):=replace_na(sum(!!as.symbol(event_cols[2]),na.rm=TRUE),0),
                       !!as.symbol(event_cols_followup[2]):=replace_na(sum(!!as.symbol(event_cols_followup[2]),
                                                                           na.rm=TRUE)/days_in_year,0))
    
    #Step 2 - Get the info on the grouped variable from the full cohort
    
    #Define relevant files
    files_fullcohort_group=files_fullcohort[names(files_fullcohort)==monthly_var_group]
    
    #Create fullcohort data - so we know the size of the groups at the start of the study 
    dat_fc=data.frame()  
    for (file in files_fullcohort_group) {
      dat_fc=rbind(dat_fc,read.csv(file.path(output_file_fullcohort_groups,file)))# %>% mutate(chunk=chunk))
    }
    
    #Create the starting data - this is the same as above but summed over the descriptor groups 
    startdat=dat_fc %>% 
      group_by(.dots=monthly_vars) %>% 
      dplyr::summarize(num_people=sum(number_of_individuals))
    
    #Step 3 - Use the starting data and the monthly death data to 
    #figure out monthly deaths of individuals, split by monthly_var
    startdat_thismonth=startdat
    monthly_deaths=data.frame()
    for (month in unique(monthlydat$yearmonth)){
      
      dat_thismonth=
        monthlydat %>%
        select(c(yearmonth,all_of(monthly_vars),number_of_deaths,sum_followup_death)) %>% 
        filter(yearmonth==month)%>%
        left_join(startdat_thismonth,by=all_of(monthly_vars)) %>% 
        mutate(individuals_survive=num_people-number_of_deaths,
               sum_followup_nodeath=individuals_survive*days_in_month(yearmonth)/days_in_year,
               sum_followup_total=sum_followup_nodeath+sum_followup_death) %>% 
        rename(individuals_started=num_people) %>% 
        ungroup()
      
      startdat_thismonth= dat_thismonth %>%
        select(individuals_survive,all_of(monthly_vars)) %>% 
        rename(num_people=individuals_survive)
      
      monthly_deaths=rbind(monthly_deaths,dat_thismonth)
    }
    
    #Join with above data
    monthly_rates=
      monthly_deaths %>% 
      left_join(monthlydat %>% 
                  select(-c(number_of_deaths,sum_followup_death)),by=c("yearmonth",all_of(monthly_vars)))
    
    #Remove Scottish and Welsh data if we have it 
    if ("region" %in% monthly_vars){
      monthly_rates=
        monthly_rates %>%
        filter(!region %in% c("Scotland","Wales"))
    }
    
    #Sanitize output for escrow
    monthly_rates$number_of_deaths=sanitize_output(monthly_rates$number_of_deaths)
    monthly_rates$individuals_started=sanitize_output(monthly_rates$individuals_started)
    monthly_rates$individuals_survive=sanitize_output(monthly_rates$individuals_survive)
    
    #Ensure relevant followups are also censored 
    monthly_rates=monthly_rates %>% 
      mutate(sum_followup_death=ifelse(is.na(monthly_rates$number_of_deaths),
                                       NA,
                                       sum_followup_death),
             individuals_started=ifelse(is.na(monthly_rates$number_of_deaths),
                                        NA,
                                        individuals_started),
             individuals_survive=ifelse(is.na(monthly_rates$number_of_deaths),
                                        NA,
                                        individuals_survive)) %>% 
      mutate(sum_followup_nodeath=ifelse(is.na(monthly_rates$number_of_deaths),
                                         NA,
                                         sum_followup_nodeath))
    
    #Ensure the count columns are sanitized 
    for (event_col in event_cols){
      monthly_rates[event_col]=sanitize_output(monthly_rates[event_col][[1]])
      
      if(event_col == "events_htn" | event_col == "events_af"){
        #As well as their associated followups
        followup_col=paste0("pt_",tail(str_split(event_col,"_")[[1]],1))
      } else {
        followup_col=paste0("pt_",tail(str_split(event_col,"_")[[1]],1))
        
      }
      
      monthly_rates=monthly_rates %>% 
        mutate(!!as.symbol(followup_col):=ifelse(is.na(monthly_rates[event_col]),
                                                 NA,
                                                 !!as.symbol(followup_col)))
    }
    
    #Save to disk 
    write.csv(monthly_rates,file=file.path(outfile_monthly_rates,
                                           type,
                                           paste0(filename_from_groupvars(monthly_vars),
                                                  ".csv")))
  }
  # }
}

create_monthly_rates_chronic=function(){
 
  #Create the monthly rates from the chunked-up monthly data
  #The rates are split by the groups in monthly_vars and saved to 
  #outfile_monthly_rates directory 
  
  #Define full cohort data - calculated in chunk loop
  files_fullcohort=list.files(output_file_fullcohort_groups)
  
  #Associate them with their type:
  names(files_fullcohort)=lapply(files_fullcohort,groupvars_from_filename)
  
  type <- "chronic" 
  #Define monthly files - calculated in chunk loop
  monthly_files=list.files(file.path(output_file_monthly,type))
  
  #Associate the groupvariables with the filenames
  names(monthly_files)=lapply(monthly_files,groupvars_from_filename)
  
  #For each monthly grouping variable
  for (monthly_var_group in unique(names(monthly_files))){
    #Get the files that hold monthly data - already calculated in chunk loop
    monthly_files_group=monthly_files[names(monthly_files)==monthly_var_group]
    # monthly_files_group=monthly_files[lapply(monthly_groups,groupvars_from_filename())]
    
    #Execute the string text of the variable name, so we can use the groups
    if(startsWith(monthly_var_group,"c(")){ #Then we are using multiple groups
      monthly_vars=eval(parse(text=monthly_var_group))
    }else{
      monthly_vars=monthly_var_group
    }
    
    #Sum data over chunks
    monthlydat=data.frame()  
    for (file in monthly_files_group){
      monthlydat=rbind(monthlydat,read.csv(file.path(output_file_monthly,type,file)))
    }
    
    #Get the relevant count columns:
    event_cols=colnames(monthlydat)[startsWith(colnames(monthlydat),"events")]
    event_cols_followup=paste0("pt",paste0("_",str_split(event_cols,"_",simplify=TRUE)[,2]))
    
    #Summarize over chunks
    monthlydat= 
      monthlydat %>% 
      dplyr::group_by(.dots=c("yearmonth",monthly_vars)) %>% 
      dplyr::summarize(number_of_deaths=replace_na(sum(number_of_deaths,na.rm=TRUE),0),
                       sum_followup_death=replace_na(sum(sum_followup_death,na.rm=TRUE)/days_in_year,0),
                       !!as.symbol(event_cols[1]):=replace_na(sum(!!as.symbol(event_cols[1]),na.rm=TRUE),0),
                       !!as.symbol(event_cols_followup[1]):=replace_na(sum(!!as.symbol(event_cols_followup[1]),
                                                                           na.rm=TRUE),0),
                       !!as.symbol(event_cols[2]):=replace_na(sum(!!as.symbol(event_cols[2]),na.rm=TRUE),0),
                       !!as.symbol(event_cols_followup[2]):=replace_na(sum(!!as.symbol(event_cols_followup[2]),
                                                                           na.rm=TRUE),0))
    
    #Step 2 - Get the info on the grouped variable from the full cohort
    
    #Define relevant files
    files_fullcohort_group=files_fullcohort[names(files_fullcohort)==monthly_var_group]
    
    #Create fullcohort data - so we know the size of the groups at the start of the study 
    dat_fc=data.frame()  
    for (file in files_fullcohort_group) {
      dat_fc=rbind(dat_fc,read.csv(file.path(output_file_fullcohort_groups,file)))# %>% mutate(chunk=chunk))
    }
    
    #Create the starting data - this is the same as above but summed over the descriptor groups 
    startdat=dat_fc %>% 
      group_by(.dots=monthly_vars) %>% 
      dplyr::summarize(num_people=sum(number_of_individuals))
    
    
    #Step 3 - Use the starting data and the monthly death data to 
    #figure out monthly deaths of individuals, split by monthly_var
    startdat_thismonth=startdat
    monthly_deaths=data.frame()
    for (month in unique(monthlydat$yearmonth)){
      
      dat_thismonth=
        monthlydat %>%
        select(c(yearmonth,all_of(monthly_vars),number_of_deaths,sum_followup_death)) %>% 
        filter(yearmonth==month)%>%
        left_join(startdat_thismonth,by=all_of(monthly_vars)) %>% 
        mutate(individuals_survive=num_people-number_of_deaths,
               sum_followup_nodeath=individuals_survive*days_in_month(yearmonth)/days_in_year,
               sum_followup_total=sum_followup_nodeath+sum_followup_death) %>% 
        rename(individuals_started=num_people) %>% 
        ungroup()
      
      startdat_thismonth= dat_thismonth %>%
        select(individuals_survive,all_of(monthly_vars)) %>% 
        rename(num_people=individuals_survive)
      
      monthly_deaths=rbind(monthly_deaths,dat_thismonth)
    }
    
    #Join with above data
    monthly_rates=
      monthly_deaths %>% 
      left_join(monthlydat %>% 
                  select(-c(number_of_deaths,sum_followup_death)),by=c("yearmonth",all_of(monthly_vars)))
    
    #Remove Scottish and Welsh data if we have it 
    if ("region" %in% monthly_vars){
      monthly_rates=
        monthly_rates %>%
        filter(!region %in% c("Scotland","Wales"))
    }
    
    #Sanitize output for escrow
    monthly_rates$number_of_deaths=sanitize_output(monthly_rates$number_of_deaths)
    monthly_rates$individuals_started=sanitize_output(monthly_rates$individuals_started)
    monthly_rates$individuals_survive=sanitize_output(monthly_rates$individuals_survive)
    
    #Ensure relevant followups are also censored 
    monthly_rates=monthly_rates %>% 
      mutate(sum_followup_death=ifelse(is.na(monthly_rates$number_of_deaths),
                                       NA,
                                       sum_followup_death),
             individuals_started=ifelse(is.na(monthly_rates$number_of_deaths),
                                        NA,
                                        individuals_started),
             individuals_survive=ifelse(is.na(monthly_rates$number_of_deaths),
                                        NA,
                                        individuals_survive)) %>% 
      mutate(sum_followup_nodeath=ifelse(is.na(monthly_rates$number_of_deaths),
                                         NA,
                                         sum_followup_nodeath))
    
    #Ensure the count columns are sanitized 
    for (event_col in event_cols){
      monthly_rates[event_col]=sanitize_output(monthly_rates[event_col][[1]])
      
      if(event_col == "events_htn" | event_col == "events_af"){
        #As well as their associated followups
        followup_col=paste0("pt_",tail(str_split(event_col,"_")[[1]],1))
      } else {
        followup_col=paste0("sum_followup_",tail(str_split(event_col,"_")[[1]],1))
        
      }
      
      monthly_rates=monthly_rates %>% 
        mutate(!!as.symbol(followup_col):=ifelse(is.na(monthly_rates[event_col]),
                                                 NA,
                                                 !!as.symbol(followup_col)))
    }
    
    #Save to disk 
    write.csv(monthly_rates,file=file.path(outfile_monthly_rates,
                                           type,
                                           paste0(filename_from_groupvars(monthly_vars),
                                                  ".csv")))
  }
  # }
}




get_pop_perc_from_age=function(age){
  
  #These are the study age bands
  age_bands=c("18-40","41-50","51-60","61-70","71-80",">81")
  
  #These are the European standard population size bands
  pop_bands=c(29100,14000,13300,11300,8500,4500)
  
  #Express them as percentages:
  pop_bands=pop_bands/sum(pop_bands)*100
  
  #Assign names
  names(pop_bands)=age_bands
  
  if (age %in% age_bands){ #If we have it hard codes
    return(0.01*as.numeric(pop_bands[names(pop_bands)==age][[1]])) #Return it
  }
  
}


create_age_standardised_rates_acute=function(){
  
  #Read in the non-standardized data
  files_counts=list.files(file.path(outfile_monthly_rates,"acute"))
  files_counts=files_counts[startsWith(files_counts,";age_group&")]
  
  #Filter for data that hasn't been run before 
  files_counts=files_counts[!endsWith(files_counts,";age_standardised.csv")]
  
  for (file in files_counts){
    
    #Get grouping variable:
    group_var=str_replace(str_split(file,"&")[[1]][2],";.csv","")
    
    dat=read.csv(file.path(outfile_monthly_rates,"acute",file))
    
    #Get the crude rates:
    dat$rate_str=dat$events_str/dat$sum_followup_total
    dat$rate_ami=dat$events_ami/dat$sum_followup_total
    
    #Get upper and lower bounds
    dat$rate_str_error=1.96*sqrt(dat$events_str/(dat$sum_followup_total^2))
    dat$rate_ami_error=1.96*sqrt(dat$events_ami/(dat$sum_followup_total^2))
    
    #Add in the weightings (1-1 mapping of the age groups to the European percentage)    
    dat$pop_perc=lapply(dat$age_group,get_pop_perc_from_age)
    
    #Apply age standardization by using a weighted sum   
    #The factor of 1000 converts to 1k person years
    age_stan_rates=
      dat %>% 
      group_by(.dots=c("yearmonth",group_var)) %>% 
      summarize(across(c(rate_str,rate_ami), ~ 1000* sum(. * as.numeric(pop_perc),na.rm=T)))
    
    #Get errors
    age_stan_rates_errors=
      dat %>% 
      group_by(.dots=c("yearmonth",group_var)) %>% 
      summarize(error_str=sum(rate_str_error,na.rm=T),
                error_ami=sum(rate_ami_error,na.rm=T))
    
    #Combine
    age_stan_rates=
      age_stan_rates %>% 
      left_join(age_stan_rates_errors) %>% 
      mutate(upper_bound_str=rate_str+abs(error_str),
             lower_bound_str=rate_str-abs(error_str),
             upper_bound_ami=rate_ami+abs(error_ami),
             lower_bound_ami=rate_ami-abs(error_ami))
    
    #Now get mean rates (ie not monthly)
    mean_rates=
      age_stan_rates %>% 
      group_by(.dots=group_var) %>% 
      summarize(rate_str=mean(rate_str),
                rate_ami=mean(rate_ami))
    
    #And the errors for these
    mean_rates_errors=
      dat %>% 
      group_by(.dots=group_var) %>% 
      summarize(error_str=sum(rate_str_error,na.rm=T),
                error_ami=sum(rate_ami_error,na.rm=T))
    
    #Combine these two
    mean_rates=
      mean_rates %>% 
      left_join(mean_rates_errors) %>% 
      mutate(upper_bound_str=rate_str+abs(error_str),
             lower_bound_str=rate_str-abs(error_str),
             upper_bound_ami=rate_ami+abs(error_ami),
             lower_bound_ami=rate_ami-abs(error_ami))
  
    #Save rate datasets
    age_stan_rates %>% 
      write_csv(file=file.path(outfile_monthly_rates,"acute",paste0(group_var,"_age_standardized_doc.csv")))
    
    #Save means
    mean_rates %>% 
      write_csv(file=file.path(outfile_monthly_rates,"acute",paste0(group_var,"_age_standardized_avg_doc.csv")))
    
  }
}


create_age_standardised_rates_chronic=function(){
  
  #Read in the non-standardized data
  files_counts=list.files(file.path(outfile_monthly_rates,"chronic"))
  files_counts=files_counts[startsWith(files_counts,";age_group&")]
  
  #Filter for data that hasn't been run before 
  files_counts=files_counts[!endsWith(files_counts,";age_standardised.csv")]
  
  for (file in files_counts){
    
    #Get grouping variable:
    group_var=str_replace(str_split(file,"&")[[1]][2],";.csv","")
    
    dat=read.csv(file.path(outfile_monthly_rates,"chronic",file))
    
    #Get the crude rates:
    dat$rate_htn=dat$events_htn/dat$sum_followup_total
    dat$rate_af=dat$events_af/dat$sum_followup_total
    
    #Get upper and lower bounds
    dat$rate_htn_error=1.96*sqrt(dat$events_htn/(dat$sum_followup_total^2))
    dat$rate_af_error=1.96*sqrt(dat$events_af/(dat$sum_followup_total^2))
    
    #Add in the weightings (1-1 mapping of the age groups to the European percentage)    
    dat$pop_perc=lapply(dat$age_group,get_pop_perc_from_age)
    
    #Apply age standardization by using a weighted sum   
    #The factor of 1000 converts to 1k person years
    age_stan_rates=
      dat %>% 
      group_by(.dots=c("yearmonth",group_var)) %>% 
      summarize(across(c(rate_htn,rate_af), ~ 1000* sum(. * as.numeric(pop_perc),na.rm=T)))
    
    #Get errors
    age_stan_rates_errors=
      dat %>% 
      group_by(.dots=c("yearmonth",group_var)) %>% 
      summarize(error_htn=sum(rate_htn_error,na.rm=T),
                error_af=sum(rate_af_error,na.rm=T))
    
    #Combine
    age_stan_rates=
      age_stan_rates %>% 
      left_join(age_stan_rates_errors) %>% 
      mutate(upper_bound_htn=rate_htn+abs(error_htn),
             lower_bound_htn=rate_htn-abs(error_htn),
             upper_bound_af=rate_af+abs(error_af),
             lower_bound_af=rate_af-abs(error_af))
    
    #Now get mean rates (ie not monthly)
    mean_rates=
      age_stan_rates %>% 
      group_by(.dots=group_var) %>% 
      summarize(rate_htn=mean(rate_htn),
                rate_af=mean(rate_af))
    
    #And the errors for these
    mean_rates_errors=
      dat %>% 
      group_by(.dots=group_var) %>% 
      summarize(error_htn=sum(rate_htn_error,na.rm=T),
                error_af=sum(rate_af_error,na.rm=T))
    
    #Combine these two
    mean_rates=
      mean_rates %>% 
      left_join(mean_rates_errors) %>% 
      mutate(upper_bound_htn=rate_htn+abs(error_htn),
             lower_bound_htn=rate_htn-abs(error_htn),
             upper_bound_af=rate_af+abs(error_af),
             lower_bound_af=rate_af-abs(error_af))
    
    
    #Save rate datasets
    age_stan_rates %>% 
      write_csv(file=file.path(outfile_monthly_rates,"chronic",paste0(group_var,"_age_standardized_doc.csv")))
    
    #Save means
    mean_rates %>% 
      write_csv(file=file.path(outfile_monthly_rates,"chronic",paste0(group_var,"_age_standardized_avg_doc.csv")))
    
  }
}


reconstitute_age_outcomes_acute=function(){
  
  #Now get the info on the ages
  age_files=list.files(output_file_age_outcomes)
  age_files=age_files[startsWith(age_files,"acute")]
  
  agedat=data.frame()  
  for (file in age_files) {
    agedat=rbind(agedat,read.csv(file.path(output_file_age_outcomes,file)))
  }
  
  
  agedat %>% 
    group_by(age_year) %>% 
    summarize(num_people_ami=sum(event_dt_ami), #This is equivalent to the number of patients because this frame has only one event per patient per age_year
              num_people_str=sum(event_dt_str)) %>% 
    summarize(mean_age_ami=weighted.mean(age_year,num_people_ami),
              sd_age_ami=sqrt(wtd.var(age_year,num_people_ami)),
              median_age_ami=median(rep(age_year,num_people_ami)),
              IQR_ages_ami=paste0(as.character(max(18,median_age_ami-0.5*IQR(rep(age_year,num_people_ami)))),
                                  "-", 
                                  as.character(median_age_ami+0.5*IQR(rep(age_year,num_people_ami)))),
              mean_age_str=weighted.mean(age_year,num_people_str),
              sd_age_str=sqrt(wtd.var(age_year,num_people_str)),
              median_age_str=median(rep(age_year,num_people_str)),
              IQR_ages_str=paste0(as.character(max(18,median_age_str-0.5*IQR(rep(age_year,num_people_str)))),
                                  "-", 
                                  as.character(median_age_str+0.5*IQR(rep(age_year,num_people_str))))) %>% 
    write_csv(file=file.path(outfile_reconstituted_ages,"age_outcomes_acute.csv"))
}


reconstitute_age_outcomes_chron=function(){
  
  #Now get the info on the ages
  age_files=list.files(output_file_age_outcomes)
  age_files=age_files[startsWith(age_files,"chron")]
  
  agedat=data.frame()  
  for (file in age_files) {
    agedat=rbind(agedat,read.csv(file.path(output_file_age_outcomes,file)))
  }
  
  agedat %>% 
    group_by(age_year) %>% 
    summarize(num_people_htn=sum(num_people_htn), 
              num_people_af=sum(num_people_af)) %>% 
    summarize(mean_age_htn=weighted.mean(age_year,num_people_htn),
              sd_age_htn=sqrt(wtd.var(age_year,num_people_htn)),
              median_age_htn=median(rep(age_year,num_people_htn)),
              quant=quantile(rep(age_year,num_people_htn))[2][[1]],
              IQR_2=paste0(as.character(quantile(rep(age_year,num_people_htn))[2][[1]]),
                           "-",
                           as.character(quantile(rep(age_year,num_people_htn))[4][[1]])),
              IQR_ages_htn=paste0(as.character(max(18,median_age_htn-0.5*IQR(rep(age_year,num_people_htn)))),
                                  "-", 
                                  as.character(median_age_htn+0.5*IQR(rep(age_year,num_people_htn)))),
              mean_age_af=weighted.mean(age_year,num_people_af),
              sd_age_af=sqrt(wtd.var(age_year,num_people_af)),
              median_age_af=median(rep(age_year,num_people_af)),
              IQR_ages_af=paste0(as.character(max(18,median_age_af-0.5*IQR(rep(age_year,num_people_af)))),
                                 "-", 
                                 as.character(median_age_af+0.5*IQR(rep(age_year,num_people_af)))))%>% 
    write_csv(file=file.path(outfile_reconstituted_ages,"age_outcomes_chron.csv"))
}


create_monthly_rates_nostrat=function(){
  
  #Read in the non-standardized data
  file=file.path(outfile_monthly_rates,"chronic",";age_group;.csv")
  dat=read.csv(file)
  
  #Combine over ages to get overall rates:
  dat_comb=dat %>% 
    group_by(yearmonth) %>% 
    summarize(events_htn=sum(events_htn),
              events_af=sum(events_af),
              pt_htn=sum(pt_htn),
              rate_htn=events_htn/pt_htn*1000,
              pt_af=sum(pt_af),
              rate_af=events_af/pt_af*1000)
  
  #Save:
  dat_comb %>% 
    write_csv("./outputs/data/last/chronic_rates.csv")
  
  
  #Get age standardized rates
  
  #Add in the weightings (1-1 mapping of the age groups to the European percentage)    
  dat$pop_perc=lapply(dat$age_group,get_pop_perc_from_age)
  
  #Apply age standardization by using a weighted sum   
  #The factor of 1000 converts to 1k person years
  age_stan_rates=
    dat %>% 
    mutate(rate_htn=events_htn/pt_htn*1000,
           rate_af=events_af/pt_af*1000) %>% 
    group_by(yearmonth) %>% 
    summarize(across(c(rate_htn,rate_af), ~ sum(. * as.numeric(pop_perc),na.rm=T)))
  
  #Save these to disk:
  age_stan_rates %>% 
    write_csv("./outputs/data/last/chronic_rates_age_standardized.csv")
  
  #Repeat for acute:
  
  #Read in the non-standardized data
  file=file.path(outfile_monthly_rates,"acute",";age_group;.csv")
  dat=read.csv(file)
  
  #Combine over ages to get overall rates:
  dat_comb=dat %>% 
    group_by(yearmonth) %>% 
    summarize(events_ami=sum(events_ami),
              events_str=sum(events_str),
              pt_ami=sum(sum_followup_total),
              rate_ami=events_ami/pt_ami*1000,
              pt_str=sum(sum_followup_total),
              rate_str=events_str/pt_str*1000)
  
  
  #Save:
  dat_comb %>% 
    write_csv("./outputs/data/last/acute_rates.csv")
  
  #Get age standardized rates
  
  #Add in the weightings (1-1 mapping of the age groups to the European percentage)    
  dat$pop_perc=lapply(dat$age_group,get_pop_perc_from_age)
  
  #Apply age standardization by using a weighted sum   
  #The factor of 1000 converts to 1k person years
  age_stan_rates=
    dat %>% 
    mutate(rate_ami=events_ami/sum_followup_total*1000,
           rate_str=events_str/sum_followup_total*1000) %>% 
    group_by(yearmonth) %>% 
    summarize(across(c(rate_ami,rate_str), ~ sum(. * as.numeric(pop_perc),na.rm=T)))
  
  #Save these to disk:
  age_stan_rates %>% 
    write_csv("./outputs/data/last/acute_rates_age_standardized.csv")

}


create_table_one=function(dat,dat_acute,chunk_num){

  groups=c("age_group",
           "SEX",
           "ETHNIC_CAT",
           "IMD_2019_DECILES",
           "cov_hx_com_diabetes_flag", 
           "cov_hx_com_hypercholesterolaemia_flag",
           "cov_hx_com_hf_flag",
           "cov_hx_com_pad_flag",
           "cov_hx_out_ami_flag",
           "cov_hx_out_stroke_flag",
           "cci_index")
  
  dat_out=data.frame()
  
  for (grouping_var in groups){
    
    dat_this=    
      dat %>% 
      group_by(.dots=grouping_var) %>% 
      summarize(overall_cohort=length(unique(PERSON_ID)),
                overall_cohort_perc=round(overall_cohort/sum(overall_cohort)*100,2),
                inc_htn=length(unique(PERSON_ID[!is.na(event_dt_htn)])),
                inc_af=length(unique(PERSON_ID[!is.na(event_dt_af)]))) %>% 
      select(!!as.symbol(grouping_var),overall_cohort,inc_htn,inc_af)
    
    #Repeat for acute data   
    dat_this_acute=    
      dat_acute %>% 
      group_by(.dots=grouping_var) %>% 
      summarize(inc_ami=length(unique(PERSON_ID[!is.na(event_dt_ami)])),
                inc_ami_events=length(unique(event_dt_ami[!is.na(event_dt_ami)])),
                inc_str=length(unique(PERSON_ID[!is.na(event_dt_str)])),
                inc_str_events=length(unique(event_dt_str[!is.na(event_dt_str)]))) %>% 
      select(!!as.symbol(grouping_var),inc_ami,inc_str)
    # 
    dat_=dat_this %>% 
      left_join(dat_this_acute,by=grouping_var) 
    
    #Save this data:
    dat_ %>% 
      mutate(chunk=chunk_num) %>% 
      write_csv(file=paste0("./outputs/data/last/table1chunk_",chunk_num,"_",grouping_var,".csv"))
     
     
  }
}


constitute_table_1=function(){
  
  
  groups=c("age_group",
           "SEX",
           "ETHNIC_CAT",
           "IMD_2019_DECILES",
           "cov_hx_com_diabetes_flag", 
           "cov_hx_com_hypercholesterolaemia_flag",
           "cov_hx_com_hf_flag",
           "cov_hx_com_pad_flag",
           "cov_hx_out_ami_flag",
           "cov_hx_out_stroke_flag",
           "cci_index")
  
  dat_out=data.frame()
  
  directory="./outputs/data/last/"
  
  files=list.files(path=directory) 
  
  
  for (grouping_var in groups){  
    
    #Get only relevant data
    group_files=files[endsWith(files,paste0(grouping_var,".csv"))]
    
    dat_comb=data.frame()
    for (file in group_files){
      dat_=read.csv(file.path(directory,file))
      dat_comb=rbind(dat_comb,dat_)
    }
     
    dat_out_=
      dat_comb %>% 
      group_by(!!as.symbol(grouping_var)) %>% 
      summarize(overall_cohort=sanitize_output(sum(overall_cohort)), 
                inc_htn=sanitize_output(sum(inc_htn)),
                inc_af=sanitize_output(sum(inc_af)),
                inc_ami=sanitize_output(sum(inc_ami)),
                inc_str=sanitize_output(sum(inc_str))) %>% 
      mutate(descriptor=paste0(grouping_var,": ",!!as.symbol(grouping_var))) %>% 
      # rename(descriptor=!!as.symbol(grouping_var)) %>%
      ungroup() %>% 
      #Add percentages and string formatting
      reframe(descriptor,
              
              overall_cohort_perc=round(overall_cohort/sum(overall_cohort)*100,2),
              str_perc=round(inc_str/sum(inc_str)*100,2),
              ami_perc=round(inc_ami/sum(inc_ami)*100,2),
              htn_perc=round(inc_htn/sum(inc_htn)*100,2),
              af_perc= round(inc_af/sum(inc_af)  *100,2),
              
              overall_cohort=paste0(overall_cohort," (", overall_cohort_perc,"%)"),
              
              inc_ami=paste0(inc_ami," (", ami_perc,"%)"),
              inc_str=paste0(inc_str," (", str_perc,"%)"),
              
              inc_htn=paste0(inc_htn," (", htn_perc,"%)"),
              inc_af=paste0(inc_af," (", af_perc,"%)")) %>% 
      select(descriptor,
             overall_cohort, 
             inc_htn,
             inc_af,
             inc_ami,
             inc_str)
    
    dat_out=rbind(dat_out,dat_out_)
    
  }
  
  #Save this:
  dat_out %>% 
    write_csv(file="./outputs/data/table1/table1.csv")
  
}


create_supplementary_tables=function(dat_acute,chunk_num){
  
  #This is for table S1 and S2 only
  
  for (grouping_var in c(sup_vars)){
    
    #AMI
    sup_ami=
    dat_acute %>% 
      filter(!is.na(event_dt_ami)) %>% 
      mutate(yearmonth=ymd(paste0(as.character(year(ymd(event_dt_ami))),"-",as.character(month(ymd(event_dt_ami))),"-01"))) %>% 
      dplyr::group_by(.dots=c("yearmonth",grouping_var)) %>%
      dplyr::summarize(individuals_ami=n()) %>% 
      ungroup() %>% 
      group_by(yearmonth) %>%
      reframe(descriptor=!!as.symbol(grouping_var),
              individuals_ami=paste0(individuals_ami," (",100*round(individuals_ami/sum(individuals_ami),4),"%)")) 
    #Stroke
    sup_str=
      dat_acute %>% 
      filter(!is.na(event_dt_str)) %>% 
      mutate(yearmonth=ymd(paste0(as.character(year(ymd(event_dt_str))),"-",as.character(month(ymd(event_dt_str))),"-01"))) %>% 
      dplyr::group_by(.dots=c("yearmonth",grouping_var)) %>%
      dplyr::summarize(individuals_str=n()) %>% 
      ungroup() %>% 
      group_by(yearmonth) %>%
      reframe(descriptor=!!as.symbol(grouping_var),
              individuals_str=paste0(individuals_str," (",100*round(individuals_str/sum(individuals_str),4),"%)")) 
    
    
    #Combine and save
    left_join(sup_ami,sup_str,by=join_by(yearmonth,descriptor)) %>% 
    write_csv(file=file.path(output_file_supplement,
                             paste0(filename_from_groupvars(grouping_var),
                                    "_chunk",
                                    first(chunk_num),
                                    ".csv")))
  } 
}


constitute_supplement=function(){
  
  directory="./outputs/data/supplement/"
  
  files=list.files(path=directory) 
  

  for (grouping_var in sup_vars) {
    
    group_files=files[startsWith(files,paste0(filename_from_groupvars(grouping_var)))]
    
    dat_comb=data.frame()
    for (file in group_files){
      dat_=read.csv(file.path(directory,file))
      dat_comb=rbind(dat_comb,dat_)
    }
    
  #Cut off the percentage, group, sum & save
      dat_comb %>% 
      mutate(individuals_ami=gsub("\\(.*\\)","",individuals_ami),
             individuals_str=gsub("\\(.*\\)","",individuals_str)) %>% 
      group_by(yearmonth,descriptor) %>% 
      summarize(individuals_ami=sanitize_output(sum(as.numeric(individuals_ami))),
                individuals_str=sanitize_output(sum(as.numeric(individuals_str)))) %>% 
      ungroup() %>% 
      group_by(yearmonth) %>%
      reframe(descriptor=descriptor,
              individuals_str=paste0(individuals_str," (",100*round(individuals_str/sum(individuals_str),4),"%)"),
              individuals_ami=paste0(individuals_ami," (",100*round(individuals_ami/sum(individuals_ami),4),"%)")) %>% 
        write_csv(file=file.path(directory,paste0(grouping_var,"_full.csv")))
  }
}
