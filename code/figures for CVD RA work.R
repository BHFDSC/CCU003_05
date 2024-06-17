library(tidyverse)
library(readxl)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(svglite)

#########################
#chronic Age
#########################
data <- read_excel("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Excel/Master file figures and tables updated.xlsx", sheet = "6.1.1 Chronic-age")

data <- data[1:228,1:14]

data <- data %>%
  mutate(yearmonth = as.Date(as.numeric(yearmonth)-2, origin = "1900-01-01"),
         events_htn = as.numeric(events_htn),
         pt_htn = as.numeric(pt_htn),
         events_af = as.numeric(events_af),
         pt_af = as.numeric(pt_af),
         AF_incid = as.numeric(AF_incid),
         `Htn incid` = as.numeric(`Htn incid`)) %>%
  select(yearmonth, age_group, events_htn, pt_htn, events_af, pt_af, AF_incid, `Htn incid`)

data <- data %>%
  mutate(sumprod_htn = `Htn incid`*pt_htn,
         sumprod_af = AF_incid*pt_af) %>%
  group_by(age_group) %>%
  arrange(age_group, yearmonth) %>%
  mutate(precovid_htn = sum(sumprod_htn[yearmonth <= as.Date("2020-02-01")]) / sum(pt_htn[yearmonth <= as.Date("2020-02-01")]),
         precovid_af = sum(sumprod_af[yearmonth <= as.Date("2020-02-01")]) / sum(pt_af[yearmonth <= as.Date("2020-02-01")]),
         moving_average_htn = ifelse( yearmonth %m-% months(1) %in% data$yearmonth & yearmonth %m+% months(1) %in% data$yearmonth, 
                                         (sumprod_htn + lag(sumprod_htn) + lead(sumprod_htn)) / 
                                           (pt_htn + lag(pt_htn) + lead(pt_htn)),
                                         NA),
         moving_average_af = ifelse( yearmonth %m-% months(1) %in% data$yearmonth & yearmonth %m+% months(1) %in% data$yearmonth, 
                                      (sumprod_af + lag(sumprod_af) + lead(sumprod_af)) / 
                                        (pt_af + lag(pt_af) + lead(pt_af)),
                                      NA),
         perc_diff_htn = ((moving_average_htn / precovid_htn)-1)*100,
         perc_diff_af = ((moving_average_af / precovid_af)-1)*100,
         events_diff_htn = round((`Htn incid` - precovid_htn) / 1000 * pt_htn),
         events_diff_af = round((`AF_incid` - precovid_af) / 1000 * pt_af)) %>%
  select(yearmonth, age_group, AF_incid, `Htn incid`, perc_diff_htn, perc_diff_af, events_diff_htn, events_diff_af) %>%
  rename(incidence_af = AF_incid,
         incidence_Htn = `Htn incid`)

write_csv(data,paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/results_by_age_group.csv"))

palette = brewer.pal(length(unique(data$age_group)), 'RdBu')

for (var in c("htn", "af")) {
  plot1 <- data %>%
    ggplot(aes (x = yearmonth, y = !!as.symbol(paste0("perc_diff_", var)), colour = factor(age_group, levels = c("18-40", "41-50", "51-60", "61-70", "71-80", ">81")))) +
    geom_hline (yintercept = 0, colour = "dark grey", size = 1.25, linetype = "dashed" )+
    geom_line( size = 1.25) +
    scale_colour_manual(values = palette) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, limits = c(as.Date("2019-12-01"),as.Date("2022-11-01")))+
    labs(y ="3 month rolling percentage change in crude rate", x = element_blank(),  colour = "Age group")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot1
  ggsave(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/svg/",var,"_percentage_change_age_group.svg"), width = 3000, height = 2000, units = "px")
  
  jpeg(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/jpeg/",var,"_percentage_change_age_group.jpeg"), width = 3000, height = 2000, res = 300)
  print(plot1)
  dev.off()
  
  i <- 1
  for (group in c("18-40", "41-50", "51-60", "61-70", "71-80", ">81")){
    temp <- data %>% filter(age_group == group)
    plot2 <- temp %>%
      ggplot(aes(x = yearmonth, y = !!as.symbol(paste0("events_diff_", var)))) +
      geom_col(fill = palette[i]) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, expand = c(0, 0))+
      labs(y = element_blank(), x = element_blank()) +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    plot2
    ggsave(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/svg/",var,"_absolute_difference_age_group_", i, ".svg"), width = 3000, height = 3000/length(unique(data$age_group)), units = "px")
    
    jpeg(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/jpeg/",var,"_absolute_difference_age_group_", i, ".jpeg"), width = 3000, height = 3000/length(unique(data$age_group)), res = 300)
    print(plot2)
    dev.off()
    
    i <- i+1
  }
}


##########################################
#Acute Age
#######################################

data <- read_excel("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Excel/Master file figures and tables updated.xlsx", sheet = "5.1.1 Acute-age")

data <- data[1:228,1:14]

data <- data %>%
  mutate(yearmonth = as.Date(as.numeric(yearmonth)-2, origin = "1900-01-01"),
         events_stroke = as.numeric(events_stroke),
         events_ami = as.numeric(events_ami),
         sum_followup_total = as.numeric(sum_followup_total),
         `incid ami`= as.numeric(`incid mi`),
         `incid stroke` = as.numeric(`incid stroke`)) %>%
  select(yearmonth, age_group,  events_stroke, events_ami, sum_followup_total, `incid ami`, `incid stroke`)

data <- data %>%
  mutate(sumprod_stroke = `incid stroke`* sum_followup_total,
         sumprod_ami = `incid ami`* sum_followup_total) %>%
  group_by(age_group) %>%
  arrange(age_group, yearmonth) %>%
  mutate(precovid_stroke = sum(sumprod_stroke [yearmonth <= as.Date("2020-02-01")]) / sum(sum_followup_total[yearmonth <= as.Date("2020-02-01")]),
         precovid_ami = sum(sumprod_ami[yearmonth <= as.Date("2020-02-01")]) / sum(sum_followup_total[yearmonth <= as.Date("2020-02-01")]),
         moving_average_stroke = ifelse( yearmonth %m-% months(1) %in% data$yearmonth & yearmonth %m+% months(1) %in% data$yearmonth, 
                                      (sumprod_stroke + lag(sumprod_stroke) + lead(sumprod_stroke)) / 
                                        (sum_followup_total + lag(sum_followup_total) + lead(sum_followup_total)),
                                      NA),
         moving_average_ami = ifelse( yearmonth %m-% months(1) %in% data$yearmonth & yearmonth %m+% months(1) %in% data$yearmonth, 
                                     (sumprod_ami + lag(sumprod_ami) + lead(sumprod_ami)) / 
                                       (sum_followup_total + lag(sum_followup_total) + lead(sum_followup_total)),
                                     NA),
         perc_diff_stroke = ((moving_average_stroke / precovid_stroke)-1)*100,
         perc_diff_ami = ((moving_average_ami / precovid_ami)-1)*100,
         events_diff_stroke = round((`incid stroke` - precovid_stroke) / 1000 * sum_followup_total),
         events_diff_ami = round((`incid ami` - precovid_ami) / 1000 * sum_followup_total)) %>%
  select(yearmonth, age_group,`incid ami`, `incid stroke` , perc_diff_stroke, perc_diff_ami, events_diff_stroke, events_diff_ami) %>%
  rename(incidence_ami = `incid ami`,
         incidence_Htn = `incid stroke`)

write_csv(data,paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/results_by_age_group.csv"))

palette = brewer.pal(length(unique(data$age_group)), 'RdBu')

for (var in c("stroke", "ami")) {
  plot1 <- data %>%
    ggplot(aes (x = yearmonth, y = !!as.symbol(paste0("perc_diff_", var)), colour = factor(age_group, levels = c("18-40", "41-50", "51-60", "61-70", "71-80", ">81")))) +
    geom_hline (yintercept = 0, colour = "dark grey", size = 1.25, linetype = "dashed" )+
    geom_line( size = 1.25) +
    scale_colour_manual(values = palette) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, limits = c(as.Date("2019-12-01"),as.Date("2022-11-01")))+
    labs(y ="3 month rolling percentage change in crude rate", x = element_blank(),  colour = "Age group")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot1
  ggsave(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/svg/",var,"_percentage_change_age_group.svg"), width = 3000, height = 2000, units = "px")
  jpeg(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/jpeg/",var,"_percentage_change_age_group.jpeg"), width = 3000, height = 2000, res = 300)
  print(plot1)
  dev.off()
  
  i <- 1
  for (group in c("18-40", "41-50", "51-60", "61-70", "71-80", ">81")){
    temp <- data %>% filter(age_group == group)
    plot2 <- temp %>%
      ggplot(aes(x = yearmonth, y = !!as.symbol(paste0("events_diff_", var)))) +
      geom_col(fill = palette[i]) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, expand = c(0,0))+
      labs(y = element_blank(), x = element_blank()) +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    plot2
    ggsave(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/svg/",var,"_absolute_difference_age_group_", i, ".svg"), width = 3000, height = 3000/length(unique(data$age_group)), units = "px")
    
    jpeg(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/jpeg/",var,"_absolute_difference_age_group_", i, ".jpeg"), width = 3000, height = 3000/length(unique(data$age_group)), res = 300)
    print(plot2)
    dev.off()
    
    i <- i+1
  }
}


#########################
#chronic Sex
########################

data <- read_excel("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Excel/Master file figures and tables updated.xlsx", sheet = "6.2.1 Chronic-sex")

data <- data[1:76,1:16]

data <- data %>%
  mutate(yearmonth = as.Date(as.numeric(yearmonth)-2, origin = "1900-01-01"),
         events_htn = as.numeric(events_htn),
         pt_htn = as.numeric(pt_htn),
         events_af = as.numeric(events_af),
         pt_af = as.numeric(pt_af),
         `AF_incid` = as.numeric(`Incid af`),
         `Htn incid` = as.numeric(`Incid htn`),
         SEX = ifelse(SEX == 1, "Male", "Female"),
         AS_rate_htn = as.numeric(AS_rate_htn),
         AS_rate_af = as.numeric(AS_rate_af)) %>%
  select(yearmonth, SEX, events_htn, pt_htn, events_af, pt_af, AF_incid, `Htn incid`, AS_rate_htn, AS_rate_af)

data <- data %>%
  mutate(sumprod_htn = `Htn incid`*pt_htn,
         sumprod_af = AF_incid*pt_af,
         sumprod_htn_as = AS_rate_htn*pt_htn,
         sumprod_af_as = AS_rate_af*pt_af) %>%
  group_by(SEX) %>%
  arrange(SEX, yearmonth) %>%
  mutate(precovid_htn = sum(sumprod_htn[yearmonth <= as.Date("2020-02-01")]) / sum(pt_htn[yearmonth <= as.Date("2020-02-01")]),
         precovid_af = sum(sumprod_af[yearmonth <= as.Date("2020-02-01")]) / sum(pt_af[yearmonth <= as.Date("2020-02-01")]),
         precovid_htn_as = sum(sumprod_htn_as[yearmonth <= as.Date("2020-02-01")]) / sum(pt_htn[yearmonth <= as.Date("2020-02-01")]),
         precovid_af_as = sum(sumprod_af_as[yearmonth <= as.Date("2020-02-01")]) / sum(pt_af[yearmonth <= as.Date("2020-02-01")]),
         moving_average_htn = ifelse( yearmonth %m-% months(1) %in% data$yearmonth & yearmonth %m+% months(1) %in% data$yearmonth, 
                                      (sumprod_htn_as + lag(sumprod_htn_as) + lead(sumprod_htn_as)) / 
                                        (pt_htn + lag(pt_htn) + lead(pt_htn)),
                                      NA),
         moving_average_af = ifelse( yearmonth %m-% months(1) %in% data$yearmonth & yearmonth %m+% months(1) %in% data$yearmonth, 
                                     (sumprod_af_as + lag(sumprod_af_as) + lead(sumprod_af_as)) / 
                                       (pt_af + lag(pt_af) + lead(pt_af)),
                                     NA),
         perc_diff_htn = ((moving_average_htn / precovid_htn_as)-1)*100,
         perc_diff_af = ((moving_average_af / precovid_af_as)-1)*100,
         events_diff_htn = round((`Htn incid` - precovid_htn) / 1000 * pt_htn),
         events_diff_af = round((`AF_incid` - precovid_af) / 1000 * pt_af)) %>%
  select(yearmonth, SEX, AF_incid, `Htn incid`, perc_diff_htn, perc_diff_af, events_diff_htn, events_diff_af, AS_rate_htn, AS_rate_af) %>%
  rename(incidence_af = AF_incid,
         incidence_Htn = `Htn incid`)

palette = brewer.pal(6, 'RdBu')
palette = palette[c(1,6)]
write_csv(data,paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/results_by_sex.csv"))

for (var in c("htn", "af")) {
  plot1 <- data %>%
    ggplot(aes (x = yearmonth, y = !!as.symbol(paste0("perc_diff_", var)), colour = factor(SEX, levels = c("Female", "Male")))) +
    geom_hline (yintercept = 0, colour = "dark grey", size = 1.25, linetype = "dashed" )+
    geom_line( size = 1.25) +
    scale_colour_manual(values = palette) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, limits = c(as.Date("2019-12-01"),as.Date("2022-11-01")))+
    labs(y ="3 month rolling percentage change in age standardised rate", x = element_blank(),  colour = "Sex")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot1
  ggsave(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/svg/",var,"_percentage_change_sex.svg"), width = 3000, height = 2000, units = "px")
  
  jpeg(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/jpeg/",var,"_percentage_change_sex.jpeg"), width = 3000, height = 2000, res = 300)
  print(plot1)
  dev.off()
  
  i <- 1
  for (group in c("Male", "Female")){
    temp <- data %>% filter(SEX == group)
    plot2 <- temp %>%
      ggplot(aes(x = yearmonth, y = !!as.symbol(paste0("events_diff_", var)))) +
      geom_col(fill = palette[i]) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, expand = c(0, 0))+
      labs(y = element_blank(), x = element_blank()) +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    plot2
    ggsave(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/svg/",var,"_absolute_difference_sex_", i, ".svg"), width = 3000, height = 3000/length(unique(data$SEX)), units = "px")
    
    jpeg(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/jpeg/",var,"_absolute_difference_sex_", i, ".jpeg"), width = 3000, height = 3000/length(unique(data$SEX)), res = 300)
    print(plot2)
    dev.off()
    
    i <- i+1
  }
}



###########################
#Acute sex
###########################
data <- read_excel("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Excel/Master file figures and tables updated.xlsx", sheet = "5.2.1 Acute-sex")

data <- data[1:76,1:16]

data <- data %>%
  mutate(yearmonth = as.Date(as.numeric(yearmonth)-2, origin = "1900-01-01"),
         events_stroke = as.numeric(events_stroke),
         events_ami = as.numeric(events_ami),
         sum_followup_total = as.numeric(sum_followup_total),
         `incid ami`= as.numeric(`incid mi`),
         `incid stroke` = as.numeric(`incid stroke`),
         SEX = ifelse(SEX == 1, "Male", "Female"),
         AS_rate_stroke = as.numeric(AS_rate_stroke),
         AS_rate_ami = as.numeric(AS_rate_ami)) %>%
  select(yearmonth, SEX,  events_stroke, events_ami, sum_followup_total, `incid ami`, `incid stroke`, AS_rate_stroke, AS_rate_ami)

data <- data %>%
  mutate(sumprod_stroke = `incid stroke`* sum_followup_total,
         sumprod_ami = `incid ami`* sum_followup_total,
         sumprod_stroke_as = AS_rate_stroke*sum_followup_total,
         sumprod_ami_as = AS_rate_ami*sum_followup_total) %>%
  group_by(SEX) %>%
  arrange(SEX, yearmonth) %>%
  mutate(precovid_stroke = sum(sumprod_stroke [yearmonth <= as.Date("2020-02-01")]) / sum(sum_followup_total[yearmonth <= as.Date("2020-02-01")]),
         precovid_ami = sum(sumprod_ami[yearmonth <= as.Date("2020-02-01")]) / sum(sum_followup_total[yearmonth <= as.Date("2020-02-01")]),
         precovid_stroke_as = sum(sumprod_stroke_as[yearmonth <= as.Date("2020-02-01")]) / sum(sum_followup_total[yearmonth <= as.Date("2020-02-01")]),
         precovid_ami_as = sum(sumprod_ami_as[yearmonth <= as.Date("2020-02-01")]) / sum(sum_followup_total[yearmonth <= as.Date("2020-02-01")]),
         moving_average_stroke = ifelse( yearmonth %m-% months(1) %in% data$yearmonth & yearmonth %m+% months(1) %in% data$yearmonth, 
                                         (sumprod_stroke_as + lag(sumprod_stroke_as) + lead(sumprod_stroke_as)) / 
                                           (sum_followup_total + lag(sum_followup_total) + lead(sum_followup_total)),
                                         NA),
         moving_average_ami = ifelse( yearmonth %m-% months(1) %in% data$yearmonth & yearmonth %m+% months(1) %in% data$yearmonth, 
                                      (sumprod_ami_as + lag(sumprod_ami_as) + lead(sumprod_ami_as)) / 
                                        (sum_followup_total + lag(sum_followup_total) + lead(sum_followup_total)),
                                      NA),
         perc_diff_stroke = ((moving_average_stroke / precovid_stroke_as)-1)*100,
         perc_diff_ami = ((moving_average_ami / precovid_ami_as)-1)*100,
         events_diff_stroke = round((`incid stroke` - precovid_stroke) / 1000 * sum_followup_total),
         events_diff_ami = round((`incid ami` - precovid_ami) / 1000 * sum_followup_total)) %>%
  select(yearmonth, SEX,`incid ami`, `incid stroke` , perc_diff_stroke, perc_diff_ami, events_diff_stroke, events_diff_ami) %>%
  rename(incidence_ami = `incid ami`,
         incidence_Htn = `incid stroke`)

palette = brewer.pal(6, 'RdBu')
palette = palette[c(1,6)]
write_csv(data,paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/results_by_sex.csv"))

for (var in c("stroke", "ami")) {
  plot1 <- data %>%
    ggplot(aes (x = yearmonth, y = !!as.symbol(paste0("perc_diff_", var)), colour = factor(SEX, levels = c("Female", "Male")))) +
    geom_hline (yintercept = 0, colour = "dark grey", size = 1.25, linetype = "dashed" )+
    geom_line( size = 1.25) +
    scale_colour_manual(values = palette) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, limits = c(as.Date("2019-12-01"),as.Date("2022-11-01")))+
    labs(y ="3 month rolling percentage change in age standardised rate", x = element_blank(),  colour = "Sex")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot1
  ggsave(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/svg/",var,"_percentage_change_sex.svg"), width = 3000, height = 2000, units = "px")
  jpeg(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/jpeg/",var,"_percentage_change_sex.jpeg"), width = 3000, height = 2000, res = 300)
  print(plot1)
  dev.off()
  
  i <- 1
  for (group in c("Male", "Female")){
    temp <- data %>% filter(SEX == group)
    plot2 <- temp %>%
      ggplot(aes(x = yearmonth, y = !!as.symbol(paste0("events_diff_", var)))) +
      geom_col(fill = palette[i]) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, expand = c(0, 0))+
      labs(y = element_blank(), x = element_blank()) +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    plot2
    ggsave(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/svg/",var,"_absolute_difference_sex_", i, ".svg"), width = 3000, height = 3000/length(unique(data$SEX)), units = "px")
    
    jpeg(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/jpeg/",var,"_absolute_difference_sex_", i, ".jpeg"), width = 3000, height = 3000/length(unique(data$SEX)), res = 300)
    print(plot2)
    dev.off()
    
    i <- i+1
  }
}


#########################
#chronic Ethnicity
########################

data <- read_excel("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Excel/Master file figures and tables updated.xlsx", sheet = "6.3.1 Chronic-ethnicity")

data <- data[1:266,1:16]

data <- data %>%
  mutate(yearmonth = as.Date(as.numeric(yearmonth)-2, origin = "1900-01-01"),
         events_htn = as.numeric(events_htn),
         pt_htn = as.numeric(pt_htn),
         events_af = as.numeric(events_af),
         pt_af = as.numeric(pt_af),
         `AF_incid` = as.numeric(`Incid AF`),
         `Htn incid` = as.numeric(`Incid HTN`),
         AS_rate_htn = as.numeric(AS_rate_htn),
         AS_rate_af = as.numeric(AS_rate_af)) %>%
  select(yearmonth, ETHNIC_CAT, events_htn, pt_htn, events_af, pt_af, AF_incid, `Htn incid`, AS_rate_htn, AS_rate_af) %>%
  filter(!ETHNIC_CAT %in% c('NA', 'Unknown'))

data <- data %>%
  mutate(sumprod_htn = `Htn incid`*pt_htn,
         sumprod_af = AF_incid*pt_af,
         sumprod_htn_as = AS_rate_htn*pt_htn,
         sumprod_af_as = AS_rate_af*pt_af) %>%
  group_by(ETHNIC_CAT) %>%
  arrange(ETHNIC_CAT, yearmonth) %>%
  mutate(precovid_htn = sum(sumprod_htn[yearmonth <= as.Date("2020-02-01")]) / sum(pt_htn[yearmonth <= as.Date("2020-02-01")]),
         precovid_af = sum(sumprod_af[yearmonth <= as.Date("2020-02-01")]) / sum(pt_af[yearmonth <= as.Date("2020-02-01")]),
         precovid_htn_as = sum(sumprod_htn_as[yearmonth <= as.Date("2020-02-01")]) / sum(pt_htn[yearmonth <= as.Date("2020-02-01")]),
         precovid_af_as = sum(sumprod_af_as[yearmonth <= as.Date("2020-02-01")]) / sum(pt_af[yearmonth <= as.Date("2020-02-01")]),
         moving_average_htn = ifelse( yearmonth %m-% months(1) %in% data$yearmonth & yearmonth %m+% months(1) %in% data$yearmonth, 
                                      (sumprod_htn_as + lag(sumprod_htn_as) + lead(sumprod_htn_as)) / 
                                        (pt_htn + lag(pt_htn) + lead(pt_htn)),
                                      NA),
         moving_average_af = ifelse( yearmonth %m-% months(1) %in% data$yearmonth & yearmonth %m+% months(1) %in% data$yearmonth, 
                                     (sumprod_af_as + lag(sumprod_af_as) + lead(sumprod_af_as)) / 
                                       (pt_af + lag(pt_af) + lead(pt_af)),
                                     NA),
         perc_diff_htn = ((moving_average_htn / precovid_htn_as)-1)*100,
         perc_diff_af = ((moving_average_af / precovid_af_as)-1)*100,
         events_diff_htn = round((`Htn incid` - precovid_htn) / 1000 * pt_htn),
         events_diff_af = round((`AF_incid` - precovid_af) / 1000 * pt_af)) %>%
  select(yearmonth, ETHNIC_CAT, AF_incid, `Htn incid`, perc_diff_htn, perc_diff_af, events_diff_htn, events_diff_af, AS_rate_htn, AS_rate_af) %>%
  rename(incidence_af = AF_incid,
         incidence_Htn = `Htn incid`)

palette = brewer.pal(6, 'RdBu')
palette = palette[-4]
write_csv(data,paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/results_by_ethnicity.csv"))

for (var in c("htn", "af")) {
  plot1 <- data %>%
    ggplot(aes (x = yearmonth, y = !!as.symbol(paste0("perc_diff_", var)), colour = factor(ETHNIC_CAT, levels = c("Asian or Asian British", "Black or Black British", "Mixed","Other" ,"White")))) +
    geom_hline (yintercept = 0, colour = "dark grey", size = 1.25, linetype = "dashed" )+
    geom_line( size = 1.25) +
    scale_colour_manual(values = palette) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, limits = c(as.Date("2019-12-01"),as.Date("2022-11-01")))+
    labs(y ="3 month rolling percentage change in age standardised rate", x = element_blank(),  colour = "Ethnicity")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot1
  ggsave(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/svg/",var,"_percentage_change_ethnicity.svg"), width = 3000, height = 2000, units = "px")
  
  jpeg(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/jpeg/",var,"_percentage_change_ethnicity.jpeg"), width = 3000, height = 2000, res = 300)
  print(plot1)
  dev.off()
  
  i <- 1
  for (group in c("Asian or Asian British", "Black or Black British", "Mixed","Other" ,"White")){
    temp <- data %>% filter(ETHNIC_CAT == group)
    plot2 <- temp %>%
      ggplot(aes(x = yearmonth, y = !!as.symbol(paste0("events_diff_", var)))) +
      geom_col(fill = palette[i]) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, expand = c(0, 0))+
      labs(y = element_blank(), x = element_blank()) +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    plot2
    ggsave(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/svg/",var,"_absolute_difference_ethnicity_", i, ".svg"), width = 3000, height = 3000/length(unique(data$ETHNIC_CAT)), units = "px")
    
    jpeg(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/jpeg/",var,"_absolute_difference_ethnicity_", i, ".jpeg"), width = 3000, height = 3000/length(unique(data$ETHNIC_CAT)), res = 300)
    print(plot2)
    dev.off()
    
    i <- i+1
  }
}


###########################
#Acute Ethnicity
###########################
data <- read_excel("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Excel/Master file figures and tables updated.xlsx", sheet = "5.3.1 Acute-ethnicity")

data <- data[1:266,1:16]

data <- data %>%
  mutate(yearmonth = as.Date(as.numeric(yearmonth)-2, origin = "1900-01-01"),
         events_stroke = as.numeric(events_stroke),
         events_ami = as.numeric(events_ami),
         sum_followup_total = as.numeric(sum_followup_total),
         `incid ami`= as.numeric(`incid mi`),
         `incid ami` = ifelse(is.na(`incid ami`), 0, `incid ami`),
         `incid stroke` = as.numeric(`incid stroke`),
         `incid stroke` = ifelse(is.na(`incid stroke`), 0, `incid stroke`),
         AS_rate_stroke = as.numeric(AS_rate_stroke),
         AS_rate_ami = as.numeric(AS_rate_ami)) %>%
  select(yearmonth, ETHNIC_CAT,  events_stroke, events_ami, sum_followup_total, `incid ami`, `incid stroke`, AS_rate_stroke, AS_rate_ami) %>%
  filter(!ETHNIC_CAT %in% c('NA', 'Unknown'))

data <- data %>%
  mutate(sumprod_stroke = `incid stroke`* sum_followup_total,
         sumprod_ami = `incid ami`* sum_followup_total,
         sumprod_stroke_as = AS_rate_stroke*sum_followup_total,
         sumprod_ami_as = AS_rate_ami*sum_followup_total) %>%
  group_by(ETHNIC_CAT) %>%
  arrange(ETHNIC_CAT, yearmonth) %>%
  mutate(precovid_stroke = sum(sumprod_stroke [yearmonth <= as.Date("2020-02-01")]) / sum(sum_followup_total[yearmonth <= as.Date("2020-02-01")]),
         precovid_ami = sum(sumprod_ami[yearmonth <= as.Date("2020-02-01")]) / sum(sum_followup_total[yearmonth <= as.Date("2020-02-01")]),
         precovid_stroke_as = sum(sumprod_stroke_as[yearmonth <= as.Date("2020-02-01")]) / sum(sum_followup_total[yearmonth <= as.Date("2020-02-01")]),
         precovid_ami_as = sum(sumprod_ami_as[yearmonth <= as.Date("2020-02-01")]) / sum(sum_followup_total[yearmonth <= as.Date("2020-02-01")]),
         moving_average_stroke = ifelse( yearmonth %m-% months(1) %in% data$yearmonth & yearmonth %m+% months(1) %in% data$yearmonth, 
                                         (sumprod_stroke_as + lag(sumprod_stroke_as) + lead(sumprod_stroke_as)) / 
                                           (sum_followup_total + lag(sum_followup_total) + lead(sum_followup_total)),
                                         NA),
         moving_average_ami = ifelse( yearmonth %m-% months(1) %in% data$yearmonth & yearmonth %m+% months(1) %in% data$yearmonth, 
                                      (sumprod_ami_as + lag(sumprod_ami_as) + lead(sumprod_ami_as)) / 
                                        (sum_followup_total + lag(sum_followup_total) + lead(sum_followup_total)),
                                      NA),
         perc_diff_stroke = ((moving_average_stroke / precovid_stroke_as)-1)*100,
         perc_diff_ami = ((moving_average_ami / precovid_ami_as)-1)*100,
         events_diff_stroke = round((`incid stroke` - precovid_stroke) / 1000 * sum_followup_total),
         events_diff_ami = round((`incid ami` - precovid_ami) / 1000 * sum_followup_total)) %>%
  select(yearmonth, ETHNIC_CAT,`incid ami`, `incid stroke` , perc_diff_stroke, perc_diff_ami, events_diff_stroke, events_diff_ami) %>%
  rename(incidence_ami = `incid ami`,
         incidence_stroke = `incid stroke`)

palette = brewer.pal(6, 'RdBu')
palette = palette[-4]
write_csv(data,paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/results_by_ethnicity.csv"))

for (var in c("stroke", "ami")) {
  plot1 <- data %>%
    ggplot(aes (x = yearmonth, y = !!as.symbol(paste0("perc_diff_", var)), colour = factor(ETHNIC_CAT, levels = c("Asian or Asian British", "Black or Black British", "Mixed","Other" ,"White")))) +
    geom_hline (yintercept = 0, colour = "dark grey", size = 1.25, linetype = "dashed" )+
    geom_line( size = 1.25) +
    scale_colour_manual(values = palette) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, limits = c(as.Date("2019-12-01"),as.Date("2022-11-01")))+
    labs(y ="3 month rolling percentage change in age standardised rate", x = element_blank(),  colour = "Ethnicity")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot1
  ggsave(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/svg/",var,"_percentage_change_ethnicity.svg"), width = 3000, height = 2000, units = "px")
  jpeg(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/jpeg/",var,"_percentage_change_ethnicity.jpeg"), width = 3000, height = 2000, res = 300)
  print(plot1)
  dev.off()
  
  i <- 1
  for (group in c("Asian or Asian British", "Black or Black British", "Mixed","Other" ,"White")){
    temp <- data %>% filter(ETHNIC_CAT == group)
    plot2 <- temp %>%
      ggplot(aes(x = yearmonth, y = !!as.symbol(paste0("events_diff_", var)))) +
      geom_col(fill = palette[i]) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, expand = c(0, 0))+
      labs(y = element_blank(), x = element_blank()) +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    plot2
    ggsave(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/svg/",var,"_absolute_difference_ethnicity_", i, ".svg"), width = 3000, height = 3000/length(unique(data$ETHNIC_CAT)), units = "px")
    
    jpeg(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/jpeg/",var,"_absolute_difference_ethnicity_", i, ".jpeg"), width = 3000, height = 3000/length(unique(data$ETHNIC_CAT)), res = 300)
    print(plot2)
    dev.off()
    
    i <- i+1
  }
}


#########################
#chronic cci
########################

data <- read_excel("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Excel/Master file figures and tables updated.xlsx", sheet = "6.4.1 Chronic-cci")

data <- data[1:190,1:16]

data <- data %>%
  mutate(yearmonth = as.Date(as.numeric(yearmonth)-2, origin = "1900-01-01"),
         events_htn = as.numeric(events_htn),
         pt_htn = as.numeric(pt_htn),
         events_af = as.numeric(events_af),
         pt_af = as.numeric(pt_af),
         `AF_incid` = as.numeric(`incid af`),
         `Htn incid` = as.numeric(`incid htn`),
         AS_rate_htn = as.numeric(AS_rate_htn),
         AS_rate_af = as.numeric(AS_rate_af)) %>%
  select(yearmonth, cci_index, events_htn, pt_htn, events_af, pt_af, AF_incid, `Htn incid`, AS_rate_htn, AS_rate_af) %>%
  filter(cci_index != "NA") %>%
  mutate(cci_index = ifelse(cci_index == "4", "4+", cci_index))

data <- data %>%
  mutate(sumprod_htn = `Htn incid`*pt_htn,
         sumprod_af = AF_incid*pt_af,
         sumprod_htn_as = AS_rate_htn*pt_htn,
         sumprod_af_as = AS_rate_af*pt_af) %>%
  group_by(cci_index) %>%
  arrange(cci_index, yearmonth) %>%
  mutate(precovid_htn = sum(sumprod_htn[yearmonth <= as.Date("2020-02-01")]) / sum(pt_htn[yearmonth <= as.Date("2020-02-01")]),
         precovid_af = sum(sumprod_af[yearmonth <= as.Date("2020-02-01")]) / sum(pt_af[yearmonth <= as.Date("2020-02-01")]),
         precovid_htn_as = sum(sumprod_htn_as[yearmonth <= as.Date("2020-02-01")]) / sum(pt_htn[yearmonth <= as.Date("2020-02-01")]),
         precovid_af_as = sum(sumprod_af_as[yearmonth <= as.Date("2020-02-01")]) / sum(pt_af[yearmonth <= as.Date("2020-02-01")]),
         moving_average_htn = ifelse( yearmonth %m-% months(1) %in% data$yearmonth & yearmonth %m+% months(1) %in% data$yearmonth, 
                                      (sumprod_htn_as + lag(sumprod_htn_as) + lead(sumprod_htn_as)) / 
                                        (pt_htn + lag(pt_htn) + lead(pt_htn)),
                                      NA),
         moving_average_af = ifelse( yearmonth %m-% months(1) %in% data$yearmonth & yearmonth %m+% months(1) %in% data$yearmonth, 
                                     (sumprod_af_as + lag(sumprod_af_as) + lead(sumprod_af_as)) / 
                                       (pt_af + lag(pt_af) + lead(pt_af)),
                                     NA),
         perc_diff_htn = ((moving_average_htn / precovid_htn_as)-1)*100,
         perc_diff_af = ((moving_average_af / precovid_af_as)-1)*100,
         events_diff_htn = round((`Htn incid` - precovid_htn) / 1000 * pt_htn),
         events_diff_af = round((`AF_incid` - precovid_af) / 1000 * pt_af)) %>%
  select(yearmonth, cci_index, AF_incid, `Htn incid`, perc_diff_htn, perc_diff_af, events_diff_htn, events_diff_af, AS_rate_htn, AS_rate_af) %>%
  rename(incidence_af = AF_incid,
         incidence_Htn = `Htn incid`)

palette = brewer.pal(6, 'RdBu')
palette = palette[-4]
write_csv(data,paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/results_by_cci.csv"))

for (var in c("htn", "af")) {
  plot1 <- data %>%
    ggplot(aes (x = yearmonth, y = !!as.symbol(paste0("perc_diff_", var)), colour = factor(cci_index, levels = c("0", "1", "2", "3", "4+")))) +
    geom_hline (yintercept = 0, colour = "dark grey", size = 1.25, linetype = "dashed" )+
    geom_line( size = 1.25) +
    scale_colour_manual(values = palette) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, limits = c(as.Date("2019-12-01"),as.Date("2022-11-01")))+
    labs(y ="3 month rolling percentage change in age standardised rate", x = element_blank(),  colour = "Charlson Score")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot1
  ggsave(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/svg/",var,"_percentage_change_cci.svg"), width = 3000, height = 2000, units = "px")
  
  jpeg(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/jpeg/",var,"_percentage_change_cci.jpeg"), width = 3000, height = 2000, res = 300)
  print(plot1)
  dev.off()
  
  i <- 1
  for (group in c("0", "1", "2", "3", "4+")){
    temp <- data %>% filter(cci_index == group)
    plot2 <- temp %>%
      ggplot(aes(x = yearmonth, y = !!as.symbol(paste0("events_diff_", var)))) +
      geom_col(fill = palette[i]) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, expand = c(0, 0))+
      labs(y = element_blank(), x = element_blank()) +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    plot2
    ggsave(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/svg/",var,"_absolute_difference_cci_", i, ".svg"), width = 3000, height = 3000/length(unique(data$cci_index)), units = "px")
    
    jpeg(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/jpeg/",var,"_absolute_difference_cci_", i, ".jpeg"), width = 3000, height = 3000/length(unique(data$cci_index)), res = 300)
    print(plot2)
    dev.off()
    
    i <- i+1
  }
}


###########################
#Acute cci
###########################
data <- read_excel("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Excel/Master file figures and tables updated.xlsx", sheet = "5.4.1 Acute-cci")

data <- data[1:190,1:16]

data <- data %>%
  mutate(yearmonth = as.Date(as.numeric(yearmonth)-2, origin = "1900-01-01"),
         events_stroke = as.numeric(events_stroke),
         events_ami = as.numeric(events_ami),
         sum_followup_total = as.numeric(sum_followup_total),
         `incid ami`= as.numeric(`incid mi`),
         `incid stroke` = as.numeric(`incid stroke`),
         AS_rate_stroke = as.numeric(AS_rate_stroke),
         AS_rate_ami = as.numeric(AS_rate_ami)) %>%
  select(yearmonth, cci_index,  events_stroke, events_ami, sum_followup_total, `incid ami`, `incid stroke`, AS_rate_stroke, AS_rate_ami)  %>%
  filter(cci_index != "NA") %>%
  mutate(cci_index = ifelse(cci_index == "4", "4+", cci_index))

data <- data %>%
  mutate(sumprod_stroke = `incid stroke`* sum_followup_total,
         sumprod_ami = `incid ami`* sum_followup_total,
         sumprod_stroke_as = AS_rate_stroke*sum_followup_total,
         sumprod_ami_as = AS_rate_ami*sum_followup_total) %>%
  group_by(cci_index) %>%
  arrange(cci_index, yearmonth) %>%
  mutate(precovid_stroke = sum(sumprod_stroke [yearmonth <= as.Date("2020-02-01")]) / sum(sum_followup_total[yearmonth <= as.Date("2020-02-01")]),
         precovid_ami = sum(sumprod_ami[yearmonth <= as.Date("2020-02-01")]) / sum(sum_followup_total[yearmonth <= as.Date("2020-02-01")]),
         precovid_stroke_as = sum(sumprod_stroke_as[yearmonth <= as.Date("2020-02-01")]) / sum(sum_followup_total[yearmonth <= as.Date("2020-02-01")]),
         precovid_ami_as = sum(sumprod_ami_as[yearmonth <= as.Date("2020-02-01")]) / sum(sum_followup_total[yearmonth <= as.Date("2020-02-01")]),
         moving_average_stroke = ifelse( yearmonth %m-% months(1) %in% data$yearmonth & yearmonth %m+% months(1) %in% data$yearmonth, 
                                         (sumprod_stroke_as + lag(sumprod_stroke_as) + lead(sumprod_stroke_as)) / 
                                           (sum_followup_total + lag(sum_followup_total) + lead(sum_followup_total)),
                                         NA),
         moving_average_ami = ifelse( yearmonth %m-% months(1) %in% data$yearmonth & yearmonth %m+% months(1) %in% data$yearmonth, 
                                      (sumprod_ami_as + lag(sumprod_ami_as) + lead(sumprod_ami_as)) / 
                                        (sum_followup_total + lag(sum_followup_total) + lead(sum_followup_total)),
                                      NA),
         perc_diff_stroke = ((moving_average_stroke / precovid_stroke_as)-1)*100,
         perc_diff_ami = ((moving_average_ami / precovid_ami_as)-1)*100,
         events_diff_stroke = round((`incid stroke` - precovid_stroke) / 1000 * sum_followup_total),
         events_diff_ami = round((`incid ami` - precovid_ami) / 1000 * sum_followup_total)) %>%
  select(yearmonth, cci_index,`incid ami`, `incid stroke` , perc_diff_stroke, perc_diff_ami, events_diff_stroke, events_diff_ami) %>%
  rename(incidence_ami = `incid ami`,
         incidence_Htn = `incid stroke`)

palette = brewer.pal(6, 'RdBu')
palette = palette[-4]
write_csv(data,paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/results_by_cci.csv"))

for (var in c("stroke", "ami")) {
  plot1 <- data %>%
    ggplot(aes (x = yearmonth, y = !!as.symbol(paste0("perc_diff_", var)), colour = factor(cci_index, levels = c("0", "1", "2", "3", "4+")))) +
    geom_hline (yintercept = 0, colour = "dark grey", size = 1.25, linetype = "dashed" )+
    geom_line( size = 1.25) +
    scale_colour_manual(values = palette) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, limits = c(as.Date("2019-12-01"),as.Date("2022-11-01")))+
    labs(y ="3 month rolling percentage change in age standardised rate", x = element_blank(),  colour = "Charlson score")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot1
  ggsave(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/svg/",var,"_percentage_change_cci.svg"), width = 3000, height = 2000, units = "px")
  jpeg(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/jpeg/",var,"_percentage_change_cci.jpeg"), width = 3000, height = 2000, res = 300)
  print(plot1)
  dev.off()
  
  i <- 1
  for (group in c("0", "1", "2", "3", "4+")){
    temp <- data %>% filter(cci_index == group)
    plot2 <- temp %>%
      ggplot(aes(x = yearmonth, y = !!as.symbol(paste0("events_diff_", var)))) +
      geom_col(fill = palette[i]) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, expand = c(0, 0))+
      labs(y = element_blank(), x = element_blank()) +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    plot2
    ggsave(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/svg/",var,"_absolute_difference_cci_", i, ".svg"), width = 3000, height = 3000/length(unique(data$cci_index)), units = "px")
    
    jpeg(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/jpeg/",var,"_absolute_difference_cci_", i, ".jpeg"), width = 3000, height = 3000/length(unique(data$cci_index)), res = 300)
    print(plot2)
    dev.off()
    
    i <- i+1
  }
}

#########################
#chronic deprivation
########################

data <- read_excel("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Excel/Master file figures and tables updated.xlsx", sheet = "6.5.1 Chronic-imd")

data <- data[1:418,1:16]

data <- data %>%
  mutate(yearmonth = as.Date(as.numeric(yearmonth)-2, origin = "1900-01-01"),
         events_htn = as.numeric(events_htn),
         pt_htn = as.numeric(pt_htn),
         events_af = as.numeric(events_af),
         pt_af = as.numeric(pt_af),
         `AF_incid` = as.numeric(`incid AF`),
         `Htn incid` = as.numeric(`incid htn`),
         AS_rate_htn = as.numeric(AS_rate_htn),
         AS_rate_af = as.numeric(AS_rate_af)) %>%
  select(yearmonth, IMD_2019_DECILES, events_htn, pt_htn, events_af, pt_af, AF_incid, `Htn incid`, AS_rate_htn, AS_rate_af) %>%
  filter(IMD_2019_DECILES != "NA") 

data <- data %>%
  mutate(sumprod_htn = `Htn incid`*pt_htn,
         sumprod_af = AF_incid*pt_af,
         sumprod_htn_as = AS_rate_htn*pt_htn,
         sumprod_af_as = AS_rate_af*pt_af) %>%
  group_by(IMD_2019_DECILES) %>%
  arrange(IMD_2019_DECILES, yearmonth) %>%
  mutate(precovid_htn = sum(sumprod_htn[yearmonth <= as.Date("2020-02-01")]) / sum(pt_htn[yearmonth <= as.Date("2020-02-01")]),
         precovid_af = sum(sumprod_af[yearmonth <= as.Date("2020-02-01")]) / sum(pt_af[yearmonth <= as.Date("2020-02-01")]),
         precovid_htn_as = sum(sumprod_htn_as[yearmonth <= as.Date("2020-02-01")]) / sum(pt_htn[yearmonth <= as.Date("2020-02-01")]),
         precovid_af_as = sum(sumprod_af_as[yearmonth <= as.Date("2020-02-01")]) / sum(pt_af[yearmonth <= as.Date("2020-02-01")]),
         moving_average_htn = ifelse( yearmonth %m-% months(1) %in% data$yearmonth & yearmonth %m+% months(1) %in% data$yearmonth, 
                                      (sumprod_htn_as + lag(sumprod_htn_as) + lead(sumprod_htn_as)) / 
                                        (pt_htn + lag(pt_htn) + lead(pt_htn)),
                                      NA),
         moving_average_af = ifelse( yearmonth %m-% months(1) %in% data$yearmonth & yearmonth %m+% months(1) %in% data$yearmonth, 
                                     (sumprod_af_as + lag(sumprod_af_as) + lead(sumprod_af_as)) / 
                                       (pt_af + lag(pt_af) + lead(pt_af)),
                                     NA),
         perc_diff_htn = ((moving_average_htn / precovid_htn_as)-1)*100,
         perc_diff_af = ((moving_average_af / precovid_af_as)-1)*100,
         events_diff_htn = round((`Htn incid` - precovid_htn) / 1000 * pt_htn),
         events_diff_af = round((`AF_incid` - precovid_af) / 1000 * pt_af)) %>%
  select(yearmonth, IMD_2019_DECILES, AF_incid, `Htn incid`, perc_diff_htn, perc_diff_af, events_diff_htn, events_diff_af, AS_rate_htn, AS_rate_af) %>%
  rename(incidence_af = AF_incid,
         incidence_Htn = `Htn incid`)

palette = brewer.pal(10, 'RdBu')
write_csv(data,paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/results_by_imd.csv"))

for (var in c("htn", "af")) {
  plot1 <- data %>%
    ggplot(aes (x = yearmonth, y = !!as.symbol(paste0("perc_diff_", var)), colour = factor(IMD_2019_DECILES, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")))) +
    geom_hline (yintercept = 0, colour = "dark grey", size = 1.25, linetype = "dashed" )+
    geom_line( size = 1.25) +
    scale_colour_manual(values = palette) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, limits = c(as.Date("2019-12-01"),as.Date("2022-11-01")))+
    labs(y ="3 month rolling percentage change in age standardised rate", x = element_blank(),  colour = "Deprivation decile")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot1
  ggsave(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/svg/",var,"_percentage_change_imd.svg"), width = 3000, height = 2000, units = "px")
  
  jpeg(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/jpeg/",var,"_percentage_change_imd.jpeg"), width = 3000, height = 2000, res = 300)
  print(plot1)
  dev.off()
  
  i <- 1
  for (group in c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")){
    temp <- data %>% filter(IMD_2019_DECILES == group)
    plot2 <- temp %>%
      ggplot(aes(x = yearmonth, y = !!as.symbol(paste0("events_diff_", var)))) +
      geom_col(fill = palette[i]) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, expand = c(0, 0))+
      labs(y = element_blank(), x = element_blank()) +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    plot2
    ggsave(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/svg/",var,"_absolute_difference_imd_", i, ".svg"), width = 3000, height = 3200/(length(unique(data$IMD_2019_DECILES))-2), units = "px")
    
    jpeg(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/jpeg/",var,"_absolute_difference_imd_", i, ".jpeg"), width = 3000, height = 3200/(length(unique(data$IMD_2019_DECILES))-2), res = 300)
    print(plot2)
    dev.off()
    
    i <- i+1
  }
}


###########################
#Acute deprivation
###########################
data <- read_excel("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Excel/Master file figures and tables updated.xlsx", sheet = "5.5.1 Acute-imd")

data <- data[1:418,1:16]

data <- data %>%
  rename(IMD_2019_DECILE = IMD_2019_DECILES) %>%
  mutate(yearmonth = as.Date(as.numeric(yearmonth)-2, origin = "1900-01-01"),
         events_stroke = as.numeric(events_stroke),
         events_ami = as.numeric(events_ami),
         sum_followup_total = as.numeric(sum_followup_total),
         `incid ami`= as.numeric(`incid mi`),
         `incid stroke` = as.numeric(`incid stroke`),
         AS_rate_stroke = as.numeric(AS_rate_stroke),
         AS_rate_ami = as.numeric(AS_rate_ami)) %>%
  select(yearmonth, IMD_2019_DECILE,  events_stroke, events_ami, sum_followup_total, `incid ami`, `incid stroke`, AS_rate_stroke, AS_rate_ami)  %>%
  filter(IMD_2019_DECILE != "NA") 

data <- data %>%
  mutate(sumprod_stroke = `incid stroke`* sum_followup_total,
         sumprod_ami = `incid ami`* sum_followup_total,
         sumprod_stroke_as = AS_rate_stroke*sum_followup_total,
         sumprod_ami_as = AS_rate_ami*sum_followup_total) %>%
  group_by(IMD_2019_DECILE) %>%
  arrange(IMD_2019_DECILE, yearmonth) %>%
  mutate(precovid_stroke = sum(sumprod_stroke [yearmonth <= as.Date("2020-02-01")]) / sum(sum_followup_total[yearmonth <= as.Date("2020-02-01")]),
         precovid_ami = sum(sumprod_ami[yearmonth <= as.Date("2020-02-01")]) / sum(sum_followup_total[yearmonth <= as.Date("2020-02-01")]),
         precovid_stroke_as = sum(sumprod_stroke_as[yearmonth <= as.Date("2020-02-01")]) / sum(sum_followup_total[yearmonth <= as.Date("2020-02-01")]),
         precovid_ami_as = sum(sumprod_ami_as[yearmonth <= as.Date("2020-02-01")]) / sum(sum_followup_total[yearmonth <= as.Date("2020-02-01")]),
         moving_average_stroke = ifelse( yearmonth %m-% months(1) %in% data$yearmonth & yearmonth %m+% months(1) %in% data$yearmonth, 
                                         (sumprod_stroke_as + lag(sumprod_stroke_as) + lead(sumprod_stroke_as)) / 
                                           (sum_followup_total + lag(sum_followup_total) + lead(sum_followup_total)),
                                         NA),
         moving_average_ami = ifelse( yearmonth %m-% months(1) %in% data$yearmonth & yearmonth %m+% months(1) %in% data$yearmonth, 
                                      (sumprod_ami_as + lag(sumprod_ami_as) + lead(sumprod_ami_as)) / 
                                        (sum_followup_total + lag(sum_followup_total) + lead(sum_followup_total)),
                                      NA),
         perc_diff_stroke = ((moving_average_stroke / precovid_stroke_as)-1)*100,
         perc_diff_ami = ((moving_average_ami / precovid_ami_as)-1)*100,
         events_diff_stroke = round((`incid stroke` - precovid_stroke) / 1000 * sum_followup_total),
         events_diff_ami = round((`incid ami` - precovid_ami) / 1000 * sum_followup_total)) %>%
  select(yearmonth, IMD_2019_DECILE,`incid ami`, `incid stroke` , perc_diff_stroke, perc_diff_ami, events_diff_stroke, events_diff_ami) %>%
  rename(incidence_ami = `incid ami`,
         incidence_Htn = `incid stroke`)

palette = brewer.pal(10, 'RdBu')
write_csv(data,paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/results_by_imd.csv"))

for (var in c("stroke", "ami")) {
  plot1 <- data %>%
    ggplot(aes (x = yearmonth, y = !!as.symbol(paste0("perc_diff_", var)), colour = factor(IMD_2019_DECILE, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")))) +
    geom_hline (yintercept = 0, colour = "dark grey", size = 1.25, linetype = "dashed" )+
    geom_line( size = 1.25) +
    scale_colour_manual(values = palette) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, limits = c(as.Date("2019-12-01"),as.Date("2022-11-01")))+
    labs(y ="3 month rolling percentage change in age standardised rate", x = element_blank(),  colour = "Deprivation decile")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot1
  ggsave(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/svg/",var,"_percentage_change_imd.svg"), width = 3000, height = 2000, units = "px")
  jpeg(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/jpeg/",var,"_percentage_change_imd.jpeg"), width = 3000, height = 2000, res = 300)
  print(plot1)
  dev.off()
  
  i <- 1
  for (group in c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")){
    temp <- data %>% filter(IMD_2019_DECILE == group)
    plot2 <- temp %>%
      ggplot(aes(x = yearmonth, y = !!as.symbol(paste0("events_diff_", var)))) +
      geom_col(fill = palette[i]) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, expand = c(0, 0))+
      labs(y = element_blank(), x = element_blank()) +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    plot2
    ggsave(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/svg/",var,"_absolute_difference_imd_", i, ".svg"), width = 3000, height = 3200/(length(unique(data$IMD_2019_DECILE))-2), units = "px")
    
    jpeg(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/jpeg/",var,"_absolute_difference_imd_", i, ".jpeg"), width = 3000, height = 3200/(length(unique(data$IMD_2019_DECILE))-2), res = 300)
    print(plot2)
    dev.off()
    
    i <- i+1
  }
}

#########################
#chronic region
########################

data <- read_excel("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Excel/Master file figures and tables updated.xlsx", sheet = "6.6.1 Chronic-region")

data <- data[1:380,1:16]

data <- data %>%
  mutate(yearmonth = as.Date(as.numeric(yearmonth)-2, origin = "1900-01-01"),
         events_htn = as.numeric(events_htn),
         pt_htn = as.numeric(pt_htn),
         events_af = as.numeric(events_af),
         pt_af = as.numeric(pt_af),
         `AF_incid` = as.numeric(`incid af`),
         `Htn incid` = as.numeric(`incid htn`),
         AS_rate_htn = as.numeric(AS_rate_htn),
         AS_rate_af = as.numeric(AS_rate_af)) %>%
  select(yearmonth, region, events_htn, pt_htn, events_af, pt_af, AF_incid, `Htn incid`, AS_rate_htn, AS_rate_af) %>%
  filter(region != "NA") 

data <- data %>%
  mutate(sumprod_htn = `Htn incid`*pt_htn,
         sumprod_af = AF_incid*pt_af,
         sumprod_htn_as = AS_rate_htn*pt_htn,
         sumprod_af_as = AS_rate_af*pt_af) %>%
  group_by(region) %>%
  arrange(region, yearmonth) %>%
  mutate(precovid_htn = sum(sumprod_htn[yearmonth <= as.Date("2020-02-01")]) / sum(pt_htn[yearmonth <= as.Date("2020-02-01")]),
         precovid_af = sum(sumprod_af[yearmonth <= as.Date("2020-02-01")]) / sum(pt_af[yearmonth <= as.Date("2020-02-01")]),
         precovid_htn_as = sum(sumprod_htn_as[yearmonth <= as.Date("2020-02-01")]) / sum(pt_htn[yearmonth <= as.Date("2020-02-01")]),
         precovid_af_as = sum(sumprod_af_as[yearmonth <= as.Date("2020-02-01")]) / sum(pt_af[yearmonth <= as.Date("2020-02-01")]),
         moving_average_htn = ifelse( yearmonth %m-% months(1) %in% data$yearmonth & yearmonth %m+% months(1) %in% data$yearmonth, 
                                      (sumprod_htn_as + lag(sumprod_htn_as) + lead(sumprod_htn_as)) / 
                                        (pt_htn + lag(pt_htn) + lead(pt_htn)),
                                      NA),
         moving_average_af = ifelse( yearmonth %m-% months(1) %in% data$yearmonth & yearmonth %m+% months(1) %in% data$yearmonth, 
                                     (sumprod_af_as + lag(sumprod_af_as) + lead(sumprod_af_as)) / 
                                       (pt_af + lag(pt_af) + lead(pt_af)),
                                     NA),
         perc_diff_htn = ((moving_average_htn / precovid_htn_as)-1)*100,
         perc_diff_af = ((moving_average_af / precovid_af_as)-1)*100,
         events_diff_htn = round((`Htn incid` - precovid_htn) / 1000 * pt_htn),
         events_diff_af = round((`AF_incid` - precovid_af) / 1000 * pt_af)) %>%
  select(yearmonth, region, AF_incid, `Htn incid`, perc_diff_htn, perc_diff_af, events_diff_htn, events_diff_af, AS_rate_htn, AS_rate_af) %>%
  rename(incidence_af = AF_incid,
         incidence_Htn = `Htn incid`)

palette = brewer.pal(10, 'RdBu')
palette = palette[-5]
write_csv(data,paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/results_by_imd.csv"))

for (var in c("htn", "af")) {
  plot1 <- data %>%
    ggplot(aes (x = yearmonth, y = !!as.symbol(paste0("perc_diff_", var)), colour = factor(region, levels = c("East Midlands", "East of England", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire and The Humber")))) +
    geom_hline (yintercept = 0, colour = "dark grey", size = 1.25, linetype = "dashed" )+
    geom_line( size = 1.25) +
    scale_colour_manual(values = palette) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, limits = c(as.Date("2019-12-01"),as.Date("2022-11-01")))+
    labs(y ="3 month rolling percentage change in age standardised rate", x = element_blank(),  colour = "Region")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot1
  ggsave(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/svg/",var,"_percentage_change_region.svg"), width = 3000, height = 2000, units = "px")
  
  jpeg(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/jpeg/",var,"_percentage_change_region.jpeg"), width = 3000, height = 2000, res = 300)
  print(plot1)
  dev.off()
  
  i <- 1
  for (group in c("East Midlands", "East of England", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire and The Humber")){
    temp <- data %>% filter(region == group)
    plot2 <- temp %>%
      ggplot(aes(x = yearmonth, y = !!as.symbol(paste0("events_diff_", var)))) +
      geom_col(fill = palette[i]) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, expand = c(0, 0))+
      labs(y = element_blank(), x = element_blank()) +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    plot2
    ggsave(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/svg/",var,"_absolute_difference_region_", i, ".svg"), width = 3000, height = 3000/(length(unique(data$region))-2), units = "px")
    
    jpeg(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/jpeg/",var,"_absolute_difference_region_", i, ".jpeg"), width = 3000, height = 3000/(length(unique(data$region))-2), res = 300)
    print(plot2)
    dev.off()
    
    i <- i+1
  }
}


###########################
#Acute region
###########################
data <- read_excel("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Excel/Master file figures and tables updated.xlsx", sheet = "5.6.1 Acute-region")

data <- data[1:380,1:16]

data <- data %>%
  mutate(yearmonth = as.Date(as.numeric(yearmonth)-2, origin = "1900-01-01"),
         events_stroke = as.numeric(events_stroke),
         events_ami = as.numeric(events_ami),
         sum_followup_total = as.numeric(sum_followup_total),
         `incid ami`= as.numeric(`incid mi`),
         `incid stroke` = as.numeric(`incid stroke`),
         AS_rate_stroke = as.numeric(AS_rate_stroke),
         AS_rate_ami = as.numeric(AS_rate_ami)) %>%
  select(yearmonth, region,  events_stroke, events_ami, sum_followup_total, `incid ami`, `incid stroke`, AS_rate_stroke, AS_rate_ami)  %>%
  filter(region != "NA") 

data <- data %>%
  mutate(sumprod_stroke = `incid stroke`* sum_followup_total,
         sumprod_ami = `incid ami`* sum_followup_total,
         sumprod_stroke_as = AS_rate_stroke*sum_followup_total,
         sumprod_ami_as = AS_rate_ami*sum_followup_total) %>%
  group_by(region) %>%
  arrange(region, yearmonth) %>%
  mutate(precovid_stroke = sum(sumprod_stroke [yearmonth <= as.Date("2020-02-01")]) / sum(sum_followup_total[yearmonth <= as.Date("2020-02-01")]),
         precovid_ami = sum(sumprod_ami[yearmonth <= as.Date("2020-02-01")]) / sum(sum_followup_total[yearmonth <= as.Date("2020-02-01")]),
         precovid_stroke_as = sum(sumprod_stroke_as[yearmonth <= as.Date("2020-02-01")]) / sum(sum_followup_total[yearmonth <= as.Date("2020-02-01")]),
         precovid_ami_as = sum(sumprod_ami_as[yearmonth <= as.Date("2020-02-01")]) / sum(sum_followup_total[yearmonth <= as.Date("2020-02-01")]),
         moving_average_stroke = ifelse( yearmonth %m-% months(1) %in% data$yearmonth & yearmonth %m+% months(1) %in% data$yearmonth, 
                                         (sumprod_stroke_as + lag(sumprod_stroke_as) + lead(sumprod_stroke_as)) / 
                                           (sum_followup_total + lag(sum_followup_total) + lead(sum_followup_total)),
                                         NA),
         moving_average_ami = ifelse( yearmonth %m-% months(1) %in% data$yearmonth & yearmonth %m+% months(1) %in% data$yearmonth, 
                                      (sumprod_ami_as + lag(sumprod_ami_as) + lead(sumprod_ami_as)) / 
                                        (sum_followup_total + lag(sum_followup_total) + lead(sum_followup_total)),
                                      NA),
         perc_diff_stroke = ((moving_average_stroke / precovid_stroke_as)-1)*100,
         perc_diff_ami = ((moving_average_ami / precovid_ami_as)-1)*100,
         events_diff_stroke = round((`incid stroke` - precovid_stroke) / 1000 * sum_followup_total),
         events_diff_ami = round((`incid ami` - precovid_ami) / 1000 * sum_followup_total)) %>%
  select(yearmonth, region,`incid ami`, `incid stroke` , perc_diff_stroke, perc_diff_ami, events_diff_stroke, events_diff_ami) %>%
  rename(incidence_ami = `incid ami`,
         incidence_Htn = `incid stroke`)

palette = brewer.pal(10, 'RdBu')
palette = palette[-5]
write_csv(data,paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/results_by_imd.csv"))

for (var in c("stroke", "ami")) {
  plot1 <- data %>%
    ggplot(aes (x = yearmonth, y = !!as.symbol(paste0("perc_diff_", var)), colour = factor(region, levels = c("East Midlands", "East of England", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire and The Humber")))) +
    geom_hline (yintercept = 0, colour = "dark grey", size = 1.25, linetype = "dashed" )+
    geom_line( size = 1.25) +
    scale_colour_manual(values = palette) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, limits = c(as.Date("2019-12-01"),as.Date("2022-11-01")))+
    labs(y ="3 month rolling percentage change in age standardised rate", x = element_blank(),  colour = "Region")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot1
  ggsave(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/svg/",var,"_percentage_change_region.svg"), width = 3000, height = 2000, units = "px")
  jpeg(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/jpeg/",var,"_percentage_change_region.jpeg"), width = 3000, height = 2000, res = 300)
  print(plot1)
  dev.off()
  
  i <- 1
  for (group in c("East Midlands", "East of England", "London", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire and The Humber")){
    temp <- data %>% filter(region == group)
    plot2 <- temp %>%
      ggplot(aes(x = yearmonth, y = !!as.symbol(paste0("events_diff_", var)))) +
      geom_col(fill = palette[i]) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, expand = c(0, 0))+
      labs(y = element_blank(), x = element_blank()) +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    plot2
    ggsave(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/svg/",var,"_absolute_difference_region_", i, ".svg"), width = 3000, height = 3000/(length(unique(data$region))-2), units = "px")
    
    jpeg(paste0("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/jpeg/",var,"_absolute_difference_region_", i, ".jpeg"), width = 3000, height = 3000/(length(unique(data$region))-2), res = 300)
    print(plot2)
    dev.off()
    
    i <- i+1
  }
}


#############################
#Overall rate changes - Hypertension
#############################
data <- read_excel("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Excel/Master file figures and tables updated.xlsx", sheet = "4.2.1 Monthly htn & af data")

data_htn_crude <- data[1:3, 2:40]
data_htn_crude <- as.data.frame(t(data_htn_crude))

data_htn_crude[,4] <- rownames(data_htn_crude)
names(data_htn_crude) <- data_htn_crude[1,]
data_htn_crude <- data_htn_crude[-1,]

data_htn_as <- data[49:50, 2:40]


data_htn_as <- as.data.frame(t(data_htn_as))
names(data_htn_as) <- data_htn_as[1,]
data_htn_as <- data_htn_as[-1,]
data_htn_as$row_num <- seq.int(nrow(data_htn_as))

data_htn_as <- data_htn_as %>% 
  rename(Age_standardised_rate = Rate)

data_htn <- merge(data_htn_as, data_htn_crude)

data_htn <- data_htn %>%
  rename(Crude_rate = Rate,
         Person_time = `Person time`) %>%
  mutate(yearmonth = as.Date(paste("01", Date), format ='%d %B %Y'),
         Age_standardised_rate = as.numeric(Age_standardised_rate),
         Counts = as.numeric(Counts),
         Person_time = as.numeric(Person_time),
         Crude_rate = as.numeric(Crude_rate)) 

data_htn <- data_htn %>%
  arrange(yearmonth) %>%
  mutate(sumprod_htn = Crude_rate*Person_time,
         sumprod_htn_as = Age_standardised_rate*Person_time,
         precovid_htn = sum(sumprod_htn[yearmonth <= as.Date("2020-02-01")]) / sum(Person_time[yearmonth <= as.Date("2020-02-01")]),
         precovid_htn_as = sum(sumprod_htn_as[yearmonth <= as.Date("2020-02-01")]) / sum(Person_time[yearmonth <= as.Date("2020-02-01")]),
         moving_average_htn = ifelse(yearmonth %m-% months(1) %in% data_htn$yearmonth & yearmonth %m+% months(1) %in% data_htn$yearmonth,
                                      (sumprod_htn_as + lag(sumprod_htn_as) + lead(sumprod_htn_as)) / 
                                        (Person_time + lag(Person_time) + lead(Person_time)),
                                      NA),
         perc_diff_htn = ((moving_average_htn / precovid_htn_as)-1)*100,
         events_diff_htn = round((Crude_rate - precovid_htn) / 1000 * Person_time)) %>%
  select(yearmonth,Crude_rate, perc_diff_htn, events_diff_htn, Age_standardised_rate)

  plot1 <- data_htn %>%
    ggplot(aes (x = yearmonth, y = perc_diff_htn) ) +
    geom_hline (yintercept = 0, colour = "dark grey", size = 1.25, linetype = "dashed" )+
    geom_line( size = 1.25, colour = "#00a3c7" ) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, limits = c(as.Date("2019-12-01"),as.Date("2022-11-01")))+
    labs(y ="3 month rolling percentage change in age standardised rate", x = element_blank(),  colour = "Age group")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot1
  ggsave("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/svg/htn_percentage_change_overall.svg", width = 3000, height = 2000, units = "px")
  jpeg("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/jpeg/htn_percentage_change_overall.jpeg", width = 3000, height = 2000, res = 300)
  print(plot1)
  dev.off()
  
    plot2 <- data_htn %>%
      ggplot(aes(x = yearmonth, y = events_diff_htn)) +
      geom_col(fill = "#00a3c7") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, expand = c(0, 0))+
      labs(y = "Absolute difference of hypertension diagnoses", x = element_blank()) +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    plot2
    ggsave("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/svg/htn_absolute_difference_overall.svg", width = 3000, height = 2000, units = "px")
    
    jpeg("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/jpeg/htn_absolute_difference_overall.jpeg", width = 3000, height = 2000, res = 300)
    print(plot2)
    dev.off()
    
#############################
#Overall rate changes - AF
#############################
data <- read_excel("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Excel/Master file figures and tables updated.xlsx", sheet = "4.2.1 Monthly htn & af data")
    

data_af <- data[5:8, 2:40]

data_af_crude <- as.data.frame(t(data_af))

names(data_af_crude) <- data_af_crude[1,]
data_af_crude <- data_af_crude[-1,]

data_af_as <- data[52:53, 2:40]


data_af_as <- as.data.frame(t(data_af_as))
names(data_af_as) <- data_af_as[1,]
data_af_as <- data_af_as[-1,]
data_af_as$row_num <- seq.int(nrow(data_af_as))

data_af_as <- data_af_as %>% 
  rename(Age_standardised_rate = Rate)

data_af <- merge(data_af_as, data_af_crude)

data_af <- data_af %>%
  rename(Crude_rate = Rate,
         Person_time = `Person time`) %>%
  mutate(yearmonth = as.Date(paste("01", Date), format ='%d %B %Y'),
         Age_standardised_rate = as.numeric(Age_standardised_rate),
         Counts = as.numeric(Counts),
         Person_time = as.numeric(Person_time),
         Crude_rate = as.numeric(Crude_rate)) 

data_af <- data_af %>%
  arrange(yearmonth) %>%
  mutate(sumprod_af = Crude_rate*Person_time,
         sumprod_af_as = Age_standardised_rate*Person_time,
         precovid_af = sum(sumprod_af[yearmonth <= as.Date("2020-02-01")]) / sum(Person_time[yearmonth <= as.Date("2020-02-01")]),
         precovid_af_as = sum(sumprod_af_as[yearmonth <= as.Date("2020-02-01")]) / sum(Person_time[yearmonth <= as.Date("2020-02-01")]),
         moving_average_af = ifelse(yearmonth %m-% months(1) %in% data_af$yearmonth & yearmonth %m+% months(1) %in% data_af$yearmonth,
                                     (sumprod_af_as + lag(sumprod_af_as) + lead(sumprod_af_as)) / 
                                       (Person_time + lag(Person_time) + lead(Person_time)),
                                     NA),
         perc_diff_af = ((moving_average_af / precovid_af_as)-1)*100,
         events_diff_af = round((Crude_rate - precovid_af) / 1000 * Person_time)) %>%
  select(yearmonth,Crude_rate, perc_diff_af, events_diff_af, Age_standardised_rate)

plot1 <- data_af %>%
  ggplot(aes (x = yearmonth, y = perc_diff_af) ) +
  geom_hline (yintercept = 0, colour = "dark grey", size = 1.25, linetype = "dashed" )+
  geom_line( size = 1.25, colour = "#00a3c7" ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, limits = c(as.Date("2019-12-01"),as.Date("2022-11-01")))+
  labs(y ="3 month rolling percentage change in age standardised rate", x = element_blank(),  colour = "Age group")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot1
ggsave("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/svg/af_percentage_change_overall.svg", width = 3000, height = 2000, units = "px")
jpeg("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/jpeg/af_percentage_change_overall.jpeg", width = 3000, height = 2000, res = 300)
print(plot1)
dev.off()

plot2 <- data_af %>%
  ggplot(aes(x = yearmonth, y = events_diff_af)) +
  geom_col(fill = "#00a3c7") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, expand = c(0, 0))+
  labs(y = "Absolute difference of atrial fibrillation diagnoses", x = element_blank()) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot2
ggsave("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/svg/af_absolute_difference_overall.svg", width = 3000, height = 2000, units = "px")

jpeg("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Chronic/jpeg/af_absolute_difference_overall.jpeg", width = 3000, height = 2000, res = 300)
print(plot2)
dev.off()


#############################
#Overall rate changes - AMI
#############################
data <- read_excel("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Excel/Master file figures and tables updated.xlsx", sheet = "4.1.1 Monthly MI & stroke data")

data_ami_crude <- data[1:3, 2:40]
data_ami_crude <- as.data.frame(t(data_ami_crude))

data_ami_crude[,4] <- rownames(data_ami_crude)
names(data_ami_crude) <- data_ami_crude[1,]
data_ami_crude <- data_ami_crude[-1,]

data_ami_as <- data[53:54, 2:40]


data_ami_as <- as.data.frame(t(data_ami_as))
names(data_ami_as) <- data_ami_as[1,]
data_ami_as <- data_ami_as[-1,]
data_ami_as$row_num <- seq.int(nrow(data_ami_as))

data_ami_as <- data_ami_as %>% 
  rename(Age_standardised_rate = Rate)

data_ami <- merge(data_ami_as, data_ami_crude)

data_ami <- data_ami %>%
  rename(Crude_rate = Rate,
         Person_time = `Person time`) %>%
  mutate(yearmonth = as.Date(paste("01", Date), format ='%d %B %Y'),
         Age_standardised_rate = as.numeric(Age_standardised_rate),
         Counts = as.numeric(Counts),
         Person_time = as.numeric(Person_time),
         Crude_rate = as.numeric(Crude_rate)) 

data_ami <- data_ami %>%
  arrange(yearmonth) %>%
  mutate(sumprod_ami = Crude_rate*Person_time,
         sumprod_ami_as = Age_standardised_rate*Person_time,
         precovid_ami = sum(sumprod_ami[yearmonth <= as.Date("2020-02-01")]) / sum(Person_time[yearmonth <= as.Date("2020-02-01")]),
         precovid_ami_as = sum(sumprod_ami_as[yearmonth <= as.Date("2020-02-01")]) / sum(Person_time[yearmonth <= as.Date("2020-02-01")]),
         moving_average_ami = ifelse(yearmonth %m-% months(1) %in% data_ami$yearmonth & yearmonth %m+% months(1) %in% data_ami$yearmonth,
                                     (sumprod_ami_as + lag(sumprod_ami_as) + lead(sumprod_ami_as)) / 
                                       (Person_time + lag(Person_time) + lead(Person_time)),
                                     NA),
         perc_diff_ami = ((moving_average_ami / precovid_ami_as)-1)*100,
         events_diff_ami = round((Crude_rate - precovid_ami) / 1000 * Person_time)) %>%
  select(yearmonth,Crude_rate, perc_diff_ami, events_diff_ami, Age_standardised_rate)

plot1 <- data_ami %>%
  ggplot(aes (x = yearmonth, y = perc_diff_ami) ) +
  geom_hline (yintercept = 0, colour = "dark grey", size = 1.25, linetype = "dashed" )+
  geom_line( size = 1.25, colour = "#00a3c7" ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, limits = c(as.Date("2019-12-01"),as.Date("2022-11-01")))+
  labs(y ="3 month rolling percentage change in age standardised rate", x = element_blank(),  colour = "Age group")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot1
ggsave("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/svg/ami_percentage_change_overall.svg", width = 3000, height = 2000, units = "px")
jpeg("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/jpeg/ami_percentage_change_overall.jpeg", width = 3000, height = 2000, res = 300)
print(plot1)
dev.off()

plot2 <- data_ami %>%
  ggplot(aes(x = yearmonth, y = events_diff_ami)) +
  geom_col(fill = "#00a3c7") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, expand = c(0, 0))+
  labs(y = "Absolute difference of acute myocardial infarction events", x = element_blank()) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot2
ggsave("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/svg/ami_absolute_difference_overall.svg", width = 3000, height = 2000, units = "px")

jpeg("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/jpeg/ami_absolute_difference_overall.jpeg", width = 3000, height = 2000, res = 300)
print(plot2)
dev.off()

#############################
#Overall rate changes - Acute Stroke
#############################
data <- read_excel("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Excel/Master file figures and tables updated.xlsx", sheet = "4.1.1 Monthly MI & stroke data")


data_stroke <- data[5:8, 2:40]

data_stroke_crude <- as.data.frame(t(data_stroke))

names(data_stroke_crude) <- data_stroke_crude[1,]
data_stroke_crude <- data_stroke_crude[-1,]

data_stroke_as <- data[56:57, 2:40]


data_stroke_as <- as.data.frame(t(data_stroke_as))
names(data_stroke_as) <- data_stroke_as[1,]
data_stroke_as <- data_stroke_as[-1,]
data_stroke_as$row_num <- seq.int(nrow(data_stroke_as))

data_stroke_as <- data_stroke_as %>% 
  rename(Age_standardised_rate = Rate)

data_stroke <- merge(data_stroke_as, data_stroke_crude)

data_stroke <- data_stroke %>%
  rename(Crude_rate = Rate,
         Person_time = `Person time`) %>%
  mutate(yearmonth = as.Date(paste("01", Date), format ='%d %B %Y'),
         Age_standardised_rate = as.numeric(Age_standardised_rate),
         Counts = as.numeric(Counts),
         Person_time = as.numeric(Person_time),
         Crude_rate = as.numeric(Crude_rate)) 

data_stroke <- data_stroke %>%
  arrange(yearmonth) %>%
  mutate(sumprod_stroke = Crude_rate*Person_time,
         sumprod_stroke_as = Age_standardised_rate*Person_time,
         precovid_stroke = sum(sumprod_stroke[yearmonth <= as.Date("2020-02-01")]) / sum(Person_time[yearmonth <= as.Date("2020-02-01")]),
         precovid_stroke_as = sum(sumprod_stroke_as[yearmonth <= as.Date("2020-02-01")]) / sum(Person_time[yearmonth <= as.Date("2020-02-01")]),
         moving_average_stroke = ifelse(yearmonth %m-% months(1) %in% data_stroke$yearmonth & yearmonth %m+% months(1) %in% data_stroke$yearmonth,
                                        (sumprod_stroke_as + lag(sumprod_stroke_as) + lead(sumprod_stroke_as)) / 
                                          (Person_time + lag(Person_time) + lead(Person_time)),
                                        NA),
         perc_diff_stroke = ((moving_average_stroke / precovid_stroke_as)-1)*100,
         events_diff_stroke = round((Crude_rate - precovid_stroke) / 1000 * Person_time)) %>%
  select(yearmonth,Crude_rate, perc_diff_stroke, events_diff_stroke, Age_standardised_rate)

plot1 <- data_stroke %>%
  ggplot(aes (x = yearmonth, y = perc_diff_stroke) ) +
  geom_hline (yintercept = 0, colour = "dark grey", size = 1.25, linetype = "dashed" )+
  geom_line( size = 1.25, colour = "#00a3c7" ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, limits = c(as.Date("2019-12-01"),as.Date("2022-11-01")))+
  labs(y ="3 month rolling percentage change in age standardised rate", x = element_blank(),  colour = "Age group")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot1
ggsave("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/svg/stroke_percentage_change_overall.svg", width = 3000, height = 2000, units = "px")
jpeg("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/jpeg/stroke_percentage_change_overall.jpeg", width = 3000, height = 2000, res = 300)
print(plot1)
dev.off()

plot2 <- data_stroke %>%
  ggplot(aes(x = yearmonth, y = events_diff_stroke)) +
  geom_col(fill = "#00a3c7") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 months", minor_breaks = NULL, expand = c(0, 0))+
  labs(y = "Absolute difference of acute stroke events", x = element_blank()) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot2
ggsave("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/svg/stroke_absolute_difference_overall.svg", width = 3000, height = 2000, units = "px")

jpeg("C:/Users/EIA/Lane Clark & Peacock LLP/CVD PRA Programme - LCP WGs - General/Working Groups/02 CVD Health/04 Analysis/Results/Figures/Acute/jpeg/stroke_absolute_difference_overall.jpeg", width = 3000, height = 2000, res = 300)
print(plot2)
dev.off()

