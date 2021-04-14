
library(dplyr)
library(tidyr)
library(lubridate)
library(portalr)
library(ggplot2)
library(ggpubr)
####load cleaned individual-level data####
Portal_data_indiv=summarize_individual_rodents(
  path = "repo",
  clean = TRUE,
  type = "Rodents",
  length = "all",
  unknowns = FALSE,
  time = "date",
  fillweight = FALSE,
  min_plots = 1,
  min_traps = 1,
  download_if_missing = TRUE,
  quiet = FALSE
)%>%filter(!(treatment=="removal"), !is.na(treatment), !is.na(sex))


#PB DATASET####

#males####

DM=Portal_data_indiv%>%filter(species=="DM", treatment=="control", !is.na(wgt)) 
head(DM)
DM_M=Portal_data_indiv%>%filter(sex=="M", species=="DM", treatment=="control", !is.na(wgt)) 
plot(DM_F$wgt~DM_F$month)

DM_dat=DM_M%>%
  group_by(month)%>%
  summarise(mean_wgt=mean(wgt))%>%
  mutate(cv=sd(mean_wgt)/mean(mean_wgt))

inter_dat=Portal_data_indiv%>%filter(treatment=="control", !is.na(wgt))%>%
  group_by(species,month)%>%
  summarise(mean_wgt=mean(wgt))%>%
  mutate(cv=sd(mean_wgt)/mean(mean_wgt))

plot(DM_dat$cv~DM_dat$month, type="l")
