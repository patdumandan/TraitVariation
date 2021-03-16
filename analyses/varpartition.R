require(portalr)
require(dplyr)

Portal_data_indiv=summarize_individual_rodents(
  path = get_default_data_path(),
  clean = TRUE,
  type = "Rodents",
  length = "all",
  unknowns = FALSE,
  time = "date",
  fillweight = FALSE,
  min_plots = 1,
  min_traps = 1,
  download_if_missing = TRUE,
  quiet = FALSE)
  
Portal_data_indiv=Portal_data_indiv%>%
  filter(!(treatment=="removal")& !is.na(treatment)& !is.na(sex) & !is.na(wgt))

#calculate interspecific trait variation across all species (remove juveniles)
#control plots (females only)
#monthly ITV and BTV####

#variance partitioning
require(ape)
require(nlme)
portal_control=Portal_data_indiv%>%
  filter(treatment=="control")%>%
  mutate(season=ifelse(month <=6, 1,
  ifelse(month >= 7, 2, NA)))

vcomp=varcomp(lme(wgt~1, random=~(1|month/season/year), data=portal_control, na.action=na.omit),1)
plot(vcomp)

control_inter_F=Portal_data_indiv%>%
  filter(treatment=="control", sex=="F")%>%
  group_by(species, month)%>%
  summarise(mean_wgt=mean(wgt))

month_inter=as.data.frame(portal_control)

require(raster)
cv(month_inter$mean_wgt) # 94.2

control_intra_F=Portal_data_indiv%>%
  filter(treatment=="control", sex=="F")%>%
  group_by(species, month)%>% 
  summarise(mean_wgt=mean(wgt))%>%
  mutate(cv=sd(mean_wgt)/mean(mean_wgt)*100)

mean(unique(control_intra_F$cv)) #11.08


