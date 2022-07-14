require(portalr)
require(dplyr)
require(tidyr)
require(ggplot2)
require(dotwhisker)
library(lubridate)
library(reshape2)
library(ggpubr)

source("https://raw.githubusercontent.com/patdumandan/ReproPhenology/main/RScripts/data_cleaning_functions_Supp.R")

Portal_data=summarize_individual_rodents(
  clean = TRUE,
  type = "Rodents",
  length = "all",
  unknowns = FALSE,
  fillweight = FALSE,
  min_plots = 1,
  min_traps = 1,
  download_if_missing = TRUE,
  quiet = FALSE
)%>%filter(!is.na(sex),!(treatment=="spectabs"), !(year<1988), !(year>2014), 
           plot %in%c(1, 2, 4, 8, 9, 11, 12, 14, 17, 22,3, 6, 13, 15, 18, 19, 20, 21))
#Note: 18 plots included based on Ellen's paper

#add note5 column to filter out dead indivs####

Portal_rodent=read.csv("https://raw.githubusercontent.com/weecology/PortalData/main/Rodents/Portal_rodent.csv")
Portal_data_indiv=left_join(Portal_data, Portal_rodent)%>%
  select(period, month, day, year, treatment, plot, stake, species, sex, reprod, age, testes, vagina, pregnant, nipples, lactation,
         hfl, wgt,tag,note2, note5)

#assign tag IDs for untagged individuals (0 and NA in tag column)####

all_tag=id_unknowns(Portal_data_indiv, 19) 

#find and remove bad periods (periods with only one day of trapping)####

Portal_rodent_trapping= read.csv("https://raw.githubusercontent.com/weecology/PortalData/main/Rodents/Portal_rodent_trapping.csv")
tdat=Portal_rodent_trapping%>%group_by(period)%>%summarise(count=sum(sampled))%>%arrange(count)
bad_periods <- filter(tdat, count < 20) #based on Sarah's code
bad_periods <- as.list(bad_periods$period)

Portal_no_badperiod=all_tag[-which(all_tag$period %in%bad_periods),]%>%
  mutate(tag=as.character(tag)) #necessary so function for starred and duplicate tags will not break

#check quality of tags####

#make sure that records with * in note2 are recognized as new individuals

tags = unique(Portal_no_badperiod$tag)
star_tags = starred_tags(dat=Portal_no_badperiod, tags=tags, spp_col=8, tag_col=19)

#locate dead individuals
tags=unique(star_tags$tag)
dead_dat=is_dead(dat=star_tags, tags=tags, spp_col=8, tag_col=19)

# locate records of duplicated tags
tags=unique(dead_dat$tag)
dup_dat= is_duplicate_tag(dat=dead_dat, tags=tags, spp_col=8, tag_col=19) #returns a list of 2
duptags = unique(dup_dat$bad$tag)
no_dup = dup_dat$data[-which(dup_dat$data$tag %in% duptags),] #delete rows flagged as duplicates without clear resolution

# identify records where multiple indivs share same tag in same period
tags = unique(no_dup$tag)
same = same_period(no_dup, tags)

#eliminate tags that appear more than once in the same period - questionable data
sametags = unique(same$tag)
Portal_no_same= no_dup[-which(no_dup$tag %in% sametags),]

# "clean" data

Portal_clean=subsetDat(Portal_no_same)
