PP_dat_F=Portal_clean%>%filter(sex=="F", species=="PP")%>%
  mutate(date=make_date(year, month, day))

PP_plot_F=PP_dat_F%>%
  group_by(plot, treatment, month, year, date)%>%
  summarise(total=n())

PP_plot_reprod=PP_dat_F%>%
  filter(vagina==c("S", "P", "B")| pregnant=="P" | 
           nipples==c("R", "E", "B") | lactation=="L")%>%
  group_by(plot, treatment, month, year, date)%>%
  summarise(reproductive=n())

PP_plot_prop=left_join(PP_plot_F, PP_plot_reprod)%>%
  mutate(proportion=reproductive/total,
         seasonID=case_when(month%in%c("12","1","2")~"1",
                            month%in%c("3","4","5")~"2",
                            month%in%c("6","7","8")~"3",
                            month%in%c("9","10","11")~"4"),
         PB_period=case_when(year%in%c(1979:1994)~"1",
                             year%in%c(1995:2010)~"2",
                             year%in%c(2011:2014)~"3"),
         plotID=case_when(plot==2~"1",
                          plot==3~"2",
                          plot==4~"3",
                          plot==8~"4",
                          plot==11~"5",
                          plot==12~"6",
                          plot==14~"7",
                          plot==15~"8",
                          plot==17~"9",
                          plot==19~"10",
                          plot==21~"11",
                          plot==22~"12"))

PP_plot_prop[is.na(PP_plot_prop)] <- 0 
#write.csv(PP_plot_prop, "PPdat_date.csv")

#biomass###

bmass=biomass(level="Plot", type="Rodents",
              clean=TRUE, plots="all", time="date", shape="crosstab")%>%
  mutate(month=month(censusdate), date=day(censusdate), year=year(censusdate))%>%
  filter(!(year<1979), !(year>2014), !(treatment %in%c("spectabs", "removal")))

DO_bmass=bmass%>%select(DO, plot, treatment, month, year, date)%>%
  group_by(month, year, treatment,plot)%>%
  summarise(bmass_DO=sum(DO))

DS_bmass=bmass%>%select(DS, plot, treatment, month, year, date)%>%
  group_by(month, year, treatment,plot)%>%
  summarise(bmass_DS=sum(DS))

DM_bmass=bmass%>%select(DM, plot, treatment, month, year, date)%>%
  group_by(month, year, treatment,plot)%>%
  summarise(bmass_DM=sum(DM))

PP_bmass=bmass%>%select(PP, plot, treatment, month, year, date)%>%
  group_by(month, year, censusdate, treatment,plot)%>%
  summarise(bmass_PP=sum(PP))

PB_bmass=bmass%>%select(PB, plot, treatment, month, year, date)%>%
  group_by(month, year, date, treatment,plot)%>%
  summarise(bmass_PB=sum(PB))

all_bmass11=left_join(PP_plot_prop,DM_bmass, by=c("month", "year", "treatment", "plot"))
all_bmass12=left_join(all_bmass11,DO_bmass, by=c("month", "year", "treatment", "plot"))
all_bmass13=left_join(all_bmass12,DS_bmass, by=c("month", "year", "treatment", "plot"))
all_bmass2=left_join(all_bmass13,PB_bmass, by=c("month", "year", "treatment", "plot"))
all_bmass3=left_join(all_bmass2,PP_bmass, by=c("month", "year", "treatment", "plot"))%>%
  rowwise()%>%
  mutate(bmass_krat=sum(c_across(bmass_DM:bmass_DS), na.rm=T))%>%ungroup()%>%
  select(-bmass_DM, -bmass_DO, -bmass_DS)

plot_comp=all_bmass3%>%
  relocate(plotID, .after=plot)%>%
  relocate(c(seasonID, PB_period), .after=month)%>%
  relocate(year, .before=month)

plot_comp=as.data.frame(plot_comp)

plot_comp$plotID=as.integer(plot_comp$plotID)
plot_comp$trt=as.integer(as.factor(plot_comp$treatment))
plot_comp$seasonID=as.integer(as.factor(plot_comp$seasonID))
plot_comp$PB_period=as.integer(as.factor(plot_comp$PB_period))

#STANDARDIZE####
plot_comp$bmass_PBs=scale(plot_comp$bmass_PB)
plot_comp$bmass_PPs=scale(plot_comp$bmass_PP)
plot_comp$bmass_krats=scale(plot_comp$bmass_krat)

plot_comp[is.na(plot_comp)] <- 0 #set non-detects to 0

str(plot_comp)
#write.csv(plot_comp, "PPdat_bmass.csv")

#rolling window of biomass data####
seas_bmass=biomass(level="Plot", type="Rodents",
                   clean=TRUE, plots="all", time="date", shape="crosstab")%>%
  mutate(month=month(censusdate), date=day(censusdate), year=year(censusdate))%>%
  filter(!(year<1979), !(year>2014), !(treatment %in%c("spectabs", "removal")))

#sample with plot 2
s1=seas_bmass%>%filter(plot==2)%>%select(censusdate, treatment, plot, PB)%>%mutate(ID=row_number())
s3=rollsum(s1$PB, 3, fill=NA, align="right")
s2=cbind(s1,s3)
s2[is.na(s2)] <- 0

#all data####

all_bmass=seas_bmass%>%group_by(plot, treatment)%>%mutate(ID=row_number(), rs=rollsum(PB, 3, fill=NA, align="right"))%>%
  select(censusdate,treatment,plot,ID,PB,rs)%>%rename(date=censusdate)
all_bmass[is.na(all_bmass)] <- 0

pc=left_join(PP_plot_prop, all_bmass)
pc=pc%>%rename(mon_PB=PB, roll_PB=rs)%>%relocate(c(seasonID, PB_period, ID), .after=date)%>%select(-plotID)
pc[is.na(pc)] <- 0

#write.csv(pc, "PPscale_data.csv")
