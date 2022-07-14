PP_dat_F=Portal_clean%>%filter(sex=="F", species=="PP")

PP_plot_F=PP_dat_F%>%
  group_by(plot, treatment, month, year)%>%
  summarise(total=n())

PP_plot_reprod=PP_dat_F%>%
  filter(vagina==c("S", "P", "B")| pregnant=="P" | 
           nipples==c("R", "E", "B") | lactation=="L")%>%
  group_by(plot, treatment, month, year)%>%
  summarise(reproductive=n())

PP_plot_prop=left_join(PP_plot_F, PP_plot_reprod)%>%
  mutate(proportion=reproductive/total,
         seasonID=case_when(month%in%c("12","1","2")~"1",
                            month%in%c("3","4","5")~"2",
                            month%in%c("6","7","8")~"3",
                            month%in%c("9","10","11")~"4"),
         PB_period=case_when(year%in%c(1979:1994)~"1",
                             year%in%c(1995:2010)~"2",
                             year%in%c(2013:2014)~"3"),
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

PP_plot_prop[is.na(PP_plot_prop)] <- 0 #set non-detects to 0

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
  group_by(month, year, treatment,plot)%>%
  summarise(bmass_PP=sum(PP))

PB_bmass=bmass%>%select(PB, plot, treatment, month, year, date)%>%
  group_by(month, year, treatment,plot)%>%
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

plot_comp[is.na(plot_comp)] <- 0 #set non-detects to 0

str(plot_comp)
