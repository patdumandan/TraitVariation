require(portalr)
require(dplyr)
require(tidyr)
require(ggplot2)
require(dotwhisker)
library(lubridate)
library(reshape2)
library(ggpubr)

#RODENT FULL DATA####

#obtain full rodent data
Portal_rodent=read.csv("https://raw.githubusercontent.com/weecology/PortalData/main/Rodents/Portal_rodent.csv")
Portal_plots=read.csv("D:/Dropbox (UFL)/PhD-stuff/TraitVariation/PortalData/SiteandMethods/Portal_plots.csv")

Portal_data_indiv=left_join(Portal_rodent, Portal_plots)
PP_dat=Portal_data_indiv%>%filter(!year<1988, !year>2014, species=="PP", sex=="F")

#obtain control data
ppcid=PP_dat%>%filter(treatment=="control")%>%select(-testes, -hfl, -wgt,-note2,-note5,-species,
                                                       -sex,-reprod)%>%
  mutate(reprodstat= case_when(
    vagina==c("S", "P", "B")~"1",
    pregnant=="P"~"1",
    lactation=="L"~"1",
    nipples==c("R", "E", "B")~"1"))%>%select(-resourcetreatment, -anttreatment, -neststk)

ppcid$reprodstat[is.na(ppcid$reprodstat)]=0
ppcid$reprodstat=as.integer(ppcid$reprodstat)

#obtain control data
ppeid=PP_dat%>%filter(treatment=="exclosure")%>%select(-testes, -hfl, -wgt,-note2,-note5,-species,
                                                     -sex,-reprod)%>%
  mutate(reprodstat= case_when(
    vagina==c("S", "P", "B")~"1",
    pregnant=="P"~"1",
    lactation=="L"~"1",
    nipples==c("R", "E", "B")~"1"))%>%select(-resourcetreatment, -anttreatment, -neststk)

ppeid$reprodstat[is.na(ppeid$reprodstat)]=0
ppeid$reprodstat=as.integer(ppeid$reprodstat)

#get rolling window cumulative amount of biomass of DMs and PBs
seas_bmass=biomass(level="Plot", type="Rodents",
                   clean=TRUE, plots="all", time="date", shape="crosstab")%>%
  mutate(month=month(censusdate), date=day(censusdate), year=year(censusdate))%>%
  filter(!(year<1979), !(year>2014), !(treatment %in%c("spectabs", "removal")))

require(zoo)
DM_bmass=seas_bmass%>%group_by(plot, treatment)%>%mutate(ID=row_number(), rs3=rollsum(DM, 3, fill=NA, align="right"),
                                                         rs2=rollsum(DM, 2, fill=NA, align="right"),
                                                         rs4=rollsum(DM, 4, fill=NA, align="right"),
                                                         rs5=rollsum(DM, 5, fill=NA, align="right"),
                                                         rs6=rollsum(DM, 6, fill=NA, align="right"))%>%
  select(censusdate,treatment,plot,ID,DM,rs2, rs3, rs4, rs5, rs6)%>%rename(date=censusdate)%>%
  mutate(month=month(date), year=year(date)) 
DM_bmass[is.na(DM_bmass)] <- 0

#get capture history of rodents with multiple captures (>1)
p1=ppcid%>%group_by(id)%>%summarise(encounters=sum(reprodstat))%>%
  filter(encounters>1)%>%filter(!id=="")
p2=left_join(p1, ppcid, by="id")%>%select(period, id, reprodstat)%>%filter(!period<201)

p2$reprodstat=as.numeric(p2$reprodstat)

p3=left_join(p1, ppcid, by="id")%>%filter(!period<201)%>%mutate(date=as.Date(month, day,year))

indiv_history=p2%>%arrange(period)%>%
  pivot_wider(names_from=period, values_from=reprodstat, values_fill = 0)%>%
  mutate(encounters=rowSums(.[2:124]))

a1=right_join(DM_bmass, p3, by=c("treatment", "plot", "month", "year"))%>%
  select(treatment,plot,month, year, id,reprodstat,DM,rs2, rs3, rs4, rs5, rs6)
a1$reprodstat=as.character(a1$reprodstat)

s1=ggplot(data=a1, aes(x=reprodstat, y=DM))+geom_boxplot()+theme_classic()
s2=ggplot(data=a1, aes(x=reprodstat, y=rs2))+geom_boxplot()+theme_classic()
s3=ggplot(data=a1, aes(x=reprodstat, y=rs3))+geom_boxplot()+theme_classic()
s4=ggplot(data=a1, aes(x=reprodstat, y=rs4))+geom_boxplot()+theme_classic()
s5=ggplot(data=a1, aes(x=reprodstat, y=rs5))+geom_boxplot()+theme_classic()
s6=ggplot(data=a1, aes(x=reprodstat, y=rs6))+geom_boxplot()+theme_classic()
dm1=ggarrange(s1,s2,s3,s4,s5,s6)

annotate_figure(dm1, top = text_grob("PP reproductive status~ DM biomass at different lags", 
                                     face = "bold", size = 14))

#pb biomass

PB_bmass=seas_bmass%>%group_by(plot, treatment)%>%
  filter(!year<1995)%>%mutate(ID=row_number(), rs3=rollsum(PB, 3, fill=NA, align="right"),
                              rs2=rollsum(PB, 2, fill=NA, align="right"),
                              rs4=rollsum(PB, 4, fill=NA, align="right"),
                              rs5=rollsum(PB, 5, fill=NA, align="right"),
                              rs6=rollsum(PB, 6, fill=NA, align="right"))%>%
  select(censusdate,treatment,plot,ID,PB,rs2, rs3, rs4, rs5, rs6)%>%
  rename(date=censusdate)%>%mutate(month=month(date), year=year(date)) 

PB_bmass[is.na(PB_bmass)] <- 0

a2=right_join(PB_bmass, p3, by=c("treatment", "plot", "month", "year"))%>%
  select(treatment,plot,month, year, id,reprodstat,PB,rs2, rs3, rs4, rs5, rs6)
a2$reprodstat=as.character(a2$reprodstat)

q1=ggplot(data=a2, aes(x=reprodstat, y=PB))+geom_boxplot()+theme_classic()
q2=ggplot(data=a2, aes(x=reprodstat, y=rs2))+geom_boxplot()+theme_classic()
q3=ggplot(data=a2, aes(x=reprodstat, y=rs3))+geom_boxplot()+theme_classic()
q4=ggplot(data=a2, aes(x=reprodstat, y=rs4))+geom_boxplot()+theme_classic()
q5=ggplot(data=a2, aes(x=reprodstat, y=rs5))+geom_boxplot()+theme_classic()
q6=ggplot(data=a2, aes(x=reprodstat, y=rs6))+geom_boxplot()+theme_classic()
pbr=ggarrange(q1,q2,q3,q4,q5,q6)

annotate_figure(pbr, top = text_grob("PP reproductive status~ PB biomass at different lags (control)", 
                                       face = "bold", size = 14))

#get capture history of rodents with multiple captures (>1)
z1=ppeid%>%group_by(id)%>%summarise(encounters=sum(reprodstat))%>%
  filter(encounters>1)%>%filter(!id=="")
z2=left_join(z1, ppeid, by="id")%>%select(period, id, reprodstat)%>%filter(!period<201)

z2$reprodstat=as.integer(z2$reprodstat)

z3=left_join(z1, ppeid, by="id")%>%filter(!period<201)%>%mutate(date=as.Date(month, day,year))

indiv_history_e=z2%>%arrange(period)%>%
  pivot_wider(names_from=period, values_from=reprodstat, values_fill = 0)%>%
  mutate(encounters=rowSums(.[2:152]))

a11=right_join(PB_bmass, z3, by=c("treatment", "plot", "month", "year"))%>%
  select(treatment,plot,month, year, id,reprodstat,PB,rs2, rs3, rs4, rs5, rs6)
a11$reprodstat=as.character(a11$reprodstat)

s11=ggplot(data=a11, aes(x=reprodstat, y=PB))+geom_boxplot()+theme_classic()
s21=ggplot(data=a11, aes(x=reprodstat, y=rs2))+geom_boxplot()+theme_classic()
s31=ggplot(data=a11, aes(x=reprodstat, y=rs3))+geom_boxplot()+theme_classic()
s41=ggplot(data=a11, aes(x=reprodstat, y=rs4))+geom_boxplot()+theme_classic()
s51=ggplot(data=a11, aes(x=reprodstat, y=rs5))+geom_boxplot()+theme_classic()
s61=ggplot(data=a11, aes(x=reprodstat, y=rs6))+geom_boxplot()+theme_classic()
pb1=ggarrange(s11,s21,s31,s41,s51,s61)

annotate_figure(pb1, top = text_grob("PP reproductive status~ PB biomass at different lags (exclosure)", 
                                     face = "bold", size = 14))
