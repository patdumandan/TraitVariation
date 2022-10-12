ppcid=PP_dat_F%>%filter(treatment=="control")%>%select(-testes, -hfl, -wgt,-note2,-note5,-species,
                                                       -sex,-reprod)%>%
  mutate(reprodstat= case_when(
    vagina==c("S", "P", "B")~"1",
    pregnant=="P"~"1",
    lactation=="L"~"1",
    nipples==c("R", "E", "B")~"1"))

ppcid$reprodstat[is.na(ppcid$reprodstat)]=0
ppcid$reprodstat=as.integer(ppcid$reprodstat)

ppeid=PP_dat_F%>%filter(treatment=="exclosure")%>%select(-testes, -hfl, -wgt,-note2,-note5,-species,
                                                       -sex,-reprod)%>%
  mutate(reprodstat= case_when(
    vagina==c("S", "P", "B")~"1",
    pregnant=="P"~"1",
    lactation=="L"~"1",
    nipples==c("R", "E", "B")~"1"))

ppeid$reprodstat[is.na(ppeid$reprodstat)]=0
ppeid$reprodstat=as.integer(ppeid$reprodstat)

#rolling window of biomass data####
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
  select(censusdate,treatment,plot,ID,DM,rs2, rs3, rs4, rs5, rs6)%>%rename(date=censusdate)
DM_bmass[is.na(DM_bmass)] <- 0


a1=right_join(DM_bmass, ppcid, by=c("treatment", "plot", "date"))      
a1$reprodstat=as.character(a1$reprodstat)

 require(ggpubr)
 p1=ggplot(data=a1, aes(x=reprodstat, y=DM))+geom_boxplot()+geom_jitter(col="blue")
 p2=ggplot(data=a1, aes(x=reprodstat, y=rs2))+geom_boxplot()+geom_jitter(col="blue")
 p3=ggplot(data=a1, aes(x=reprodstat, y=rs3))+geom_boxplot()+geom_jitter(col="blue")
 p4=ggplot(data=a1, aes(x=reprodstat, y=rs4))+geom_boxplot()+geom_jitter(col="blue")
 p5=ggplot(data=a1, aes(x=reprodstat, y=rs5))+geom_boxplot()+geom_jitter(col="blue")
 p6=ggplot(data=a1, aes(x=reprodstat, y=rs6))+geom_boxplot()+geom_jitter(col="blue")

 ggarrange(p1,p2,p3,p4,p5,p6)
 
 #pb biomass
 
 PB_bmass=seas_bmass%>%group_by(plot, treatment)%>%
   filter(!year<1995)%>%mutate(ID=row_number(), rs3=rollsum(PB, 3, fill=NA, align="right"),
                               rs2=rollsum(PB, 2, fill=NA, align="right"),
                               rs4=rollsum(PB, 4, fill=NA, align="right"),
                               rs5=rollsum(PB, 5, fill=NA, align="right"),
                               rs6=rollsum(PB, 6, fill=NA, align="right"))%>%
   select(censusdate,treatment,plot,ID,PB,rs2, rs3, rs4, rs5, rs6)%>%rename(date=censusdate)
 
 PB_bmass[is.na(PB_bmass)] <- 0
 
 
 a2=right_join(PB_bmass, ppeid, by=c("treatment", "plot", "date"))      
 a2$reprodstat=as.character(a2$reprodstat)
 
 s1=ggplot(data=a2, aes(x=reprodstat, y=PB))+geom_boxplot()+geom_jitter(col="blue")
 s2=ggplot(data=a2, aes(x=reprodstat, y=rs2))+geom_boxplot()+geom_jitter(col="blue")
 s3=ggplot(data=a2, aes(x=reprodstat, y=rs3))+geom_boxplot()+geom_jitter(col="blue")
 s4=ggplot(data=a2, aes(x=reprodstat, y=rs4))+geom_boxplot()+geom_jitter(col="blue")
 s5=ggplot(data=a2, aes(x=reprodstat, y=rs5))+geom_boxplot()+geom_jitter(col="blue")
 s6=ggplot(data=a2, aes(x=reprodstat, y=rs6))+geom_boxplot()+geom_jitter(col="blue")
 
 ggarrange(s1,s2,s3,s4,s5,s6)
 indiv=ppcid%>%
   select(period, tag, reprodstat)%>%
   pivot_wider(names_from=period, values_from=reprodstat, values_fill = 0)%>%
   mutate(encounters=rowSums(.[2:176]))
 
 multindiv=indiv%>%filter(encounters>1)%>%select(tag, encounters)
 
 pcid=left_join(multindiv, ppcid)
 
 a1=right_join(DM_bmass, pcid, by=c("treatment", "plot", "date"))%>%mutate(month=month(date))     
 a1$reprodstat=as.character(a1$reprodstat)
 
 require(ggpubr)
 p1=ggplot(data=a1, aes(x=reprodstat, y=DM))+geom_boxplot()+geom_jitter(col="blue")
 p2=ggplot(data=a1, aes(x=reprodstat, y=rs2))+geom_boxplot()+geom_jitter(col="blue")
 p3=ggplot(data=a1, aes(x=reprodstat, y=rs3))+geom_boxplot()+geom_jitter(col="blue")
 p4=ggplot(data=a1, aes(x=reprodstat, y=rs4))+geom_boxplot()+geom_jitter(col="blue")
 p5=ggplot(data=a1, aes(x=reprodstat, y=rs5))+geom_boxplot()+geom_jitter(col="blue")
 p6=ggplot(data=a1, aes(x=reprodstat, y=rs6))+geom_boxplot()+geom_jitter(col="blue")
 
 ggarrange(p1,p2,p3,p4,p5,p6)
 
 a2=right_join(PB_bmass, pcid, by=c("treatment", "plot", "date"))%>%mutate(month=month(date))       
 a2$reprodstat=as.character(a2$reprodstat)
 
 s1=ggplot(data=a2, aes(x=reprodstat, y=PB))+geom_boxplot()+geom_jitter(col="blue")
 s2=ggplot(data=a2, aes(x=reprodstat, y=rs2))+geom_boxplot()+geom_jitter(col="blue")
 s3=ggplot(data=a2, aes(x=reprodstat, y=rs3))+geom_boxplot()+geom_jitter(col="blue")
 s4=ggplot(data=a2, aes(x=reprodstat, y=rs4))+geom_boxplot()+geom_jitter(col="blue")
 s5=ggplot(data=a2, aes(x=reprodstat, y=rs5))+geom_boxplot()+geom_jitter(col="blue")
 s6=ggplot(data=a2, aes(x=reprodstat, y=rs6))+geom_boxplot()+geom_jitter(col="blue")
 
 ggarrange(s1,s2,s3,s4,s5,s6)
 
 ppeid=PP_dat_F%>%filter(treatment=="exclosure")%>%select(-testes, -hfl, -wgt,-note2,-note5,-species,
                                                          -sex,-reprod)%>%
   mutate(reprodstat= case_when(
     vagina==c("S", "P", "B")~"1",
     pregnant=="P"~"1",
     lactation=="L"~"1",
     nipples==c("R", "E", "B")~"1"))
 
 #EXCLOSURE PPS-PB biomass###
 
 ppeid$reprodstat[is.na(ppeid$reprodstat)]=0
 ppeid$reprodstat=as.integer(ppeid$reprodstat)
 
 indiv_e=ppeid%>%
   select(period, tag, reprodstat)%>%
   pivot_wider(names_from=period, values_from=reprodstat, values_fill = 0)%>%
   mutate(encounters=rowSums(.[2:172]))
 
 multindiv_e=indiv_e%>%filter(encounters>1)%>%select(tag, encounters)
 
 peid=left_join(multindiv_e, ppeid)
 
 b1=right_join(PB_bmass, peid, by=c("treatment", "plot", "date"))%>%mutate(month=month(date))       
 b1$reprodstat=as.character(b1$reprodstat)
 
 require(ggpubr)
 r1=ggplot(data=b1, aes(x=reprodstat, y=PB))+geom_boxplot()+geom_jitter(col="blue")
 r2=ggplot(data=b1, aes(x=reprodstat, y=rs2))+geom_boxplot()+geom_jitter(col="blue")
 r3=ggplot(data=b1, aes(x=reprodstat, y=rs3))+geom_boxplot()+geom_jitter(col="blue")
 r4=ggplot(data=b1, aes(x=reprodstat, y=rs4))+geom_boxplot()+geom_jitter(col="blue")
 r5=ggplot(data=b1, aes(x=reprodstat, y=rs5))+geom_boxplot()+geom_jitter(col="blue")
 r6=ggplot(data=b1, aes(x=reprodstat, y=rs6))+geom_boxplot()+geom_jitter(col="blue")
 
 ggarrange(r1,r2,r3,r4,r5,r6)
 
 
 