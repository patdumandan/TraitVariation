PP_dat_F=Portal_clean%>%filter(sex=="F", species=="PP")

PP_F_plot4=PP_dat_F%>%
  filter(plot==4)%>%
  group_by(month, year)%>%
  summarise(total=n())

PP_F_plot4_reprod=PP_dat_F%>%
  filter(plot==4,vagina==c("S", "P", "B")| pregnant=="P" | 
           nipples==c("R", "E", "B") | lactation=="L")%>%
  group_by(month, year)%>%
  summarise(reproductive=n())

PP_F_control=PP_dat_F%>%
  filter(treatment=="control")%>%
  group_by(month, year)%>%
  summarise(total=n())

PP_F_control_reprod=PP_dat_F%>%
  filter(treatment=="control",vagina==c("S", "P", "B")| pregnant=="P" | 
           nipples==c("R", "E", "B") | lactation=="L")%>%
  group_by(month, year)%>%
  summarise(reproductive=n())

PP_4_prop=left_join(PP_F_plot4, PP_F_plot4_reprod)%>%
  mutate(proportion=reproductive/total, plot="4")

PP_control_prop=left_join(PP_F_control, PP_F_control_reprod)%>%
  mutate(proportion=reproductive/total, treatment="control")

PP_F_site=Portal_clean%>%filter(species=="PP", sex=="F")%>%
  group_by(month, year)%>%
  summarise(total=n())

PP_F_site_reprod=PP_dat_F%>%filter(vagina==c("S", "P", "B")| pregnant=="P" | 
                                     nipples==c("R", "E", "B") | lactation=="L")%>%
  group_by(month, year)%>%
  summarise(reproductive=n())

PP_site_prop=left_join(PP_F_site, PP_F_site_reprod)%>%
  mutate(proportion=reproductive/total, level="site")

PP_4_prop[is.na(PP_4_prop)] <- 0 #set non-detects to 0
PP_control_prop[is.na(PP_control_prop)] <- 0 #set non-detects to 0
PP_site_prop[is.na(PP_site_prop)] <- 0 #set non-detects to 0

#weather variables 
prod=ndvi(level="monthly", sensor="landsat", fill=TRUE)%>%mutate(year=lubridate::year(date), month=lubridate::month(date))

prod$ndvis=(prod$ndvi-mean(prod$ndvi))/(2*sd(prod$ndvi))

temp=weather(level="monthly", fill=TRUE, horizon=90)%>%
  select(year,month,meantemp, mintemp, maxtemp, precipitation, warm_precip, cool_precip)
temp$years=(temp$year-mean(temp$year))/(2*sd(temp$year))
temp$meantemps=(temp$meantemp-mean(temp$meantemp))/(2*sd(temp$meantemp))
temp$mintemps=(temp$mintemp-mean(temp$mintemp))/(2*sd(temp$mintemp))
temp$maxtemps=(temp$maxtemp-mean(temp$maxtemp))/(2*sd(temp$maxtemp))
temp$coolprecips=(temp$cool_precip-mean(temp$cool_precip))/(2*sd(temp$cool_precip))
temp$warmprecips=(temp$warm_precip-mean(temp$warm_precip))/(2*sd(temp$warm_precip))
temp$precip=(temp$precipitation-mean(temp$precipitation))/(2*sd(temp$precipitation))

temp$date=as.Date(paste(temp$year, temp$month, 01), "%Y %m %d")

envt=right_join(prod, temp)
plot4_envt=inner_join(PP_4_prop, envt)
cont_envt=inner_join(PP_control_prop, envt)
site_envt=inner_join(PP_site_prop, envt)

#LT exclosure plots####
PP_F_plot20=PP_dat_F%>%
  filter(plot==20)%>%
  group_by(month, year)%>%
  summarise(total=n())

PP_F_plot20_reprod=PP_dat_F%>%
  filter(plot==20,vagina==c("S", "P", "B")| pregnant=="P" | 
           nipples==c("R", "E", "B") | lactation=="L")%>%
  group_by(month, year)%>%
  summarise(reproductive=n())

PP_20_prop=left_join(PP_F_plot20, PP_F_plot20_reprod)%>%
  mutate(proportion=reproductive/total, plot="20")

PP_F_exclosure=PP_dat_F%>%
  filter(treatment=="exclosure")%>%
  group_by(month, year)%>%
  summarise(total=n())

PP_F_exclosure_reprod=PP_dat_F%>%
  filter(treatment=="exclosure",vagina==c("S", "P", "B")| pregnant=="P" | 
           nipples==c("R", "E", "B") | lactation=="L")%>%
  group_by(month, year)%>%
  summarise(reproductive=n())

PP_exclosure_prop=left_join(PP_F_exclosure, PP_F_exclosure_reprod)%>%
  mutate(proportion=reproductive/total, treatment="exclosure")

PP_20_prop[is.na(PP_20_prop)] <- 0 #set non-detects to 0
PP_exclosure_prop[is.na(PP_exclosure_prop)] <- 0 #set non-detects to 0

plot20_envt=inner_join(PP_20_prop, envt)
ex_envt=inner_join(PP_exclosure_prop, envt)

#visualization:
par(mfrow=c(1,3))
plot(PP_4_prop$proportion~PP_4_prop$month, ylab="proportion", xlab="month", main="plot 4")
plot(PP_control_prop$proportion~PP_control_prop$month, ylab="proportion", xlab="month", main="control")
plot(PP_site_prop$proportion~PP_site_prop$month, ylab="proportion", xlab="month", main="site")

p1=ggplot(PP_4_prop, aes(y=proportion, x=month)) +
  geom_point() +
  ylab("P(breeding)")+
  stat_smooth(method = 'gam', formula = y ~ s(x))+
  ggtitle("plot 4 (mean=0.16)")+ 
  scale_x_discrete(name="month", limits=c("Jan", "Feb", "Mar", "Apr","May", "Jun",
                                          "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))+
  theme(axis.text.x= element_text(angle = 90, vjust=0.5, hjust=1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p2=ggplot(PP_control_prop, aes(y=proportion, x=month)) +
  geom_point() +
  ylab("P(breeding)")+
  stat_smooth(method = 'gam', formula = y ~ s(x))+
  ggtitle("all controls (mean=0.13)")+ 
  scale_x_discrete(name="month", limits=c("Jan", "Feb", "Mar", "Apr","May", "Jun",
                                          "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))+
  theme(axis.text.x= element_text(angle = 90, vjust=0.5, hjust=1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p3=ggplot(PP_site_prop, aes(y=proportion, x=month)) +
  geom_point() +
  ylab("P(breeding)")+
  stat_smooth(method = 'gam', formula = y ~ s(x))+
  ggtitle("site (mean=0.14)")+ 
  scale_x_discrete(name="month", limits=c("Jan", "Feb", "Mar", "Apr","May", "Jun",
                                          "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))+
  theme(axis.text.x= element_text(angle = 90, vjust=0.5, hjust=1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggarrange(p1,p2,p3)
