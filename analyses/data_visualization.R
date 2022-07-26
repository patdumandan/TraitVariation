require(ggplot2)

#spatial####
sp=ggplot(plot_comp, aes(x=bmass_PB, y=proportion, col=treatment))+ggtitle("plot")+
  geom_point()+geom_smooth(method="lm")+facet_wrap(~plot)+xlab("PB biomass")

st=ggplot(plot_comp, aes(x=bmass_PB, y=proportion, col=treatment))+
  geom_point()+geom_smooth(method="lm")+xlab("PB biomass")+ggtitle("treatment")

#temporal####
ggplot(plot_comp, aes(x=bmass_PB, y=proportion))+
  geom_point()+geom_smooth(method="gam")+facet_wrap(~month)+ggtitle("month")

ggplot(plot_comp, aes(x=bmass_PB, y=proportion))+
  geom_point()+geom_smooth(method="gam")+facet_wrap(~PB_period)+ggtitle("PB period")

#temporalxspatial####
ggplot(plot_comp, aes(x=bmass_PB, y=proportion, col=treatment))+
  geom_point()+geom_smooth(method="lm")+facet_wrap(~month)+ggtitle("month X treatment")

ggplot(plot_comp, aes(x=bmass_PB, y=proportion, col=plot))+
  geom_point()+geom_smooth(method="lm")+facet_wrap(~month)+ggtitle("month X treatment")


#plot
#now we could look at the variation in the regression coefficients between the groups doing caterpillar plots
posterior=extract(ppf_mod4)
ind_coeff<-apply(posterior$delta,c(2,3),quantile,probs=c(0.025,0.5,0.975))
df_ind_coeff<-data.frame(Coeff=rep(c("PB biomass","PP biomass","krat biomass"),each=12),LI=c(ind_coeff[1,,1],ind_coeff[1,,2],ind_coeff[1,,3]),Median=c(ind_coeff[2,,1],ind_coeff[2,,2],ind_coeff[2,,3]),HI=c(ind_coeff[3,,1],ind_coeff[3,,2],ind_coeff[3,,3]))
gr<-paste("Plot",unique(plot_comp$plot))
df_ind_coeff$Plot<-factor(gr,levels=gr)
#we may also add the population-level median estimate
pop_lvl<-data.frame(Coeff=c("PB biomass","PP biomass","krat biomass"),
                    Median=apply(posterior$gamma,2,quantile,probs=0.5))

ggplot(df_ind_coeff,aes(x=Plot,y=Median))+geom_point()+
  geom_linerange(aes(ymin=LI,ymax=HI))+coord_flip()+
  facet_grid(.~Coeff)+
   geom_hline(data=pop_lvl,aes(yintercept=Median),color="blue",linetype="dashed")+
  labs(y="Effect size")

#treatment
posterior=extract(ppf_mod3)
ind_coeff<-apply(posterior$b2,c(2,3),quantile,probs=c(0.025,0.5,0.975))
df_ind_coeff<-data.frame(Coef=rep(c("X1","X2","X3"),each=2),LI=c(ind_coeff[1,,1],ind_coeff[1,,2],ind_coeff[1,,3]),Median=c(ind_coeff[2,,1],ind_coeff[2,,2],ind_coeff[2,,3]),HI=c(ind_coeff[3,,1],ind_coeff[3,,2],ind_coeff[3,,3]))
gr<-paste("Treatment",unique(plot_comp$treatment))
df_ind_coeff$Trt<-factor(gr,levels=gr)
#we may also add the population-level median estimate
pop_lvl<-data.frame(Coeff=c("X1","X2","X3"),
                    Median=apply(posterior$b2,3,quantile,probs=0.5))

ggplot(df_ind_coeff,aes(x=Trt,y=Median))+geom_point()+
  geom_linerange(aes(ymin=LI,ymax=HI))+coord_flip()+
  facet_grid(.~Coef)+geom_hline(yintercept=0, linetype="dashed")+
  # geom_hline(data=pop_lvl,aes(yintercept=Median),color="blue",linetype="dashed")+
  labs(y="Effect size")

