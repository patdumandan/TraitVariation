#load packages

require(rstan)
require(splines)
require(dplyr)
require(portalr)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

dat_list=list(N=length(plot_comp$month),
              Y=plot_comp$reproductive,
              n=plot_comp$total,
              K=3,
              X=plot_comp[,15:17])

dat_list2=list(N=length(plot_comp$month),
              Y=plot_comp$reproductive,
              n=plot_comp$total,
              J=length(unique(plot_comp$plot)),
              plot=plot_comp$plotID,
              K=3,
              X=plot_comp[,15:17])

dat_list3=list(N=length(plot_comp$month),
               Y=plot_comp$reproductive,
               n=plot_comp$total,
               J=length(unique(plot_comp$plot)),
               plot=plot_comp$plotID,
               trt=plot_comp$trt,
               Z=length(unique(plot_comp$trt)),
               K=3,
               X=plot_comp[,15:17])

ppf_mod1<-stan(file="D:/pdumandan/UF files/dissertation/PhD stuff/TraitVariation/analyses/PPf_simple.stan",
               iter=500,chains=3,
               data =dat_list)

ppf_mod2<-stan(file="D:/pdumandan/UF files/dissertation/PhD stuff/TraitVariation/analyses/PPf_MM.stan",
               iter=300,chains=3,
               data =dat_list2)

ppf_mod3<-stan(file="D:/pdumandan/UF files/dissertation/PhD stuff/TraitVariation/analyses/PPf_MM.stan",
               iter=300,chains=3,
               data =dat_list3)

posterior_d=extract(ppf_mod2)$delta

stan_plot(ppf_con1, pars=c('betas'))

#plot
#now we could look at the variation in the regression coefficients between the groups doing caterpillar plots
posterior=extract(ppf_mod3)
ind_coeff<-apply(posterior$delta,c(2,3),quantile,probs=c(0.025,0.5,0.975))
df_ind_coeff<-data.frame(Coef=rep(c("X1","X2","X3"),each=12),LI=c(ind_coeff[1,,1],ind_coeff[1,,2],ind_coeff[1,,3]),Median=c(ind_coeff[2,,1],ind_coeff[2,,2],ind_coeff[2,,3]),HI=c(ind_coeff[3,,1],ind_coeff[3,,2],ind_coeff[3,,3]))
gr<-paste("Plot",unique(plot_comp$plot))
df_ind_coeff$Plot<-factor(gr,levels=gr)
#we may also add the population-level median estimate
pop_lvl<-data.frame(Coeff=c("X1","X2","X3"),
                    Median=apply(posterior$betas,2,quantile,probs=0.5))

ggplot(df_ind_coeff,aes(x=Plot,y=Median))+geom_point()+
  geom_linerange(aes(ymin=LI,ymax=HI))+coord_flip()+
  facet_grid(.~Coef)+geom_hline(yintercept=0, linetype="dashed")+
  # geom_hline(data=pop_lvl,aes(yintercept=Median),color="blue",linetype="dashed")+
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

print(ppf_mod3, pars=c("b2", "b1", "sigma", "tau"))
