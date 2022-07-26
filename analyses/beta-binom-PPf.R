#load packages

require(rstan)
require(splines)
require(dplyr)
require(portalr)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
#data

plot_comp=read.csv("https://raw.githubusercontent.com/patdumandan/TraitVariation/main/data/PPdat_bmass.csv")

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
              X=plot_comp[,16:18])

dat_list3=list(N=length(plot_comp$month),
               Y=plot_comp$reproductive,
               n=plot_comp$total,
               J=length(unique(plot_comp$plot)),
               plot=plot_comp$plotID,
               trt=plot_comp$trt,
               Z=length(unique(plot_comp$trt)),
               K=3,
               X=plot_comp[,16:18])

ppf_mod1<-stan(file="D:/pdumandan/UF files/dissertation/PhD stuff/TraitVariation/analyses/PPf_simple.stan",
               iter=500,chains=3,
               data =dat_list)

ppf_mod2<-stan(file="D:/pdumandan/UF files/dissertation/PhD stuff/TraitVariation/analyses/PPf_MM.stan",
               iter=300,chains=3,
               data =dat_list3)

ppf_mod3<-stan(file="D:/pdumandan/UF files/dissertation/PhD stuff/TraitVariation/analyses/PPf_HMM.stan",
               iter=3000,chains=3,
               data =dat_list3)

ppf_mod4<-stan(file="D:/pdumandan/UF files/dissertation/PhD stuff/TraitVariation/analyses/PP_varslope.stan",
               iter=300,chains=3,
               data =dat_list3)

print(ppf_mod4, pars=c("b1"))
