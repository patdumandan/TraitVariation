#standardize data####

#exploratory work####
#LT control plots####

pp_plot4_mod=mgcv::gam(proportion~s(month,bs="cc")+s(year)+ndvis+meantemps+warmprecips+coolprecips, data=plot4_envt, method = 'REML', weights = total, family = binomial)
summary(pp_plot4_mod)

pp_cont_mod=mgcv::gam(proportion~s(month,bs="cc")+s(year)+ndvis+meantemps+warmprecips+coolprecips, data=cont_envt, method = 'REML', weights = total, family = binomial)
summary(pp_cont_mod)

pp_site_mod=mgcv::gam(proportion~s(month,bs="cc")+s(year)+ndvis+meantemps+warmprecips+coolprecips, data=site_envt, method = 'REML', weights = total, family = binomial)
summary(pp_site_mod)

dwplot(list(pp_plot4_mod, pp_cont_mod, pp_site_mod))

#at increasing levels of spatial scale, uncertainty in the effect of a predictor decreases
#at this scale where local interactions occur, there could be higher variation?
#in which the actual individuals change seasonally/annually, you will expect
#higher variation in response to factors 

#LT exclosure plots####
pp_plot20_mod=mgcv::gam(proportion~s(month,bs="cc")+s(year)+ndvis+meantemps+warmprecips+coolprecips, data=plot20_envt, method = 'REML', weights = total, family = binomial)
summary(pp_plot20_mod)

pp_ex_mod=mgcv::gam(proportion~s(month,bs="cc")+s(year)+ndvis+meantemps+warmprecips+coolprecips, data=ex_envt, method = 'REML', weights = total, family = binomial)
summary(pp_ex_mod)

pp_site_mod=mgcv::gam(proportion~s(month,bs="cc")+s(year)+ndvis+meantemps+warmprecips+coolprecips, data=site_envt, method = 'REML', weights = total, family = binomial)
summary(pp_site_mod)

dwplot(list(pp_plot20_mod, pp_ex_mod, pp_site_mod))

#within-plot variation in response to env'tal factors higher in plot 20 (an exclosure)
#compared to plot 4(a control).. why??
#warm-precip response is more uncertain in plot 20. why?
#plot 20 isn't necessarily representative of the response to environmental factors but plot 4 is
#so like if we want to fit a model based on plot 4's data to controls it'll be fine but not
#if we fit a model based on plot 20 to exclosures
#how to propagate that uncertainty in scale effect if we only build a site-wide model??
