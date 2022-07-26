//hierarchical beta-binomial 

data { 
  int N; //rows of observations 
  int Y[N]; //response variable (# of reproductive)
  int n[N]; //total female PPs
  vector[N] X; //PB biomass
  
  //effect of covariates at different spatial scales
  
  int<lower=1> J; //number of plots
  int plot[N]; //plot ID
  
  int<lower=1, upper=2> trt[N]; //vector of treatment ID
  int <lower=1> Z; // no. of treatments

}

parameters {
  
 real b0; //intercept
 real <lower = 0> phi; //level 1 process error
 
 real mu_b1; //plot-level intercept
 real mu_b2; //trt-level intercept
 
 real <lower = 0> sigma_b1; //plot-level variance
 real <lower = 0> sigma_b2; //trt-level variance
 
 vector[J] b1; //plot random effect
  vector[Z] b2;//trt random effect
}

transformed parameters {
 
  vector[N] mu; //mean proportion~linear preds
  vector [N] alpha; //shape parameter 1
  vector [N] beta; //shape param 2
  
  for (i in 1:N) 
    
  mu[i]= inv_logit(b0+ X[i]* b1[plot[i]]+ X[i]* b2[trt[i]]);
 
 alpha= mu*phi;
 beta=(1-mu)*phi;
  
 }
 
 model {
  
  b0 ~ normal(0,10);
  b1 ~ normal(0,10);
  b2 ~ normal(0,10);
  phi ~ cauchy(0,2.5);
  
  mu_b1 ~ normal(b1, sigma_b1);
  mu_b2 ~ normal(b2, sigma_b2);
  sigma_b1 ~ cauchy(0,2.5);
  sigma_b2 ~ cauchy(0,2.5);
  
  Y ~ beta_binomial(n,alpha,beta);
 }
 
 //generated quantities{ 
 
 //vector [N]y_rep;

//    for (x in 1:N){
  //  y_rep[x]=beta_binomial_rng(n[x], alpha[x], beta[x]);

    // }
  //}
  