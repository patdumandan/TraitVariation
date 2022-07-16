//beta-binomial simple

data { 
  int N; //rows of observations 
  int Y[N]; //response variable (# of reproductive)
  int n[N]; //total female PPs
  int K; //no of covariates
  matrix [N, K] X; //model matrix
  
  //effect of covariates at different scales
  
  int<lower=1> J; //number of plots
  int plot[N]; //plot ID
  
  int<lower=1, upper=2> trt[N]; //vector of treatment ID
  int <lower=1> Z; // no. of treatments

}

parameters {
//  real b0; //population intercept
  vector[K] betas;//population-wide coefs
  vector<lower=0>[K] tau;//process error
  
   vector[K] b1[J]; //  plot-level coefs
   vector[K] b2[Z]; //  trt-level coefs
   real<lower=0> sigma; // plot variance
   real<lower=0> delta; //treatment variance
}

transformed parameters {
 
  vector[N] mu; //mean proportion~linear preds
  vector [N] alpha; //shape parameter 1
  vector [N] beta; //shape param 2
  
  for (i in 1:N) 
    
  mu[i]= inv_logit(X[i,]* b1[plot[i]]+ X[i,]* b2[trt[i]]);
 
 alpha= mu*sigma;
 beta=(1-mu)*sigma;
  
 }

 model {  
  sigma ~ normal(0, 1);
  
  betas[1] ~ cauchy(0,10); //prior for the intercept following Gelman 2008

  for(i in 1:J)
   b1[i] ~ cauchy(betas,tau);//prior for the slopes following Gelman 2008
  
  for (a in 1:Z)
  b2[a]~ cauchy (betas, delta); 
  
  Y ~ beta_binomial(n,alpha,beta);
}


//generated quantities{ 
  
//vector [N]y_rep;

//for (x in 1:N){

//y_rep[x]=beta_binomial_rng(n[x], alpha[x], beta[x]);
//}
//  }
  