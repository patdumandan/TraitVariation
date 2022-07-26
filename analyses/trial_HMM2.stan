/*A simple example of an hierarchical model*/
data {
  int<lower=1> N; //the number of observations
  int<lower=1> J; //the number of plots
  int<lower=1> K; //number of columns in the model matrix
  int<lower=1,upper=J> plot[N]; //vector of group indeces
  matrix[N,K] X; //the model matrix
  int Y[N]; //response variable (# of reproductive)
  int n[N]; //total female PPs
}
parameters {
  
  //process model
  real b0; //intercept
  vector[K] gamma; //population-level regression coefficients
  vector<lower=0>[K] tau; //the standard deviation of the regression coefficients

//data model
  vector[K] delta[J]; //matrix of group-level regression coefficients
  real<lower=0> sigma; //standard deviation of the individual observations
}

transformed parameters {
 
  vector[N] mu; //mean proportion~linear preds
  vector [N] alpha; //shape parameter 1
  vector [N] beta; //shape param 2
  
  for (i in 1:N) 
    
  mu[i]= inv_logit(b0+X[i,]* delta[plot[i]]);
 
 alpha= mu*sigma;
 beta=(1-mu)*sigma;
  
 }
model {
  //priors
  b0~ normal(0,5);
  gamma ~ normal(0,5); //weakly informative priors on the regression coefficients
  tau ~ cauchy(0,2.5); //weakly informative priors, see section 6.9 in STAN user guide
  sigma ~ gamma(2,0.1); //weakly informative priors, see section 6.9 in STAN user guide
  
  for(j in 1:J){
   delta[j] ~ normal(gamma,tau); //fill the matrix of group-level regression coefficients 
  }
  
  //likelihood
   Y ~ beta_binomial(n,alpha,beta);
}

//figure out non-centered parameterization
