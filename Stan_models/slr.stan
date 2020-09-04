
// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real alpha; // intercept
  real beta; // slope
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model{
  //priors
  alpha ~ normal(0,10);
  beta ~ normal(0,10);
  sigma ~ normal(0,1);
  //liklihood
  y ~ normal(alpha + beta * x, sigma);
}

generated quantities{
  //simulated data from posterior
  vector[N] y_sim;

  for(i in 1:N){
    y_sim[i]=normal_rng(alpha+beta*x[i], sigma);
  }
}
