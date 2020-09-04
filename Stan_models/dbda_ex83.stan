data {
  int<lower=0> N;
  int<lower=0> N1;
  int<lower=0> N2;
  int<lower=0, upper=1> y1[N1];
  int<lower=0, upper=1> y2[N2];
}

parameters {
  real<lower=0, upper=1> theta1;
  real<lower=0, upper=1> theta2;
}

transformed parameters{
  real x;
  real xt;
  real y;
  real yt;

  xt =  sin(2 * 3.14 * N * x) / (2 * 3.14 * N ) + x;
  yt = 3 * y + 0.333;
}


model {
  //priors
  for(i in 1:N1) y1[i] ~ bernoulli(theta1) ;
  for(i in 1:N2) y2[i] ~ bernoulli(theta2) ;

  x ~ uniform(0,1);
  y ~ uniform(0,1);

  // model
  theta1 ~ pow(xt, yt);
  theta2 ~ y;
}

