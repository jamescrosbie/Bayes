
data {
  int<lower=0> N1;
  int<lower=0> N2;
  int<lower=0, upper=1> y1[N1];
  int<lower=0, upper=1> y2[N2];
}

parameters {
  real<lower=0, upper=1> theta1;
  real<lower=0, upper=1> theta2;
}

model {
  //priors
  theta1 ~ beta(30,10);
  theta2 ~ beta(30,10);
  //liklihood
  for(n in 1:N1)  y1[n] ~ bernoulli(theta1);
  for(n in 1:N2)  y2[n] ~ bernoulli(theta2);
}

generated quantities {
  real delta;
  delta = theta1 - theta2;
}
