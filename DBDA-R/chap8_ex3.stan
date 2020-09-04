
data {
  int<lower=0> N;
  int<lower=0> N1;
  int<lower=0> N2;
  int<lower=0, upper=1> y1[N1];
  int<lower=0, upper=1> y2[N2];
}

parameters {
  real x;
  real y;
}

transformed parameters{
  real xt;
  real xtt;
  real yt;
  real theta1;
  real theta2;

  xt = sin(2*3.14*N*x)/(2*3.14*N) + x;
  yt = 3*y + (1.0/3.0);
  xtt = pow(xt, yt);

  theta1 = xtt;
  theta2 = y;
}

model {
  //priors
  x ~ uniform(0,1);
  y ~ uniform(0,1);
  //liklihood
  for(n in 1:N1)  y1[n] ~ bernoulli(theta1);
  for(n in 1:N2)  y2[n] ~ bernoulli(theta2);
}

