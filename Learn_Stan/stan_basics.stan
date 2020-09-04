
data {
  // The input data is a vector 'y' of length 'N'.
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
}

parameters {
  //intercept
  real beta0;
  //gradient
  real beta1;
  //variation
  real<lower=0> sigma;
}

model {
  y ~ normal(beta0 + beta1 * x, sigma);
}

