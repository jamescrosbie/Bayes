
library(rstan)
library(ggmcmc)


options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7')


#create data
x <- 1:10
y <- 5*x + 12 + rnorm(10)

# define the data to pass to stan
data <- list(N=length(x), x=x, y=y)

#fit data
fit <- stan(file = './Learn_Stan/stan_basics.stan', data = data,
            chains = 4, iter =1000, warmup=500, seed=123)

#examine fit
print(fit)
plot(fit)
pairs(fit, pars = c("beta0", "beta1", "sigma"))



# extract the chains
la <- extract(fit) # return a list of arrays
mu <- la$mu

### return an array of three dimensions: iterations, chains, parameters
a <- extract(fit, permuted = FALSE)

### use S3 functions on stan-fit objects
a2 <- as.array(fit)
m <- as.matrix(fit)
d <- as.data.frame(fit)



y <- as.matrix(read.table('https://raw.github.com/wiki/stan-dev/rstan/rats.txt', header = TRUE))
x <- c(8, 15, 22, 29, 36)
xbar <- mean(x)
N <- nrow(y)
T <- ncol(y)
rats_fit <- stan('https://raw.githubusercontent.com/stan-dev/example-models/master/bugs_examples/vol1/rats/rats.stan')



samples <- ggmcmc::ggs(fit)
ggmcmc(samples)




