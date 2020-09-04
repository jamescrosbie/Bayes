
library(rstan)

data(iris)

versicolor <- which(iris$Species == "versicolor")
x <- iris$Sepal.Length[versicolor]
y <- iris$Sepal.Width[versicolor]

data=list(N=length(x), x=x, y=y)

fit <- stan(file="C./stan_models/slr.stan", data=data)


summary(fit)

#plot fit
plot(x,y)
params <- extract(fit)
alpha <- mean(params$alpha)
beta <- mean(params$beta)
sigma <- mean(params$sigma)
abline(a=alpha, b=beta)

#get posterior interval
xr <- seq(4.5,7.5,0.1)
yCI <- sapply(xr, function(k) quantile(params$beta * k +params$alpha, probs=c(0.05,0.95)))
lines(xr, yCI[1, ], col='red')
lines(xr, yCI[2, ], col='red')


#Look at simulated data from posterior
#compare dist of our data with simulated data
plot(density(y), xlim=c(0,6), ylim=c(0,2))
params <- extract(fit)
for(i in 1:10){ lines(density(params$y_sim[i,]), col="red")}

#can we recover same paramters from simulated data?
y_new <- params$y_sim[20,]
data_new <- list(N=length(x), x=x, y=y_new)
fit_new <- stan(file="./stan_models/slr.stan", data=data_new)

#now compare the parameters
params_new <- extract(fit_new)
plot(density(params$alpha))
lines(density(params_new$alpha), col="red")

plot(density(params$beta))
lines(density(params_new$beta), col="red")


