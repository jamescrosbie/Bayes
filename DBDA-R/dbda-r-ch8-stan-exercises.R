
rm(list=ls())
graphics.off()

library(rstan)
library(bayesplot)
library(here)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
theme_set(bayesplot::theme_default(base_family = "sans"))
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7')


#data
seed <- 12345
dataList = list( N1 = 7, N2 = 7,
                 y1 = c( 1,1,1,1,1,0,0 ),
                 y2 = c( 1,1,0,0,0,0,0 )
)

#stan setup
nChains <- 4
nPost <- 1000
nBurn <- 1000
nThin <- 1
nIter <- (nPost + nBurn)*nThin

#fit
fit <- stan(file = './DBDA-R/chap8_ex.stan',
            data = dataList,
            pars=c("theta1", "theta2"),
            chains=nChains,
            iter=nIter,
            warmup=nBurn,
            thin=nThin,
            seed=seed)

save(fit, file=here("outputs", paste0("", "_", Sys.Date()), ".RData"))

summary(fit)
print(fit)
plot(fit)
pairs(fit, pars = c("theta1", "theta2"))



#all the chains together
mcmc_hist(fit, pars='theta1')
mcmc_hist(fit, pars='theta2')

#separate chains
out <- rstan::extract(fit, permuted = FALSE, inc_warmup = TRUE)
chain1 <- out[,1,]
chain2 <- out[,2,]

#plot histograms
mcmc_areas(fit,
           pars = c("theta1", "theta2"),
           prob = 0.95) +
    ggtitle("Posterior distributions",
            "with medians and 95% intervals")

#examine the chains
color_scheme_set("mix-blue-pink")

mcmc_hist_by_chain(fit, pars = c("theta1", "theta2"))

mcmc_trace(out, pars = c("theta1", "theta2"), n_warmup = nBurn,
                facet_args = list(nrow = 2, labeller = label_parsed)) +
    facet_text(size = 15)


mcmc_dens_overlay(fit, pars = c("theta1", "theta2"))
mcmc_violin(fit, pars = c("theta1", "theta2"), probs = c(0.1, 0.5, 0.9))

mcmc_acf(fit, pars = c("theta1", "theta2"), lags = 10)

#################################################################################
# Exercise 8.1

library(rstan)
library(bayesplot)
library(here)
library(shinystan)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
theme_set(bayesplot::theme_default(base_family = "sans"))
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7')


#data
seed <- 12345
dataList = list( N1 = 285, N2 = 53,
                 y1 = c( rep(1,251), rep(0,34) ),
                 y2 = c( rep(1, 48), rep(0,5) )
)

#stan setup
nChains <- 4
nPost <- 1000
nBurn <- 1000
nThin <- 1
nIter <- (nPost + nBurn)*nThin

#fit
fit2 <- stan(file = './DBDA-R/chap8_ex.stan', data = dataList,
            iter=nIter,
            warmup=nBurn,
            thin=nThin,
            chains=nChains,
            seed=seed)

launch_shinystan(fit2)

out <- as.data.frame(fit2)

mcmc_hist(fit2, pars='delta')
mean(out$delta)

plot(x=out$theta1, y=out$theta2, xlim=c(0.7,0.95), ylim=c(0.7,0.95))


mcmc_dens_overlay(fit2, pars = c("delta"))

mcmc_areas(fit2,
           pars = c("theta1", "theta2", "delta"),
           prob = 0.95) +
    ggtitle("Posterior distributions",
            "with medians and 95% intervals")


lp_cp <- log_posterior(fit2)
head(lp_cp)

mcmc_parcoord(fit2, pars=c("theta1", "theta2", "delta"), np =  nuts_params(fit2))


#################################################################################
# Exercise 8.3

library(rstan)
library(bayesplot)
library(here)
library(shinystan)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
theme_set(bayesplot::theme_default(base_family = "sans"))
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7')


#data
dataList = list( N=4, N1 = 7, N2 = 7,
                 y1 = c( 1,1,1,1,1,0,0 ),
                 y2 = c( 1,1,0,0,0,0,0 )
)

#stan setup
fit3 <- stan(file = './DBDA-R/chap8_ex3.stan', data = dataList,
             iter=2000, warmup=100, chains=4, seed=12345)

launch_shinystan(fit3)

out <- as.data.frame(fit3)


plot(x=out$theta1, y=out$theta2)
mean(out$theta1)
mean(out$theta2)

