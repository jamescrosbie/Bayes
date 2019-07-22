
rm(list=ls())
dir <- dirname(rstudioapi::getSourceEditorContext()$path)
source(paste0(dir,"/dbda-r-ch5.R"))

# Exercises 5
# 5.1a
prior <- c(4,4) #prior shape
data <- c(1)    #One head
postShape <- BernBeta( prior , data)

#5b
data <- c(1)
postShape <- BernBeta(postShape, data)

#5c
data <- c(0)
postShape <- BernBeta(postShape, data)

#5d
data_THH <- c(0, 1, 1)
postShape_THH <- BernBeta(postShape, data_THH)

data_HHT <- c(1, 1, 0)
postShape_HHT <- BernBeta(postShape, data_HHT)

#5.2
prior <- c(1,2)
data <- rep(c(1,0), c(58,42))
posterior <- BernBeta(prior, data)

data <- rep(c(1,0), c(57,43))
posterior <- BernBeta(posterior, data)

#5.5
#strong prior beliefs as minted coin
prior <- c(100,100)
data <- rep(c(0,1), c(1,9))
posterior <- BernBeta(prior, data)
# p(h|history) ~ 0.52 maximum of posterior

#strong prior beliefs that unfair, but dont know which way
prior <- c(0.1,0.1)
data <- rep(c(0,1), c(1,9))
posterior <- BernBeta(prior, data)
# p(h|history) ~ 0.85 maximum of posterior

#5.6
prior1 <- c(10,10)
data <- rep(c(0,1), c(5,15))
posterior1 <- BernBeta(prior1, data)
evid1 <- 2.45*10^-6

prior2 <- c(0.1,0.1)
posterior2 <- BernBeta(prior2, data)
evid2 <- 7.32*10^-7

#Evidence 2 is samller, therefore suggests model 1 is more likely
print(paste("Relative in favour of model 2: ", round(evid2/evid1,2)))

