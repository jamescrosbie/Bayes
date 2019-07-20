
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
