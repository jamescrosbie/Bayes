
# check works
source("C:/Users/james/Documents/Projects/Bayes/DBDA-R/dbda-r-ch6.r")

binwidth = 1/1000
thetagrid = seq( from=binwidth/2 , to=1-binwidth/2 , by=binwidth )

relprob = pmin(thetagrid,1-thetagrid) # relative prob at each theta
prior = relprob / sum(relprob) # probability mass at each theta

datavec = c( rep(1,3) , rep(0,1) ) # 3 heads, 1 tail
posterior = BernGrid( Theta=thetagrid , pTheta=prior , Data=datavec )

#6.2
pTheta <- c(50:1, rep(1,50), 1:50, 50:1, rep(1,50), 1:50)
pTheta <- pTheta / sum(pTheta)
width <- 1 / length(pTheta)
Theta <- seq(from=width/2, to=1-width/2, by=width)

data <- c(rep(1,15), rep(0,5))
posterior = BernGrid( Theta=Theta, pTheta=pTheta , Data=data )

#6.3
pTheta <- c(50:1, rep(1,50), 1:50, 50:1, rep(1,50), 1:50)
pTheta <- pTheta / sum(pTheta)
width <- 1 / length(pTheta)
Theta <- seq(from=width/2, to=1-width/2, by=width)

data <- c(rep(1,3), rep(0,1))
posterior = BernGrid( Theta=Theta, pTheta=pTheta , Data=data )

#b
data <- c(rep(1,12), rep(0,4))
posterior = BernGrid( Theta=Theta, pTheta=posterior , Data=data )

#6.4
data <- c(rep(0, 58), rep(1, 42) ) # A=0, B=1
pTheta = rep(1,100)
pTheta = pTheta / sum( pTheta )
width = 1 / length(pTheta)
Theta = seq(from=width/2, to=1-width/2, by=width)
posterior = BernGrid( Theta=Theta, pTheta=pTheta , Data=data )

#c
data <- c(rep(0, 57), rep(1, 43))
posterior <- posterior/sum(posterior)
posterior = BernGrid( Theta=Theta, pTheta=posterior, Data=data )


#6.6
width <- 1/1000
Theta = seq(from=width/2 , to=1-width/2 , by=width )
relprob = Theta^2
prior = relprob / sum(relprob)
data <- c(rep(1,2),rep(0,2))
posterior = BernGrid( Theta, prior, data )
predprob = sum( thetagrid * posterior )
predprob
