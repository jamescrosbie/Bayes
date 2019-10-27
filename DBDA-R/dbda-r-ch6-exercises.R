
source("C:/Users/james/Documents/Projects/Bayes/DBDA-R/dbda-r-ch6.r")

binwidth = 1/1000
thetagrid = seq( from=binwidth/2 , to=1-binwidth/2 , by=binwidth )

relprob = pmin(thetagrid,1-thetagrid) # relative prob at each theta
prior = relprob / sum(relprob) # probability mass at each theta

datavec = c( rep(1,3) , rep(0,1) ) # 3 heads, 1 tail
posterior = BernGrid( Theta=thetagrid , pTheta=prior , Data=datavec )
