# DBDA Chapter 4
# Bayes Update

#clear
rm(list=ls())
graphics.off()

#set up parameters
nTheta <- 30
data <- c(rep(1,30), rep(0,90))

#outcomes
nHeads <- sum(data)
nTails <- length(data) - nHeads

#set up the distribution of Theta
Theta <- seq(from=1/(nTheta+1), to=nTheta/(nTheta+1), by=1/(nTheta+1) )
pTheta <- pmin(Theta, 1-Theta)
pTheta <- pTheta/sum(pTheta)

# likelihood
pDataGivenTheta <- pTheta^nHeads * (1-pTheta)^nTails

pData <- sum(pDataGivenTheta * pTheta)

# posterior
pThetaGivenData = pDataGivenTheta * pTheta / pData

# Plot layout
windows(7, 10)
layout(matrix(c(1, 2, 3), nrow=3, ncol=1, byrow=FALSE ))
par(mar=c(3, 3, 1, 0))         # number of margin lines: bottom,left,top,right
par(mgp=c(2, 1, 0))           # which margin lines to use for labels
par(mai=c(0.5, 0.5, 0.3, 0.1)) # margin size in inches: bottom,left,top,right

# Prior
plot(Theta, pTheta,
      xlim=c(0, 1), ylim=c(0, 1.1*max(pThetaGivenData)),
      main="Prior",
      xlab=bquote(theta), ylab=bquote(p(theta)),
      cex.axis=1.2 , cex.lab=1.5 , cex.main=1.5,
      type="h", lwd=3, col="blue" )

#Likelihood:
plot(Theta, pDataGivenTheta,
     xlim=c(0, 1), ylim=c(0, 1.1*max(pDataGivenTheta)),
     main="Likelihood",
     xlab=bquote(theta), ylab=bquote(paste("p(D|",theta,")")),
     cex.axis=1.2, cex.lab=1.5, cex.main=1.5,
     type="h", lwd=3, col="blue")
text(0.55 , 0.85*max(pDataGivenTheta),
      cex=2.0, adj=c(0, 0.5),
      bquote(paste("H=", .(nHeads), " T=", .(nTails) ) ))

#Posterior:
plot(Theta, pThetaGivenData,
     xlim=c(0, 1), ylim=c(0, 1.1*max(pThetaGivenData)),
     main="Posterior",
     xlab=bquote(theta), ylab=bquote(paste("p(",theta,"|D)")),
     cex.axis=1.2 , cex.lab=1.5 , cex.main=1.5,
     type="h", lwd=3, col="blue" )
text( 0.55 , 0.85*max(pThetaGivenData),
      cex=2.0, adj=c(0, 0.5),
      bquote("p(D)=" * .(signif(pData, 3)))  )
