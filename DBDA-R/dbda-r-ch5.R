BernBeta = function( priorShape, dataVec, credMass=0.95, saveGr=FALSE,
                     funcPath="C:/Users/james/Documents/Projects/Bayes/Bayes-R-Funcs") {
    # Bayesian updating for Bernoulli likelihood and beta prior.
    # Input arguments:
    #   priorShape
    #     vector of parameter values for the prior beta distribution.
    #   dataVec
    #     vector of 1's and 0's.
    #   credMass
    #     the probability mass of the HDI.
    # Output:
    #   postShape
    #     vector of parameter values for the posterior beta distribution.
    # Graphics:
    #   Creates a three-panel graph of prior, likelihood, and posterior
    #   with highest posterior density interval.
    # Example of use:
    # > postShape = BernBeta( priorShape=c(1,1) , dataVec=c(1,0,0,1,1) )

    source( paste(funcPath, "/HDIofICDF.R", sep="") )
    source( paste(funcPath, "/openGraphSaveGraph.R", sep="") )

    # Check for errors in input arguments:
    if ( length(priorShape) != 2 ) {
        stop("priorShape must have two components.") }
    if ( any( priorShape <= 0 ) ) {
        stop("priorShape components must be positive.") }
    if ( any( dataVec != 1 & dataVec != 0 ) ) {
        stop("dataVec must be a vector of 1s and 0s.") }
    if ( credMass <= 0 | credMass >= 1.0 ) {
        stop("credMass must be between 0 and 1.") }

    # Rename the prior shape parameters
    a <- priorShape[1]
    b <- priorShape[2]

    # Create summary values of the data
    z <- sum( dataVec == 1 )
    N <- length( dataVec )

    # Compute the posterior shape parameters:
    postShape <- c(a+z, b+N-z)

    # Compute the evidence, p(D):
    pData <- beta(z+a, N-z+b) / beta(a, b)

    # Determine the limits of the highest density interval.
    hpdLim <- HDIofICDF(qbeta, shape1=postShape[1], shape2=postShape[2],
                       credMass=credMass)

    # Plots
    # Construct grid of theta values, used for graphing.
    binwidth <- 0.005
    Theta <- seq(from=binwidth/2, to=1-(binwidth/2), by=binwidth)

    # Prior at each value of theta.
    pTheta <- dbeta(Theta, a, b)

    # Likelihood
    pDataGivenTheta <- Theta^z * (1-Theta)^(N-z)

    # Posterior
    pThetaGivenData <- dbeta(Theta, a+z, b+N-z)

    # Open a window with three panels.
    openGraph(width=7, height=10, mag=0.7)
    layout( matrix( c( 1,2,3 ), nrow=3, ncol=1, byrow=FALSE) )
    par( mar=c(3,3,1,0), mgp=c(2,1,0), mai=c(0.5,0.5,0.3,0.1) )

    maxY <- max( c(pTheta, pThetaGivenData) )

    # Prior.
    plot( Theta, pTheta,
          xlim=c(0,1), ylim=c(0,maxY),
          main="Prior",
          xlab=bquote(theta) , ylab=bquote(p(theta)),
          type="l", lwd=3, col="skyblue",
          cex.axis=1.2, cex.lab=1.5, cex.main=1.5)

    if (a > b) {
        textx <- 0
        textadj <- c(0,1)
    } else {
        textx <- 1
        textadj <- c(1,1)
    }
    text( textx, 1.0*max(pThetaGivenData),
          bquote( "beta(" * theta * "|" * .(a) * "," * .(b) * ")"  ),
          cex=2.0 ,adj=textadj)

    # Likelihood
    plot( Theta, pDataGivenTheta,
          xlim=c(0,1), ylim=c(0,1.1*max(pDataGivenTheta)),
          main="Likelihood" ,
          xlab=bquote(theta), ylab=bquote( "p(D|" * theta * ")" ),
          type="l", lwd=3, col="skyblue",
          cex.axis=1.2, cex.lab=1.5, cex.main=1.5)

    if (z > 0.5*N) {
        textx <- 0
        textadj <- c(0,1)
    } else {
        textx <- 1
        textadj <- c(1,1)
    }

    text( textx, 1.0*max(pDataGivenTheta),
          bquote( "Data: z=" * .(z) * ",N=" * .(N) ),
          adj=textadj, cex=2.0)

    # Posterior
    plot( Theta, pThetaGivenData ,
          xlim=c(0,1), ylim=c(0,maxY),
          main="Posterior",
          xlab=bquote(theta) , ylab=bquote( "p(" * theta * "|D)" ),
          type="l", lwd=3, col="skyblue",
          cex.axis=1.2, cex.lab=1.5, cex.main=1.5)


    if (a+z > b+N-z) {
        textx <- 0
        textadj <- c(0,1)
    } else {
        textx <- 1
        textadj <- c(1,1)
    }
    text( textx, 1.00*max(pThetaGivenData),
          bquote( "beta(" * theta * "|" * .(a+z) * "," * .(b+N-z) * ")"  ),
          adj=textadj, cex=2.0 )
    text( textx, 0.75*max(pThetaGivenData),
          bquote( "p(D)=" * .(signif(pData,3)) ),
          adj=textadj, cex=2.0 )

    # Mark the HDI in the posterior.
    hpdHt <- mean( c( dbeta(hpdLim[1], a+z, b+N-z),
                     dbeta(hpdLim[2], a+z, b+N-z) ))
    lines( c(hpdLim[1], hpdLim[1]), c(-0.5, hpdHt),
           type="l", lty=2, lwd=1.5)
    lines( c(hpdLim[2], hpdLim[2]), c(-0.5, hpdHt),
           type="l", lty=2, lwd=1.5)
    lines( hpdLim, c(hpdHt, hpdHt),
           type="l", lwd=2 )
    text( mean(hpdLim), hpdHt, bquote( .(100*credMass) * "% HDI" ),
          adj=c(0.5, -1.0), cex=2.0)
    text( hpdLim[1], hpdHt, bquote( .(round(hpdLim[1], 3))),
          adj=c(1.1, -0.1), cex=1.2 )
    text( hpdLim[2], hpdHt, bquote( .(round(hpdLim[2], 3))),
          adj=c(-0.1, -0.1), cex=1.2 )


    # Construct filename for saved graph, and save the graph.
    if (saveGr) {
        filename <- paste("BernBeta_", a, "_", b, "_", z, "_", N, sep="")
        saveGraph(file=filename, type="eps")
    }
    return(postShape)
}