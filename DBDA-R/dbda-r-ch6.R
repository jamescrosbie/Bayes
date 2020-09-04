
BernGrid = function( Theta, pTheta, Data,
                     credib=.95, nToPlot=length(Theta),
                     funcPath="C:/Users/james/Documents/Projects/Bayes/Bayes-R-Funcs") {
    # Bayesian updating for Bernoulli likelihood and prior specified on a grid.
    # Input arguments:
    #  Theta is a vector of theta values, all between 0 and 1.
    #  pTheta is a vector of corresponding probability _masses_.
    #  Data is a vector of 1's and 0's, where 1 corresponds to a and 0 to b.
    #  credib is the probability mass of the credible interval, default is 0.95.
    #  nToPlot is the number of grid points to plot; defaults to all of them.
    #  funcPath path to source files
    # Output:
    #  pThetaGivenData is a vector of posterior probability masses over Theta.
    #  Also creates a three-panel graph of prior, likelihood, and posterior
    #  probability masses with credible interval.

    # Example of use:
    #  # Create vector of theta values.
    #  > binwidth = 1/1000
    #  > thetagrid = seq( from=binwidth/2 , to=1-binwidth/2 , by=binwidth )
    #  # Specify probability mass at each theta value.
    #  > relprob = pmin(thetagrid,1-thetagrid) # relative prob at each theta
    #  > prior = relprob / sum(relprob) # probability mass at each theta
    #  # Specify the data vector.
    #  > datavec = c( rep(1,3) , rep(0,1) ) # 3 heads, 1 tail
    #  # Call the function.
    #  > posterior = BernGrid( Theta=thetagrid , pTheta=prior , Data=datavec )

    library(ggplot2)
    library(gridExtra)
    source(paste(funcPath, "/HDIofGrid.R", sep=""))
    source(paste(funcPath, "/openGraphSaveGraph.R", sep=""))


    # Check for input errors:
    if ( any( Theta > 1 | Theta < 0 ) ) {
        stop("Theta values must be between 0 and 1")
    }
    if ( any( pTheta < 0 ) ) {
        stop("pTheta values must be non-negative")
    }
    if ( sum(pTheta) != 1.0 ) {
        stop("pTheta values must sum to 1.0")
    }
    if ( !all( Data == 1 | Data == 0 ) ) {
        stop("Data values must be 0 or 1")
    }


    # Create summary values of Data
    z <- sum( Data == 1 )
    N <- length( Data )

    # Compute the likelihood
    pDataGivenTheta <- Theta^z * (1-Theta)^(N-z)

    # Compute the evidence and the posterior
    pData <- sum( pDataGivenTheta * pTheta )
    pThetaGivenData <- pDataGivenTheta * pTheta / pData

    #collect HDI info
    HDIinfo <- HDIofGrid( pThetaGivenData, credMass=credib )

    # Plotting
    # Collect results in a dataframe for ggplot
    theta_df <- data.frame(Theta, pTheta, pDataGivenTheta, pThetaGivenData)

    # Prior
    meanTheta <- sum( Theta * pTheta )
    if ( meanTheta > 0.5 ) {
        textx = 0.2
    } else {
        textx = 0.8
    }
    textToAdd <- bquote("E(" * theta * ")=" * .(signif(meanTheta,3)))

    prior <- ggplot(theta_df, aes(x=Theta, y=pTheta)) +
        geom_line()+
        geom_point(size=1.5, color="steelblue")+

        annotate("text", x=textx, y=0.5*max(pTheta),
                 label=deparse(textToAdd), parse=TRUE, size=8)+

        xlim(0,1)+
        ylim(0,1.1*max(pTheta))+
        labs(title="Prior", x=bquote(theta), y=bquote("p("*theta*")")) +
        jc_theme

    # likelihood
    if ( z > 0.5*N ) {
        textx = 0.2
        } else {
        textx = 0.8
    }
    textToAdd <- bquote( "Data: z=" * .(z) * ", N=" * .(N) )

    liklihood <- ggplot(theta_df, aes(x=Theta, pDataGivenTheta)) +
        geom_line()+
        geom_point(size=1.5, color="steelblue")+

        annotate("text", x=textx, y=0.5*max(pDataGivenTheta),
                 label=deparse(textToAdd), parse=TRUE, size=8)+
        xlim(0,1)+
        ylim(0, 1.1*max(pDataGivenTheta))+
        labs(title="Liklihood", x=bquote(theta), y=bquote("p(D|"*theta*")"))+
        jc_theme

    # Posterior
    meanThetaGivenData <- sum( Theta * pThetaGivenData )
    if ( meanThetaGivenData > 0.5 ) {
        textx = 0.2
    } else {
        textx = 0.8
    }
    HDImidpoint <- (Theta[ HDIinfo$indices[1] ] + Theta[tail(HDIinfo$indices, n=1)])/2


    textToAdd <- bquote( "E(" * theta * "|D)=" * .(signif(meanThetaGivenData,3)) )
    textToAdd2 <- bquote("p(D)=" * .(signif(pData,3)))
    textToAdd4 <- bquote( .(100*signif(HDIinfo$mass,3)) * "% HDI" )
    textToAdd3 <- bquote("HDI = [" * .(signif(Theta[ HDIinfo$indices[1] ], 3)) * " - " *
                            .(signif(Theta[ tail(HDIinfo$indices, n=1) ], 3)) * "]")

    posterior <- ggplot(theta_df, aes(x=Theta, pThetaGivenData)) +
        geom_line()+
        geom_point(size=1.5, color="steelblue")+

        geom_ribbon(data=subset(theta_df,
                                Theta[ HDIinfo$indices[1] ] <= Theta &
                                           Theta <= Theta[tail(HDIinfo$indices, n=1)]),
                    aes(ymin=0, ymax=pThetaGivenData), fill="blue", alpha="0.5")+

        # geom_segment(aes(x=Theta[ HDIinfo$indices[1] ],
        #                  y=HDIinfo$height,
        #                  xend=Theta[tail(HDIinfo$indices, n=1)],
        #                  yend=HDIinfo$height), linetype=2, size=1, color="black")+

        geom_segment(aes(x=Theta[ HDIinfo$indices[1] ],
                         y = 0,
                         xend=Theta[ HDIinfo$indices[1] ] ,
                         yend=HDIinfo$height), linetype=1, size=1.5, color="black")+

        geom_segment(aes(x=Theta[tail(HDIinfo$indices, n=1)],
                         y = 0,
                         xend=Theta[tail(HDIinfo$indices, n=1)] ,
                         yend=HDIinfo$height), linetype=1, size=1.5, color="black")+

        # annotate("text", x=textx, y=0.2*max(pThetaGivenData),
        #          label=deparse(textToAdd3), parse=TRUE, size=6)+

        annotate("text", x=textx, y=0.5*max(pThetaGivenData),
                 label=deparse(textToAdd), parse=TRUE, size=8)+

        annotate("text", x=textx, y=0.35*max(pThetaGivenData),
                 label=deparse(textToAdd3), parse=TRUE, size=8)+

        annotate("text", x=HDImidpoint, y=0.5*max(pThetaGivenData),
                label=deparse(textToAdd4), parse=TRUE, size=8)+

        xlim(0,1)+
        ylim(0, 1.1*max(pThetaGivenData))+
        labs(title="Posterior", x=bquote(theta), y=bquote("p("*theta*"|D)"))+
        jc_theme

    grid.arrange(prior, liklihood, posterior, nrow = 3)

    return( pThetaGivenData )
}