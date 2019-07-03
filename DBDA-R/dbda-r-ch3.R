# Chapter 3 DBDA
#

# Running proportion
rm(list=ls())

flips <- 1000
p <- 0.5
x <- 1:flips
nHeads <- sample(c(0,1), size=flips, replace=TRUE, prob=c(1-p,p))
propH <- cumsum(nHeads)/x


plot(x, propH,
     xlab = "Number of flipts", ylab="Proportion of Heads",
     main="Running proportion of Heads",
     ylim=c(0,1),
     type="o", col="blue" )
lines(c(1,flips), c(0.5,0.5), col="red", lty=2)



# Integration
mu <- 1
sigma <- 2.0
delta <- 0.1
x <- seq(from=mu-3*sigma, to=mu+3*sigma, by=delta)
y <- 1/(sigma*sqrt(2*pi))*exp(-1/(2*sigma)*(x-mu)^2)
area <- sum(delta*y)

plot(x,y,
     type="h",
     xlab="x", ylab=bquote(paste("p(x| ", mu, ",", sigma, ")")),
     main="Probability density function")
lines(x,y)

text(-1.5*sigma, 0.8*max(y),
     bquote(paste(mu, "=", .(mu), ", ", sigma, "=", .(sigma))))
text(1.5*sigma, 0.8*max(y),
     bquote(paste(Sigma, " ", delta, " p(x)= ", .(area))))
