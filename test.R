op <- par(no.readonly=TRUE)
par(mfrow=c(1,1))

mu1 <- mu2 <- 2
sigma1 <- 1; sigma2 <- 2

f1 <- function(x)
  dnorm(x,mean=mu1,sd=sigma1)

f2 <- function(x)
  dnorm(x,mean=mu2,sd=sigma2)

curve(f1,xlim=c(mu2-3*sigma2,mu2+3*sigma2),ylab="Density",main=expression(list(mu[1]*"="*mu[2],sigma[1]!=sigma[2])))

curve(f2,add=T,col=2,lty=2)
abline(v=mu1,col=4,lty=3,lwd=1)
legend(4,0.35,legend=c(expression(X[1]*"~N(2,1)"),expression(X[2]*"~N(2,4)")),lty=1:2,col=1:2)

X1~N(0,2^2); X2~N(2,2^2)
mu1 <- 0; mu2 <- 2
sigma1 <- sigma2 <- 2

curve(f1,xlim=c(mu1-3*sigma2, mu2+3*sigma2),ylab="Density",main=expression(list(mu[1]!=mu[2],sigma[1]*"="*sigma[2])))
curve(f2,add=T,col=2,lty=2)

abline(v=mu1,col=1,lty=3,lwd=1)
abline(v=mu2,col=2,lty=3,lwd=1)
legend(4.5,0.16,legend=c(expression(X[1]*"~N(0.4)"),expression(X[2]*"~N(2,4)")),lty=1:2,col=1:2)

par(op)