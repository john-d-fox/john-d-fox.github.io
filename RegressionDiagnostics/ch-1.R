# Regression Diagnostics, 2nd Edition
# John Fox
# last modified: 2019-06-18

# R script for Ch. 1

# Fig. 1.1: Anscombe's Quartet

Anscombe <- read.table("Anscombe.txt", header=TRUE)
  # assumes that Anscombe.txt is in the current directory, adjust as necessary

par <- par(mfrow=c(2, 2), oma=c(2, 0, 0, 0))

plot(y.1 ~ x, xlab=expression(x[1]), ylab=expression(y[1]), pch=16, 
     col="gray40",
     cex=1.25, main="(a)", xlim=c(4, 19), ylim=c(3, 13), data=Anscombe, las=1)
abline(print(lm(y.1 ~ x, data=Anscombe)), col="black", lwd=2)

plot(y.2 ~ x, xlab=expression(x[2]), ylab=expression(y[2]), pch=16, 
     col="gray40",
     cex=1.25, main="(b)", xlim=c(4, 19), ylim=c(3, 13), data=Anscombe, las=1)
abline(print(lm(y.2 ~ x, data=Anscombe)), col="black", lwd=2)
text(15, 11, "nonlinearity", pos=2)

plot(y.3 ~ x, xlab=expression(x[3]), ylab=expression(y[3]), pch=16, 
     col="gray40",
     cex=1.25, main="(c)", xlim=c(4, 19), ylim=c(3, 13), data=Anscombe, las=1)
points(y.3 ~ x, cex=1.5, data=Anscombe, subset=3)
abline(print(lm(y.3 ~ x, data=Anscombe)), col="black", lwd=2)
text(13, 12.5, "outlier", pos=2)

plot(y.4 ~ x.4, xlab=expression(x[4]), ylab=expression(y[4]), pch=16, 
     col="gray40",
     cex=1.25, main="(d)", xlim=c(4, 19), ylim=c(3, 13), data=Anscombe, las=1)
points(y.4 ~ x.4, cex=1.5, data=Anscombe, subset=8)
abline(print(lm(y.4 ~ x.4, data=Anscombe)), col="black", lwd=2)
text(18, 12.5, "influential point", pos=2)

mtext(expression(hat(y) == 3 + 0.5*x ~~~~ r == 0.82), side=1, line=0, 
      outer=TRUE)

par(par) # restore graphical parameters
