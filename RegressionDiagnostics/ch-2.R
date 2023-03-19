# Regression Diagnostics, 2nd Edition
# John Fox
# last modified: 2019-06-18

# R script for Ch. 2

# Fig. 2.1: Joint confidence region

CIA <- read.table("CIA.txt", header=TRUE)
  # assumes that CIA.txt is in the current directory, adjust as necessary

library(car)
library(sfsmisc)

mod <- lm(infant ~ gdp + health, data=CIA)
cis <- confint(mod)
b <- coef(mod)

confidenceEllipse(mod, segments=500, levels=c(0.85, 0.95), col="black", 
                  fill=TRUE, 
                  axes=FALSE, ann=FALSE, xlab="", ylab="", grid=FALSE)
                  # warnings are innocuous
box()
usr <- par("usr")
abline(v=cis[2, ], h=cis[3, ], lty=2)
lines(x=c(usr[1], b[2]), y=c(b[3], b[3]))
lines(x=c(b[2], b[2]), y=c(usr[3], b[3]))

par <- par("xpd"=TRUE)
p.arrows(cis[2, 1], usr[3], cis[2, 2], usr[3], lwd=3, fill="black", 
         xpd=TRUE, size=1.25)
p.arrows(cis[2, 2], usr[3], cis[2, 1], usr[3], lwd=3, fill="black", 
         xpd=TRUE, size=1.25)
p.arrows(usr[1], cis[3, 1], usr[1], cis[3, 2], lwd=3, fill="black", 
         xpd=TRUE, size=1.25)
p.arrows(usr[1], cis[3, 2], usr[1], cis[3, 1], lwd=3, fill="black", 
         xpd=TRUE, size=1.25)
mtext(side=1, at=b[2], line=0.75, text=expression(b[1]))
mtext(side=2, at=b[3], line=0.75, text=expression(b[2]), las=1)
text(usr[2], usr[3], labels=expression(~beta[1]), adj=-0.2)
text(usr[1], usr[4], labels=expression(~beta[2]), adj=c(0.5, -0.35))

par(par) # restore graphical parameters

