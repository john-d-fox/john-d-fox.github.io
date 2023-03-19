# Regression Diagnostics, 2nd Edition
# John Fox
# last modified: 2019-06-18

# R script for Ch. 7

library(car)
library(MASS)

# VIFs for Duncan's regression

Duncan <- read.table("Duncan.txt", header=TRUE)
  # assumes that Duncan.txt is in the current directory, adjust as necessary
mod.duncan <- lm(prestige ~ income + education, data=Duncan)
S(mod.duncan)
with(Duncan, cor(cbind(income, education)))
sqrt(vif(mod.duncan))

mod.duncan.1 <- update(mod.duncan, subset=-c(6, 16)) 
                        # remove minister, conductor
S(mod.duncan.1)
with(Duncan[-c(6, 16), ], cor(cbind(income, education)))
sqrt(vif(mod.duncan.1))

# Fig. 7.1: data and confidence ellipses for regressions on contrived data

set.seed(13221) # for reproducibility
X1 <- mvrnorm(200, mu=c(0, 0), Sigma=matrix(c(1, 0, 0, 1), 2, 2))
X2 <- mvrnorm(200, mu=c(0, 0), Sigma=matrix(c(1, 0.95, 0.95, 1), 2, 2))
cor(X1)
cor(X2)

set.seed(123)
y1 <- X1 %*% c(2, 3) + rnorm(200, sd=10)
mod1 <- lm(y1 ~ X1)
brief(mod1)

set.seed(123)
y2 <- X2 %*% c(2, 3) + rnorm(200, sd=10)
mod2 <- lm(y2 ~ X2)
brief(mod2) 

par <- par(mfrow=c(2, 2))
dataEllipse(X1, levels=0.95, xlim=c(-3, 3), ylim=c(-3, 3), col="black", 
            center.cex=2,
            main="(a)", xlab=expression(x[1]), ylab=expression(x[2]), las=1, 
            segments=1000)
dataEllipse(X2, levels=0.95, xlim=c(-3, 3), ylim=c(-3, 3), col="black", 
            center.cex=2,
            main="(b)", xlab=expression(x[1]), ylab=expression(x[2]), las=1, 
            segments=1000)
confidenceEllipse(mod1, col="black",xlim=c(-5, 10), ylim=c(-5, 10), fill=TRUE, 
                  center.cex=2,
                  levels=c(0.85, 0.95), fill.alpha=0.2,
                  main="(c)", xlab=expression(beta[1]), 
                  ylab=expression(beta[2]), 
                  las=1, segments=1000)
abline(h=0, v=0)
points(0, 0, pch=15, cex=2)
confidenceEllipse(mod2, col="black", xlim=c(-5, 10), ylim=c(-5, 10), 
                  fill=TRUE, center.cex=2,
                  levels=c(0.85, 0.95), fill.alpha=0.2,
                  main="(d)", xlab=expression(beta[1]), 
                  ylab=expression(beta[2]), las=1, segments=1000)
abline(h=0, v=0)
points(0, 0, pch=15, cex=2)

par(par)

# Fig. 7.2: marginal vs added-variable plots for the contrived regressions
#     (note: the figure in the text uses some custom code not in mcPlot() )

par <- par(mfrow=c(1, 2))
mcPlot(mod1, "X11", col.cond="black", col.marg="black", col.arrows=gray(0.5), 
       pch=c(1, 16), grid=FALSE)
mcPlot(mod2, "X21", col.cond="black", col.marg="black", col.arrows=gray(0.5), 
       pch=c(1, 16), grid=FALSE)
par(par)
  # warnings are innocuous

# GVIFs for the regression of log(infant mortality) on for the CIA data

CIA <- read.table("CIA.txt", header=TRUE)
  # assumes that CIA.txt is in the current directory, adjust as necessary

mod.cia <- lm(log(infant) ~ log(gdp) + poly(health, 2, raw=TRUE) + gini, 
              data=CIA)
S(mod.cia)
vif(mod.cia)
