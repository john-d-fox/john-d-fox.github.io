##-----------------------------------------------------##
##  An R Companion to Applied Regression, 3rd Edition  ##
##      J. Fox and S. Weisberg, Sage Publications      ##
##   Script for  Appendix on Time-Series Regression    ##
##-----------------------------------------------------##

library("car")
brief(Hartnagel, c(6, 2))

plot(fconvict ~ year, type="n",data=Hartnagel,
    ylab="Convictions per 100,000 Women")
grid(lty=1)
with(Hartnagel, points(year, fconvict, type="o", pch=16))

mod.ols <- lm(fconvict ~ tfr + partic + degrees + mconvict, data=Hartnagel)
summary(mod.ols)

plot(Hartnagel$year, residuals(mod.ols), type="o", pch=16,
    xlab="Year", ylab="OLS Residuals")
abline(h=0, lty=2)

acf(residuals(mod.ols))
acf(residuals(mod.ols), type="partial")

durbinWatsonTest(mod.ols, max.lag=5)

library("lmtest")
dwtest(mod.ols, alternative="two.sided") 

library("nlme")
mod.gls <- gls(fconvict ~ tfr + partic + degrees + mconvict,
    data=Hartnagel, correlation=corARMA(p=2), method="ML")
summary(mod.gls)

mod.gls.3 <- update(mod.gls, correlation=corARMA(p=3))
mod.gls.1 <- update(mod.gls, correlation=corARMA(p=1))
mod.gls.0 <- update(mod.gls, correlation=NULL)
anova(mod.gls, mod.gls.1)  # AR(2) vs AR(1)
anova(mod.gls, mod.gls.0)  # AR(2) vs uncorrelated errors
anova(mod.gls.3, mod.gls)  # AR(3) vs AR(2)
