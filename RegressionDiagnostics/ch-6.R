# Regression Diagnostics, 2nd Edition
# John Fox
# last modified: 2019-06-18

# R script for Ch. 6

# Fig. 6.1: plots of residuals vs x

par <- par(mfcol=c(2, 2), mar=c(3, 3, 3, 3))
set.seed(2468) # for reproducibility
x <- runif(200)
e <- rnorm(200, 0, 0.05)
y1 <- x^2 + e
y2 <- (x - 0.5)^2 + e

plot(x, y1, axes=FALSE, frame=TRUE, xlab="", ylab="", main="(a)")
mtext("x", 1, line=1, at=0.95)
mtext("y", 2, line=1, at=0.95, las=1)
m1 <- lm(y1 ~ x)
abline(m1, lwd=2)

plot(x, residuals(m1), axes=FALSE, frame=TRUE, xlab="", ylab="", main="(a')")
mtext("x", 1, line=1, at=0.95)
mtext("e", 2, line=1, at=0.21, las=1)
abline(0, 0, lwd=2)

plot(x, y2, axes=FALSE, frame=TRUE, xlab="", ylab="", main="(b)")
mtext("x", 1, line=1, at=0.95)
mtext("y", 2, line=1, at=0.28, las=1)
m2 <- lm(y2 ~ x)
abline(m2, lwd=2)

plot(x, residuals(m2), axes=FALSE, frame=TRUE, xlab="", ylab="", main="(b')")
mtext("x", 1, line=1, at=0.95)
mtext("e", 2, line=1, at=0.21, las=1)
abline(0, 0, lwd=2)

par(par)

library(car)
CIA <- read.table("CIA.txt", header=TRUE)
  # assumes that CIA.txt is in the current directory, adjust as necessary

# initial regression for log(infant mortality)

mod.2 <- lm(log(infant) ~ gdp + health + gini, data=CIA)
S(mod.2)

# simple versions of component-plus-residuals and CERES plots

crPlots(mod.2) # convential
crPlots(mod.2, order=2) # quadratic
ceresPlots(mod.2)

# Fig. 6.2: component-plus-residual plots for initial CIA model

par <- par(fig=c(0, .5, .5, 1), mar=c(5.1, 4.1, 2.1, 1.1)) # top-left panel
crPlot(mod.2, "gdp", xlab="GDP per Captia", col.lines=c("black", "black"), 
       ylab="Component+Residual", 
       smooth=list(span=3/4), main="(a)")
par(fig=c(.5, 1, .5, 1)) # top-right panel
par(new=TRUE)
crPlot(mod.2, "health", xlab="Health Expenditures", 
       col.lines=c("black", "black"), ylab="Component+Residual", 
       smooth=list(span=3/4), main="(b)")
par(fig=c(.25, .75, 0, .5)) # bottom panel
par(new=TRUE)
crPlot(mod.2, "gini", xlab="Gini Coefficient", 
       col.lines=c("black", "black"), ylab="Component+Residual", 
       smooth=list(span=3/4), main="(c)")
par(par)

# respecified model for the CIA data

mod.2b <- update(mod.2, . ~ log(gdp) + poly(health, 2, raw=TRUE) + gini)
S(mod.2b)

# Fig. 6.3: component-plus-residual plots for the respecified CIA model

par <- par(fig=c(0, .5, .5, 1), mar=c(5.1, 4.1, 2.1, 1.1)) # top-left panel
crPlot(mod.2b, "log(gdp)", xlab="log(GDP per Captia)", 
       col.lines=c("black", "black"), ylab="Component+Residual", 
       smooth=list(span=3/4), main="(a)")
par(fig=c(.5, 1, .5, 1)) # top-right panel
par(new=TRUE)
crPlot(mod.2b, "poly(health, 2, raw = TRUE)", 
       xlab="Health Expenditures (quadratic)", 
       col.lines=c("black", "black"), ylab="Component+Residual", 
       smooth=list(span=3/4), main="(b)")
par(fig=c(.25, .75, 0, .5)) # bottom panel
par(new=TRUE)
crPlot(mod.2b, "gini", xlab="Gini Coefficient", col.lines=c("black", "black"), 
       ylab="Component+Residual", 
       smooth=list(span=3/4), main="(c)")
par(par)

# Fig. 6.4: component-plus-residual plots against the untransformed xs

library(effects)

plot(Effect("gdp", mod.2b, residuals=TRUE), 
     lines=list(col=c("black", "black"), lty=2), 
     axes=list(grid=TRUE), confint=FALSE, 
     partial.residuals=list(plot=TRUE, smooth.col="black", lty=1, span=3/4), 
     xlab="GDP per Capita", ylab="Component+Residual", main="(a)")

plot(Effect("health", mod.2b, residuals=TRUE), 
     lines=list(col=c("black", "black"), lty=2), 
     axes=list(grid=TRUE), confint=FALSE, 
     partial.residuals=list(plot=TRUE, smooth.col="black", lty=1, span=3/4),
     xlab="Health Expenditures", ylab="Component+Residual", main="(b)")

# Fig. 6.5: comparison of component-plus-residual, 
#           quadratic component-plus-residual and CERES plots

par <- par(mfrow=c(1, 3))
crPlot(mod.2, "gdp", xlab="GDP per Captia", col.lines=c("black", "black"), 
       ylab="Component+Residual", 
       smooth=list(span=3/4), main="(a)")
crPlot(mod.2, "gdp", xlab="GDP per Captia", col.lines=c("black", "black"), 
       ylab="Component+Residual", 
       smooth=list(span=3/4), order=2, main="(b)")
ceresPlot(mod.2, "gdp", xlab="GDP per Captia", col.lines=c("black", "black"), 
          ylab="Component+Residual", 
       smooth=list(span=3/4), main="(c)")
par(par)

# regression model with region interactions for the CIA data

CIA$region <- factor(CIA$region, # reorder factor levels
                  levels=c("Europe", "America", "Oceania", "Asia", "Africa"))
mod.3 <- lm(log(infant) ~ region*(gdp + health + gini), data=CIA)
S(mod.3)

# Table 6.1

Anova(mod.3)

# Fig. 6.6: predictor effect plots for the model with region interactions

plot(predictorEffect("gdp", mod.3, residuals=TRUE), 
     lines=list(col=c("black", "black"), lty=2), 
     axes=list(grid=TRUE), 
     id=list(n=2, labels=ifelse(rownames(CIA) %in% 
                c("United.States", "Canada", "Luxembourg", "Singapore"), 
                rownames(CIA), "")),
     partial.residuals=list(plot=TRUE, smooth.col="black", lty=1, span=1.0, 
                            pch=20), main="(a)",
     xlab="GDP per Capita ($1000s)", ylab="log(Infant Mortality)", 
     lattice=list(layout=c(5, 1)))

plot(predictorEffect("health", mod.3, residuals=TRUE), 
     lines=list(col=c("black", "black"), lty=2), 
     axes=list(grid=TRUE), id=list(n=1, 
                      labels=ifelse(rownames(CIA) == "United.States", 
                                    rownames(CIA), "")),
     partial.residuals=list(plot=TRUE, smooth.col="black", lty=1, span=1.0, 
                            pch=20), main="(b)",
     xlab="Health Expeditures per Capita ($1000s)", 
     ylab="log(Infant Mortality)", lattice=list(layout=c(5, 1)))

plot(predictorEffect("gini", mod.3, residuals=TRUE), 
     lines=list(col=c("black", "black"), lty=2), 
     axes=list(grid=TRUE),
     partial.residuals=list(plot=TRUE, smooth.col="black", lty=1, span=1.0, 
                            pch=16), main="(c)",
     xlab="Gini Coefficient of Income Inequality", 
     ylab="log(Infant Mortality)", lattice=list(layout=c(5, 1)))

  # a simpler command producing a less customized version of Fig. 6.6

plot(predictorEffects(mod.3, ~ gdp + health + gini, residuals=TRUE), 
     partial.residuals=list(span=0.9))

# Fig. 6.7: marginal model plots for the original CIA regression

par <- par(mfrow=c(2, 2))

mmp(mod.2, variable=CIA$gdp, col.line=c(data=gray(.50), model="black"), 
    xlab="GDP per Capita", ylab="log(Infant Mortality)", 
    col=gray(.50), main="(a)", key=FALSE, sd=TRUE)
mmp(mod.2, variable=CIA$health, col.line=c(data=gray(.50), model="black"), 
    xlab="Health Expenditures",  ylab="log(Infant Mortality)", 
    col=gray(.50), main="(b)", key=FALSE, sd=TRUE)
mmp(mod.2, variable=CIA$gini, col.line=c(data=gray(.50), model="black"), 
    xlab="Gini Coefficient",  ylab="log(Infant Mortality)", 
    col=gray(.50), main="(c)", key=FALSE, sd=TRUE)
mmp(mod.2, col.line=c(data=gray(.50), model="black"),  
    ylab="log(Infant Mortality)", 
    col=gray(.50), main="(d)", key=FALSE, sd=TRUE)

  # a simpler version of Fig. 6.7
mmps(mod.2, sd=TRUE)

# Fig. 6.8: marginal model plots for the respecified CIA regression

mmp(mod.2b, variable=CIA$gdp, col.line=c(data=gray(.50), model="black"), 
    xlab="GDP per Capita", ylab="log(Infant Mortality)",
    col=gray(.50), main="(a)", key=FALSE, 
    id=list(n=2, method="mahal", cex=0.7))
mmp(mod.2b, variable=CIA$health, col.line=c(data=gray(.50), model="black"), 
    xlab="Health Expenditures", ylab="log(Infant Mortality)",
    col=gray(.50), main="(b)", key=FALSE)
mmp(mod.2b, variable=CIA$gini, col.line=c(data=gray(.50), model="black"), 
    xlab="Gini Coefficient", ylab="log(Infant Mortality)",
    col=gray(.50), main="(c)", key=FALSE)
mmp(mod.2b, col.line=c(data=gray(.50), model="black"), 
    ylab="log(Infant Mortality)",
    col=gray(.50), main="(d)", key=FALSE, id=list(n=1, method="mahal", 
                                                  cex=0.7))

par(par)

  # a simpler version of Fig. 6.8 
  #  (note different treatment of health expenditures)

mmps(mod.2b)

# regression for GSS vocabuilary data

GSS <- read.table("GSS.txt", header=TRUE)
  # assumes that GSS.txt is in the current directory, adjust as necessary

set.seed(818218) # for reproducibility
sel <- sample(nrow(GSS), 5000)
GSS.a <- GSS[sel, ]  # exploratory subsample
GSS.b <- GSS[-sel, ] # validation subsample

modv.1 <- lm(vocab ~ age + educ + year + gender + nativeBorn, data=GSS.a)
S(modv.1)

# Fig. 6.9: component-plus-residual plots for the linear vocabulary regression

par(fig=c(0, .5, .5, 1), mar=c(5.1, 4.1, 2.1, 1.1)) # top-left panel
crPlot(modv.1, "age", main="(a)", col.lines=c("black", "black"), cex=0.5, 
       col="darkgray",
       smooth=list(span=0.2), xlab="Age (years)", ylab="Component+Residual")

par(fig=c(.5, 1, .5, 1)) # top-right panel
par(new=TRUE)
crPlot(modv.1, "educ", main="(b)", col.lines=c("black", "black"), cex=0.5, 
       col="darkgray",
       smooth=list(span=0.2), xlab="Education (years)", 
       ylab="Component+Residual")

par(fig=c(.25, .75, 0, .5)) # bottom panel
par(new=TRUE)
crPlot(modv.1, "year", main="(c)", col.lines=c("black", "black"), cex=0.5, 
       col="darkgray",
       smooth=list(span=0.2), xlab="Year", ylab="Component+Residual")

# Table 6.2: nonlinearity tests for age

modv.2a0 <- lm(vocab ~ as.factor(educ) + as.factor(year) + gender + nativeBorn,
               data=GSS.b)
modv.2a1 <- update(modv.2a0, . ~ . + age)
modv.2a2 <- update(modv.2a0, . ~ . + poly(age, 2))
modv.2a3 <- update(modv.2a0, . ~ . + as.factor(age))
anova(modv.2a0, modv.2a1, modv.2a2, modv.2a3)

# Table 6.3: nonlinearity tests for education

modv.2e0 <- lm(vocab ~ as.factor(age) + as.factor(year) + gender + nativeBorn,
               data=GSS.b)
modv.2e1 <- update(modv.2e0, . ~ . + educ)
modv.2e2 <- update(modv.2e0, . ~ . + educ + I(educ == 12) + I(educ == 16))
modv.2e3 <- update(modv.2e0, . ~ . + as.factor(educ))
anova(modv.2e0, modv.2e1, modv.2e2, modv.2e3)

# Table 6.4 nonlinearity test for year

modv.2y0 <- lm(vocab ~ as.factor(age) + as.factor(educ) + gender + nativeBorn, 
               data=GSS.b)
modv.2y1 <- update(modv.2y0, . ~ . + year)
modv.2y2 <- update(modv.2y0, . ~ . + as.factor(year))
anova(modv.2y0, modv.2y1, modv.2y2)

# Fig. 6.10: regression spline

par(mfrow=c(1, 1))
library(splines)
set.seed(123)
x <- sort(runif(200, 0, 10))
Ey <- (x^(2/3))*cos(x) + x
y <- Ey + rnorm(200)
plot(x, y, xlab="", ylab="")
mtext("x", 1, line=2, at=9)
mtext("y", 2, line=2, at=11, las=1)
mtext(c(expression(k[1]), expression(k[2]), expression(k[3])), 3, line=0, 
      at=seq(2.5, 7.5, by=2.5))
lines(x, Ey, lty=2, lwd=2)
abline(v=seq(2.5, 7.5, by=2.5), lty=2)
abline(v=c(0, 10))
m <- lm(y ~ bs(x, knots=seq(2.5, 7.5, by=2.5)))
brief(m)
lines(x, fitted(m), lty=1, lwd=2)
lines(x, fitted(lm(y ~ poly(x, 6))), lty=4)

# Box-Tidwell model fit to CIA data

boxTidwell(log(infant) ~ gdp + gini, other.x=~poly(health, 2, raw=TRUE), 
           data=CIA)

# Fig 6.11: constructed-variable plots for Box-Tidwell regression

btmod <- lm(log(infant) ~ gdp + gini + poly(health, 2, raw=TRUE) + 
              I(gdp*log(gdp)) + I(gini*log(gini)), data=CIA)
S(btmod) # shows score tests

par <- par(mfrow=c(1, 2))
avPlot(btmod, "I(gdp * log(gdp))", main="(a)", id=list(method="mahal", n=1), 
       xlab="Constructed Variable(GDP) | Others", 
       ylab="log(Infant Mortality) | Others",
       col.lines="black")
avPlot(btmod, "I(gini * log(gini))", main="(b)", id=FALSE, 
       xlab="Constructed Variable(Gini) | Others", 
       ylab="log(Infant Mortality) | Others",
       col.lines="black")

par(par)
