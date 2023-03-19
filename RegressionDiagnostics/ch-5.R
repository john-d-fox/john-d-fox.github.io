# Regression Diagnostics, 2nd Edition
# John Fox
# last modified: 2019-06-18

# R script for Ch. 5

library(car)
CIA <- read.table("CIA.txt", header=TRUE)
  # assumes that CIA.txt is in the current directory, adjust as necessary

mod.1 <- lm(infant ~ gdp + health + gini, data=CIA)
S(mod.1)

# Fig. 5.1: QQ plot of studentized residuals

set.seed(54321) # for reproducibility
qqPlot(mod.1, reps=1000, id=FALSE, col.lines="black", 
       ylab="Ordered Studentized Residuals")

# Fig. 5.2: density plot of studentized residuals

densityPlot(rstudent(mod.1), adjust=0.75, n=1000, xlab="Studentized Residuals")

# Fig. 5.3: boxplots of studentized residuals for 
#           various transformations of infant mortality

rstud <- matrix(rstudent(mod.1))
for (lambda in c(0.5, 0, -0.5, -1)){
    m <- update(mod.1, bcPower(infant, lambda) ~ .)
    rstud <- cbind(rstudent(m), rstud)
}
colnames(rstud) <- c(-1, -0.5, "log", 0.5, 1)
par(mar=c(5.1, 5.1, 4.1, 1.1))
Boxplot(rstud, id=FALSE, xlab=expression("Powers," ~ lambda),
        ylab=expression(
          "Studentized Residuals for   " ~ t[BC](Infant, lambda)))
text(locator(1), labels="Luxembourg") # identify with mouse-click

# regression for log(infant mortality)

mod.2 <- update(mod.1, log(infant) ~ .)
S(mod.2)

# Box-Cox transformation of infant mortality 

p1 <- powerTransform(mod.1)
sqrt(p1$invHess) # SE(lambda-hat)

summary(p1)

# Fig. 5.4: constructed-variable plot for Box-Cox transformation

mod.bc <- update(mod.1, . ~ . + boxCoxVariable(infant) )
S(mod.bc) # shows score test for constructed variable
avPlot(mod.bc, "boxCoxVariable(infant)", col.lines="black", id=FALSE,
       main="", ylab="Infant | Other xs", 
       xlab="Constructed Variable | Other xs")

# Fig. 5.5: plots of studentized residuals vs. fitted values
#           for artificial data

set.seed(12345)
x <- runif(500, 1, 10)
y1 <- x + rnorm(500, 0, 1)
y2 <- x + rnorm(500, 0, x)
m1 <- lm(y1 ~ x)
m2 <- lm(y2 ~ x)
scatterplot(fitted(m1), rstudent(m1), smooth=list(span=2/3, lwd.smooth=3, 
                                  lwd.spread=3), regLine=FALSE, axes=FALSE,
            boxplots=FALSE, col=c("black", "black"), main="(a)",
            xlab="Fitted Values", ylab="Studentized Residuals")
abline(0, 0)

scatterplot(fitted(m2), rstudent(m2), smooth=list(span=0.5, lwd.smooth=3, 
                                  lwd.spread=3), regLine=FALSE, axes=FALSE,
            boxplots=FALSE, col=c("black", "black"), main="(b)",
            xlab="Fitted Values", ylab="Studentized Residuals")
abline(0, 0)

# Fig. 5.6: plots of studentized residuals vs fitted values 
#           for the two CIA regressions

  # infant mortality untransformed

scatterplot(fitted(mod.1), rstudent(mod.1), smooth=list(span=2/3, 
                                lwd.smooth=3, lwd.spread=3), regLine=FALSE,
            boxplots=FALSE, col=c("black", "black"), main="(a)",
            xlab="Fitted Values", ylab="Studentized Residuals")
abline(0, 0)

  # infant mortality log-transformed

scatterplot(fitted(mod.2), rstudent(mod.2), smooth=list(span=2/3, 
                                lwd.smooth=3, lwd.spread=3), regLine=FALSE,
            boxplots=FALSE, col=c("black", "black"), main="(b)",
            xlab="Fitted Values", ylab="Studentized Residuals")
abline(0, 0)

# Fig. 5.7: spread-level plots for the two CIA regressions

spreadLevelPlot(mod.1, main="(a)", smooth=FALSE, col.lines="black")
spreadLevelPlot(mod.2, main="(b)", smooth=FALSE, col.lines="black")

# Bruesch-Pagan tests of nonconstant error variance for the CIA regressions

ncvTest(mod.1)
ncvTest(mod.2)

# Table 5.1: Robust and bootstrapped SEs for the CIA regression

S(mod.1, vcov.=hccm(mod.1))

set.seed(32973742) # for reproducibility
boot.1 <- Boot(mod.1, R=1000)
S(mod.1, vcov.=vcov(boot.1))
