# Regression Diagnostics, 2nd Edition
# John Fox
# last modified: 2019-06-18

# R script for Ch. 4

# Fig 4.1: regression outlier

library(MASS)
library(car)
set.seed(1468) # for reproducibility
x <- mvrnorm(50, mu=c(0, 0), Sigma=matrix(c(1, .85, .85, 1), 2, 2), 
             empirical=TRUE)
colnames(x) <- c("x", "y")

plot(x, xlab="", ylab="", axes=FALSE, xlim=c(-2.8, 2.8), ylim=c(-2.8, 2.8))
dataEllipse(x, level=c(.50, .75, .95), add=TRUE, col=c("black", "darkgray"), 
            lwd=2)
points(mean(x[, 1]), mean(x[, 2]), pch=16, col="darkgray", cex=2)
points(mean(x[, 1]), mean(x[, 2]), cex=2)
box()
abline(lm(x[, 2] ~ x[, 1]), lwd=2)
points(-1.25, 1.25, pch=16, cex=2)
lines(c(-3, -1.25), c(1.25, 1.25), lty=2)
lines(c(-1.25, -1.25), c(1.25, -4), lty=2)
mtext(side=1, text="x", at=2.5, line=1)
mtext(side=2, text="y", at=2.5, line=1, las=1)
rug(c(x[, 1], -1.25), ticksize=-0.025, side=1, lwd=1)
rug(c(x[, 2], 1.25), ticksize=-0.025, side=2, lwd=1)


# Fig 4.2: outliers, leverage influence

set.seed(12345)
x <- seq(3, 6, length=8)
x <- x + runif(8, -0.25, 0.25)
y <- 0.5*x + rnorm(8, sd=0.1)

x1 <- c(x, mean(x))
m <- lm(y ~ x)
b <- coef(m)
y1 <- c(y, b[1] + b[2]*mean(x) + 4)

x2 <- c(x, 12)
y2 <- c(y, b[1] + b[2]*12 + 4)

x3 <- c(x, 12)
y3 <- c(y, b[1] + b[2]*12)

xlim <- range(c(x1, x2, x3))
ylim <- range(c(y1, y2, y3))

par <- par(mar=c(3.1, 3.1, 4.1, 3.1))

par(fig=c(0, .5, .5, 1)) # top-left panel
plot(x1, y1, axes=FALSE, frame=TRUE, xlab="", ylab="", main="(a)", cex=2,
     xlim=xlim, ylim=ylim)
mtext(side=1, text="x", at=xlim[2] - 0.5, line=1)
mtext(side=2, text="y", at=ylim[2] - 0.5, line=1, las=1)
points(x1[9], y1[9], pch=16, cex=2)
points(x1[1:8], y1[1:8], pch=21, cex=2, bg="darkgray")
abline(lm(y1 ~ x1, subset=1:8), lty=2, lwd=2)
abline(lm(y1 ~ x1), lty=1, lwd=2)
rug(x1, side=1, ticksize=-0.025, lwd=1)
rug(y1, side=2, ticksize=-0.025, lwd=1)

par(fig=c(.5, 1, .5, 1)) # top-right panel
par(new=TRUE)
plot(x2, y2, axes=FALSE, frame=TRUE, xlab="", ylab="", main="(b)", cex=2,
     xlim=xlim, ylim=ylim)
mtext(side=1, text="x", at=xlim[2] - 0.5, line=1)
mtext(side=2, text="y", at=ylim[2] - 0.5, line=1, las=1)
points(x2[9], y2[9], pch=16, cex=2)
points(x2[1:8], y2[1:8], pch=21, cex=2, bg="darkgray")
abline(lm(y2 ~ x2, subset=1:8), lty=2, lwd=2)
abline(lm(y2 ~ x2), lty=1, lwd=2)
rug(x2, side=1, ticksize=-0.025, lwd=1)
rug(y2, side=2, ticksize=-0.025, lwd=1)

par(fig=c(.25, .75, 0, .5)) # bottom panel
par(new=TRUE)
plot(x3, y3, axes=FALSE, frame=TRUE, xlab="", ylab="", main="(c)", cex=2,
     xlim=xlim, ylim=ylim)
mtext(side=1, text="x", at=xlim[2] - 0.5, line=1)
mtext(side=2, text="y", at=ylim[2] - 0.5, line=1, las=1)
points(x3[9], y3[9], pch=16, cex=2)
points(x3[1:8], y3[1:8], pch=21, cex=2, bg="darkgray")
abline(lm(y3 ~ x3, subset=1:8), lty=2, lwd=2)
b3 <- coef(lm(y3 ~ x3))
abline(b3[1] + 0.1, b3[2], lty=1, lwd=2)
rug(x3, side=1, ticksize=-0.025, lwd=1)
rug(y3, side=2, ticksize=-0.025, lwd=1)

par(par) # restore graphical parameters

# Fig. 4.3 : measured and reported weight

Davis <- read.table("Davis.txt", header=TRUE) 
  # assumes that Davis.txt is in the current directory, adjust as necessary

  # reorder levels of gender:
Davis$gender <- factor(Davis$gender, levels=c("male", "female"))

scatterplot(repwt ~ weight | gender, data=Davis, xlab="Measured Weight (kg)", 
            ylab="Reported Weight (kg)",
            smooth=FALSE, main="(a)", col=c("darkgray", "black"), 
            pch=c("M", "F"), cex=0.75, jitter=list(x=0.3, y=0.3),
            legend=list(title="Gender", inset=0.02, coords="topleft"), 
            id=list(n=1, cex=0.75))

scatterplot(weight ~ repwt | gender, data=Davis, ylab="Measured Weight (kg)", 
            xlab="Reported Weight (kg)",
            smooth=FALSE, main="(b)", col=c("darkgray", "black"), 
            pch=c("M", "F"), cex=0.75, jitter=list(x=0.3, y=0.3),
            legend=FALSE, id=list(n=1, cex=0.75))

# Davis's regressions

  # of reported weight on measured weight and gender
S(mod.1 <- lm(repwt ~ weight*gender, data=Davis))

  # of measured weight on reported weight and gender
S(lm(weight ~ repwt*gender, data=Davis))

  # with corrected data
Davis2 <- Davis
Davis2[12, c("weight", "height")] <- Davis2[12, c("height", "weight")]
S(lm(repwt ~ weight*gender, data=Davis2))

# Fig. 4.4: contours of constant leverage

set.seed(54321) # for reproducibility
x <- mvrnorm(50, mu=c(0, 0), Sigma=matrix(c(1, .75, .75, 1), 2, 2), 
             empirical=TRUE)
dataEllipse(x, xlim=c(-3.5, 3.5), ylim=c(-3, 3), levels=c(0.5, 0.75, 0.9), 
            center.pch="",
            col=c("black", "black"), segments=500, xlab="", ylab="", 
            axes=FALSE, lwd=1, grid=FALSE)
dataEllipse(x, xlim=c(-3.5, 3.5), ylim=c(-3, 3), levels=0.99, center.pch="",
            col=c("black", "black"), segments=500, xlab="", ylab="", 
            axes=FALSE, lwd=2, add=TRUE, grid=FALSE)
  # warnings are innocouous
points(x=c(3.0355569, -0.9980941), y=c(2.955924, 1.263335), pch=16, cex=1.5)
points(x=mean(x[, 1]), y=mean(x[, 2]), pch=21, cex=2, bg="darkgray")
mtext(side=1, text=expression(x[1]), at=3, line=1)
mtext(side=2, text=expression(x[2]), at=3, line=1, las=1)
box()

# various diagnostic statistics for Davis's regression

S(mod.1)
max(hatvalues(mod.1))
which.max(hatvalues(mod.1))
outlierTest(mod.1)
max(cooks.distance(mod.1))
which.max(cooks.distance(mod.1))
dffits(mod.1)[12]
which.max(abs(dffits(mod.1)))
round(dfbeta(mod.1)[12, ], 5)
round(dfbetas(mod.1)[12, ], 5)

# Fig. 4.5: joint influence

par <- par(mar=c(3.1, 3.1, 4.1, 3.1))

set.seed(543)
x <- 1:5
y <- -x + rnorm(5, 0, 0.5)

par(fig=c(0, .5, .5, 1)) # top-left panel
x1 <- c(x, 8, 8)
y1 <- c(y, 2, 2.25)
plot(x1, y1, pch=c(rep(21, 5), 15, 17), cex=c(rep(1.5, 5), 1.5, 1.5),
     axes=FALSE, xlab="", ylab="", frame=TRUE, bg="darkgray", main="(a)")
mtext(side=1, text="x", at=7.5, line=1)
mtext(side=2, text="y", at=2, line=1, las=1)
abline(lm(y1 ~ x1, subset=1:5), lty="dotdash", lwd=2)
abline(lm(y1 ~ x1), lwd=2)
abline(lm(y1 ~ x1, subset=1:6), lwd=2, lty="dashed")

par(fig=c(.5, 1, .5, 1)) # top-right panel
par(new=TRUE)
x2 <- c(x, -3, 9)
y2 <- c(y, -8, 2)
plot(x2, y2, pch=c(rep(21, 5), 15, 17), cex=c(rep(1.5, 5), 1.5, 1.5),
     axes=FALSE, xlab="", ylab="", frame=TRUE, bg="darkgray", main="(b)")
mtext(side=1, text="x", at=9, line=1)
mtext(side=2, text="y", at=2, line=1, las=1)
abline(lm(y2 ~ x2, subset=1:5), lty="dotdash", lwd=2)
abline(lm(y2 ~ x2), lwd=2)
abline(lm(y2 ~ x2, subset=1:6), lwd=2, lty="dashed")

par(fig=c(.25, .75, 0, .5)) # bottom panel
par(new=TRUE)
x3 <- c(x, 8, 8)
y3 <- c(-y, 10.5, 7.0)
plot(x3, y3, pch=c(rep(21, 5), 15, 17), cex=c(rep(1.5, 5), 1.5, 1.5),
     axes=FALSE, xlab="", ylab="", frame=TRUE, bg="darkgray", main="(c)")
mtext(side=1, text="x", at=7.5, line=1)
mtext(side=2, text="y", at=9.25, line=1, las=1)
abline(lm(y3 ~ x3, subset=1:5), lty="dotdash", lwd=2)
abline(lm(y3 ~ x3), lwd=2)
abline(lm(y3 ~ x3, subset=1:6), lwd=2, lty="dashed")

par(par)

# Duncan's occupational prestige regression

Duncan <- read.table("Duncan.txt", header=TRUE)
  # assumes that Duncan.txt is in the current directory, adjust accordingly
mod.duncan <- lm(prestige ~ income + education, data=Duncan)
S(mod.duncan)

# Fig. 4.6: added-variable plots for Duncan's regression

par <- par(mfrow=c(1, 2))
avPlot(mod.duncan,"income", main="(a)", id=list(method="mahal", n=3), 
       xlab="Income | Education", ylab="Prestige | Education",
       col.lines="black")
avPlot(mod.duncan,"education", main="(b)", id=list(method="mahal", n=3), 
       xlab="Education | Income", ylab="Prestige | Income",
       col.lines="black")
par(par)

# Fig. 4.7: influence bubble plot for Duncan's regression

influencePlot(mod.duncan, xlab="Hatvalues")

# refitting Duncan's regression removing jointly influential cases

S(update(mod.duncan, subset=-c(6, 16))) # remove minister, conductor
S(update(mod.duncan, subset=-c(6, 16, 27))) # remove RR engineer too
