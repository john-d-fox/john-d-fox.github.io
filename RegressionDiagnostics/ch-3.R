# Regression Diagnostics, 2nd Edition
# John Fox
# last modified: 2019-06-18

# R script for Ch. 3

# Fig. 3.1: univariate displays

CIA <- read.table("CIA.txt", header=TRUE)
  # assumes that CIA.txt is in the current directory, adjust as necessary

library(car)
library(RcmdrMisc)

par(mfrow=c(2, 2))
Hist(CIA$infant, xlab="Infant Mortality Rate per 1000", ylab="Frequency", 
     col="gray", main="(a)")
Boxplot(~infant, data=CIA, main="(b)")
densityPlot(CIA$infant, from=0, normalize=TRUE, 
            xlab="Infant Mortality Rate per 1000", main="(c)")
qqPlot(~infant, data=CIA, ylab="Infant Mortality Rate per 1000", 
       xlab="Normal Quantiles", main="(d)",
       id=list(method=c(TRUE, rep(FALSE, 132), TRUE)), col.lines="black")

# Fig. 3.2: QQ plots

par <- par(mar=c(5.1, 5.1, 4.1, 2.1))

set.seed(123) # for reproducibility
x.norm <- rnorm(50, 100, 15)
par(fig=c(0, .5, .5, 1)) # top-left panel
qqPlot(x.norm, id=FALSE, col.lines="black", xlab="Quantiles of N(0, 1)", 
       ylab="Sample Drawn from N(100, 15)", main="(a)")

set.seed(321)
x.t <- rt(50, df=2)
par(fig=c(.5, 1, .5, 1)) # top-right panel
par(new=TRUE)
qqPlot(x.t, id=FALSE, col.lines="black", xlab="Quantiles of N(0, 1)", 
       ylab=expression("Sample Drawn from"~~t[2]), main="(b)")

set.seed(666)
x.chisq <- rchisq(50, df=2)
par(fig=c(.25, .75, 0, .5)) # bottom panel
par(new=TRUE)
qqPlot(x.chisq, id=FALSE, col.lines="black", xlab="Quantiles of N(0, 1)", 
       ylab=expression("Sample Drawn from"~~chi[2]^2), main="(c)")

par(par) # restore graphical parameters
par(mfrow=c(1, 1))

# Fig. 3.3: Box-Cox family

n <- 500
x <- seq(0.1, 3, length=n)
x1 <- bcPower(x, 1)
x0.5 <- bcPower(x, 0.5)
x0 <- bcPower(x, 0)
xm0.5 <- bcPower(x, -0.5)
xm1 <- bcPower(x, -1)
x2 <- bcPower(x, 2)
x3 <- bcPower(x, 3)
xlim <- range(c(x1, x0.5, x0, xm0.5, xm1, x2, x3))

plot(range(x)+ c(-0.6, 0.5), c(-5, 10), type="n", xlab="", ylab="", las=1)
usr <- par("usr")
text(usr[2], usr[3] - 1, label="x", xpd=TRUE)
text(usr[1] - 0.2, usr[4] + 0.75, label=expression(t[BC](x, lambda)), xpd=TRUE)
lines(x, x1, lwd=2)
text(x[n]+0.0625, x1[n], labels=expression(lambda == 1), adj=c(0, 0.2))
lines(x, x2, lwd=2)
text(x[n]+0.0625, x2[n], labels=expression(lambda == 2), adj=c(0, 0.2))
lines(x, x3, lwd=2)
text(x[n]+0.0625, x3[n], labels=expression(lambda == 3), adj=c(0, 0.2))
lines(x, x0.5, lwd=2)
text(x[1]-0.025, x0.5[1], labels=expression(lambda == 0.5), adj=c(1, 0.3))
lines(x, x0, lwd=2)
text(x[1]-0.025, x0[1], labels=expression(lambda == 0), adj=c(1, 0.3))
lines(x, xm0.5, lwd=2)
text(x[1]-0.025, xm0.5[1], labels=expression(lambda == -0.5), adj=c(1, 0.3))
lines(x=c(1, 1), y=c(usr[3], 0), lty=2)
lines(x=c(usr[1], 1), y=c(0, 0), lty=2)

# Fig. 3.4: symmetry boxplots

symbox(~infant, data=CIA, xlab=expression("Powers,"~lambda), ylab="", 
       powers = c(-1, -0.5, 0, 0.33, 0.5, 1))
mtext(2, 1, text=expression(t[BC]("Infant Mortality",~lambda)))

# Fig. 3.5: log-transformed infant mortality

densityPlot(~log(infant), data=CIA, adjust=0.75, xlab="log(Infant Mortality)")
basicPowerAxis(0, side="above", at=c(1, 5, 10, 20, 50, 100), 
               axis.title="Infant Mortality Rate per 1000")

# Estimation of transformation of infant mortality

S(pt <- powerTransform(infant ~ 1, data=CIA))
pt$lambda # estimated lambda
sqrt(pt$invHess) # SE

# Fig. 3.6: scatterplot matrix for CIA data

scatterplotMatrix(~infant + gdp + gini + health, data=CIA, 
                  var.labels=c("Infant Mortality", "GDP per Capita", 
                               "Gini Coefficient", "Health Spending"),
                  smooth=list(smoother=loessLine, var=FALSE, lwd.smooth=3), 
                  col="black")

# Fig. 3.7: scatterplot matrix for transformed CIA data

scatterplotMatrix(~log(infant) + basicPower(gdp, 0.2) + log(gini) + 
                    log(health), data=CIA,
                  var.labels=c(expression(log("Infant Mortality")), 
                               expression("GDP per Capita"^0.2), 
                               expression(log("Gini Coefficient")), 
                               expression(log("Health Spending"))),
                  smooth=list(smoother=loessLine, var=FALSE, lwd.smooth=3), 
                  col="black")

# Table 3.2: estimates of transformation parameters

S(pt4 <- powerTransform(cbind(infant, gdp, gini, health) ~ 1, data=CIA))

# Fig. 3.8: patterns of nonlinearity

par <- par(mar=c(2, 3, 3, 2))

par(fig=c(0, .5, .5, 1)) # top-left panel
x <- seq(0, 1, length=200)
Ey <- rev(1 - x^2)
y <- Ey + 0.1*rnorm(200)
plot(x, y, axes=FALSE, frame=TRUE, main="(a) monotone, simple", 
     cex.main=1, xlab="", ylab="", col="darkgray", cex=0.75)
lines(x, Ey, lwd=2)
mtext("x", side=1, adj=1)
mtext("y ", side=2, at=max(y), las=1)

par(fig=c(.5, 1, .5, 1)) # top-right panel
par(new=TRUE)
x <- seq(0.02, 0.99, length=200)
Ey <- log(x/(1 - x))
y <- Ey + 0.5*rnorm(200)
plot (x, y, axes=FALSE, frame=TRUE, main="(b) monotone, not simple", 
      cex.main=1, xlab="", ylab="", col="darkgray", cex=0.75)
lines(x, Ey, lwd=2)
mtext("x", side=1, adj=1)
mtext("y ", side=2, at=max(y), las=1)

par(fig=c(.25, .75, 0, .5)) # bottom panel
par(new=TRUE)
x <- seq(0.2, 1, length=200)
Ey <- (x - 0.5)^2
y <- Ey + 0.04*rnorm(200)
plot(x, y, axes=FALSE, frame=TRUE, main="(c) non-monotone, simple", 
     cex.main=1, xlab="", ylab="", col="darkgray", cex=0.75)
lines(x, Ey, lwd=2)
mtext("x", side=1, adj=1)
mtext("y ", side=2, at=max(y), las=1)

par(par)

# Fig. 3.9: bulging rule 

library(plotrix)
library(sfsmisc)
library(MASS)

par <- par(mar=c(3, 5, 3, 5), mfrow=c(1,1))
eqscplot(c(-1, 1), c(-1, 1), axes=FALSE, ann=FALSE, type="n")
points(0, 0, cex=2, pch=16)

p.arrows(0, 0, 0.9, 0, fill="black")
p.arrows(0, 0, -0.9, 0, fill="black")
p.arrows(0, 0, 0, 0.9, fill="black")
p.arrows(0, 0, 0, -0.9, fill="black")

draw.arc(0, 0, radius=0.8, lwd=2, deg1=10, deg2=80, n=500)
draw.arc(0, 0, radius=0.8, lwd=2, deg1=100, deg2=170, n=500)
draw.arc(0, 0, radius=0.8, lwd=2, deg1=190, deg2=260, n=500)
draw.arc(0, 0, radius=0.8, lwd=2, deg1=280, deg2=350, n=500)

text(0.925, 0.075, labels="x up:", xpd=TRUE, adj=0)
text(0.925, -0.075, labels=expression(paste(x^2, ",", x^3, ",", ldots)), 
     xpd=TRUE, adj=0)
text(-0.925, 0.075, labels="x down:", xpd=TRUE, adj=1)
text(-0.925, -0.075, labels=expression(paste(sqrt(x), ",", log(x), ",", 
                                             ldots)), xpd=TRUE, adj=1)
text(0, 0.945, labels="y up:", xpd=TRUE, adj=0.5)
text(0, 1.125, labels=expression(paste(y^2, ",", y^3, ",", ldots)),
     xpd=TRUE, adj=0.5)
text(0, -0.945, labels="y down:", xpd=TRUE, adj=0.5)
text(0, -1.125, labels=expression(paste(sqrt(y), ",", log(y), ",", ldots)), 
     xpd=TRUE, adj=0.5)

z <- sqrt(0.64/2)
p.arrows(z, z, z + 0.15, z + 0.15, fill="black")
p.arrows(z, -z, z + 0.15, -z - 0.15, fill="black")
p.arrows(-z, z, -z - 0.15, z + 0.15, fill="black")
p.arrows(-z, -z, -z - 0.15, -z - 0.15, fill="black")

par(par)

# Fig. 3.10: scatterplot of infant mortality vs GDP

scatterplot(infant ~ gdp, data=CIA, smooth=list(smoother=loessLine, var=FALSE, 
                                                lwd.smooth=3), col="black",
            regLine=list(lwd=3),
            xlab="GDP per Capita ($1000s)", 
            ylab="Infant Mortality Rate (per 1000)")

# Fig. 3.11: scatterplot of log(infant mortality) vs log(GDP)

par <- par(oma=c(0, 0, 0, 2))
scatterplot(log(infant) ~ log(gdp), data=CIA, 
            smooth=list(smoother=loessLine, var=FALSE, lwd.smooth=3), 
            col="black",
            regLine=list(lwd=3),
            xlab="log(GDP per Capita)", ylab="log(Infant Mortality)", 
            reset.par=FALSE)
basicPowerAxis(0, side="right", at=c(1, 5, 10, 20, 50, 100), 
               axis.title="Infant Mortality Rate per 1000")
basicPowerAxis(0, side="above", at=c(1, 2, 5, 10, 20, 50), 
               axis.title="GDP per Capita ($1000s)")
par(par)

# regression of log(infant mortality) on log(GDP)

S(lm(log(infant) ~ log(gdp), data=CIA))

# Fig. 3.12: how loess works

par <- par(mfrow=c(2, 2), las=1)   # 2 x 2 array of graphs

D <- CIA[, c("gdp", "infant")]
gdp <- D$gdp
infant <- D$infant
ord <- order(gdp)   # sort data by gdp
gdp <- gdp[ord]
infant <- infant[ord]

x0 <- gdp[96]           # focal x = x_(96) (Latvia)
dist <- abs(gdp - x0)   # distance from focal x
h <- sort(dist)[89]     # bandwidth for span of 2/3 (where n = 134)
pick <- dist <= h       # observations within window

plot(gdp, infant, xlab="GDP per Capita ($1000s)", 
     ylab="Infant Mortality Rate per 1000",
     type="n", main="(a) Observations Within the Window\nspan = 2/3")
points(gdp[pick], infant[pick], col="black")
points(gdp[!pick], infant[!pick], col="gray")
points(gdp[96], infant[96], pch=16, cex=1.5, col="black") # focal point
abline(v=x0, col="black")    # at focal x
abline(v=c(x0 - h, x0 + h), lty=2, col="black")  # window
text(x0, par("usr")[4] + 5, expression(x[(96)]), xpd=TRUE, col="black")

plot(range(gdp), c(0,1), xlab="GDP per Capita ($1000s)",
     ylab="Tricube Kernel Weight",
     type="n", main="(b) Tricube Weights")
abline(v=x0, col="black")
abline(v=c(x0 - h, x0 + h), lty=2, col="black")

tricube <- function(x, x0, h) {
    z <- abs(x - x0)/h
    ifelse(z < 1, (1 - z^3)^3, 0)
}
tc <- function(x) tricube(x, x0, h) # to use with curve
curve(tc, min(gdp), max(gdp), n=1000, lwd=2, add=TRUE, col="black")
points(gdp[pick], tricube(gdp, x0, h)[pick], pch=16, col="black")
abline(h=c(0, 1), col="gray")

plot(gdp, infant, xlab="GDP per Capita ($1000s)", 
     ylab="Infant Mortality Rate per 1000",
     type="n", main="(c) Local Quadratic Regression")
points(gdp[pick], infant[pick], col="black")
points(gdp[!pick], infant[!pick], col="gray")
abline(v=x0, col="black")
abline(v=c(x0 - h, x0 + h), lty=2, col="black")
mod <- lm(infant ~ poly(gdp, 2), weights=tricube(gdp, x0, h))
new <- data.frame(gdp=seq(x0 - h, x0 + h, length=51))
fit <- predict(mod, newdata=new)
  # local regression line
lines(seq(x0 - h, x0 + h, length=51), fit, lwd=2, col="black") 
points(x0, fit[26], pch=16, cex=2, col="black")
text(x0, par("usr")[4] + 5, expression(x[(96)]), xpd=TRUE, col="black")
text(x0 + 1, fit[26] + 2.5, expression(hat(y)[(96)]), adj=c(0, 0), col="black")

plot(gdp, infant, xlab="GDP per Capita  ($1000s)", 
     ylab="Infant Mortality Rate (per 1000)",
     main="(d) Complete Local-Quadratic Estimate", col=gray(0.25))
yhat <- numeric(length(gdp))
for (i in 1:length(gdp)){   # kernel estimate at each x
    x0 <- gdp[i]
    dist <- abs(gdp - x0)
    h <- sort(dist)[89]
    mod <- update(mod, weights=tricube(gdp, x0, h))
    yhat[i] <- predict(mod, newdata=data.frame(gdp=x0))
}
lines(gdp, yhat, lwd=2, col="black")

par(par)
par(mfrow=c(1, 1))

# Fig. 3.13: Boxplots of GDP vs region

Boxplot(gdp ~ region, data=CIA, id=list(location="lr"), 
        ylab="GDP per Capita ($1000s)", xlab="Region of the World")

# Fig. 3.14: spread-level plot for GDP by region

  # reorder levels of region
CIA$region <- factor(CIA$region, levels=c("Europe", "America", 
                                          "Oceania", "Asia", "Africa"))

spreadLevelPlot(gdp ~ region, data=CIA, main="", 
                ylab="Inter-Quartile Range", col.lines="black")

# Fig. 3.15: boxplots for transformed GDP by region

par <- par(mar=c(5.1, 4.1, 3.1, 5.5), mfrow=c(1, 2))

Boxplot(gdp^(1/3) ~ region, data=CIA, id=list(location="lr"), 
        ylab=expression(GDP^{1/3}), xlab="Region of the World", main="(a)")
basicPowerAxis(1/3, side="right", at=c(1, 2, 5, 10, 20, 50, 100), 
               axis.title="GDP per Capita ($1000s)")

Boxplot(log(gdp) ~ region, data=CIA, id=list(location="lr"), 
        ylab="log(GDP)", xlab="Region of the World", main="(b)")
basicPowerAxis(0, side="right", at=c(1, 2, 5, 10, 20, 50, 100), 
               axis.title="GDP per Capita ($1000s)")

par(par)
