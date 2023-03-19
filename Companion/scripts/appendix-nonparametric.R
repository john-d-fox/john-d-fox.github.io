##-----------------------------------------------------##
##  An R Companion to Applied Regression, 3rd Edition  ##
##      J. Fox and S. Weisberg, Sage Publications      ##
##  Script for  Appendix on Nonparametric Regression   ##
##-----------------------------------------------------##

library("carData")

p.arrows <- function (x1, y1, x2, y2, size = 1, width = (sqrt(5) - 1)/4/cin,
    fill = 1, ...)
{
    cin <- size * par("cin")[2]
    uin <- if (is.R())
        1/xyinch()
    else par("uin")
    segments(x1, y1, x2, y2, ...)
    x <- sqrt(seq(0, cin^2, length = floor(35 * cin) + 2))
    delta <- 0.005/2.54
    x.arr <- c(-x, -rev(x))
    wx2 <- width * x^2
    y.arr <- c(-wx2 - delta, rev(wx2) + delta)
    deg.arr <- c(atan2(y.arr, x.arr), NA)
    r.arr <- c(sqrt(x.arr^2 + y.arr^2), NA)
    theta <- atan2((y2 - y1) * uin[2], (x2 - x1) * uin[1])
    lx <- length(x1)
    Rep <- rep(length(deg.arr), lx)
    x2 <- rep(x2, Rep)
    y2 <- rep(y2, Rep)
    theta <- rep(theta, Rep) + rep(deg.arr, lx)
    r.arr <- rep(r.arr, lx)
    polygon(x2 + r.arr * cos(theta)/uin[1], y2 + r.arr * sin(theta)/uin[2],
        col = fill, ...)
}

par(mfrow=c(2,2), cex.main=1)   # 2 x 2 array of graphs

income <- Prestige$income
prestige <- Prestige$prestige
ord <- order(income)   # sort data by income
income <- income[ord]
prestige <- prestige[ord]

x0 <- income[80]           # focal x = x_(80)
dist <- abs(income - x0)   # distance from focal x
h <- sort(dist)[50]     # bandwidth for span of .4 (where n = 102)
pick <- dist <= h       # cases within window

plot(income, prestige, xlab="Average Income (dollars)",
    ylab="Prestige",
    type="n", main="(a)")
points(income[pick], prestige[pick], pch=16, col="black")
points(income[!pick], prestige[!pick], col="gray60")
abline(v=x0)    # focal x
abline(v=c(x0 - h, x0 + h), lty=2)  # window
text(x0, 1.05*par("usr")[4], expression(x[(80)]), xpd=TRUE)

plot(range(income), c(0,1), xlab="Average Income (dollars)",
    ylab="Tricube Kernel Weight",
    type="n", main="(b)")
abline(v=x0)
abline(v=c(x0 - h, x0 + h), lty=2)
    # function to calculate tricube weights:
tricube <- function(z) ifelse(abs(z) < 1, (1 - (abs(z))^3)^3, 0)
x <- seq(min(income), max(income), length=1000)
z <- (x - x0)/h
lines(spline(x, tricube(z)), lwd=2)
points(income[pick], tricube(((income-x0)/h)[pick]), pch=16, col="black", cex=1.3)
abline(h=c(0,1), col="gray")

plot(income, prestige, xlab="Average Income (dollars)",
    ylab="Prestige",
    type="n", main="(c)")
points(income[pick], prestige[pick], pch=16, col="black")
points(income[!pick], prestige[!pick], col="gray60")
abline(v=x0)
abline(v=c(x0 - h, x0 + h), lty=2)
mod <- lm(prestige ~ income, weights=tricube((income - x0)/h))
yhat <- predict(mod, newdata=data.frame(income=c(x0 - h, x0, x0 + h)))
lines(c(x0 - h, x0 + h), c(yhat[1], yhat[3]), col="magenta", lwd=3)
points(x0, yhat[2], pch=16, cex=2, col="magenta")
p.arrows(18000, 45, x0 + 100, yhat[2], size=.75)
text(18500, 45, expression(~hat(y)[(80)]), adj=0)


plot(income, prestige, xlab="Average Income (dollars)",
    ylab="Prestige",
    main="(d)")
yhat <- rep(0, length(income))
for (i in 1:length(income)){   # kernel estimate at each x
    x0 <- income[i]
    dist <- abs(income - x0)
    h <- sort(dist)[50]
    mod <-  lm(prestige ~ income, weights=tricube((income - x0)/h))
    yhat[i] <- predict(mod, newdata=data.frame(income=x0))
    }
lines(income, yhat, lwd=2)


mod.lo <- loess(prestige ~ income + education, span=.5, degree=1, data=Prestige)
summary(mod.lo)

inc <- with(Prestige, seq(min(income), max(income), len=25))
ed <- with(Prestige, seq(min(education), max(education), len=25))
newdata <- expand.grid(income=inc, education=ed)
fit.prestige <- matrix(predict(mod.lo, newdata), 25, 25)
persp(inc, ed, fit.prestige, theta=45, phi=30, ticktype="detailed",
    xlab="Income", ylab="Education", zlab="Prestige", expand=2/3,
    shade=0.5)

mod.lo.inc <- loess(prestige ~ income, span=.7, degree=1,
    data=Prestige)  # omitting education
mod.lo.ed <- loess(prestige ~ education, span=.7, degree=1,
    data=Prestige)  # omitting income
anova(mod.lo.inc, mod.lo)  # test for education
anova(mod.lo.ed, mod.lo)  # test for income

mod.lo.inc  # previously fit loess model
plot(prestige ~ income, data=Prestige)
inc.100 <- with(Prestige, seq(min(income), max(income), len=100))  # 100 x-values
pres <- predict(mod.lo.inc, data.frame(income=inc.100))  # fitted values
lines(inc.100, pres, lty=2, lwd=2)  # loess curve
lines(with(Prestige, smooth.spline(income, prestige, df=3.85),
    lwd=2))  # smoothing spline

library("mgcv")
mod.gam <- gam(prestige ~ s(income) + s(education), data=Prestige)
summary(mod.gam)

fit.prestige <- matrix(predict(mod.gam, newdata), 25, 25)
persp(inc, ed, fit.prestige, theta=45, phi=30, ticktype="detailed",
    xlab="Income", ylab="Education", zlab="Prestige", expand=2/3,
    shade=0.5)

par(mfrow=c(1,2))
plot(mod.gam, select=1)
plot(mod.gam, select=2)

library("car")
remove(list=objects())  # clean up everything
Mroz$k5f <- factor(Mroz$k5)
Mroz$k618f <- factor(Mroz$k618)
Mroz$k5f <- recode(Mroz$k5f, "3 = 2")
Mroz$k618f <- recode(Mroz$k618f, "6:8 = 5")
mod.1 <- gam(lfp ~ s(age) + s(inc) + k5f + k618f + wc + hc,
    family=binomial, data=Mroz)
summary(mod.1)

anova(mod.1)

par(mfrow=c(1,2))
plot(mod.1, select=1)
plot(mod.1, select=2)

mod.2 <- gam(lfp ~ age + s(inc) + k5f + k618f + wc + hc,
    family=binomial, data=Mroz)
anova(mod.2, mod.1, test="Chisq")

mod.3 <- gam(lfp ~ s(age) + inc + k5f + k618f + wc + hc,
    family=binomial, data=Mroz)
anova(mod.3, mod.1, test="Chisq")

mod.4 <- update(mod.1, . ~ . - s(age))
anova(mod.4, mod.1, test="Chisq")

