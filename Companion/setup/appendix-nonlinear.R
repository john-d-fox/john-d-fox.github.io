##-----------------------------------------------------##
##  An R Companion to Applied Regression, 3rd Edition  ##
##      J. Fox and S. Weisberg, Sage Publications      ##
##   Script for  Appendix on Nonlinear Regression,     ##
## Nonlinear Least Squares, and Nonlinear Mixed Models ##
##-----------------------------------------------------##

library("car")
brief(USPop)
plot(population ~ year, data=USPop, main="(a)")
abline(lm(population ~ year, data=USPop))

curve(1/(1+exp(-(-1 + 1*x))), from=-5, to=5, main="(b)")
abline(h=1/2, v=1, lty=2)

args(nls)

args(nls.control)

lm(logit(population/400) ~ year, USPop)

pop.mod <- nls(population ~ theta1/(1 + exp(-(theta2 + theta3*year))),
     start=list(theta1 = 400, theta2 = -49, theta3 = 0.025),
     data=USPop, trace=TRUE)

summary(pop.mod)

deltaMethod(pop.mod, "-theta2/theta3")

pop.mod <- update(pop.mod, trace=FALSE)  # to suppress tracing in profiling

Confint(pop.mod)

plot(population ~ year, USPop, xlim=c(1790, 2100), ylim=c(0, 450))
with(USPop, lines(seq(1790, 2100, by=10),
      predict(pop.mod, data.frame(year=seq(1790, 2100, by=10))), lwd=2))
points(2010, 308.745538, pch=16, cex=1.3)
abline(h=0, lty=2)
abline(h=coef(pop.mod)[1], lty=2)
abline(h=0.5*coef(pop.mod)[1], lty=2)
abline(v= -coef(pop.mod)[2]/coef(pop.mod)[3], lty=2)

with(USPop, plot(year, residuals(pop.mod), type='b'))
abline(h=0, lty=2)

pop.ss <- nls(population ~ SSlogis(year, phi1, phi2, phi3), data=USPop)
summary(pop.ss)

deltaMethod(pop.mod, "1/theta3")

USPop$decade <- (USPop$year - 1790)/10
pop.ss.rescaled <- nls(population ~ SSlogis(decade, nu1, nu2, nu3), data=USPop)
compareCoefs(pop.ss, pop.ss.rescaled)
sigmaHat(pop.ss.rescaled)

set.seed(12345) # for repeatability
out4 <- Boot(pop.ss)
summary(out4)
hist(out4)
Confint(out4)

brief(CanPop)
popData <- data.frame(rbind(data.frame(country="US", USPop[,1:2]),
                         data.frame(country="Canada", CanPop)))
brief(popData)

scatterplot(population ~ year|country, data=popData, box=FALSE,
   reg=FALSE)

library("nlme")
m.list <- nlsList(population ~ SSlogis(year, phi1, phi2, phi3)|country,
    pool=FALSE, data=popData)
summary(m.list)
(sds <- sapply(m.list, sigmaHat))

(betas <- lapply(m.list, coef))
(vars  <- lapply(m.list, vcov))

(betas <- unlist(betas))
zero <- matrix(0, nrow=3, ncol=3)
(var <- rbind( cbind(vars[[1]], zero), cbind(zero, vars[[2]])))
deltaMethod(betas, "US.phi3 - Canada.phi3", vcov=var)
deltaMethod(betas, "US.phi2 - Canada.phi2", vcov=var)

w <- ifelse(popData$country == "Canada", (sds[1]/sds[2])^2, 1)
head(w)  # first few for U.S.
tail(w)  # last few for Canada

popData$can <- ifelse(popData$country == "Canada", 1, 0)

form1 <- population ~ (1 - can)*(phi11/(1 + exp(-(year - phi21)/phi31))) +
                      can*(phi12/(1 + exp(-(year - phi22)/phi32)))
b <- coef(m.list)
m1 <- nls(form1, data=popData, weights=w, 
          start=list(phi11=b[1, 1], phi12=b[1, 2], # U.S.
       phi21=b[1, 2], phi22=b[2, 2], phi31=b[1, 3], phi32=b[2, 3])) # Canada

form2 <- population ~ (1 - can)*(phi11/(1 + exp(-(year - phi21)/phi3))) +
                      can*(phi12/(1 + exp(-(year - phi22)/phi3)))
m2 <- nls(form2, data=popData, weights=w, start=list(phi11=b[1, 1], phi12=b[1, 2],
       phi21=b[1, 2], phi22=b[2, 2], phi3=b[1, 3]))


Confint(m2)

model <- function(theta1, theta2, theta3, year){
     yhat <- theta1/(1 + exp(-(theta2 + theta3*year)))
     term <- exp(-(theta2 + theta3*year))
     gradient <- cbind((1 + term)^-1, # in proper order
         theta1*(1 + term)^-2 * term,
         theta1*(1 + term)^-2 * term * year)
     attr(yhat, "gradient") <- gradient
     yhat
     }

(nls(population ~ model(theta1, theta2, theta3, year),
     data=USPop, start=list(theta1=400, theta2=-49, theta3=0.025)))

(model2 <- deriv(~ theta1/(1 + exp(-(theta2 + theta3*year))), # rhs of model
     c("theta1", "theta2", "theta3"), # parameter names
     function(theta1, theta2, theta3, year){} # arguments for result
     ))
(nls(population ~ model2(theta1, theta2, theta3, year),
     data=USPop, start=list(theta1=400, theta2=-49, theta3=0.025)))

ord <- with(Wong, order(id, days))
Wong <- Wong[ord, ]
brief(Wong, rows=c(10, 10))
nrow(Wong)
patients <- unique(with(Wong, id))
length(patients)
table(xtabs(~id, data=Wong))
with(Wong, sum(days > 1000))
plot(piq ~ days, xlab="Days Post Coma",
     ylab="PIQ", xlim=c(0, 1000),
     data=Wong, subset = days <= 1000)
for (patient in patients){
    with(Wong, lines(days[id==patient], piq[id == patient], col="gray"))
}
with(Wong, lines(lowess(days, piq), lwd=2, col="darkblue"))

library(sfsmisc) # for p.arrows()
plot(c(0, 10), c(0, 5), axes=FALSE, xlab="", ylab="", type="n")
axis(1, at=0)
mtext("Time", side=1, line=1, adj=0.9)
text(-1, 5, labels="PIQ", xpd=TRUE)
axis(2, at=c(0, 4), labels=c(expression(0), expression(theta["1i"])), las=1)
box()
curve(4 - 3*exp(-0.5*x), 0, 10, lwd=2, add=TRUE)
abline(h=c(1, 2.5, 4), lty=2)
p.arrows(0, 1, 0, 4, fill="black")
text(0.5, 3.5, labels=expression(-theta["2i"]))
p.arrows(log(2)/0.5, 1, log(2)/0.5, 2.5, fill="black")
text(2.25, 2.0, labels=expression(-theta["2i"]/2))
p.arrows(0, 1, log(2)/0.5, 1, fill="black")
text(1.25, 0.75, labels=expression((log~2)/theta["3i"]))

Wong.first <- aggregate(Wong[, c("piq", "duration", "days")],
    by=list(id=Wong$id), function(x) x[1])
Wong.first <- subset(Wong.first, duration <= 100)
brief(Wong.first)
plot(piq ~ sqrt(duration), data=Wong.first,
     xlab="Days in Coma (square-root scale)", ylab="Initial PIQ",
     axes=FALSE, frame=TRUE)
axis(2)
axis(1, at=sqrt(c(0, 5, 10, seq(20, 100, by=20))),
    labels=c(0, 5, 10, seq(20, 100, by=20)))
(mod.ag <- lm(piq ~ sqrt(duration), data=Wong.first))
abline(mod.ag, lwd=2, lty=2)
with(Wong.first,
    lines(lowess(sqrt(duration), piq), lwd=2, col="darkblue"))

piq.mod.1 <- nlme(piq ~ theta1 + theta2*exp(-theta3*days), data=Wong,
    fixed=list(
        theta1 ~ 1 + sqrt(duration),
        theta2 ~ 1 + sqrt(duration),
        theta3 ~ 1),
    random=list(id = list(theta1 ~ 1, theta2 ~ 1, theta3~1)),
    start=list(fixed=c(100, -2, -10, 0, 0.007)),
    control=nlmeControl(msMaxIter=500))

piq.mod.2 <- nlme(piq ~ theta1 + theta2*exp(-theta3*days), data=Wong,
    fixed=list(
        theta1 ~ 1 + sqrt(duration),
        theta2 ~ 1 + sqrt(duration),
        theta3 ~ 1),
    random=list(id = list(theta1 ~ 1, theta2 ~ 1)),
    start=list(fixed=c(100, -2, -10, 0, 0.007)))

piq.mod.3 <- nlme(piq ~ theta1 + theta2*exp(-theta3*days), data=Wong,
    fixed=list(
      theta1 ~ 1 + sqrt(duration),
      theta2 ~ 1 + sqrt(duration),
      theta3 ~ 1),
    random=list(id = list(theta1 ~ 1)),
    start=list(fixed=c(100, -2, -10, 0, 0.007)))

anova(piq.mod.1, piq.mod.2, piq.mod.3)

compareCoefs(piq.mod.1, piq.mod.2, piq.mod.3)


deltaMethod(piq.mod.3, "log(2)/beta5", parameterNames=paste0("beta", 1:5))

newdata <- expand.grid(duration=c(1, 10, 20, 50, 100, 200),
                       days=seq(0, 1000, 20))
newdata$piq <- predict(piq.mod.3, newdata, level=0)
plot(piq ~ days, type="n", xlab="Days Post Coma", ylab="Average PIQ",
    ylim=c(20, 100), xlim=c(-100, 1000), data=newdata, axes=FALSE, frame=TRUE)
axis(2) # left
axis(4) # right
axis(1, at=seq(0, 1000, by=100)) # bottom
grid(lty=2, col="gray")
for (dur in c(1, 10, 20, 50, 100, 200)){
    with(newdata, {
        lines(spline(seq(0, 1000, 20), piq[duration == dur]), lwd=2)
        text(-25, piq[duration == dur][1], labels=dur, adj=0.9)
    })
}
text(-100, 95, labels="Duration\nof Coma", adj=0)

