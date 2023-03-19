##-----------------------------------------------------##
##  An R Companion to Applied Regression, 3rd Edition  ##
##      J. Fox and S. Weisberg, Sage Publications      ##
##   Script for  Appendix on Bootstrapping Regression  ##
##                     Models                          ##
##-----------------------------------------------------##

library("car")
library("MASS")
mod.duncan.hub <- rlm(prestige ~ income + education, data=Duncan, maxit=200)
summary(mod.duncan.hub)


set.seed(12345) # for reproducibility
system.time(duncan.boot <- Boot(mod.duncan.hub, R=1999))

summary(duncan.boot, high.moments=TRUE)

vcov(duncan.boot)

Confint(duncan.boot, level=.90, type="norm")
Confint(duncan.boot, parm=2:3, level=c(.68, .90, .95), type="perc")
Confint(duncan.boot, level=.95, type="bca")

hist(duncan.boot, legend="separate")

dataEllipse(duncan.boot$t[, 2], duncan.boot$t[, 3],
    xlab="income coefficient", ylab="education coefficient",
    cex=0.3, levels=c(.5, .95, .99), robust=TRUE)

library("boot")

duncan.boot

duncan.array <- boot.array(duncan.boot)
duncan.array[1:2, ]

par(mfcol=c(2, 1))
jack.after.boot(duncan.boot, index=2, main="(a) income coefficient")
jack.after.boot(duncan.boot, index=3, main="(b) education coefficient")

boot.huber <- function(data, indices, maxit=20){
    data <- data[indices, ]  # select cases in bootstrap sample
    mod <- rlm(prestige ~ income + education, data=data, maxit=maxit) # refit model
    coef(mod)  # return coefficient vector
}


set.seed(54321) # for reproducibility
summary(duncan.fix.boot <- Boot(mod.duncan.hub, R=1999, method="residual"))

par(mfcol=c(2, 1))
jack.after.boot(duncan.fix.boot, index=2, main="(a) income coefficient")
jack.after.boot(duncan.fix.boot, index=3, main="(b) education coefficient")

(d <- deltaMethod(mod.duncan.hub, "income - education"))

(z.diff <-  d[1, 1] / d[1, 2])

c(two.tail.p=2*pnorm(z.diff, lower.tail=FALSE))

f.diff <- function(mod){
   d <- deltaMethod(mod, "income-education")
   d[1, 1]/d[1, 2]
}

set.seed(2468) # for reproducibility
boot.diff <- Boot(mod.duncan.hub, R=999, f=f.diff, labels="z-diff",
                  method="residual")
hist(boot.diff, ci="none")

R <- 1999
c(bootp = (1 + sum(abs(boot.diff$t[, 1]) > abs(boot.diff$t0[1])))/(R + 1))

library("betareg")
data("ReadingSkills", package="betareg")
m <- betareg(accuracy ~ iq*dyslexia | iq + dyslexia, data=ReadingSkills)
summary(m)

class(m)


hatvalues.myreg <- function(model, ...) 1

b <- Boot(m, R=250)
sqrt(diag(vcov(b)))

