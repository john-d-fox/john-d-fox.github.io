##-----------------------------------------------------##
##  An R Companion to Applied Regression, 3rd Edition  ##
##      J. Fox and S. Weisberg, Sage Publications      ##
##      Script for  Appendix on Robust Regression      ##
##-----------------------------------------------------##

ls <- function(x, type=c("rho", "psi", "wt")) {
       type <- match.arg(type)
       switch(type, rho=x^2, psi=e, wt=rep(1, length(e)))
       }
huber <- function(x, type=c("rho", "psi", "wt"), k=1.345) {
       type=match.arg(type)
       r <- ifelse(abs(e) < k, e^2/2, k*abs(e) - k^2/2)
       p <- sign(e) * ifelse(abs(e) < k, abs(e), k)
       w <- p/e
       switch(type,rho=r, psi=p, wt=w)
       }
bisquare <- function(x, type=c("rho", "psi", "wt"), k=4.685) {
       type=match.arg(type)
       r <- ifelse(abs(e) < k,
              (k^2/6)*(1 - (1 - (e/k)^2)^3), k^2/6)
       w <- ifelse(abs(e) < k,
              (1 - (e/k)^2)^2, 0)
       p <- e * w
       switch(type,rho=r, psi=p, wt=w)
       }
e <- seq(-6, 6, length=500)
par(mfrow=c(3,3))
plots <- function(x, method=ls, ...) {
    meth <- switch(deparse(substitute(method)), ls="OLS", huber="Huber", bisquare="bisquare")
   plot(e, method(x, "rho"), type="l", ylab=expression(rho(e)), ...)
   plot(e, method(x, "psi"), type="l", ylab=expression(psi(e)), ...)
   mtext(meth, line=1)
   plot(e, method(x, "wt"),  type="l", ylab=expression(w(e)), ...)
   }
plots(e, ls)
plots(e, huber)
plots(e, bisquare)

library("car")
mod.ls <- lm(prestige ~ income + education, data=Duncan)
S(mod.ls)

mod.ls.2 <- update(mod.ls, subset=-c(6, 16))
brief(mod.ls, mod.ls.2)

library("MASS")

mod.huber <- rlm(prestige ~ income + education, data=Duncan)
compareCoefs(mod.ls, mod.ls.2, mod.huber)

plot(mod.huber$w, ylab="Huber Weight")
smallweights <- which(mod.huber$w < 0.8)
showLabels(1:45, mod.huber$w, rownames(Duncan),
           method=smallweights, cex=0.6)

mod.bisq <- rlm(prestige ~ income + education, data=Duncan, method="MM")
compareCoefs(mod.huber, mod.bisq)

plot(mod.bisq$w, ylab="Bisquare Weight")
showLabels(1:45, mod.bisq$w, rownames(Duncan),
     method=which(mod.bisq$w < 0.8), cex=0.6)

(mod.lts <- ltsreg(prestige ~ income + education, data=Duncan))

library("quantreg")
library("MASS") # for the mvrnorm function

set.seed(10131986) # for reproducibility
l1.data <- function(n1=100, n2){
  data <- mvrnorm(n=n1,mu=c(0, 0),
                  Sigma=matrix(c(1, .9, .9, 1), ncol=2))
# generate n2 'bad' cases
  data <- rbind(data, mvrnorm(n=n2,
                  mu=c(1.5, -1.5), Sigma=.2*diag(c(1, 1))))
  data <- data.frame(data)
  names(data) <- c("X", "Y")
  ind <- c(rep(1, n1),rep(2, n2))
  plot(Y ~ X, data, pch=c("x", "o")[ind],
       col=c("black", "magenta")[ind],
       main=substitute(list(N[1] == n1, N[2] == n2), list(n1=n1, n2=n2)))
  r1 <-rq(Y ~ X, data=data, tau=0.5, ci=FALSE)
  abline(r1, lwd=2)
  abline(lm(Y ~ X, data), lty=2, lwd=2, col="magenta")
  abline(lm(Y ~ X, data, subset=1:n1), lty=4, lwd=2, col="blue")
  legend("topleft", c("L1","OLS","OLS on good"),
         inset=0.02, lty=c(1, 2, 4), lwd=2, col=c("black", "magenta", "blue"),
         cex=.9)}
par(mfrow=c(2, 2))
l1.data(100, 20)
l1.data(100, 30)
l1.data(100, 75)
l1.data(100, 100)

library("alr4") # for data
library("splines")

fdom <- with(salarygov, NW/NE > .8)
taus <- c(.1, .5, .9)  # estimate 10%, median, and 90% quantiles
ltys <- c(2, 1, 2)
cols <- c("blue", "magenta", "blue")
x <- 100:1000
plot(MaxSalary ~ Score, data=salarygov,
    xlim=c(100, 1000), ylim=c(1000, 10000),
    pch=c(2, 16)[fdom + 1], col=c("black", "cyan")[fdom + 1])

mods <- rq(MaxSalary ~ bs(Score, 5), tau=c(.1, .5, .9),
           data=salarygov[!fdom, ])
mods
predictions <- predict(mods, data.frame(Score=x))
for( j in 1:3) lines(x, predictions[, j], col=cols[j], lty=ltys[j], lwd=2)
legend("topleft", legend=taus, title="Quantile", lty=ltys, lwd=2,
    col=cols, inset=0.01)
legend("bottomright", legend=c("Non-Female-Dominated","Female-Dominated"),
    pch=c(2, 16), inset=0.01, col=c("black", "cyan"))

mod.quant <- rq(prestige ~ income + education, data=Duncan)
summary(mod.quant)
