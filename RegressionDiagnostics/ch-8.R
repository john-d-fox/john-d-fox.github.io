# Regression Diagnostics, 2nd Edition
# John Fox
# last modified: 2019-06-18

# R script for Ch. 8

library(car)

# logistic regression for Mroz data
Mroz <- read.table("Mroz", header=TRUE)
  # assumes that Mroz.txt is in the current directory, adjust as necessary

mod.mroz <- glm(lfp ~ k5 + k618 + age +wc + hc + inc + lwg, 
                family=binomial, data=Mroz)
S(mod.mroz)

# Fig. 8.1: influence bubble plot for Mroz logistic regression

influencePlot(mod.mroz, id=FALSE, xlab="Hatvalues")   

# Bonferroni outlier test for Mroz logistic regression

outlierTest(mod.mroz)

# Fig. 8.2 added-variable plots for Mroz logistic regression

avPlots(mod.mroz, id=FALSE, col.lines="black", main="")

# Fig. 8.3: component-plus-residual plots for Mroz logistic regression

crPlots(mod.mroz, ~ k5 + k618 + age + inc + lwg, 
        col.lines=c("black", "black"), 
        ylab="Component + Residual", main="", layout=c(2, 3))

# test for nonlinearity of k5 in Mroz logistic regression

mod.mroz.2 <- update(mod.mroz, . ~ . - k5 + as.factor(k5))
anova(mod.mroz, mod.mroz.2, test="LRT")

# Fig. 84: marginal model plots for Mroz logistic regression

mmps(mod.mroz, col.line=c(data=gray(.50), model="black"), col=gray(.50),  
     main="", key=FALSE)
  # warning is innocuous

# variance inflation factors for Mroz logistic regression

vif(mod.mroz)

# Table 8.1: linear and logistic quasi-binomial models 
#            fit to the GSS vocabulary data

GSS <- read.table("GSS.txt", header=TRUE)
  # assumes that GSS.txt is in the current directory, adjust as necessary

mod.lin <-lm(vocab ~ age + educ + year + gender + nativeBorn, data=GSS)
S(mod.lin)

mod.qb <-glm(cbind(vocab, 10 - vocab) ~ age + educ + year + gender + nativeBorn, 
             family=quasibinomial, data=GSS)
S(mod.qb)

  # adjust logistic regression coefficients and SEs to compare with linear model
10*coef(mod.qb)/4 
10*sqrt(diag(vcov(mod.qb)))/4 # SEs
