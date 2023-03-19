##-----------------------------------------------------##
##  An R Companion to Applied Regression, 3rd Edition  ##
##      J. Fox and S. Weisberg, Sage Publications      ##
##   Script for  Appendix on Multiple Imputatation     ##
##                 of Missing Data                     ##
##-----------------------------------------------------##

library("carData")
summary(UN98)

library("carEx")
library("mice")

md.pattern(UN98, plot=FALSE)

mod.un <- lm(log(infantMortality) ~ region + log(GDPperCapita) + educationFemale,
             data=UN98)
S(mod.un)

crPlots(mod.un, smooth=list(span=0.9)) # large span for 76 cases
avPlots(mod.un)
outlierTest(mod.un)

UN98$Iraq <- rownames(UN98) == "Iraq"
mod.un.2 <- update(mod.un, . ~ . + Iraq, data=UN98)
compareCoefs(mod.un, mod.un.2)

UN2 <- UN98[, c(1, 2, 3, 5, 7, 8, 9, 11, 13)]
names(UN2)

scatterplotMatrix(UN2[, -1], smooth=list(span=0.9)) # dropping region

UN2$lifeFemale <- UN2$lifeFemale - 30
summary(p<- powerTransform(UN2[, -1]))

UN.t <- as.data.frame(mapply(basicPower, UN2[, -1], p$roundlam))
scatterplotMatrix(UN.t, smooth=list(span=0.9))

UN.t$region <- UN2$region
UN.t$Iraq <- rownames(UN2) == "Iraq"
UN.t$eaf2 <- UN.t$economicActivityFemale^2

system.time(UN.imps <- mice(UN.t, m=10, maxit=20, printFlag=FALSE, seed=423249))

plot(UN.imps, layout=c(4, 5))

meth <- make.method(UN.t)
meth["eaf2"] <- '~ I(economicActivityFemale^2)'
system.time(UN.imps.2 <- mice(UN.t, m=20, maxit=50, method=meth, 
                            printFlag=FALSE, seed=423249))
plot(UN.imps.2, layout=c(4, 5))

un.mods <- with(UN.imps.2, {
    infantMortality <- exp(infantMortality)
    GDPperCapita <- exp(GDPperCapita)
    lm(log(infantMortality) ~ region + log(GDPperCapita) + educationFemale + Iraq) 
})

(un.p <- pool(un.mods))
summary(un.p)

Anova(un.mods)

compareCoefs(mod.un.2, un.mods)

