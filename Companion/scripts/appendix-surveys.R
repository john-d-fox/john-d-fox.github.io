##-----------------------------------------------------##
##  An R Companion to Applied Regression, 3rd Edition  ##
##      J. Fox and S. Weisberg, Sage Publications      ##
##   Script for  Appendix on Fitting Regression Models ##
##               to Data From Complex Surveys          ##
##-----------------------------------------------------##

library("survey")
library("car")

brief(CES11)
CES11$education <- factor(CES11$education, levels=c("lessHS", "HS", "somePS",
                            "college", "bachelors", "higher"))

CES.svy <- svydesign(ids=~id, strata=~province, fpc=~population,
                     weights=~weight, data=CES11)

prop.table(xtabs(~abortion, data=CES11))
svymean(~abortion, design=CES.svy)

mod.abortion.svy <- svyglm(abortion ~ importance + gender  + education + urban,
                           design=CES.svy, family=quasibinomial)
summary(mod.abortion.svy)
Anova(mod.abortion.svy)

mod.abortion <- glm(abortion ~ importance + gender  + education + urban, data=CES11,
                    family=binomial)
compareCoefs(mod.abortion.svy, mod.abortion)
Anova(mod.abortion, test.statistic="Wald")

CES.svy.2 <- svydesign(ids=~0, strata=~province, weights=~weight, data=CES11)
mod.abortion.svy.2 <- svyglm(abortion ~ importance + gender  + education + urban,
                             design=CES.svy.2, family=quasibinomial)
compareCoefs(mod.abortion.svy, mod.abortion.svy.2)

