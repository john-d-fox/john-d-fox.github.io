##-----------------------------------------------------##
##  An R Companion to Applied Regression, 3rd Edition  ##
##      J. Fox and S. Weisberg, Sage Publications      ##
##               Script for Chapter  1                 ##
##-----------------------------------------------------##

library("car")


2 + 3 # addition
2 - 3 # subtraction
2*3   # multiplication
2/3   # division
2^3   # exponentiation

4^2 - 3*2

1 - 6 + 4

(4^2) - (3*2)

(4 + 3)^2

4 + 3^2

-2--3
-2 - -3

log(100)

log(100, base=10)

log10(100) # equivalent

log(100, b=10)

args("log")

log(100, 10)

c(1, 2, 3, 4)

1:4 # integer sequence
4:1 # descending
-1:2 # negative to positive
seq(1, 4) # equivalent to 1:4
seq(2, 8, by=2) # specify interval between elements
seq(0, 1, by=0.1) # noninteger sequence
seq(0, 1, length=11) # specify number of elements

c(1, 2, 3, 4)/2
c(1, 2, 3, 4)/c(4, 3, 2, 1)
log(c(0.1, 1, 10, 100), base=10)

c(1, 2, 3, 4) + c(4, 3) # no warning

x <- c(1, 2, 3, 4) # assignment
x # print

x/2  # equivalent to c(1, 2, 3, 4)/2
(y <- sqrt(x))

set.seed(12345)
(x <- rnorm(100))  # 100 standard normal random numbers

summary(x)

(words <- c("To", "be", "or", "not", "to", "be"))

paste(words, collapse=" ")

(logical.values <- c(TRUE, TRUE, FALSE, TRUE))

!logical.values

sum(logical.values)
sum(!logical.values)

c("A", FALSE, 3.0)

c(10, FALSE, -6.5, TRUE)

x[12]             # 12th element
words[2]          # second element
logical.values[3] # third element

x[6:15]       # elements 6 through 15
x[c(1, 3, 5)] # 1st, 3rd, 5th elements

x[-(11:100)] # omit elements 11 through 100

v <- 1:4
v[c(TRUE, FALSE, FALSE, TRUE)]

1 == 2
1 != 2
1 <= 2
1 < 1:3
3:1 > 1:3
3:1 >= 1:3
TRUE & c(TRUE, FALSE)
c(TRUE, FALSE, FALSE) | c(TRUE, TRUE, FALSE)
TRUE && FALSE
TRUE || FALSE

(z <- x[1:10]) # first 10 elements of x
z < -0.5 # is each element less than -0.5?
z > 0.5  # is each element greater than 0.5
z < -0.5 | z > 0.5  #  < and > are of higher precedence than |
abs(z) > 0.5  # absolute value, equivalent to last expression
z[abs(z) > 0.5] # values of z for which |z| > 0.5
z[!(abs(z) > 0.5)] # values z for which |z| <= 0.5

mean(x)

sum(x)/length(x)

myMean <- function(x){
    sum(x)/length(x)
}

myMean(x)
y # from sqrt(c(1, 2, 3, 4))
myMean(y)
myMean(1:100)
myMean(sqrt(1:100))

mySD <- function(x){
    sqrt(sum((x - myMean(x))^2)/(length(x) - 1))
}
mySD(1:100)
sd(1:100) # check

mySD
myMean

letters
mySD(letters)


head(Duncan, n=10)
dim(Duncan)

summary(Duncan)

with(Duncan, hist(prestige))

scatterplotMatrix( ~ prestige + education + income,
    id=list(n=3), data=Duncan)

(duncan.model <- lm(prestige ~ education + income, data=Duncan))

summary(duncan.model)

densityPlot(rstudent(duncan.model))

set.seed(12345)

qqPlot(duncan.model, id=list(n=3))

outlierTest(duncan.model)

influenceIndexPlot(duncan.model, vars=c("Cook", "hat"),
    id=list(n=3))

avPlots(duncan.model,
    id=list(cex=0.75, n=3, method="mahal"))

crPlots(duncan.model)

ncvTest(duncan.model)
ncvTest(duncan.model, var.formula= ~ income + education)

whichNames(c("minister", "conductor"), Duncan)
duncan.model.2 <- update(duncan.model, subset=-c(6, 16))
summary(duncan.model.2)

compareCoefs(duncan.model, duncan.model.2)

summary(Duncan$type)

summary(Duncan$prestige)

summary(Duncan)

summary(lm(prestige ~ education + income, data=Duncan))

class(Duncan$type)
class(Duncan$prestige)
class(Duncan)

duncan.model <- lm(prestige ~ education + income, data=Duncan)
class(duncan.model)

summary

args(summary.lm)

mod.mroz <- glm(lfp ~ ., family=binomial, data=Mroz)
class(mod.mroz)

