# Input the data in Table 2.1 (Fox and Guyer data) into a data frame
cooperation <- c(49, 64, 37, 52, 68, 54, 61, 79, 64, 29, 27, 58, 52, 41, 30, 40, 39, 44, 34, 44)
condition <- rep(c("public", "anonymous"), c(10, 10))
sex <- rep(rep(c("male", "female"), c(5, 5)), 2)
Guyer <- data.frame(cooperation, condition, sex)

#  alternative method, using just one command:
Guyer <- data.frame(
  cooperation = c(49, 64, 37, 52, 68, 54, 61, 79, 64, 29, 27, 58, 52, 41, 30, 40, 39, 44, 34, 44),
  condition = rep(c("public", "anonymous"), c(10, 10)),
  sex = rep(rep(c("male", "female"), c(5, 5)), 2)
)