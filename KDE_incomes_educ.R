library(ggplot2)
library(dplyr)

data <- read.csv("usa_00006.csv")

education <- data$EDUC
income <- data$INCTOT

data_income_educ <- data.frame(education, income)

data_income_educ <- data_income_educ[data_income_educ$income != 9999999, ] 
data_income_educ <- data_income_educ[data_income_educ$income != 0, ]


new <- filter(data_income_educ, education == "6")
Y1 <- density(as.numeric(new$income), bw=1000,
              n=10000, from=0, to=100000)

ggplot(mapping=aes(matrix(unlist(Y1[1])), matrix(unlist(Y1[2])))) + geom_line()


















