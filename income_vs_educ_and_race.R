library(ggplot2)
library(tidyverse)
library(dplyr)
library(heatmaply)

data <- read.csv("usa_00006.csv")

subsamp <- select(data, RACE, INCTOT, EDUC)


subsamp["RACE"][subsamp["RACE"] == 1] <- "White"
subsamp["RACE"][subsamp["RACE"] == 2] <- "Black/African American"
subsamp["RACE"][subsamp["RACE"] == 3] <- "American Indian or Alaska Native"
subsamp["RACE"][subsamp["RACE"] == 4] <- "Chinese"
subsamp["RACE"][subsamp["RACE"] == 5] <- "Japanese"
subsamp["RACE"][subsamp["RACE"] == 6] <- "Other Asian or Pacific Islander"
subsamp["RACE"][subsamp["RACE"] == 7] <- "Other race, nec	"
subsamp["RACE"][subsamp["RACE"] == 8] <- "More than 2 races"
subsamp["RACE"][subsamp["RACE"] == 9] <- "More than 2 races"

subsamp["EDUC"][subsamp["EDUC"] == 00] <- "N/A or no schooling"
subsamp["EDUC"][subsamp["EDUC"] == 01] <- "Nursery school to grade 4"
subsamp["EDUC"][subsamp["EDUC"] == 02] <- "Grade 5, 6, 7, or 8"
subsamp["EDUC"][subsamp["EDUC"] == 03] <- "Grade 9"
subsamp["EDUC"][subsamp["EDUC"] == 04] <- "Grade 10"
subsamp["EDUC"][subsamp["EDUC"] == 05] <- "Grade 11"
subsamp["EDUC"][subsamp["EDUC"] == 06] <- "Grade 12"
subsamp["EDUC"][subsamp["EDUC"] == 07] <- "1 year of college"
subsamp["EDUC"][subsamp["EDUC"] == 08] <- "2 years of college"
subsamp["EDUC"][subsamp["EDUC"] == 09] <- "3 years of college"
subsamp["EDUC"][subsamp["EDUC"] == 10] <- "4 years of college"
subsamp["EDUC"][subsamp["EDUC"] == 11] <- "5+ years of college"

subsamp = subsamp[subsamp$INCTOT >= 1, ]
subsamp = subsamp[subsamp$INCTOT != 9999999, ]
subsamp$EDUC <- factor(subsamp$EDUC, levels=c("N/A or no schooling",
                                              "Nursery school to grade 4",
                                              "Grade 5, 6, 7, or 8",
                                              "Grade 9","Grade 10",
                                              "Grade 11",
                                              "Grade 12",
                                              "1 year of college",
                                              "2 years of college",
                                              "3 years of college",
                                              "4 years of college",
                                              "5+ years of college"))

mat <- aggregate(x=as.numeric(subsamp$INCTOT),
                 by=list(subsamp$RACE,subsamp$EDUC),
                 FUN=mean)
colnames(mat) <- c("race", "education", "avg_income")
head(mat)

ggplot(mat, aes(x = race, y = education, fill = log(avg_income))) +
  geom_tile()


str(mat)
mat <- as.matrix(mat)
p <- heatmaply(mat)






