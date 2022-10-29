
#try to commit smthg
library(ggplot2)
library(tidyverse)
library(dplyr)

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



subsamp = subsamp[subsamp$INCTOT >= 1, ]
subsamp = subsamp[subsamp$INCTOT != 9999999, ]

min(subsamp$INCTOT)


ggplot( mapping = aes(x = subsamp$RACE, y =log(subsamp$INCTOT))) +
          geom_boxplot()

######################################
######################################

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

ggplot( mapping = aes(x = subsamp$EDUC, y =log(subsamp$INCTOT))) +
  geom_boxplot()

        