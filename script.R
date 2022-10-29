
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

subsamp["INCTOT"][subsamp["INCTOT"] == 0.1] <- 1

y <- log(subsamp$INCTOT)
ggplot( mapping = aes(x = subsamp$RACE, y=subsamp$INCTOT)) +
          geom_boxplot()

        