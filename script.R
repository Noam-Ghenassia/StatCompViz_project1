
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

subsamp = subsamp[subsamp$INCTOT >= 1, ]
subsamp = subsamp[subsamp$INCTOT != 9999999, ]


subsamp$EDUC <- factor(subsamp$EDUC, levels=c("N/A or no schooling", "Nursery school to grade 4", "Grade 5, 6, 7, or 8","Grade 9","Grade 10","Grade 11","Grade 12","1 year of college", "2 years of college", "3 years of college", "4 years of college","5+ years of college"))
ggplot( mapping = aes(x = subsamp$EDUC, y =log(subsamp$INCTOT)), cex.axis=0.2) +
  geom_boxplot() + labs(y= "log Income in dollars/year", x = "Education attainment")

###########################################################################

subsamp["EDUC"][subsamp["EDUC"] == 00] <- "Grade 12 or less"
subsamp["EDUC"][subsamp["EDUC"] == 01] <- "Grade 12 or less"
subsamp["EDUC"][subsamp["EDUC"] == 02] <- "Grade 12 or less"
subsamp["EDUC"][subsamp["EDUC"] == 03] <- "Grade 12 or less"
subsamp["EDUC"][subsamp["EDUC"] == 04] <- "Grade 12 or less"
subsamp["EDUC"][subsamp["EDUC"] == 05] <- "Grade 12 or less"
subsamp["EDUC"][subsamp["EDUC"] == 06] <- "Grade 12 or less"
subsamp["EDUC"][subsamp["EDUC"] == 07] <- "1+ year of college"
subsamp["EDUC"][subsamp["EDUC"] == 08] <- "1+ year of college"
subsamp["EDUC"][subsamp["EDUC"] == 09] <- "1+ year of college"
subsamp["EDUC"][subsamp["EDUC"] == 10] <- "1+ year of college"
subsamp["EDUC"][subsamp["EDUC"] == 11] <- "1+ year of college"


subsamp = subsamp[subsamp$INCTOT >= 1, ]
subsamp = subsamp[subsamp$INCTOT != 9999999, ]



subsamp$EDUC <- factor(subsamp$EDUC, levels=c("Grade 12 or less","1+ year of college"))
ggplot( mapping = aes(x = subsamp$EDUC, y =log(subsamp$INCTOT)), cex.axis=0.2) +
  geom_boxplot() + labs(y= "log Income in dollars/year ", x = "Education attainment")


#############################################################3
#education <- data$EDUC
#income <- data$INCTOT

#data_income_educ <- data.frame(education, income)

#####################CECI NE MARCHE PAS
##########################C DE LA MERDE
###########################NE RUNNEZ PAS CA
subsamp <- select(data, RACE, INCTOT, EDUC)
subsamp = subsamp[subsamp$INCTOT >= 1, ]
subsamp = subsamp[subsamp$INCTOT != 9999999, ]


labs <- c("Nursery school to grade 4", "Grade 5, 6, 7, or 8", "Grade 9", "Grade 10", "Grade 11", "Grade 12", "1 year of college", "2 years of college", "3 years of college", "4 years of college", "5+ years of college")
values <- c("FF3333", "FF9933", "FFFF33", "99FF33", 
           "33FF33", "33FFFF", "3333FF", "99004C", 
           "336600", "CCE5FF")

numy <- c(0,1,2,3,4,5,6,7,8,10,11)
p <- ggplot()
for (i in numy){
  print(i)
  print(labs[i])
  new <- filter(subsamp, EDUC == i)
  Y <- density(as.numeric(new$INCTOT), bw=1000,n=10000, from=0, to=100000)
  p <- p  + geom_line(aes(x=matrix(unlist(Y[1])), y = log(matrix(unlist(Y[2]))), color=values[i]))
  
}
p
p + scale_colour_manual( 
                      breaks = labs,
                      values = c("FF3333", "FF9933", "FFFF33", "99FF33", 
                                 "33FF33", "33FFFF", "3333FF", "99004C", 
                                 "336600", "CCE5FF"))

p
#ggplot() + 
#geom_line(aes(x=matrix(unlist(Y0[1])), y = log(matrix(unlist(Y0[2]))), colour="N/A or no schooling")) + 
 # geom_line(aes(x=matrix(unlist(Y0[1])), y = log(matrix(unlist(Y1[2]))), colour="Nursery school to grade 4")) +
  #geom_line(aes(x=matrix(unlist(Y0[1])), y = log(matrix(unlist(Y2[2]))), colour="Grade 5, 6, 7, or 8")) +
  #scale_colour_manual("", 
    #                  breaks = c("n=100", "n=500", "n=1000"),
     #                 values = c("red", "green", "blue"))

#ggarrange(plot1, plot2, plot3, ncol=1)

        