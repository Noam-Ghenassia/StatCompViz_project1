library(ggplot2)
library(dplyr)

data <- read.csv("usa_00006.csv")

education <- data$EDUC
income <- data$INCTOT

data_income_educ <- data.frame(education, income)

data_income_educ <- data_income_educ[data_income_educ$income != 9999999, ] 
data_income_educ <- data_income_educ[data_income_educ$income != 0, ]

bw = 1600
max= 100000

df <- data.frame()

data_1 <- filter(data_income_educ, education == "1")
Y1 <- density(as.numeric(data_1$income), bw=bw,
              n=10000, from=0, to=max)
df1 <- data.frame(income = matrix(unlist(Y1[1])), density1 =  matrix(unlist(Y1[2])))
df <- df1

data_2 <- filter(data_income_educ, education == "2")
Y2 <- density(as.numeric(data_2$income), bw=bw,
              n=10000, from=0, to=max)
df2 <- data.frame(density2 =  matrix(unlist(Y2[2])))
df <-cbind(df, df2)

data_3 <- filter(data_income_educ, education == "3")
Y3 <- density(as.numeric(data_3$income), bw=bw,
              n=10000, from=0, to=max)
df3 <- data.frame(density3 =  matrix(unlist(Y3[2])))
df <-cbind(df, df3)

data_4 <- filter(data_income_educ, education == "4")
Y4 <- density(as.numeric(data_4$income), bw=bw,
              n=10000, from=0, to=max)
df4 <- data.frame(density4 =  matrix(unlist(Y4[2])))
df <-cbind(df, df4)

data_5 <- filter(data_income_educ, education == "5")
Y5 <- density(as.numeric(data_5$income), bw=bw,
              n=10000, from=0, to=max)
df5 <- data.frame(density5 =  matrix(unlist(Y5[2])))
df <-cbind(df, df5)

data_6 <- filter(data_income_educ, education == "6")
Y6 <- density(as.numeric(data_6$income), bw=bw,
              n=10000, from=0, to=max)
df6 <- data.frame(density6 =  matrix(unlist(Y6[2])))
df <-cbind(df, df6)

data_7 <- filter(data_income_educ, education == "7")
Y7 <- density(as.numeric(data_7$income), bw=bw,
              n=10000, from=0, to=max)
df7 <- data.frame(density7 =  matrix(unlist(Y7[2])))
df <-cbind(df, df7)

data_8 <- filter(data_income_educ, education == "8")
Y8 <- density(as.numeric(data_8$income), bw=bw,
              n=10000, from=0, to=max)
df8 <- data.frame(density8 =  matrix(unlist(Y8[2])))
df <-cbind(df, df8)

data_10 <- filter(data_income_educ, education == "10")
Y10 <- density(as.numeric(data_10$income), bw=bw,
              n=10000, from=0, to=max)
df10 <- data.frame(density10 =  matrix(unlist(Y10[2])))
df <-cbind(df, df10)

data_11 <- filter(data_income_educ, education == "11")
Y11 <- density(as.numeric(data_11$income), bw=bw,
              n=10000, from=0, to=max)
df11 <- data.frame(density11 =  matrix(unlist(Y11[2])))
df <-cbind(df, df11)



ggplot(data=df, mappping=aes(x=income)) +
  geom_line(aes(x=income, y=density1, colour="Nursery school to grade 4")) +
  geom_line(aes(x=income, y=density2, colour="Grade 5, 6, 7, or 8")) +
  geom_line(aes(x=income, y=density3, colour="Grade 9")) +
  geom_line(aes(x=income, y=density4, colour="Grade 10")) +
  geom_line(aes(x=income, y=density5, colour="Grade 11")) +
  geom_line(aes(x=income, y=density6, colour="Grade 12")) +
  geom_line(aes(x=income, y=density7, colour="1 year of college")) +
  geom_line(aes(x=income, y=density8, colour="2 years of college")) +
  geom_line(aes(x=income, y=density10, colour="4 years of college")) +
  geom_line(aes(x=income, y=density11, colour="5+ years of college")) +
  scale_colour_manual("", 
                      breaks = c("Nursery school to grade 4",
                                 "Grade 5, 6, 7, or 8",
                                 "Grade 9", "Grade 10",
                                 "Grade 11",
                                 "Grade 12",
                                 "1 year of college",
                                 "2 years of college",
                                 "3 years of college",
                                 "4 years of college",
                                 "5+ years of college"),
                      values = c("red", "green", "blue", "yellow", "orange",
                                 "pink", "purple", "brown", "black", "magenta"))














