
library(tidyverse)

setwd("C:/Users/winbase/MIDS/w203/lab_2-master")
list.files()

survey = read_csv("anes_pilot_2018.csv")

summary(survey$ftpolice)
summary(survey$ftjournal)
