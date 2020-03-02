
library(tidyverse)
library(dplyr)

setwd("C:/Users/winbase/MIDS/w203/lab_2-master")
list.files()

survey = read.csv("anes_pilot_2018.csv")


# work for question 1

# unsure if we should throw out data frames or just set to zero
#valid_journalists = subset(survey$ftjournal,  survey$ftjournal >= 0)
survey$ftjournal[survey$ftjournal < 0] <- 0 # just setting them to zero but I don't know if that's right

summary(survey$ftpolice)
summary(survey$ftjournal) # shows a -7, which we'll want to discard I think, so we can bracket to 0-100

question1frame = data.frame(group = factor(rep(c("police","journalist"), each=2500)), labels=c("police", "journalists"), pjt = c(survey$ftpolice, survey$ftjournal))
cdat <- ddply(question1frame, "group", summarise, trust.mean=mean(pjt))


ggplot(question1frame, aes(x = pjt, fill=group)) + geom_histogram(binwidth=5, alpha=.5) + geom_vline(data=cdat, aes(xintercept=trust.mean,  colour=group), linetype="dashed", size=1) + facet_grid(group ~ .) + xlab("Trust")
