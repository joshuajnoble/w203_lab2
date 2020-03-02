
library(tidyverse)
library(dplyr)

setwd("C:/Users/winbase/MIDS/w203/lab_2-master")
list.files()

survey = read.csv("anes_pilot_2018.csv")


############################################################################################### 
# work for question 1
###############################################################################################

# unsure if we should throw out data frames or just set to zero
##UPDATE : -7 is no answer, we need to throw them out and redo some of this code :/
#valid_journalists = subset(survey$ftjournal,  survey$ftjournal >= 0)
survey$ftjournal[survey$ftjournal < 0] <- 0 # just setting them to zero but I don't know if that's right

summary(survey$ftpolice)
summary(survey$ftjournal) # shows a -7, which we'll want to discard I think, so we can bracket to 0-100

question1frame = data.frame(group = factor(rep(c("police","journalist"), each=2500)), pjt = c(survey$ftpolice, survey$ftjournal))
cdat <- ddply(question1frame, "group", summarise, trust.mean=mean(pjt))


ggplot(question1frame, aes(x = pjt, fill=group)) + geom_histogram(binwidth=5, alpha=.5) + geom_vline(data=cdat, aes(xintercept=trust.mean,  colour=group), linetype="dashed", size=1) + facet_grid(group ~ .) + xlab("Trust")


(police_trust = question1frame$pjt[question1frame$group == "police"])
(journalist_trust = question1frame$pjt[question1frame$group == "journalist"])

# t.test to see our difference:

t.test(police_trust, journalist_trust)

############################################################################################### 
# work for question 2
###############################################################################################

democrat = survey[which(survey$pid1d == 1),]
republican = survey[which(survey$pid1d == 2),]

parties = c()
parties = c( rep("republican", nrow(republican)), rep("democrat", nrow(democrat)))

question2frame = data.frame(party = factor(parties), age = c(2018 - republican$birthyr), 2018 - democrat$birthyr)
age_dat <- ddply(question2frame, "party", summarise, age.mean=mean(age))

ggplot(question2frame, aes(x = age, fill=party)) + geom_histogram(binwidth=5, position="dodge") + 
  geom_vline(data=age_dat, aes(xintercept=age.mean,  colour=party), linetype="dashed", size=1) + 
  xlab("Age") + 
  scale_fill_manual(values=c("#0000ff", "#ff0000"))
