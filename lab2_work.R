
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

# probably want wilcoxon

wilcox.test(police_trust, journalist_trust, alternative = "two.sided")

############################################################################################### 
# work for question 2
###############################################################################################

democrat = survey[which(survey$pid1d == 1),]
republican = survey[which(survey$pid1d == 2),]

#filter by all of our voters who are sure they voted
democrat = democrat[which(democrat$turnout18 < 4),]
republican = republican[which(republican$turnout18 < 4),]

#make a list of dems
parties = c( rep("republican", nrow(republican)), rep("democrat", nrow(democrat)))
#make a list of repubs
party_colors = c( rep("ff0000", nrow(republican)), rep("0000ff", nrow(democrat)))

#make a dataframe for our q
question2frame = data.frame(party = factor(parties), age = c(2018 - republican$birthyr), 2018 - democrat$birthyr, colors = party_colors)
#find the means
age_dat <- ddply(question2frame, "party", summarise, age.mean=mean(age))

#plot it
ggplot(question2frame, aes(x = age, fill=party)) + geom_histogram(binwidth=5, position="dodge") + 
  facet_grid(party ~ .) + 
  geom_vline(data=age_dat, aes(xintercept=age.mean), linetype="dashed", size=1) + 
  xlab("Age") + 
  scale_fill_manual(values=c("#0000ff", "#ff0000"))

#I think we can just get away with a student t-test?
t.test(2018 - republican$birthyr, 2018 - democrat$birthyr)

#doesn't look like there's much age difference tbh


################## jills work


party_count <- length(party)
# hist(party)
year <- 2018 - survey$birthyr
summary(year)
#q2 <- data.frame(year, party)
q2 <- data.frame(year, party, 2018 - year)
#names(q2)[3] <- "age"
q2 <- q2[which (q2$party == c(1,2)),]
# head(q2)
t.test(year ~ party, data = q2)

############################################################################################### 
# work for question 3
###############################################################################################

independents = survey[which(survey$pid1d == 3),] #get all of our independents
summary(independents$coord16) #do they think the Trump campaign coordinated with the Russians

# how do we land with our indies?
coord = independents[which(independents$coord16==1),]
nocoord = independents[which(independents$coord16==2),]

############################################################################################### 
# work for question 4
###############################################################################################

voters = survey[which(survey$turnout18 < 4),] #get all of our voters who are sure they voted
voters = voters[which(as.numeric(voters$geangry) > 0),] #get all of our voters who answered angry and afraid
voters = voters[which(as.numeric(voters$geafraid) > 0),] #get all of our voters who answered angry and afraid

question4frame = data.frame(emotion = c(rep("anger", nrow(voters)), rep("fear", nrow(voters))), response = c(voters$geangry, voters$geafraid))

p = ggplot(question4frame, aes(x = question4frame$response, fill=emotion), stat="count") + geom_histogram(binwidth=1, position="dodge") + scale_x_continuous(breaks = c(1,2,3,4,5), labels=c("Not at all", "A little","Somewhat", "Very", "Extremely"))
p

#not sure if we can just run both?
wilcox.test(voters$geangry, voters$geafraid,  alternative = "greater")
wilcox.test(voters$geafraid, voters$geangry,  alternative = "greater")
