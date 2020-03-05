
library(tidyverse)
library(dplyr)
library(effsize)

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


party <- survey$pid1d
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

############################################################################################### 
# work for question 5
###############################################################################################

#is there a correlation between age and donating to political campaigns?
cohen.d(birthyr ~ give, survey) # d estimate: -0.4745838 (small)

#is there a correlation between age and persuading others?
cov(survey$persuade, 2018 - survey$birthyr)
cor(survey$persuade, 2018 - survey$birthyr, method = "pearson") # -0.09644592
cohen.d(birthyr ~ persuade, survey) # d estimate: -0.2003081 (small)

#is there a correlation between age and talking online?
cov(survey$online, 2018 - survey$birthyr)
cor(survey$online, 2018 - survey$birthyr, method = "pearson") # -0.1720164
cohen.d(birthyr ~ online, survey) # d estimate: -0.2003081 (small)

#is there a correlation between age and donating to political orgs?
cov(survey$givefut, 2018 - survey$birthyr)
cor(survey$givefut, 2018 - survey$birthyr, method = "pearson") # -0.1660366
cohen.d(birthyr ~ givefut, survey) # d estimate: -0.3502668 (small)

#is there a correlation between age and meeting to talk
cov(survey$meet, 2018 - survey$birthyr)
cor(survey$meet, 2018 - survey$birthyr, method = "pearson") # -0.01604404
cohen.d(birthyr ~ meet, survey) # d estimate: -0.03799891 (negligible)

# lets see donations and age
givers = survey[which(as.numeric(survey$give) < 2),]
ggplot(givers, aes(x = 2018 - givers$birthyr)) + geom_histogram(binwidth=5)

donaters = survey[which(as.numeric(survey$givefut) < 2),]
ggplot(donaters, aes(x = 2018 - donaters$birthyr)) + geom_histogram(binwidth=5)

persuaders = survey[which(as.numeric(survey$persuade) < 2),]
ggplot(persuaders, aes(x = 2018 - persuaders$birthyr)) + geom_histogram(binwidth=5)

onliners = survey[which(as.numeric(survey$online) < 2),]
ggplot(onliners, aes(x = 2018 - onliners$birthyr)) + geom_histogram(binwidth=5)

meeters = survey[which(as.numeric(survey$meet) < 2),]
ggplot(meeters, aes(x = 2018 - meeters$birthyr)) + geom_histogram(binwidth=5)

q5DataFrame = rbind(data.frame(pred = 2018 - givers$birthyr, var = 'give'), 
                    data.frame(pred = 2018 - meeters$birthyr, var = 'meet'),
                    data.frame(pred = 2018 - persuaders$birthyr, var = 'persuade'),
                    data.frame(pred = 2018 - onliners$birthyr, var = 'online'),
                    data.frame(pred = 2018 - donaters$birthyr, var = 'donate'))

#this doesn't show us how many people we have in each bin though, so it needs tweaking
ggplot(q5DataFrame,aes(x=pred, fill=var)) + geom_histogram(alpha = 0.5, position = "dodge", bins = 5)


t.test(2018 - givers$birthyr, 2018 - survey$birthyr) #  p-value = 7.931e-15
t.test(2018 - donaters$birthyr, 2018 - survey$birthyr) # p-value = 2.903e-10
t.test(2018 - persuaders$birthyr, 2018 - survey$birthyr) # p-value = 0.001138
t.test(2018 - onliners$birthyr, 2018 - survey$birthyr) #p =  p-value = 1.182e-07
t.test(2018 - meeters$birthyr, 2018 - survey$birthyr) #p = 0.5347

activePolitics = survey[which(as.numeric(survey$follow) == 1),]
ggplot(activePolitics, aes(x = 2018 - activePolitics$birthyr)) + geom_histogram(binwidth=5)
t.test(2018 - activePolitics$birthyr, 2018 - survey$birthyr)

semiActivePolitics = survey[which(as.numeric(survey$follow) == 2),]
ggplot(semiActivePolitics, aes(x = 2018 - semiActivePolitics$birthyr)) + geom_histogram(binwidth=5)
t.test(2018 - semiActivePolitics$birthyr, 2018 - survey$birthyr)

inActivePolitics = survey[which(as.numeric(survey$follow) > 2),]
ggplot(inActivePolitics, aes(x = 2018 - inActivePolitics$birthyr)) + geom_histogram(binwidth=5)
t.test(2018 - inActivePolitics$birthyr, 2018 - survey$birthyr)

validWealthRespondents = survey[which(as.numeric(survey$faminc_new) > 0 & as.numeric(survey$faminc_new) < 17),]
hist(validWealthRespondents$faminc_new)

followLabels = factor(levels = c(1,2,3,4), labels = c("Most of the time", "Some of the time", "Only now and then", "Hardly ever"))
boxplot(validWealthRespondents$faminc_new ~ validWealthRespondents$follow, names=c("Most of the time", "Some of the time", "Only now and then", "Hardly ever"), ylab="Family Income", xlab="Following Politics", yaxt="n")
axis(2, at=c(5, 10, 15),labels=c("$40k-$49k", "$100k-$119k", "$350k-$499k"), las=3)

