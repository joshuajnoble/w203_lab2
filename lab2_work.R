
library(tidyverse)
library(effsize)
library(plyr)
library(reshape2)


setwd("C:/Users/winbase/MIDS/w203/lab_2-master")
anes_data = read.csv("anes_pilot_2018.csv")


############################################################################################### 
# work for question 1
###############################################################################################

anes_data$ftjournal[anes_data$ftjournal < 0] <- 0 # just setting them to zero but I don't know if that's right
valid_jpt = subset(anes_data,  anes_data$ftjournal >= 0 & anes_data$ftpolice >= 0)

summary(valid_jpt$ftpolice)
summary(valid_jpt$ftjournal) # shows a -7, which we'll want to discard I think, so we can bracket to 0-100

question1frame = data.frame(group = factor(rep(c("police","journalist"), each=2500)), pjt = c(anes_data$ftpolice, anes_data$ftjournal))
cdat <- ddply(question1frame, "group", summarise, trust.mean=mean(pjt))


ggplot(question1frame, aes(x = pjt, fill=group)) + geom_histogram(binwidth=5, alpha=.5) + geom_vline(data=cdat, aes(xintercept=trust.mean,  colour=group), linetype="dashed", size=1) + facet_grid(group ~ .) + xlab("Trust")


(police_trust = question1frame$pjt[question1frame$group == "police"])
(journalist_trust = question1frame$pjt[question1frame$group == "journalist"])

# t.test to see our difference:

t.test(police_trust, journalist_trust)

# probably want wilcoxon

wilcox.test(police_trust, journalist_trust, alternative = "two.sided")

press <- anes_data$ftjournal
police <- anes_data$ftpolice
press_range <- range(press) # [-7,100]
police_range <- range(police) # [0,100]
# press_range # [-7,100]
# police_range # [0,100]
press_data <- data.frame(police,press)
press_data = subset(press_data, ftjournal >= 0 & ftpolice >= 0)


############################################################################################### 
# work for question 2
###############################################################################################

democrat = anes_data[which(anes_data$pid1d == 1),]
republican = anes_data[which(anes_data$pid1d == 2),]

#filter by all of our voters who are sure they voted
democratVoted = democrat[which(democrat$turnout18 < 4),]
republicanVoted = republican[which(republican$turnout18 < 4),]

democrat.age = 2018 - democrat$birthyr
republican.age = 2018 - republican$birthyr

#make a list of dems
parties = c( rep("republican", nrow(republican)), rep("democrat", nrow(democrat)))
#make a list of repubs
party_colors = c( rep("ff0000", nrow(republican)), rep("0000ff", nrow(democrat)))

#make a dataframe for our q
#question2frame = data.frame(party = factor(parties), age = c(2018 - republican$birthyr, 2018 - democrat$birthyr), colors = party_colors)
question2frame = rbind(democratVoted, republicanVoted)
question2frame2 = rbind(democrat, republican)
#find the means
age_dat = ddply(question2frame, "party", summarise, age.mean=mean(age))
#age_dat = data.frame(mean(question2frame$age))

age_brackets = cut(2018 - question2frame$birthyr, 12, include.lowest=TRUE)

#population pyramid?
ggplot(question2frame, aes(x = 2018-birthyr, fill=pid1d)) +   # Fill column
  geom_bar(data=subset(question2frame, pid1d==1)) + 
  geom_bar(data=subset(question2frame, pid1d==2), aes(y=..count..*(-1))) + 
  # scale_y_continuous(breaks = seq(-400, 400, 10),
  #                   labels = paste0(as.character(c(40:0, 1:40)))) +
  coord_flip()
#  scale_fill_brewer(palette = "Set1")

#density plots
ggplot(question2frame2, aes(2018-birthyr)) + geom_density(aes(fill=factor(pid1d)), alpha=0.4) + 
  labs(title="Age Distibution of All Respondents",  x="Age", fill="Party Affiliation") + 
  scale_fill_manual(values=c("#0000ff", "#ff0000"),  breaks=c(1, 2), labels=c("Democrat", "Republican"))


ggplot(question2frame, aes(2018-birthyr)) + geom_density(aes(fill=factor(pid1d)), alpha=0.4) + 
  labs(title="Age Distibution of 2018 Election Voters", x="Age", fill="Party Affiliation") + 
  scale_fill_manual(values=c("#0000ff", "#ff0000"),  breaks=c(1, 2), labels=c("Democrat", "Republican"))

#plot it
ggplot(question2frame, aes(x = age, fill=party)) + geom_histogram(binwidth=5, position="dodge") + 
  facet_grid(party ~ .) + 
  geom_vline(data=age_dat, aes(xintercept=age.mean), linetype="dashed", size=1) + 
  xlab("Age") + 
  scale_fill_manual(values=c("#0000ff", "#ff0000"))

voted <- anes_data$turnout18
q2_vote = data.frame(party, age, voted)
q2_vote <- q2_vote[which (q2_vote$party == c(1,2)),]  # isolate for [1,2] = [D,R]
q2_vote <- q2_vote[which (q2_vote$voted == c(1,2,3)),] # isolate for those that voted 2018
t.test(age ~ party, data = q2_vote)

#I think we can just get away with a student t-test?
t.test(2018 - republican$birthyr, 2018 - democrat$birthyr)

#doesn't look like there's much age difference tbh

############################################################################################### 
# work for question 3
###############################################################################################

independents = anes_data[which(anes_data$pid1d == 3),] #get all of our independents
summary(independents$coord16) #do they think the Trump campaign coordinated with the Russians

# how do we land with our indies?
coord = independents[which(independents$coord16==1),]
nocoord = independents[which(independents$coord16==2),]

############################################################################################### 
# work for question 4
###############################################################################################

voters16 = anes_data[which(anes_data$turnout16 == 1),] #get all of our voters from 2016 who are sure they voted
voters18 = anes_data[which(anes_data$turnout18 == c(1,2,3)),] #get all of our voters from 2018 who are sure they voted

nrow(voters18) - nrow(voters16)

summary(voters16$geangry)
summary(voters18$geangry)

summary(voters16$geafraid)
summary(voters18$geafraid)

#get all of our voters who answered angry and afraid
geVoters18 = voters18[which(as.numeric(voters18$geangry) > 0 & as.numeric(voters18$geafraid) > 0),] 

question4frame = data.frame(emotion = c(rep("angry", nrow(geVoters18)), rep("afraid", nrow(geVoters18))), response = c(geVoters18$geangry, geVoters18$geafraid))

ggplot(question4frame, aes(x = question4frame$response, fill=emotion), stat="count") + geom_histogram(binwidth=1, position="dodge") + scale_x_continuous(breaks = c(1,2,3,4,5), labels=c("Not at all", "A little","Somewhat", "Very", "Extremely")) + labs(title="Anger and Fear in 2018 Election Voters", y="", x=" How do you feel about the way things are going in the country these days?")

geVoters18Frame = data.frame(anger = voters18$geangry, fear = voters18$geafraid)

ggplot(question4frame, aes(response)) + geom_density(aes(fill=factor(emotion)), alpha=0.4) + 
  labs(title="Sentiment",  x="Age", fill="Party Affiliation")
  scale_fill_manual(values=c("#0000ff", "#ff0000"),  breaks=c(1, 2, 3, 4, 5), labels=c("Not at all", "A little","Somewhat", "Very", "Extremely"))

#not sure if we can just run both?
wilcox.test(geVoters18$geangry, geVoters18$geafraid,  alternative = "two.sided")
wilcox.test(voters$geafraid, voters$geangry,  alternative = "greater")

############################################################################################### 
# work for question 5
###############################################################################################

#is there a correlation between age and donating to political campaigns?
cohen.d(birthyr ~ give, anes_data) # d estimate: -0.4745838 (small)

#is there a correlation between age and persuading others?
cov(anes_data$persuade, 2018 - anes_data$birthyr)
cor(anes_data$persuade, 2018 - anes_data$birthyr, method = "pearson") # -0.09644592
cohen.d(birthyr ~ persuade, anes_data) # d estimate: -0.2003081 (small)

#is there a correlation between age and talking online?
cov(anes_data$online, 2018 - anes_data$birthyr)
cor(anes_data$online, 2018 - anes_data$birthyr, method = "pearson") # -0.1720164
cohen.d(birthyr ~ online, anes_data) # d estimate: -0.2003081 (small)

#is there a correlation between age and donating to political orgs?
cov(anes_data$givefut, 2018 - anes_data$birthyr)
cor(anes_data$givefut, 2018 - anes_data$birthyr, method = "pearson") # -0.1660366
cohen.d(birthyr ~ givefut, anes_data) # d estimate: -0.3502668 (small)

#is there a correlation between age and meeting to talk
cov(anes_data$meet, 2018 - anes_data$birthyr)
cor(anes_data$meet, 2018 - anes_data$birthyr, method = "pearson") # -0.01604404
cohen.d(birthyr ~ meet, anes_data) # d estimate: -0.03799891 (negligible)

# lets see donations and age
givers = anes_data[which(as.numeric(anes_data$give) < 2),]
ggplot(givers, aes(x = 2018 - givers$birthyr)) + geom_histogram(binwidth=5)

donaters = anes_data[which(as.numeric(anes_data$givefut) < 2),]
ggplot(donaters, aes(x = 2018 - donaters$birthyr)) + geom_histogram(binwidth=5)

persuaders = anes_data[which(as.numeric(anes_data$persuade) < 2),]
ggplot(persuaders, aes(x = 2018 - persuaders$birthyr)) + geom_histogram(binwidth=5)

onliners = anes_data[which(as.numeric(anes_data$online) < 2),]
ggplot(onliners, aes(x = 2018 - onliners$birthyr)) + geom_histogram(binwidth=5)

meeters = anes_data[which(as.numeric(anes_data$meet) < 2),]
ggplot(meeters, aes(x = 2018 - meeters$birthyr)) + geom_histogram(binwidth=5)

q5DataFrame = rbind(data.frame(pred = 2018 - givers$birthyr, var = 'give'), 
                    data.frame(pred = 2018 - meeters$birthyr, var = 'meet'),
                    data.frame(pred = 2018 - persuaders$birthyr, var = 'persuade'),
                    data.frame(pred = 2018 - onliners$birthyr, var = 'online'),
                    data.frame(pred = 2018 - donaters$birthyr, var = 'donate'))

#this doesn't show us how many people we have in each bin though, so it needs tweaking
ggplot(q5DataFrame,aes(x=pred, fill=var)) + geom_histogram(alpha = 0.5, position = "dodge", bins = 5)


t.test(2018 - givers$birthyr, 2018 - anes_data$birthyr) #  p-value = 7.931e-15
t.test(2018 - donaters$birthyr, 2018 - anes_data$birthyr) # p-value = 2.903e-10
t.test(2018 - persuaders$birthyr, 2018 - anes_data$birthyr) # p-value = 0.001138
t.test(2018 - onliners$birthyr, 2018 - anes_data$birthyr) #p =  p-value = 1.182e-07
t.test(2018 - meeters$birthyr, 2018 - anes_data$birthyr) #p = 0.5347

activePolitics = anes_data[which(as.numeric(anes_data$follow) == 1),]
ggplot(activePolitics, aes(x = 2018 - activePolitics$birthyr)) + geom_histogram(binwidth=5)
t.test(2018 - activePolitics$birthyr, 2018 - anes_data$birthyr)

semiActivePolitics = anes_data[which(as.numeric(anes_data$follow) == 2),]
ggplot(semiActivePolitics, aes(x = 2018 - semiActivePolitics$birthyr)) + geom_histogram(binwidth=5)
t.test(2018 - semiActivePolitics$birthyr, 2018 - anes_data$birthyr)

inActivePolitics = anes_data[which(as.numeric(anes_data$follow) > 2),]
ggplot(inActivePolitics, aes(x = 2018 - inActivePolitics$birthyr)) + geom_histogram(binwidth=5)
t.test(2018 - inActivePolitics$birthyr, 2018 - anes_data$birthyr)

validWealthRespondents = anes_data[which(as.numeric(anes_data$faminc_new) > 0 & as.numeric(anes_data$faminc_new) < 20),]
hist(validWealthRespondents$faminc_new)

#classic r plot
boxplot(validWealthRespondents$faminc_new ~ validWealthRespondents$follow, names=c("Most of the time", "Some of the time", "Only now and then", "Hardly ever"), ylab="Family Income", xlab="Following Politics", yaxt="n")
axis(2, at=c(5, 10, 15),labels=c("$40k-$49k", "$100k-$119k", "$350k-$499k"), las=3)

#ggplot version
install.packages("wesanderson")
library(wesanderson)
palette = wes_palette("Zissou1", 4, type = "discrete")

q5frame = data.frame(validWealthRespondents)
ggplot(q5frame, aes(x=factor(q5frame$follow), y=q5frame$faminc_new, fill=factor(q5frame$follow))) + 
  geom_boxplot() + 
  scale_fill_manual(values = wes_palette("Zissou1", n = 4), name="Following politics", labels = c("Most of the time", "Some of the time", "Only now and then", "Hardly ever")) + 
  xlab("How closely do you follow politics?") + 
  scale_x_discrete(labels = c("Most of the time", "Some of the time", "Only now and then", "Hardly ever")) + 
  scale_y_continuous(name="Household Income", labels=c("$40k-$49k", "$100k-$119k", "$350k-$499k"), breaks=c(5, 10, 15))


activeWithWealth = validWealthRespondents[which(as.numeric(validWealthRespondents$follow) == 1),]
inActiveWithWealth = validWealthRespondents[which(as.numeric(validWealthRespondents$follow) == 4),]

wilcox.test(activeWithWealth$faminc_new, validWealthRespondents$faminc_new)
wilcox.test(inActiveWithWealth$faminc_new, validWealthRespondents$faminc_new)

cor(validWealthRespondents$faminc_new, 4 - validWealthRespondents$follow, method = "spearman")
