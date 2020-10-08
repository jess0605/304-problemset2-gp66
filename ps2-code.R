####################### STA304: Problem Set 2 ########################
## A Potential Solution to Gain Popularity in the Previously Swing Riding
##  - Social Media


# Below is the detail of code supporting this analysis:
### preparation
# set op or download pakages
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(ggplot2)
#install.packages("ggthemes")
library(ggthemes)
# install.packages("sampling")
library(sampling)
library(dplyr)
library(tibble)
library(gridExtra)
library(knitr)

### data simulation
# set population size as 78500
N <- 78500
set.seed(1999) # set seed so that the result could be reproductible
# According to the designed survey questions, simulate dataset
#============Question 1: Simulate data for gender==============
gender_list <- c("Female","Male","Other")
gender <- sample(x=gender_list, N,replace=TRUE,prob=c(0.58,0.41,0.01))

#============Question 2: Simulate age spreads==================
age_list <- c("18 to 24 Years","25 to 34 Years","35 to 44 Years",
              "45 to 54 Years","55 to 64 Years", "65 to 74 Years",
              "75 Years and older") 
age <- sample(x=age_list, N, replace=TRUE,
              prob=c(0.12,0.18,0.2,0.18,0.21,0.09,0.02))

#============Question 3: Simulate participants' annual income===========
income_list<-c("$0 to $10,000 ","$10,000 to $39,999","$40,000 to $69,999",
               "$70,000 to $99,999","$100,000 and over")
income <- sample(x=income_list, N, replace = TRUE,
                 prob=c(0.18,0.37,0.23,0.15,0.09))

#============Question 4: Simulate participants' last votes==============
party_list <- c("New Democratic","Liberal","Communist","Independent",
                "Conservative","Green","Others")
last_voted_party <-sample(x = party_list, N, replace =TRUE,
                          prob=c(0.41,0.44,0.005,0.002,0.10,0.033,0.01))

#============Question 5: Simulate satisfaction level of the current ruling party=========
# 1-extremely unsatisfied 2-somehow unsatisfied 3-no opinion
# 4-somehow satisfied 5-exremely satisfied
satisfication_list <- c(1:5)
satisfaction <- rbinom(N, 5, 0.75)

# Generate Population Dataset
mydata <- tibble(gender, age, satisfaction, income, last_voted_party)

#============Question 6: Simulate participants' next vote=============
party_list <- c("New Democratic","Liberal","Communist","Independent",
                "Conservative","Green","Others")
mydata$next_vote <- NA # create a new variable: next_vote

# group the population by measurements' last votes
last_vote_ND <- nrow(mydata[mydata$last_voted_party=="New Democratic",])
last_vote_lib <- nrow(mydata[mydata$last_voted_party=="Liberal",])
last_vote_com <- nrow(mydata[mydata$last_voted_party=="Communist",])
last_vote_indep <- nrow(mydata[mydata$last_voted_party=="Independent",])
last_vote_conser <- nrow(mydata[mydata$last_voted_party=="Conservative",])
last_vote_green <- nrow(mydata[mydata$last_voted_party=="Green",])
last_other <- nrow(mydata[mydata$last_voted_party=="Others",])

# simulate their next votes with different probabilities
will_vote_ND <- sample(party_list, last_vote_ND, replace=TRUE,
                       prob=c(0.95,0.023,0.008,0.002,0.003,0.004,0.01))
will_vote_lib <- sample(party_list, last_vote_lib, replace=TRUE,
                        prob=c(0.14,0.77,0.005,0.002,0.04,0.033,0.01))
will_vote_com <- sample(party_list, last_vote_com, replace=TRUE,
                        prob=c(0.07,0.05,0.85,0.005,0.005,0.01,0.01))
will_vote_indep <- sample(party_list, last_vote_indep, replace=TRUE,
                          prob=c(0.004,0.009,0.005,0.96,0.01,0.002,0.01))
will_vote_conser <- sample(party_list, last_vote_conser, replace=TRUE,
                           prob=c(0.01,0.002,0.001,0.023,0.95,0.004,0.01))
will_vote_green <-sample(party_list, last_vote_green, replace=TRUE,
                         prob=c(0.139,0.04,0.004,0.002,0.005,0.8,0.01))
will_vote_other <- sample(party_list, last_other, replace=TRUE,
                          prob=c(0.1,0.038,0.005,0.002,0.10,0.03,0.86))

# simulate participants' next voting based on their votes on last election
mydata$next_vote[mydata$last_voted_party=="New Democratic"] <- will_vote_ND
mydata$next_vote[mydata$last_voted_party=="Liberal"] <- will_vote_lib
mydata$next_vote[mydata$last_voted_party=="Communist"] <- will_vote_com
mydata$next_vote[mydata$last_voted_party=="Independent"] <- will_vote_indep
mydata$next_vote[mydata$last_voted_party=="Conservative"] <- will_vote_conser
mydata$next_vote[mydata$last_voted_party=="Green"] <- will_vote_green
mydata$next_vote[mydata$last_voted_party=="Others"] <- will_vote_other

#============Question 7: Simulate the audience opinion on media effect on voting======
# set the scales according to the survey question on participants' opinion on media
media_list <- c("strongly agree","agree","no opinion","disagree","strongly disagree")
mydata <- mutate(mydata, media_effect=NA) #create a new empty variable

# record the measurements that plan to vote for different party
vote_diff <- nrow(mydata[mydata$last_voted_party!=mydata$next_vote,])
vote_same <- nrow(mydata[mydata$last_voted_party==mydata$next_vote,])

# simulate party-switching participants' opinions on media effect
agree_opinion <- sample(media_list, vote_same, replace = TRUE,
                        prob = c(0.15,0.45, 0.15, 0.1, 0.15))
disagree_opinion <- sample(media_list, vote_diff, replace = TRUE,
                           prob = c(0.05,0.2,0.15,0.45,0.15))
# frame the simulation into dataset
mydata$media_effect[mydata$last_voted_party==mydata$next_vote] <- agree_opinion
mydata$media_effect[mydata$last_voted_party!=mydata$next_vote] <- disagree_opinion

### sampling process
# perform random selection without replacement
# we set ideal sample size n as 1500
srswor_sample <- sample(1:N, 1500, replace = FALSE)
# store the sample dataset for the following analysis
mydata <- mydata %>% slice(srswor_sample)

### quantitative analysis
# simulated distribution of satisfaction data
# assuming satisfaction scores follows a binomial distribution
# binomial(N, 5, 0.75)
# ~N(5*0.75, 5*0.75*(1-0.75)) -> N(3.75, 0.9375)

n<-1500 #define the sample size
# calculate 95% confidence interval of population mean
# get sample mean and its standard deviation
summary(mydata$satisfaction)
sd(mydata$satisfaction)
# calculate the marginal error
margin_error <- qt(.95,n-1)*sd(mydata$satisfaction)/sqrt(n)
# calculate the CI
lower_bound <- mean(mydata$satisfaction) - margin_error
upper_bound <- mean(mydata$satisfaction) + margin_error
lower_bound
upper_bound
# calculate estimation of the variance of population satisfaction scores
pop_satisfaction_var <- (1-(n/N))*var(mydata$satisfaction)/n
pop_satisfaction_var

### visualizations
mydata %>%
  ggplot(aes(x=`income`,fill=`next_vote`)) +
  geom_bar(position="dodge")+
  scale_color_fivethirtyeight() +
  ylab("numbers of respondents") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ggtitle("Figure 1: Political Orientation Between Respondents with Different Earnings")

mydata %>%
  ggplot(aes(x=`income`,fill=`next_vote`)) +
  geom_bar(position="fill") +
  scale_color_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ylab('proportion withn each earning level') +
  ggtitle("Figure 2: Different Earning Respondents' Political Orientation(within income groups)")

# want to plot the opinion on media effect of audience who plan to vote differently
# first, create new column to record differences between two votes
mydata$party_choice <-ifelse(mydata$last_voted_party == mydata$next_vote,
                             'remain', 'switch')
p1 <- mydata %>% 
  filter(party_choice=="remain") %>%
  ggplot(aes(x=media_effect, group=party_choice)) +
  geom_bar(fill='#8CBD8C',col='grey') +
  xlab("participants' opinion on media effect") +
  ylab("number of people remain their vote choice") +
  ggtitle('Figure 3.1: Opinions on Media Effect From Respondents Who Remain Vote Choices')

p2 <- mydata %>% 
  filter(party_choice !="remain") %>%
  ggplot(aes(x=media_effect, group=party_choice)) +
  geom_bar(fill='#8CBD8C',col='grey') +
  xlab("participants' opinion on media effect") +
  ylab("number of people change their vote choice") +
  ggtitle('Figure 3.2: Opinions on Media Effect From Respondents Who Change Vote Choices')

p1
p2

mydata %>%
  ggplot(aes(x=`income`,fill=`media_effect`)) +
  geom_bar(position="fill") +
  scale_color_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ylab("Proportion within each Income Level") +
  xlab("Annual Incomes of Respondents") +
  ggtitle("Figure 4: Opinions on Media Effect Among Different Earnings")

mydata %>%
  ggplot(aes(x=`age`,fill=`media_effect`)) +
  geom_bar(position="fill") +
  scale_color_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ylab("Proportion Within Each Age Group") +
  xlab("Age of the Respondents") +
  ggtitle("Figure 5: Different Age Groups' Opinion on Media Effect")

mydata %>%
  ggplot(aes(x=`last_voted_party`,fill=`media_effect`)) +
  geom_bar(position="fill") +
  scale_color_fivethirtyeight()+
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ylab("proportion under each former voted group") +
  xlab("repondents previous political orientation") +
  ggtitle("Figure 6: Opinions on media effect based on former voting")

mydata %>%
  ggplot(aes(x=`gender`,fill=`media_effect`)) +
  geom_bar(position="fill") +
  scale_color_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ylab("proportion within each gender group") +
  xlab("Gender of the Respondents") +
  ggtitle("Figure 7: Opinion on Media Effects among Gender Groups")

# screen shots of survey
knitr::include_graphics("/Users/jingxihuang/0001.jpg")
knitr::include_graphics("/Users/jingxihuang/0002.jpg")
knitr::include_graphics("/Users/jingxihuang/0003.jpg")
knitr::include_graphics("/Users/jingxihuang/0004.jpg")

