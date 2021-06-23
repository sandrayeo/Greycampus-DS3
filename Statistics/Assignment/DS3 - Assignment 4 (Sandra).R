#I. Data: The World Values Survey is an ongoing worldwide survey that polls the world population 
#about perceptions of life, work, family, politics, etc. 
#The most recent phase of the survey that polled 77,882 people from 57 countries estimates that 
#36.2% of the world's population agrees with the statement "Men should have more right to a job than women."
#The survey also estimates that 13.8% of people have a university degree or higher and that 3.6% of 
#people fit both criteria. 

#Q1:Are agreeing with the statement "Men should have more right to a job than women" and having a university degree or higher disjoint events?

#Answer: No, they are not disjoint events as they are not mutually exclusive. However, they are independent. 
#Disjoint events are events that never occur at the same time, and are also known as mutually exclusive events.

#Q2: Draw a Venn diagram summarizing the variables and their associated probabilities.

install.packages("VennDiagram")
install.packages("grid")
install.packages("futile.logger")
library(VennDiagram)

?sample
set1 <- sample(77882, 28193, replace=F) #men should have more rights
set2 <- sample(77882, 10747, replace=F) #have uni degree or above
set3 <- sample(77882, 2803, replace=F) #both
  
venn.diagram(
  x = list (set1, set2, set3), 
  category.names = c("Agree with statement that men has more right to a job than women", "has univerisity degree or higher", "fit both criteria"), 
  filename = 'venndiagram.png', 
  output = TRUE
)


#Q3: What is the probability that a randomly drawn person has a university degree or higher or agrees with the statement 
#about men having more right to a job than women?


Probability = 0.138+0.362-0.036 
View(Probability) #0.464. 


#Q4: What percent of the world population do not have a university degree and disagree with the statement about men having more right to a job than women?

Probability2 = 1-0.464 
View(Probability2) #0.536


#Q5: Does it appear that the event that someone agrees with the statement is independent of the event that they have a university degree or higher?

#Answer: independent means P(A and B) = P(A)*P(B)
# P(AՈB) = 0.036 and P(A)*P(B) = 0.362*0.138 = 0.0499
# They are not independent.


#Q6: What is the probability that at least 1 in 5 randomly selected people to agree with the statement about men having more right to a job than women?

# Answer: P(at least 1 in 5 people agree) = 1- P(none out of 5 agree)
#  = 1 – P(person disagree)^5
#  = 1-  (1-0.362)^5
#  = 1- 0.638^5
#  = 0.8942

#II. Data: As of 2009, Swaziland had the highest HIV prevalence in the world. 
#25.9% of this country's population is infected with HIV. 
#The ELISA test is one of the first and most accurate tests for HIV. 
#For those who carry HIV, the ELISA test is 99.7% accurate. (True Positive)
#For those who do not carry HIV, the test is 92.6% accurate. (True Negative) 
#If an individual from Swaziland has tested positive, what is the probability that he carries HIV? Create a tree diagram to calculate the probability.


#Answer: (0.259*0.997)/[(0.259*0.997)+(0.741*0.074)] = 0.824. 

#Tree diagram
#Total population segment into HIV (0.259) vs No HIV (0.741) 
#Under "HIV, split into has tested positive (0.997) vs tested negative (0.003)
#Under "No HIV", split into has tested positive (0.074) vs tested negative (0.926)

#Not sure how to create tree diagram in R. 
install.packages("ggraph")
install.packages("igraph")
install.packages("DiagrammeR")
install.packages("collapsibleTree")
library(ggraph)
library(igraph)
library(tidyverse)
library(DiagrammeR)
library(collapsibleTree)


#Q1:If an individual from Swaziland has tested positive, what is the probability that he carries HIV?

#Answer: 0.824


#Q2: According to a 2013 Gallup poll, worldwide only 13% of employees are engaged at work (psychologically committed to their jobs and likely to be making positive contributions to their organizations). 
#Among a random sample of 10 employees, what is the probability that 8 of them are engaged at work?

dbinom(8, 10, 0.13)   #2.77842e-06.


#Q3: Recent study: “Facebook users get more than they give”
#friend requests: 40% made, 63% received at least one
#likes: liked 14 times, had their content “liked” 20 times, on average
#messages: sent 9 messages, received 12, on average
#tags:12% tagged a friend in a photo, but 35% tagged other findings:
#25% considered power users
#average Facebook user has 245 friends
#P(70 or more power user friends) = ?

sum(dbinom(70:245, size = 245, p = 0.25)) #0.1127


#Q4: According to a 2014 Gallup poll, 56% of uninsured Americans who plan to get health 
#insurance say they will do so through a government health insurance exchange. What is the probability that in a random sample of 10 people exactly 6 plan to get health insurance through a government health insurance exchange?


dbinom(6, 10, 0.56)   #0.242



