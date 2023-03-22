#3. One regression model, with one dependent variable and at least three independent variables. 
# The model should make logical sense 
# i.e. do not pick variables at random to see if they are related. 
# Write the code in R to test if there are significant relationships in your model. 
# Run the analysis and record the findings. 
# Provide a short paragraph (max 300 words) describing what the analysis you provide represents.
library(ggplot2)
library(plyr)    
library(ggplot2)
library(pbkrtest)
library(statmod)
library(sjPlot)
library(lme4)

data <- read.csv('Customer Data Set.csv')
data <- data[-c(94, 98, 91, 100, 92, 99, 115, 116, 4), ]

#Membership tuenure has to be change into two columns to make it dependent variable
Membership <- cut(data$Membership.Tenure, breaks = c(0,12,100), labels = c("< 1 Year","> 1 Year"), right = FALSE)
data$Membership.Tenure[1:10]
Membership[1:10]

# Adding the different subgroup of questions to get total scores
# Questions in the survey were grouped based on what part of the Ridgeline service they related to

Customer <- data$CUSSAT1 + data$CUSSAT2 +data$CUSSAT3 + data$CUSSAT4
Coaches <- data$CUSSAT1 + data$COASAT2 +data$COASAT3 + data$COASAT4
Climate <- data$CLIM1 + data$CLIM2 + data$CLIM3 + data$CLIM4 + data$CLIM5 + data$CLIM6
Conditions <- data$COND1 + data$COND2 + + data$COND3 + data$COND4 + data$COND5 + data$COND6

# Creating new dataframe
new_data <- data.frame(Membership, data$Age, Customer, Coaches, Climate, Conditions)
# Changing name of age column
names(new_data)[names(new_data)== "data.Age"] <- "Ages"
anyNA(new_data)
# Removing N/A from new dataframe
new_data <- new_data[-c(92), ]


# Plotting the model
layout(matrix(1:2, ncol = 2))
cdplot(Membership ~ Customer, data = new_data)
cdplot(Membership ~ Coaches, data = new_data)
cdplot(Membership ~ Climate, data = new_data)
cdplot(Membership ~ Conditions, data = new_data)
cdplot(Membership ~ Ages, data = new_data)


# Getting statistical data on performance of model
m1 <- glm(Membership ~ Ages, data = new_data, 
          family = binomial())

# Talks about summary at 12:40 in video 1
summary(m1)
# Odd-ratio
exp(coef(m1)['Ages'])
# 95% CI around coefficient 
exp(confint(m1, parm = 'Ages'))

## Mutivariate - Five variables
m2 <- glm(Membership ~ Ages + Customer + Coaches + Climate + Conditions, 
          data = new_data, 
          family = binomial())
summary(m2)

# Which model is better?
anova(m1,m2, test = "Chisq")

prob <- predict(m2, type = "response")

# Visualisations
plot(Ages ~ Conditions,data = new_data, pch = 16, col = 'blue')

plot(Ages ~ Climate,data = new_data, pch = 16, col = 'blue')

plot(Climate ~ Conditions,data = new_data, pch = 16, col = 'blue')

# Plotting a clearer version of the reuslts
tab_model(m2)
model2 <- plot_model(m2, type = 'pred')
plot_grid(model2)

###############################################
# Generalised Linear Model (GLM) analysis to test and compare the relationship between membership tenure with the age of the members and topics of the survey questions in the Crossfitt Ridgeline Survey. The length of membership provided by participants in the survey was separated into two catgeories those who have been members for less than a year (<12 months) and those who have been member for a year or more (>12 months). The survey consisted of twenty-two likert scale questions starting from 1 (definitely disagree) to 5 (definitely agree).  For this analysis the questions were grouped into four categories based on different aspects relating to the services provided by CrossFit Ridgeline: 
  
#  1.	Customers	 (CUSSAT 1-4)
#  2.	Coaches	 (COASAT 1-5)
#  3.	Climate	 (CLIM 1-6)
#  4.	Conditions	 (COND 1-6)

# The goal of this analysis is to determine which area of the Crossfit Ridgeline service has the greatest impact on the length of membership tenure by customers. 
# Therefore membership was held as a constant variable. 
# Starting off the age of each member was added to the model to predict the length of their membership. However as predicted the age of the members is a good predictor of membership length as older members are less likely to participate in crossfit.  
# Therefore the other four variables listed above were added to the model to identify any specific are of the business that predicts the length of membership tenure.

# The Pr(>|z|) column represents the p-value associated with the value in the z value column. If the p-value is less than (a = .05) this indicates that the predictor variable has a statistically significant relationship with the response variable in the model.
# From the results above it can be seen that Ages, Conditions and Climate are all significant variables as they are less than (p=0.05) signifying they do have a statistically significant relationship with the response variable in the model. 

