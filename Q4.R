# 4.	A second regression model, with one dependent variable, 
# one independent variable, and one interacting variable. 
# Write the code in R to test if the independent variables interact to predict the dependent variable. 
# Run the analysis and record the findings. Provide a short paragraph (max 300 words) 
# describing what the analysis you provide represents.


data <- read.csv('Customer Data Set.csv')
data <- data[-c(94, 98, 91, 100, 92, 99, 115, 116, 4), ]
str(data)


####
#Membership tuenure has to be change into two columns to make it dependent variable
Membership <- cut(data$Membership.Tenure, breaks = c(0,12,100), labels = c("< 1 Year","> 1 Year"), right = FALSE)
data$Membership.Tenure[1:10]
Membership[1:10]


# Creating new dataframe
new_data <- data.frame(data$Gender ,Membership, data$Age, 
                       data$COASAT1, data$COASAT2, data$COASAT3, data$COASAT4, data$COASAT5)

# Changing name of age column
names(new_data)[names(new_data)== "data.Age"] <- "Ages"
names(new_data)[names(new_data)== "data.Gender"] <- "Gender"
names(new_data)[names(new_data)== "data.COASAT1"] <- "Coach1"
names(new_data)[names(new_data)== "data.COASAT2"] <- "Coach2"
names(new_data)[names(new_data)== "data.COASAT3"] <- "Coach3"
names(new_data)[names(new_data)== "data.COASAT4"] <- "Coach4"
names(new_data)[names(new_data)== "data.COASAT5"] <- "Coach5"
anyNA(new_data)

# Plotting the model
layout(matrix(1:2, ncol = 2))
cdplot(Membership ~ Coach5, data = new_data)


####
#new_data$Gender <-as.factor(new_data$Gender)
#new_data$Membership.Tenure <-as.factor(new_data$Membership)
#new_data$Customer <-as.factor(new_data$Customer)

# Do gender and how highly customers rate coaches affect membership?
m1 <- new_data$Membership ~ new_data$Ages + new_data$Gender + new_data$Coach5
glm1 <- glm(m1, data = new_data, 
            family = binomial())

summary(glm1)

m2 <- Membership ~ Ages * Coach5
glm2 <- glm(m2, data = new_data, 
            family = binomial())

summary(glm2)
#####################################################
# Example                                 # my data

# gender    = factor (male and female)    # Gender    = factor (male and female)
# education = integer 1-20                # Coasat1   = integer 1-5
# agree/dis = fm1
# fm1 = Agree/dissagree , Gender , Education

###########################

data <- read.csv('Customer Data Set.csv')
data <- data[-c(94, 98, 91, 100, 92, 99, 115, 116, 4), ]


Total_Coach_Score <- data$COASAT1 + data$COASAT2 +data$COASAT3 + data$COASAT4 + data$COASAT5
Total_Conditions_Score <- data$COND1 + data$COND2 + + data$COND3 + data$COND4 + data$COND5 + data$COND6

# Creating new dataframe
new_data <- data.frame(data$Gender ,data$Membership.Tenure, data$Age, Total_Coach_Score, Total_Conditions_Score)
names(new_data)[names(new_data)== "data.Age"] <- "Ages"
names(new_data)[names(new_data)== "data.Membership.Tenure"] <- "Membership"
names(new_data)[names(new_data)== "data.Gender"] <- "Gender"

# Summary stats
summary(Total_Coach_Score)

# Visualisations
plot(Total_Coach_Score ~ Total_Conditions_Score, pch = 16, col = 'blue',
     xlab = "Total Conditions Score (COND 1-6)",
     ylab = "Total Coach Score (COASAT 1-5)")

plot(Total_Coach_Score ~ new_data$Membership,pch = 16, col = 'blue',
     xlab = "Membership Tenure",
     ylab = "Total Coach Score (COASAT 1-5)")

# Creating GLM model
model1 <- glm(Total_Coach_Score ~ Membership + Total_Conditions_Score, data = new_data, 
              family = poisson())
summary(model1)


# Do they have an interaction? 
model2 <- glm(Total_Coach_Score ~ Membership * Total_Conditions_Score, data = new_data, 
              family = poisson())
summary(model2)

########################################################

# Generalised Linear Model (GLM) analysis to test and compare the relationship between the total score to the questions regarding coaches in Crossfitt Ridgeline. 
# The variable Total_Coach_Score was derived by adding the scores given by each member to the COASAT questions. 
# There was a culmination of five questions. The questions from the survey were on a likert scale starting at 1 (definitely disagree) to 5 (definitely agree) which ment the higher the score the happier clients were with the all round service of the coaches. 
# The highest posiible score to achieve was 25.

# Summary statistics above show the lowest score the received for the coaching services was 15/25 while the mean was around 23/25. 
# The goal of this analysis is to determine whether the total score given to the questions relating to Crossfit Ridgeline coaches in the survey was predicted by the membership tenure of the customers and the total score they gave to questions relating to the climate. 
# Similarily the to Total_Coach score the Total_ Conditions_Score was derived from questions relating to the climate in the gym (CON1-CON6). 

# As seen above the the membership of the members was not a good predictor of whether they would give the coaches a high rating in the survey (p=834857). 
# However there was a strong correlation between the scores given for the questions regarding conditions in the gym and the coaches (p=0.000925).

# Further analysis was done to determine whether membership and the total conditions scores interact with one another to predict the scores given to coaches. 
# As seen in the table above with a score of (p = 0.9373) no significant values below (p = 0.05) were observed therfore there was no interaction between these variables to predict the total coach score.