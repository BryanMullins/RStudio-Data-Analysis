# Boxplot Analysis

library(ggplot2)
library(plyr)    
library(ggplot2)
library(EnvStats)

# Reading in csv file
data <- read.csv('Customer Data Set.csv')

# Exploring the data
dim(data)
str(data)
summary(data)
colnames(data)
head(data)
anyNA(data)
nrow(data)

# Removing survey submissions with NA's
data <- data[-c(94, 98, 91, 100, 92, 99, 115, 116, 4), ]
anyNA(data)
nrow(data)

# Changing Gender variable factor
data$Gender <-as.factor(data$Gender)

# Changing the name to Male/Female
data$Gender<-factor(data$Gender,
                    levels=c(1,2),
                    labels=c("Male", "Female"))
with(data, summary(Gender))
with(data, summary(Membership.Tenure))


#######Graph 1
# Plotting Boxplot using GGPLOT2
ggplot(data, aes(x = Gender, y = Membership.Tenure)) + 
  geom_boxplot(fill = "#0099f7", varwidth = TRUE) +
  stat_summary(fun = mean, color = 'black', shape = 8) +
  labs(
    title = "Number of months Membership Tenure",
    subtitle = "For Males and Females",
    caption = "Source:Crossfit Ridgeline Survey",
    x = "Gender",
    y = "Membership Tenure (Months)",
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "Black", size = 14, face = "bold", hjust = 0.0),
    plot.subtitle = element_text(face = "bold.italic", hjust = 0.1),
    plot.caption = element_text(face = "italic"),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  ) +
  stat_n_text(y.pos=60)
  
###################### Graph two jitter
# Plotting Boxplot using GGPLOT2
ggplot(data, aes(x = Gender, y = Membership.Tenure)) + 
  geom_boxplot(fill = "#0099f7", varwidth = TRUE) +
  geom_jitter(alpha = 0.8, width = 0.2, height = 6, color = 'red') +
  labs(
    title = "Number of months Membership Tenure",
    subtitle = "For Males and Females",
    caption = "Source:Crossfit Ridgeline Survey",
    x = "Gender",
    y = "Membership Tenure (Months)",
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "Black", size = 14, face = "bold", hjust = 0.0),
    plot.subtitle = element_text(face = "bold.italic", hjust = 0.1),
    plot.caption = element_text(face = "italic"),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  ) 

# Boxplot analysis to compare the duration of membership tenure between males and females who participated in the Crossfitt Ridgeline Survey. 
# From analysis of figure one above the number of females who did the survey (n=74) was almost double compared to males (n=33). 
# Taking into account the nine other submissions that were removed from this analysis because they were not fully completed. 
# The horizontal line in the box represents the median tenure for males (17.5 months) and females (14 months). 
# The median divides the lower 50% of observations from the upper 50% of observations. 
# The asterix symbol in each box seen in figure 1 signifies the mean tenure. The mean tenure for males is around 20 months and just below that for females.
# Figure two displays each data point and shows exactly where they are located on each boxplot. 
# The area the boxes represents the 25th percentile (Q1) and 75th percentile (Q3). 
# Fifty percent of the membership tenures lies between these ranges. Around 10-38 months for males and 10-22 months for females.
# There are five outliers for females with very high membership tenures compared to the rest of the survey entries and no outliers for males. 
# These female outliers have had membership for over fifty-four months. 
# Although containing outliers the female boxplot is more condensed which signifies it varies less than the male membership tenures. 
# Females more consistent mean membership should make predictions more dependable than the male data.
