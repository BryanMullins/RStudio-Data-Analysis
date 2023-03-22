# Analysis of the responses to the CrossFit Ridgeline survey relating to the conditions in the gym (COND 1-6). The six questions asked were as follows:

#  1.	Is the equipment adequate for a fitness gym?
#  2.	Is the equipment in good condition and safe for use?
#  3.	Is music loudness appropriate?
#  4.	Is it a safe environment as far as injury is concerned?
#  5.	Is the choice of music appropriate?
#  6.	Is the gym clean and organized? 
  
# The survey consisted of 106 respondents. 
# Participants in the survey were asked to rate the above question on a scale from 1 (strongly disagree) to 5 (strongly agree).  
# As seen from the results below the consensus from the respondents was significantly positive. 

# For the six questions analysed at least 60% percent of respondents ‘strongly agreed’ with all statements with a score of 5 given. An overwhelming response was received to question one with (93%) of respondents completely agreeing that the equipment in the gym was adequate. 
# The blue area of the graph below represents participants who neither agreed nor disagreed. This selection was most prominent in question six relating to the cleanliness of the gym (11%). 
# Although an overall positive response, some members disagreed with the cleanliness of the gym (4%) and whether it was a safe environment for injuries (4%), this may be something managers at CrossFit Ridgeline may want to investigate to further improve the environment in the gym.

########################################
library("plyr")
library("dplyr")
library('ggplot2')

data <- read.csv('Customer Data Set.csv')
str(data)

###################################################################

# Libraries
library("likert")
library("plyr")
library("dplyr")
library('ggplot2')

# Reading in the data
data <- read.csv('Customer Data Set.csv')

# removing outliers
data <- data[-c(94, 98, 91, 100, 92, 99, 115, 116, 4), ]
str(data)
items <- select(data, starts_with(c("COND")))
summary(items)
items <- items[-c(96), ]

# Rename the columns
names(items) <- c(
  COND1 ="CrossFit Ridgeline is a safe environment, as far as injury is concerned.",
  COND2 ="CrossFit Ridgeline's gym space is clean and organized.",
  COND3 ="CrossFit Ridgeline's equipment is in good condition and safe for use.",
  COND4 ="CrossFit Ridgeline's range of equipment is adequate for a fitness gym.",
  COND5 ="CrossFit Ridgeline's music choices are appropriate for a fitness gym.",
  COND6 = "CrossFit Ridgeline's music loudness level is appropriate for a fitness gym. ")

# Function to put numerical responses into factors
likert_funtion <- function(x) {
  y <- ifelse(is.na(x), NA,
              ifelse(x == 1, "Strongly disagree",
                     ifelse(x == 2, "Disagree",
                            ifelse(x == 3, "Neutral",
                                   ifelse(x == 4, "Agree", "Strongly agree")))))
  
  y <- factor(y, levels = c("Strongly disagree", "Disagree","Neutral", "Agree", "Strongly agree"))
  
  return(y)
}

# Changing items to likert object
likert_object <- items %>%
  mutate_all(likert_funtion) %>%
  likert()


likert.bar.plot(likert_object, plot.percents=TRUE, 
                plot.percent.high= FALSE, 
                plot.percent.low=FALSE, 
                center = 3,
                low.color = "brown1", 
                high.color = "chartreuse2", 
                neutral.color = "lightblue",
                legend.position = "bottom", text.size=3.5,
                wrap = 25)







b########################################################### other graphs
# Activate the dplyr package
library("dplyr")

# Correlation matrix of items
cormat <- data %>%
  select(starts_with(c("COND"))) %>%
  cor(., use = "pairwise.complete.obs")

# Activate the corrplot package
library("corrplot")
install.packages("corrplot")

# Correlation matrix plot
corrplot(cormat, # correlation matrix
         order = "hclust", # hierarchical clustering of correlations
         addrect = 2) # number of rectangles to draw around clusters

# Activate the ggcorrplot package
install.packages("ggcorrplot")
library("ggcorrplot")

# Correlation matrix plot
ggcorrplot(cormat, # correlation matrix
           type = "lower", # print the lower part of the correlation matrix
           hc.order = TRUE, # hierarchical clustering of correlations
           lab = TRUE) # add correlation values as labels




###################
# Create a stacked bar chart
plot(items_likert, 
     # Group the items alphabetically
     group.order=names(items),
     #colors
     low.color = "grey72", 
     high.color = "red3", 
     neutral.color = "lightsalmon",
     # Plot the percentages for each response category
     plot.percents = TRUE,
     # Plot the total percentage for negative responses
     plot.percent.low = TRUE,
     # Plot the total percentage for positive responses
     plot.percent.high = FALSE,
     # Whether response categories should be centered
     # This is only helpful when there is a middle response
     # option such as "neutral" or "neither agree nor disagree"
     centered = FALSE,
     # Wrap label text for item labels
     wrap=25)

