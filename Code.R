###Load required libraries###
library(ggplot2)
library(maps)
library(dplyr)
library(stringr)
library(lubridate)

###import csv file from local file system###
getwd()
setwd()
dat <- read.csv("Hotel_Reviews.csv")
head(dat)

cor.test(dat$Reviewer_Score, as.numeric(dat$City))
##0.001925496 
cor.test(dat$Reviewer_Score, as.numeric(dat$Country))
##-0.0316696 

##view a correlation matrix of the whole dataset##

dat.cor <- sapply(dat, as.numeric)
##removing the excess columns for the correlation matrix##
dat.cor <- dat.cor[, -18]
dat.cor <- dat.cor[, -19]

mydata <- cor(dat.cor[])
heatmap(mydata, symm=T)

##Hypothesis from data visualisation

#1. Negative Review correlates with Total negative Word Counts -> people who are dissatisfied tend to express it with words
#1.1 Are the negative review words more on average than the positive ones? 

#2. Average Score correlated highly with Total Number of Reviews 

#3. Total number of reviews given correlates with Country and Nationality 
#3.1. Check which countries have the highest ratings on average
#3.2. Check nationality distribution vs. ratings (group them into categorical: low, low-average, average, average-high, high). 

#4. Extract the rating per hotel: check if it would tie to anything else (such as number of + or - words, nationality, etc.)

#First, delete unnecessary columns#

dat$Hotel_Address = NULL ##we already extracted the Country and City
dat$days_since_review = NULL
#Turn the Reviewer Score into more neat column: it has way too many factor levels!#

##Clean the tags into separate columns each##

library(stringr)

tags <- dat$tags
tags <- gsub("''", "", tags) ##remove ''
tags <- tags %>% str_replace_all("\\[|\\]", "") ##remove [ ]
tags <- print(tags, quote = F) ##remove " "

##Feature engineering
##154640 users have given 1 review only, the remaining 361098 users have on average review count is 10

##visualize frequency of users and reviews
table(dat$Total_Number_of_Reviews_Reviewer_Has_Given)

##greater than 1
table(subset(dat$Total_Number_of_Reviews_Reviewer_Has_Given, dat$Total_Number_of_Reviews_Reviewer_Has_Given>1))

##Create UserID column and fill it with random number of users. This will enable the Imtem Recommender engine.

table(dat$Total_Number_of_Reviews_Reviewer_Has_Given)
users <- seq(from=1, to=192031, length.out = 192031)
head(users)
users <- as.integer(users)
dat$UserID <-sample(192031, size = nrow(dat), replace = TRUE)
head(dat$UserID)
str(dat$UserID)
View(dat)

###Export DF to CSV###
write.csv(dat,"C:/Data/Hotel Recommender//FileName.csv", row.names = FALSE)
