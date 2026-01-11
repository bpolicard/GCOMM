# +++++++++++++++++++++++++
# +++ R BOOTCAMP Exam+++
# +++++++++++++++++++++++++
# +++ EXAM Solutions +++
# + AUTHOR: Brian Policard +
# +++++++++++++++++++++++++
chooseCRANmirror(graphics=FALSE, ind=1)
knitr::opts_chunk$set(echo = TRUE)
#set working directory
setwd("/Users/brian/WorkingDirectory/7240_R_Bootcamp_Data")

# Question 1
#read/load in tabular data, "App.txt", as a data frame, "appDat"
appDat <- read.table(file = "App.txt", header = FALSE, sep = "\t", col.names = c("AppName", "Version", "TimesOpened", "Platform"))
#___________
#Question 2
library(foreign)
#load SPSS file as a data frame
empDat <- read.spss("empDat.sav", to.data.frame = TRUE,)
#show the first few rows
head(empDat)
#show the structure of the data frame; all factor
str(empDat)
#Show a summary of the data
summary(empDat)
#___________
#Question 3
#Read CSV file
adoptDat <- read.csv("AdoptData.csv")
#___________
#Question 4
#create a frequency table
table(adoptDat$VIQ)
#___________
#Question 5
#Frequency table rounded to the nearst 10
table(round((adoptDat$VIQ), digits = -1))
#___________
#Question 6
#return the birthwieght (BWT) of the 14th row (or 14th adopted child)
adoptDat[14, "Bwt"] 
adoptDat$Bwt[14] 
#___________
#Question 7
#number of children with birthweights under 3000
nrow( subset(adoptDat, Bwt < 3000) )  
#___________
#Question 8
#Of the above, return the number with a VIQ above 115
nrow( subset(adoptDat, Bwt < 3000 & VIQ > 115) ) 
#___________
#Question 9
#Return the 37th percentile of birthweight
quantile(adoptDat$Bwt, c(.37))
#___________
#Question 10 
#histogram of birthweights
hist(adoptDat$Bwt)
#____________
#Qeustion 11
#create a histogram with border in a cyan color and fill in a beige color
hist(adoptDat$Bwt, border = "cyan4", axes = TRUE, col = "beige")
#____________
#Question 12
#create a density plot callled "dens" with red border
dens <- density(adoptDat$Bwt)
plot(dens, main="Density of Birthweights")
polygon(dens, border="red")
#____________
#Question 13
#install package contents
install.packages("measurements")
#load the package to be used
library(measurements)
#convert adoptDat$Bwt units from grams to pounds and store the data in a vectore called "birthweightPounds"
birthweightPounds <- c(conv_unit(adoptDat$Bwt, "g", "lbs"))
#____________
#Question 14
##plotting birthweight data against VIQ data
plot(adoptDat$Bwt,adoptDat$VIQ)
### Answer: the relationship would not change even with the converted Bwt data since this would simply be a linear transformation, thereby preserving this linear relationship, so only the units and of the converted axis (in this case, the X acis) would change.
##plotting the same thing using converted birthweight data.
plot(birthweightPounds,adoptDat$VIQ)
#______________
#Question 15
## creating a vector called "count" containing consecutive numbers from 101 to 200
count <-  seq(101, 200, by = 1)
#_______________
#Question 16
## using "sum" function to find the sum of the vector "count"
sum(count)
#_______________
#Question 17
## display all odd numbers within the "count" vector
count[lapply(count, "%%", 2) == 1]
#_______________
#Question 18
## Returns the quantity of numbers withing the "count" that can be evenly divided by 13
length(count[lapply(count, "%%", 13) == 0])
#_________________
#Question 19
##load the "Seatbelts" dataset 
data("Seatbelts")
##Returns the mean of each specified column
mean(Seatbelts[,"DriversKilled"])
mean(Seatbelts[,"drivers"])
mean(Seatbelts[,"front"])
mean(Seatbelts[,"rear"])
mean(Seatbelts[,"kms"])
mean(Seatbelts[,"PetrolPrice"])
mean(Seatbelts[,"VanKilled"])
mean(Seatbelts[,"law"])
##Returns the medians
median(Seatbelts[,"DriversKilled"])
median(Seatbelts[,"drivers"])
median(Seatbelts[,"front"])
median(Seatbelts[,"rear"])
median(Seatbelts[,"kms"])
median(Seatbelts[,"PetrolPrice"])
median(Seatbelts[,"VanKilled"])
median(Seatbelts[,"law"])
#__________________
#Question 20
##load the faithful dataset
data("faithful")
summary(faithful)
## plot wait time and duration of eruptions, title the plot "Old Faithful Waiting times and Eruptions" and label the axes 
plot(
    faithful$waiting, faithful$eruptions, 
     main = "Old Faithful Waiting times and Eruptions", 
     xlab = "waiting times", ylab = "eruption duration"
    )
##on Average, this shows that there's a positive, linear relationship between waiting times and eruption durations of the Old Faithful Gyser; that is, the longer the waiting time, the longer the eruption tends to last on average. 
#__________________
#Question 21
##load ggplot package
library(ggplot2)
## create a plot using the "faithful" dataset, then make the points 3x bigger and blue, then label the axes and title it, then add a red regression line
ggplot(faithful, aes(x = waiting, y = eruptions)) + 
  geom_point(color='blue', size = 3) +
  ggtitle("Old Faithful Waiting times and Eruptions") + xlab("waiting times") + ylab("eruption duration") +
  geom_smooth(method = "lm", se = FALSE, colour = "Red")

