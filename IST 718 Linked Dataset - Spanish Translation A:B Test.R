## IST 718: Advanced Information Analytics

## Linked Dataset Assignment

## Spanish Translation A/B Test

## Author: Mitul Shah

## Installing the required packages
install.packages("dplyr")
install.packages("rpart")
install.packages("ggplot2")

## Loading these packages
library(dplyr)
library(rpart)
library(ggplot2)

## Importing the data into R
test <- read.csv(file.choose(), header = T)
user <- read.csv(file.choose(), header = T)

## Assess Data Modes by viewing the structure of the data
str(test)
str(user)

## Assess Completeness of values using summary
summary(test)
summary(user)

## Identify the completeness of records
## Are there any duplicates?
length(unique(test$user_id)) == length(test$user_id) 

length(unique(user$user_id)) == length(user$user_id) 

length(user$user_id) - length(test$user_id)

## Convert the mode of Date variable from factor to date
test$date = as.Date(test$date)

## Merge the datasets
data = merge(test,user, by = "user_id", all.x = TRUE)

## View the merged dataset
View(data)

## Viewing the summary of this dataset
summary(data)

## Defining another dataset to check the conversion rate of Spain in control group as compared to other countries
data_conversion_country =  data %>%
  group_by(country) %>%
  summarize( conversion = mean(conversion[test == 0])) %>%
  arrange (desc(conversion))

## Check the first six rows of this data
head(data_conversion_country)

## Visualize this in the form of barplot
barplot(data_conversion_country$conversion, names.arg = data_conversion_country$country
        ,xpd       = TRUE
        ,col       = c("#3CA0D0")
        ,main      = "Conversion rate before translation"
        ,xlab      = "Country",
        ,ylab      = "Conversion_rate"
        ,yaxt      = 'n'
        ,cex.names = 0.50
        ,cex.lab   = 0.80)

## Analyzing the conversion rate after each country had its translation written by local

## Removing the Spain users
data_test = subset(data, country != "Spain")

## Using t-test on test users and control users to compare their conversion rate
t.test(data_test$conversion[data_test$test == 1], data_test$conversion[data_test$test == 0])

## Trying to find out the reason for bad A/B test results

## Installing the package vioplot
install.packages("vioplot")

## Loading this package
library(vioplot)

## Plotting the density and range of ages in different countries
## Choosing the countries 
x1 <- data_test$age[data_test$country == "Chile"]
x2 <- data_test$age[data_test$country == "Mexico"]
x3 <- data_test$age[data_test$country == "Colombia"]
x4 <- data_test$age[data_test$country == "Argentina"]
x5 <- data_test$age[data_test$country == "Peru"]
x6 <- data_test$age[data_test$country == "Venezuela"]

## Visualizing by violin plot
vioplot(x1, x2, x3, x4, x5, x6, names = c("Chile", "Mexico", "Colombia", "Argentina", "Peru", "Venezuela"), col = "gold")

## Checking the standard deviations in these countries
sd(x1)
sd(x2)
sd(x3)
sd(x4)
sd(x5)
sd(x6)

## Analyzing these results by date
data_test_by_day = data_test %>%
  group_by(date) %>%
  summarize(test_vs_control = mean(conversion[test==1])/
              mean(conversion[test==0])
  )

## Line plot
qplot(date, test_vs_control, data= data_test_by_day, geom = "line")

## Using the tree to find the bias 

# We remove conversion. Doesn't matter now.
tree = rpart(test ~ .,data_test[,-8], control = rpart.control(minbucket = nrow(data_test)/100, maxdepth = 2))
## We only look for segments representing at least 1% of the populations.
## Here, we are using tree as a statistics tool.
## We are not too interested in its predictive power. 

tree

## After we control for country, the test clearly appears non-significant. 
## Given that the goal was to improve the conversion rate, it’s not a great success,
## but at least we know that a localized translation didn’t make things worse.

