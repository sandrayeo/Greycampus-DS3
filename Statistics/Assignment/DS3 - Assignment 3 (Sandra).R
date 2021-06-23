getwd()
setwd("~/Documents")

install.packages ("tidyverse")
install.packages ("dplyr")
install.packages ("ggplot2")
install.packages ("readxl")
install.packages ("readr")
install.packages("DT")
library(DT)
install.packages("moments")
library(moments)
library (tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(readr)

getwd()
file = read.csv("covid1.csv", stringsAsFactors = FALSE)
view (file)
?slice

#Step 1: Keep rows containing country information and remove the rest of the rows. Apart from the country, you should not be having any other row. Move on to the next step only after finishing this.

#Remove first 8 rows
clean_data = slice(file, 9:n())
view (clean_data)

#Remove last 8 rows
clean_data = slice (clean_data, 1:221)
view (clean_data)

#remove first and second column
clean_data2 = clean_data[-1:-2]
view (clean_data2)

str(clean_data2)

#Replace blank values with NA
?replace
clean_data3 = clean_data2 %>% replace (.=="", NA)
view (clean_data3)


#Step 2:Calculate the missing percentage of each column using a function. 

missingcol <- function(x) {
  (sum(is.na(x))/length(x))*100
}
missing = apply(clean_data3, 2, missingcol)
view(missing)

sort(missing) #10 columns have missing data more than 5%. 


?which
na_perc = which(missing>10)  #changed this to 10%. #not sure what is the result showing - the column row? 
na_perc
view(na_perc)

# If any column has missing data more than 5%, please remove it.

clean_data4 = clean_data3[-na_perc]
view(clean_data4)
str(clean_data4)  #Left with 14 columns. 

#Step 3:Give a better column name after cleaning your data.

colnames(clean_data4) = c("Country", "TotalCases", "TotalDeaths", "TotalRecovered", "ActiveCases", "TotalCasesperMillionPopulation", "Deathspermillionpopulation", "Totaltests", "Testpermillionpopulation", "Population", "Continent", "XcasepereveryXppl", "XdeathpereveryXppl", "XtestpereveryXppl")
view(clean_data4)  #code ok
str(clean_data4)

#Converting data types
clean_data4$Continent = as.factor(clean_data4$Continent)
view(clean_data4)
str(clean_data4)  #code ok

for (i in c(2:10, 12:14)){
  clean_data4[, i] = as.numeric(gsub(",", "", clean_data4[,i]))  #why does it all appear as NA. Need to use gsub to convert comma into nothing?
}

view(clean_data4)
str(clean_data4)
summary (clean_data4)

#3.1 Create plots for total cases, total death, and total recovery. Explain with a figure for each.

boxplot(clean_data4$TotalCases)
boxplot(clean_data4$TotalDeaths)
boxplot(clean_data4$TotalRecovered)

boxplot (cbind(clean_data4$TotalCases, clean_data4$TotalDeaths, clean_data4$TotalRecovered), main = 'Cases', 
         names = c("Total cases", "Total Deaths", "Total Recover"), frame = TRUE, na.rm = TRUE)

install.packages("ggpubr")
library("ggpubr")

ggdensity(clean_data4$TotalCases, 
          main = "Density plot for Total Cases",
          xlab = "Total Cases")


#3.2. Create a plot to examine the correlation between total cases and total population. 
#Explain if there is any correlation between total cases and total population.

plot(clean_data4$TotalCases, clean_data4$Population, type = "b")  #scatterplot

cor(clean_data4$TotalCases, clean_data4$Population, method = c("pearson"))

#No, there is a correlation?


#3.3 Create a plot to examine the correlation between Tot Cases/1M pop and total population. 
#Explain if there is any correlation between them?

plot(clean_data4$TotalCasesperMillionPopulation, clean_data4$Population, type = "b")

#not sure if there's any correlation?


#3.4 Which column do you feel is better for comparison purposes, total cases or TotCases/1M pop. Explain.

#Answer: Totalcases per 1m population, as it is the average/more objective measurement. Total cases is subjective, as the higher the population, the more cases there will be. 


#3.5 Create a plot to examine the correlation between total cases and total death. Explain the figure.


plot(clean_data4$TotalCases, clean_data4$TotalDeaths, type = "b")

?ggplot  #not sure how to set different colors/shapes. 
clean_data4 %>% ggplot (aes(x = TotalCases, y = TotalDeaths)) + geom_point(alpha = 0.5, color = "red") + labs(title = "cases and deaths", x = "Total Cases", y="Total Deaths") + stat_smooth(method = "lm", col = "blue", se = FALSE)

#3.6. Create a plot to examine the correlation between total cases and Deaths/1M pop. 
#Explain the figure. Which column is more suitable to compare the result, total death or Death/1Mpop?

clean_data4 %>% ggplot (aes(x = TotalCases, y = Deathspermillionpopulation)) + geom_point(alpha = 0.5, color = "red") + labs(title = "cases and deaths", x = "Total Cases", y="Total Deaths") + stat_smooth(method = "lm", col = "blue", se = FALSE)

#There is no correlation. Column of Total Death is more suitable to compare the results with Total Cases, as it is a subset of it. 


#3.7. Compare Tot Cases/1M pop by continent, and explain your result. 

?ggplot
clean_data4 %>% ggplot (aes (x = TotalCasesperMillionPopulation, y = Continent)) + geom_bar(fill = "white", color = "green", stat = "identity", width = 0.5)

#Europe has the most cases per 1m population, while Australia has the least.


#3.8. Compare Deaths/1M pop by continent, and explain your result. 

clean_data4 %>% ggplot (aes (x = Deathspermillionpopulation, y = Continent)) + geom_bar(fill = "white", color = "green", stat = "identity", width = 0.5)

#Europe has the most cases per 1m population, while Australia has the least.


#3.9. Which country is best among testing the COVID19 and which country is worst? 
#There are two columns total test vs. test/M. Choose appropriate column. 

sort(clean_data4$XtestpereveryXppl)
?arrange
clean5 = arrange(clean_data4, desc(XtestpereveryXppl))
view(clean5)

clean_data4$Country[which.max(clean_data4$XtestpereveryXppl)]
clean_data4$Country[which.min(clean_data4$XtestpereveryXppl)]

#country Yemen is the best, while UAE is the worst. 


#3.10 Compare your COVID19 test results by continent. There are two columns total test vs test/M. Choose appropriate column.

clean_data4 %>% ggplot (aes (x = Testpermillionpopulation, y = Continent)) + geom_bar(fill = "white", color = "green", stat = "identity", width = 0.5)


#Europe is the best, followed by Asia, then NA, SA, Africa, and lastly Australia.


#3.11 Check if Tests/1M pop is skewed or normally distributed.

?hist
hist(clean_data4$Testpermillionpopulation)
summary(clean_data4$Testpermillionpopulation). #the mean is 448000, while the median is 191000. 

#it is skewed to the right. 




