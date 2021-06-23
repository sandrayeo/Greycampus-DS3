#Question 1: The final score of the World Cup finals of football is France  4 and Croatia 2. 
#Please use a control structure to print the results as Team ---- Wins -----.

france = 4
croatia = 2
if (france > croatia) {
print ("Team France wins")
} else if (france == croatia) {  #use ==
print ("Draw") 
} else {
print ("Team Croatia wins")
}

#[NEED HELP] Question 2: The mtcars data set has several factor variables. 
#However, R is reading them as numeric. Please convert them into factors using a for loop. 
#Please use column 8 to 11 for the loop.

data(mtcars)
mtcars
str(mtcars)

#Answer 1 - Code is wrong, but I'm not sure what's wrong.
for (i in mtcars[, 8:11]) {
  i <- as.factor(i)
}
str(mtcars)

#Answer 2 - From Google
column <- c(8:11)
for (column in column) {    #what does column in column mean?
  mtcars[, column ] <- as.factor(mtcars[, column])
}
str(mtcars)

#You will receive a data set (Dataset 2.1). This data set is a subset of a real data set.  
library (readr)
library(readxl)
setwd("~/Desktop/Grey Campus")
Dataset2_1 <- read_excel("Dataset - 2.1.xlsx")
str(Dataset2_1)
View(Dataset2_1)

##Question 3: Write a function to get the percentage of NAs in each column.

colMeans(is.na(Dataset2_1))

##Question 4: Write a function to get the percentage of NAs in each row.

rowMeans(is.na(Dataset2_1))

##[NEED HELP] Question 5: Write a function to get a summary of numeric columns (use the summary function) such as THC, CO, CO2, and so on in the data set. 
#With the same function, try to generate box plot using base R.

sapply(Dataset2_1, summary)  #Unable to extract out only the numerics columns. 

boxplot(Dataset2_1$THC)
boxplot(Dataset2_1$CO)
boxplot(Dataset2_1$CO2)

# [NEED HELP] Question 6: Write a function to create histograms of numeric columns, such as THC, CO, CO2, and so on in the data set. 
#Use ggpot2 to generate figures.

#Do not know how to write the function to create histograms only for numeric columns. 

hist(Dataset2_1$THC)
hist(Dataset2_1$CO)
hist(Dataset2_1$CO2)

install.packages("ggplot2")
library(ggplot2)
ggplot(Dataset2_1, aes(THC)) + geom_histogram()
ggplot(Dataset2_1, aes(CO)) + geom_histogram()
ggplot(Dataset2_1, aes(CO2)) + geom_histogram()


#You will receive a data set (Dataset 2.2). This data set is a subset of a real data set.

library(tidyverse)
setwd("~/Desktop/Grey Campus")
Dataset2_2 <- read.csv("Dataset - 2.2.csv")
View(Dataset2_2)

#[Need help] sQuestion 7: The data set contains date columns. All of these date columns are untidy. 
#Please create a better formatted data set. The date should be dd/mm/yyyy in the final format. 
#Use columns 2, 3, and 5 only.

date<- c(2,3,5,8)

install.packages("lubridate")
library(lubridate)

for (date in date) {    
  Dataset2_2[, date] <- as.Date(Dataset2_2[, date], tryFormats = ("%d/%m/%y"))
}

#code does not run. Error message: Error in strptime(x, format, tz = "GMT") : invalid 'format' argument

View(Dataset2_2)

#[Need Help] #Question 8: The date of birth column contains months in string format. Please create a tidy data column with months in numeric format. 
#Now your data should be similar to the previous question.

  

#[Need Help] #Question 9: Convert all dates into date format; they are currently in character variable format.



#[Need Help] #Question 10: Create a new column age based on the date of birth column and the first FD column. Date format is necessary to do basic arithmetic.

#Unsure of how to create a new column based on 2 existing columns
Dataset2_2$new <- c(Dataset2_2[,8] - Dataset2_2[,2]) #Failed code.

                   