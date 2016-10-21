data <- read.csv('math_test_data.csv')
data <- read.csv("MERGED2013_PP.csv")

library(dplyr)




distinct(data, INSTURL)

#Get a list of all the levels of each variable
sums <- lapply(data, FUN = 'nlevels')
#Turn that list into a vector with the names of each variable
new_vec <- unlist(sums)
#Remove the names of the variables from the vector
newer_vec <- unname(new_vec)

#If the value in the list is 1 make it a zero and if it is anything else make it a 1
#In this chunk of code I am removing all of the variables that only have one level to them, and in a sense are useless information
replace_ones <- as.numeric(sub(1, 0, newer_vec))
replace_ones[!replace_ones == 0 ] <- 1
which(replace_ones == 1)
trunk_data <- select(data, which(replace_ones == 1))


sums


sum(sums)

data$married
sum(data$married)
levels(data$married)
one_level <- sums[sums == 1]



data$ACTCM25











