##WORKING WITH FUNCTIONS.

library(tidyverse)
library(janitor)
library(readr)

hiv_data <- read_csv("R functions/HIVDiseaseBurden.csv") |> 
  clean_names()

hiv_data <- as.data.frame(hiv_data) #Convert the object into a dataframe, if it is not already one
  
View(hiv_data)

class(hiv_data)
summary(hiv_data)
#Quite a nice dataset we have here.

##Using the attached data file “HIVDiseaseBurden.csv”, create a function that does the following:
# •	Takes as input a number “N” representing total funding dollars, which can be modified by the user.
# •	The function should then distribute the funding to all countries in the data file according to their share of people living with HIV (disease burden).
# •	Your function should ensure that no country gets less than $500,000, if a country-disease gets less than $500,000 then it should be increased by taking proportionally from all other countries.
# •	Your function should ensure that no country gets more than 10% of the total funding.  If a country is in breach of this limit it should be reduced to 10% of the total funding and the amount in excess should be redistributed to other countries proportionally with their disease burden.
# •	The output of the function should be a dataframe containing (at least, but not limited to) the following columns: ISO3, DiseaseBurden, AllocatedAmount, where allocated amount is the amount of funding for each country resulting from the distribution described above.

fund_allocation <- function(data, total_funds){
  #calculate the initial allocation of funds
  #Create a new column named allocated_amount for each country's allocation
  data$allocated_amount <- (data$people_living_with_hiv / sum(data$people_living_with_hiv)) * total_funds
  
  
  #Group countries as either underfunded or overfunded
  
  #Underfunded
  underfunded <- data$allocated_amount < 500000
    
  #Calculate amount needed to boost underfunded countries
  needed_amount <- sum(500000 - data$allocated_amount[underfunded])
    
  #Fixing the issue of underfunding by taking proportionally from other countries
  normal <- !underfunded
  total_available <- sum(data$allocated_amount[normal])
  proportion <- data$allocated_amount[normal] / total_available
  data$allocated_amount[normal] <- data$allocated_amount[normal] - proportion * needed_amount
  data$allocated_amount[underfunded] <- 500000
  
  #Fix the issue of overfunded countries
  over_cap <- total_funds * 0.10
  
  overfunded <- data$allocated_amount > over_cap
 
   #Calculate how much excess money the overfunded countries receive
  
  excess_amount <- sum(data$allocated_amount [overfunded] - over_cap)
  
  data$allocated_amount[overfunded] <- over_cap

  
  #Redistributing excess to remaining 
   remaining <- !overfunded
   total_remaining <- sum(data$allocated_amount[remaining])
   proportion_remaining <- data$allocated_amount[remaining] / total_remaining
   data$allocated_amount[remaining] <- data$allocated_amount[remaining] + proportion_remaining * excess_amount
   
   
   result <- result <- data %>% select(iso3, people_living_with_hiv, allocated_amount)
   
   return(result)
   
  
}

fund_allocation(hiv_data, 1000000000)
