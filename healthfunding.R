#Load the libraries and packages
install.packages("stats")
install.packages("ggfortify")

library(readr)
library(data.table)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(stats)
library(ggfortify)


#Import the data and clean the names to have them in snake case
#Use the janitor library
country_eligibility <- read.csv("https://data-service.theglobalfund.org/file_download/eligibility_dataset/CSV") |> 
  janitor::clean_names()

#Data Exploration

names(country_eligibility)
dim(country_eligibility)
attributes(country_eligibility)
head(country_eligibility)
tail(country_eligibility)
#Get the summary and structure
summary(country_eligibility)
str(country_eligibility)

#Handle the missing values
columns_with_missing_values <- c("income_level", "disease_burden", "eligibility_year")

#Replace the missing values with "Unknown"
country_eligibility <- country_eligibility |> 
  mutate(across(
    all_of(columns_with_missing_values),
    ~ifelse(is.na(.), "Unknown", .)
  ))

#View the wrangled dataset
View(country_eligibility)

#Converting the categorical columns into factors
country_eligibility <- country_eligibility |> 
  mutate(across(all_of(c("component", "income_level", "disease_burden", "is_eligible")), as.factor))

str(country_eligibility)

#EDA
#Plot the number of eligible countries per year globally and by continent
#Globally
country_eligibility |> 
  filter(is_eligible == "True") |> 
  group_by(eligibility_year) |> 
  ggplot(aes(x = eligibility_year))+
  geom_bar()

#By continent
country_eligibility |> 
  filter(is_eligible == "True") |> 
  group_by(continent1, eligibility_year) |> 
  ggplot(aes(x = eligibility_year, fill = continent1))+
  geom_bar(position = "dodge")

#Compare Disease burden across income level and continent
country_eligibility |> 
  filter(disease_burden != "Unknown", income_level != "Unknown") |> 
  ggplot(aes(x = continent1, fill = disease_burden)) +
  geom_bar()+
  facet_grid(income_level ~ .)+
  labs(title = "Graph of Disease burden by Income level for each continent",
       x = "Continent")

# Visualize IsEligible by IncomeLevel and Component using bar/mosaic plots.
country_eligibility |> 
  filter(income_level != "Unknown") |> 
  ggplot(aes(x = component, fill = income_level))+
  geom_bar(position = "dodge")+
  facet_wrap(is_eligible ~ .)

#Policy Pattern analysis
#Identify the most common PolicyReference entries linked with eligibility.

country_eligibility |> 
  select(is_eligible, policy_reference) |> 
  group_by(policy_reference, is_eligible) |> 
  count()|> 
  filter(is_eligible == "True")|> 
  arrange(desc(n)) 

# Analyze how FundingStream varies across disease types and income levels.



#Build a logistic regression model
logistic <- glm(is_eligible ~ eligibility_year , data = country_eligibility, family = "binomial")
summary(logistic)

logistic1 <- glm(is_eligible ~ disease_burden, data = country_eligibility, family = "binomial")
summary(logistic1)

logistic2 <- glm(is_eligible ~ income_level, data = country_eligibility, family = "binomial")
summary(logistic2)
#Shows a positive trend over time, the eligibility is more likely in the recent years

logistic4 <- glm(is_eligible ~ eligibility_year + disease_burden + income_level, data = country_eligibility, family = "binomial")
summary(logistic4)

country_eligibility$predicted_probabilities <- predict(logistic4, country_eligibility, type = "response")

country_eligibility |> 
  group_by(eligibility_year, continent1, income_level) |> 
  summarise(mean = mean(predicted_probabilities)) |> 
  ggplot(aes(eligibility_year, mean, color = continent1))+
  geom_point()+
  geom_line()+
  facet_wrap(~income_level)+
  theme_bw()+
  scale_color_brewer()


#Geospatial Mapping 
mapdata <- map_data("world")

mapdata <- mapdata |> 
  rename(geography_name1 = region)

mapdata <- left_join(mapdata, country_eligibility, by = "geography_name1")


mapdata <- mapdata |> filter(!(is.na(is_eligible)))

View(mapdata)

getmap <- function(selected_year){
  mapdata |> 
    filter(selected_year == eligibility_year) |> 
    ggplot(aes(x = long, y = lat, group = group))+
    geom_polygon(aes(fill = is_eligible), color = "black")+
    labs(title = paste("Eligibility Map for", selected_year)) +
    theme_void()
}
getmap(2012)

#Creating animations

p <- mapdata |>
  filter(geography_name1 == "Afghanistan") |> 
  ggplot(aes(x =long , y =lat, group = group))+
  geom_polygon(aes(fill = is_eligible), color = "Black")+
  labs(title = 'Year: {frame_time}')+
  transition_time(eligibility_year)
animate(p)

#Clustering
# K-means Clustering

#Scaling
scaled_data <- country_eligibility |> 
  select(geography_name1,disease_burden,income_level, is_eligible) |> 
  mutate(disease_burden = recode(disease_burden, "Extreme" = 1,"High" = 2,"Low" = 3, "Moderate" = 4, "Not High" = 5, "Severe" = 6, "Unknown" = 7 )) |> 
  mutate(income_level = recode(income_level, "Upper middle income" = 1,  "Low income" = 2, "Lower-Lower middle income" = 3, "Lower middle income" = 4, "Small Island Economy" = 5, "Upper-Lower middle income" = 6, "High income" = 7, "Unknown" = 8)) |> 
  mutate(is_eligible = recode(is_eligible, "True" = 1, "False" = 0)) |> 
  select(-geography_name1) |> 
  scale()

scaled_data  

#WSS Plot
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}
wssplot(scaled_data)

kmeans(scaled_data, 7)


