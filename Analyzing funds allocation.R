# Analyzing Global Health Fund Allocation Patterns Across Diseases and Regions
#Use the Global Fund allocation dataset to explore how funds are distributed across continents, subcontinents, diseases, and over time. Use EDA, visualizations, and summary stats to uncover trends, equity issues, and insights.
#options(scipen = 999) - To avoid scientific numbering

#Load the libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(janitor)

# Get the dataset
allocation_data <- read_csv("https://data-service.theglobalfund.org/file_download/allocations_dataset/CSV") |> 
  janitor::clean_names()

dim(allocation_data)

head(allocation_data)

tail(allocation_data)


#Data wrangling
#Reomove duplicates
allocation_data <- allocation_data |> 
  distinct(geography_name1, allocation_cycle, component, .keep_all = TRUE) |> 
  rename(country_name = geography_name1)

dim(allocation_data)

View(allocation_data)

#Check for NAs
summary(allocation_data)

#The date_time_updated column has all NAs but that is ok because the data has not yet been updated. 

#Create summary Tables
#Allocation by continent
allocation_data |> 
  select(continent1, allocation_amount_reference_rate) |> 
  group_by(continent1) |> 
  summarise(sum = sum(allocation_amount_reference_rate)) 

#I need to get the summaries for the subcontinent and countries and the disease. Writing 1 block of code or each is repetitive so instead I can use a function

summarize_allocation <- function(data, group_var){
  data |> 
    group_by({{group_var}}) |> 
    #Embracing the variable tells dplyr to use the value stored inside the argument, not the argument as the literal variable name
    summarise(
      total_allocation = sum(allocation_amount_reference_rate),
      average = mean(allocation_amount_reference_rate),
      max_value = max(allocation_amount_reference_rate),
      min_value = min(allocation_amount_reference_rate),
      count = n()
    ) 
}


#Allocation by continent(again, to confirm that the result is the same)
allocation_data |> summarize_allocation(continent1)

#Allocation by subcontinent
allocation_data |> summarize_allocation(sub_continent1)

#Allocation by component
allocation_data |> summarize_allocation(component)

#Allocation by country
allocation_data |> summarize_allocation(country_name)


#Visualizations
#Funding by Disease
allocation_data |> summarize_allocation(component) |> 
  ggplot(aes(x = component , y = total_allocation))+
  geom_col()+
  scale_y_continuous(labels = scales::label_comma()) + #add scale formatter to avoid scientific labeling of scales
  coord_flip()+
  theme_classic()+
  labs(title = "Graph of Funding by Disease",
       x = "Disease",
       y =  "Total Allocation")

#Funding by Continent(Treemaps)
library(treemap)

allocation_data1 <- allocation_data |> 
  group_by(continent1, sub_continent1, .groups = "drop") |> 
  summarise(total_allocation = sum(allocation_amount_reference_rate))

treemap(allocation_data1,
        index = c("continent1", "sub_continent1"),
        vSize = "total_allocation",
        title = "Funding allocation by Continent",
        palette = "RdYlBu",
        sortID = "-total_allocation")
 
#Funding Over Cycles for countries
# Explore how funding patterns changed over different allocation cycles.
# Highlight whether newer cycles gave more or less money to the same country or region.
allocation_data2 <- allocation_data |> 
  group_by(country_name, allocation_cycle) |> 
  summarise(total_allocation = sum(allocation_amount_reference_rate)) |> 
  arrange(total_allocation) |> 
  filter(country_name %in% c("Nigeria" , "Mozambique" , "Congo (Democratic Republic)" , "Tanzania (United Republic)" , "Uganda")) 
  
allocation_data2

allocation_data2 |> 
  ggplot(aes(allocation_cycle, total_allocation))+
  geom_point(aes(colour = country_name, shape = country_name))+
  geom_line(aes(group = country_name))+
  scale_y_log10(labels = scales::label_comma())+
  theme_minimal()+
  labs(title = "Funding trends across top countries")

# For the selected countries (Nigeria, Mozambique, DRC, Tanzania, and Uganda) that receive very huge allocations, the total funding allocations have generally increased across allocation cycles, indicating either a growing disease burden, increased program scope, or prioritization in global funding strategies.

#Funding by Country + Disease(Stacked Bars)
allocation_data3 <- allocation_data |> 
  group_by(country_name, component) |> 
  summarise(total_allocation = sum(allocation_amount_reference_rate)) |> 
  arrange(country_name, component) |> 
  filter(country_name %in% c("Nigeria" , "Mozambique" , "Congo (Democratic Republic)" , "Tanzania (United Republic)" , "Uganda")) 

allocation_data3 |> 
  ggplot(aes(x = country_name, y = total_allocation, fill = component))+
  geom_col()+
  theme_bw()+
  scale_y_continuous(labels = scales::label_comma())+
  labs(title = "Total funds per country by disease")

#Function to analyze funding by diseases and region
summarize_funding_by_disease_region <- function(data, disease, region_var, region_value){
  data |>
    group_by(country_name) |> 
    filter(
      component == disease,
      {{region_var}} == region_value 
    ) |> 
    summarise(
      total_allocation = sum(allocation_amount_reference_rate),
      average = mean(allocation_amount_reference_rate),
      max_value = max(allocation_amount_reference_rate),
      min_value = min(allocation_amount_reference_rate),
      count = n_distinct(country_name)
    )
}

allocation_data |> summarize_funding_by_disease_region("Tuberculosis", country_name, "Kenya")

allocation_data |> summarize_funding_by_disease_region("HIV/AIDS", continent1, "Africa")

allocation_data |> summarize_funding_by_disease_region("HIV/AIDS", sub_continent1, "Eastern Africa")

allocation_data |> summarize_funding_by_disease_region("HIV/AIDS", country_name, "Kenya")

allocation_data |> summarize_funding_by_disease_region("HIV/AIDS", country_name, "Uganda")

allocation_data |> summarize_funding_by_disease_region("HIV/AIDS", country_name, "Tanzania (United Republic)")

#Evaluate equity in funding
# Use boxplots to visualize variation in allocation per disease by continent.
allocation_data |> 
  filter(!is.na(continent1), !is.na(sub_continent1)) |> 
  group_by(continent1, component, country_name) |>
  summarise(total_allocation = sum(allocation_amount_reference_rate))|> 
  ggplot(aes(x = component,y = total_allocation))+
  geom_boxplot()+
  facet_wrap(~continent1)+
  scale_y_continuous(labels = scales::label_comma())+
  geom_jitter(alpha = 0.3, width = 0.2)


# Identify countries with disproportionately low or high allocations relative to others in their region.

allocation_data |> 
  filter(!is.na(continent1), !is.na(sub_continent1)) |> 
  group_by(continent1, component, country_name) |> 
  summarise(
    total_allocation = sum(allocation_amount_reference_rate)
  ) |> 
  group_by(continent1, component) |> 
  mutate(
    average = mean(total_allocation),
    sd_allocation = sd(total_allocation),
    z_score = (total_allocation - average) / sd_allocation 
  ) |> 
  filter(z_score >= 1.5 | z_score <= -1.5) |> 
  arrange(desc(z_score)) |> 
  select(country_name, z_score) 



# Which disease got the highest funding in each continent?
  
# Are there regions where one disease is prioritized more than others?

# By Continent
allocation_data |> 
  group_by(continent1, component) |> 
  filter(!is.na(continent1)) |> 
  summarise(total_allocation = sum(allocation_amount_reference_rate)) |> 
  slice_max(total_allocation, n = 1)

#By Subcontinent
allocation_data |> 
  group_by(sub_continent1, component) |> 
  filter(!is.na(sub_continent1)) |> 
  summarise(total_allocation = sum(allocation_amount_reference_rate)) |> 
  slice_max(total_allocation, n = 1)

#HIV/AIDS is generally the disease that is more prioritized in funding as it receives the most funding in most continents and sub continents. Tuberculosis on the other hand is the least funded by continent and subcontinent. This pattern suggests global health priorities focus more on HIV/AIDS, possibly due to its broader impact or visibility, while tuberculosis may be underrepresented despite its burden in many low-income areas.



# Build a function to return a country's full allocation history

# Input: country name.

# Output: All rows for that country sorted by allocation cycle, including visual summaries.

allocation_history <- function(data, country){
  data |> 
    filter(country_name == country) |> 
    select(country_name,component, allocation_cycle, allocation_amount_reference_rate) |> 
    ggplot(aes(x = allocation_cycle, y = allocation_amount_reference_rate, fill = component))+
    geom_col()+
    scale_y_continuous(labels = scales::label_comma())+
    labs(title = paste("A Graph of", country, "allocation history"),
         x = "Allocation Cycle",
         y = "Allocation Amount")+
    theme_classic()
    
} 

allocation_history(allocation_data, "Belize")





