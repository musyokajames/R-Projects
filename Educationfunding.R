#Get the libraries
library(tidyverse)
library(dplyr)
library(readr)
library(janitor)

#Import the data and standardize the column names
ed_stats_data <- read_csv("data/edstats.csv") |> 
  clean_names()

#Data wrangling
problems(ed_stats_data)
View(ed_stats_data)
head(ed_stats_data)
tail(ed_stats_data)
nrow(ed_stats_data)

#Removing unwanted rows
ed_stats_data |> 
  slice_head(n = 125)
#Data Cleaning
ed_stats_data <- ed_stats_data |> 
  select(-time_code) |> 
  filter(row_number() < nrow(ed_stats_data) - 2) |> 
  drop_na(country_name)
View(ed_stats_data)

tail(ed_stats_data)
#Get data summary
glimpse(ed_stats_data)
summary(ed_stats_data)
names(ed_stats_data)
summary(ed_stats_data)

#Covert data to numeric form
ed_stats_data <- ed_stats_data |> 
  rename(year = time) |> 
  mutate(across
         (all_of(names(ed_stats_data[4:ncol(ed_stats_data)])), as.numeric))

glimpse(ed_stats_data)
View(ed_stats_data)

#Exploratory data analysis
ed_stats_data |> 
  ggplot(aes(year, literacy_rate_youth_ages_15_24_gender_parity_index_gpi_se_adt_1524_lt_fm_zs))+
  geom_point(aes(shape = country_name, colour = country_name))+
  facet_wrap(~country_name)+
  labs(title = "Literacy rate youth GPI index",
       y = "Literacy rate youth ages 15-24",
       x = "Year")
#The literacy rate GPI index in most of the countries is below one indicating that the literacy rate in boys of that age group is higher than that of girls in the same age group in those countries, we do have some exceptions however like in Uganda and Rwanda where the index is above one which shows that the opposite is true in this countries at certain times.

#Now we check if school enrollment.
ed_stats_data |> 
  ggplot(aes(year, school_enrollment_primary_and_secondary_gross_gender_parity_index_gpi_se_enr_prsc_fm_zs))+
  geom_point(aes(colour = country_name, shape = country_name))+
  facet_wrap(~country_name)+
  labs(title = "Primary and Secondary school enrollment GPI",
       y = "School enrollment index")+
  theme_bw()

#The upward trend in gross enrollment GPI for primary and secondary schools in most countries indicates that the number of girls enrolling is surpassing that of boys. This could reflect targeted government initiatives to promote girls' education, international campaigns supporting the girl child, or shifts in cultural attitudes toward female education.It could also be influenced by demographic factors, such as a higher number of females in the school-age population in these countries.

#Checking the percentage of school going children that are enrolled to preprimary school
ed_stats_data |> 
  ggplot(aes(year, school_enrollment_preprimary_percent_gross_se_pre_enrr))+
  geom_point(aes(colour = country_name, shape = country_name))+
  facet_wrap(~country_name)+
  labs(title = "Pre-primary school enrollment(Gross %)",
       y = "Pre-primary school enrollment")+
  theme_bw()

#The percentage of pre-primary school enrollment ranges from 0% to about 80% across the countries studied. In Kenya, enrollment increases steadily over time, reaching the highest levels. In Burundi, enrollment starts at 0%, remains low for several years, then begins to rise. Uganda shows consistently low enrollment, while Rwanda and Tanzania shows gradual improvement.
# These trends suggest that access to and participation in pre-primary education remains limited in some countries, potentially due to factors such as inadequate infrastructure, low prioritization of early childhood education, or socio-economic barriers. Low enrollment rates in pre-primary do not necessarily indicate low numbers of school-age children, but rather that many eligible children are not yet participating in early education  


#Plot how expenditure changes over time
ed_stats_data |> 
  ggplot(aes(year, expenditure_on_primary_education_percent_of_government_expenditure_on_education_se_xpd_prim_zs))+
  geom_col(aes(fill = country_name))+
  facet_wrap(~country_name)+
  labs(title = "Government expenditure on education for primary education",
       x = "Year",
       y = "Expenditure on Primary education")+
  theme_minimal()
  
ed_stats_data |> 
  ggplot(aes(year,expenditure_on_secondary_education_percent_of_government_expenditure_on_education_se_xpd_seco_zs))+
  geom_col(aes(fill = country_name))+
  facet_wrap(~country_name)+
  labs(title = "Government expenditure on education for secondary education",
       x = "Year",
       y = "Expenditure on Secondary education")+
  theme_minimal()
  
#Earlier on more money was given to primary schools but as time progresses there is a shift and in most countries studied the percentage of allocation that is given to secondary school education has increased.

ptratio_data <- ed_stats_data |> 
  select(country_name, year, pupil_teacher_ratio_primary_se_prm_enrl_tc_zs, pupil_teacher_ratio_secondary_se_sec_enrl_tc_zs) |> 
  pivot_longer(cols = c(pupil_teacher_ratio_primary_se_prm_enrl_tc_zs, pupil_teacher_ratio_secondary_se_sec_enrl_tc_zs), names_to = "school_level", values_to = "ratio") 
  
ptratio_data |> 
  ggplot(aes(year, ratio, fill = school_level))+
  geom_col(position = "dodge")+
  facet_wrap(~country_name)+
  scale_fill_manual(
    values = c("pupil_teacher_ratio_primary_se_prm_enrl_tc_zs" = "blue", 
               "pupil_teacher_ratio_secondary_se_sec_enrl_tc_zs" = "red"),
    labels = c("Primary", "Secondary")
  )+
  labs(title = "Pupil Teacher Ratio in schools",
       x = "Year",
       y = "Ratio")+
  theme_classic()

#The pupil teacher ratio is higher in primary schools as compared to secondary schools, this may indicate a higher number of students at primary school level than in secondary school.

#Relationships
#Check enrollment rate with current expenditure
ed_stats_data |> 
  ggplot(aes(adjusted_net_enrollment_rate_primary_percent_of_primary_school_age_children_se_prm_tenr, current_education_expenditure_primary_percent_of_total_expenditure_in_primary_public_institutions_se_xpd_cprm_zs, colour = country_name))+
  geom_point()+
  facet_grid(~country_name)+
  labs(title = "Current Expenditure Vs. Enrollment Rate",
       x = "Enrollment rate for primary schools",
       y = "Current Government Expenditure on primary schools")+
  theme_bw()

#Correlation
#Pearson correlation
names(ed_stats_data)

cor(ed_stats_data$literacy_rate_youth_ages_15_24_gender_parity_index_gpi_se_adt_1524_lt_fm_zs,ed_stats_data$current_education_expenditure_primary_percent_of_total_expenditure_in_primary_public_institutions_se_xpd_cprm_zs, use = "complete.obs")

#>0.04504688
#>Knowing how one variable changes gives us very little info about how the other changes.

cor.test(ed_stats_data$adjusted_net_enrollment_rate_primary_percent_of_primary_school_age_children_se_prm_tenr,ed_stats_data$pupil_teacher_ratio_primary_se_prm_enrl_tc_zs)

#As countries enroll more children in primary school, they may struggle with having enough teachers, hence the pupil-teacher ratio rises.



