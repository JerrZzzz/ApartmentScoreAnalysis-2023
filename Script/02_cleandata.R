#### Preamble ####
# Purpose: All data except simluations are from the OpenDataToronto website, here @ https://open.toronto.ca/
# Author: Zhijun Zhong
# Date: 20 Jan 2024
# Contact: Jerryzz.zhong@mail.utoronto.ca
# Data set: From open data Toronto, url @ https://open.toronto.ca/dataset/apartment-building-evaluation/ 


### Setup ###
library(opendatatoronto)
library(knitr)
library(janitor)
library(tidyverse)
library(lubridate)
library(dplyr)

### read the csv file saved in download data ###

be_clean <-
  read_csv(
    "/cloud/project/Input/Data /Building_Evaluation_Raw.csv",
    show_col_types = FALSE
  )

##Clean the column names

be_clean <-
  clean_names(be_clean)

##Filter all the property type to private

be_clean <- be_clean %>% 
  filter(property_type == "PRIVATE")

##Select column we want

be_clean <-
  be_clean |>
  select(year_built, site_address, confirmed_storeys,
         confirmed_units, current_building_eval_score, parking_areas,
         elevator_maintenance, windows, building_cleanliness
  )

### Get rid of Outliers by remove 1% for top scores and 1% of low scores ###
p1 <- quantile(be_clean$current_building_eval_score, 0.01)
p99 <- quantile(be_clean$current_building_eval_score, 0.99)

be_clean <- be_clean %>% 
  filter(current_building_eval_score >= p1 & current_building_eval_score <= p99)

##See all the unique year built we have

be_clean$year_built |>
  unique()

##Drop Na value in year built 

be_cleaned <- na.omit(be_clean, cols = "year_built")

##Create new column called decade

be_cleaned$Decade <- floor(be_cleaned$year_built / 10) * 10

##set N/A for parking areas to 0

be_cleaned$parking_areas[be_cleaned$parking_areas == "N/A"] <- 0

be_cleaned$parking_areas <- as.numeric(be_cleaned$parking_areas)

##Set N/A for elevator Maintenance to 0

be_cleaned$elevator_maintenance[be_cleaned$elevator_maintenance == "N/A"] <- 0

be_cleaned$elevator_maintenance <- as.numeric(be_cleaned$elevator_maintenance)

##Set N/A for building_cleanliness to 0

be_cleaned$building_cleanliness[be_cleaned$building_cleanliness == "N/A"] <- 0

be_cleaned$building_cleanliness <- as.numeric(be_cleaned$building_cleanliness)

##Find average score for each decade 

average_scores <- be_cleaned %>%
  group_by(Decade) %>%
  summarize(Average_Score = mean(current_building_eval_score, na.rm = TRUE))

be_cleaned <- be_cleaned %>%
  left_join(average_scores, by = "Decade")

### Rename column 

be_cleaned <- be_cleaned %>%
  rename(score = current_building_eval_score, 
         year = year_built)

### Test Data ###

# Test 1: I expect my decade column to have 16 unique decades 

unique_values <- unique(be_cleaned$Decade)
num_unique_values <- length(unique_values)
num_unique_values == 16

# Test 2: I expect to have 16 unique value in average_scores

unique_scores <- unique(be_cleaned$Average_Score)
num_unique_scores <- length(unique_scores)
num_unique_scores == 16

# Test 3: I expect to have all the current_building_eval_score to be less than 100 

if (any(be_cleaned$current_building_eval_score > 100)) {
  print("There are values exceeding 100 in the current_building_eval_score column.")
} else {
  print("All values in the current_building_eval_score column are 100 or less.")
}

# find how many obs of all full marks in 2000 decades 

count_pa_in_2000 <- sum(be_cleaned$Decade == 2000 & be_cleaned$parking_areas == 3, na.rm = TRUE)
count_em_in_2000 <- sum(be_cleaned$Decade == 2000 & be_cleaned$elevator_maintenance == 3, na.rm = TRUE)
count_w_in_2000 <- sum(be_cleaned$Decade == 2000 & be_cleaned$windows == 3, na.rm = TRUE)
count_bc_in_2000 <- sum(be_cleaned$Decade == 2000 & be_cleaned$building_cleanliness == 3, na.rm = TRUE)

## calculate percentage for full marks in 2000
f_pa2000 <- count_pa_in_2000/nrow(filter(be_cleaned, Decade == 2000))
f_em2000 <- count_em_in_2000/nrow(filter(be_cleaned, Decade == 2000))
f_w2000 <- count_w_in_2000/nrow(filter(be_cleaned, Decade == 2000))
f_bc2000 <- count_bc_in_2000/nrow(filter(be_cleaned, Decade == 2000))

## find how many obs of all full marks in 1950 decades 
count_pa_in_1950 <- sum(be_cleaned$Decade == 1950 & be_cleaned$parking_areas == 3, na.rm = TRUE)
count_em_in_1950 <- sum(be_cleaned$Decade == 1950 & be_cleaned$elevator_maintenance == 3, na.rm = TRUE)
count_w_in_1950 <- sum(be_cleaned$Decade == 1950 & be_cleaned$windows == 3, na.rm = TRUE)
count_bc_in_1950 <- sum(be_cleaned$Decade == 1950 & be_cleaned$building_cleanliness == 3, na.rm = TRUE)

## calculate percentage for full marks in 1950
f_pa1950 <- count_pa_in_1950/nrow(filter(be_cleaned, Decade == 1950))
f_em1950 <- count_em_in_1950/nrow(filter(be_cleaned, Decade == 1950))
f_w1950 <- count_w_in_1950/nrow(filter(be_cleaned, Decade == 1950))
f_bc1950 <- count_bc_in_1950/nrow(filter(be_cleaned, Decade == 1950))

## create a table of those 8 values 
data_frequency <- data.frame(
  Year = c(rep(2000, 4), rep(1950, 4)),
  Category = c("parking", "elevator", "window", "cleanliness", "parking", "elevator", "window", "cleanliness"),
  Value = c(f_pa2000, f_em2000, f_w2000, f_bc2000, f_pa1950, f_em1950, f_w1950, f_bc1950)
)

## rename columns for those evaluation standards. 
be_cleaned <- be_cleaned %>%
  rename(
    address = site_address,
    storeys = confirmed_storeys,
    units = confirmed_units,
    parkings = parking_areas,
    elevators = elevator_maintenance,
    windows = windows,
    cleanliness = building_cleanliness
  )



##Write the Cleaned data as csv (save)

write_csv(
  x = be_cleaned,
  file = "/cloud/project/Output/Data/cleaned_Buildingeval_data.csv"
)

write_csv(
  x = data_frequency,
  file = "/cloud/project/Output/Data/data_frequency.csv"
)

