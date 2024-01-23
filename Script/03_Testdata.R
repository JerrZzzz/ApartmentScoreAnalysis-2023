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

###Read all csv from clean data and simulation data###

file_path <- "/cloud/project/Output/Data/cleaned_Buildingeval_data.csv"
test_c_data <- read_csv(file_path, show_col_types = FALSE)

file_path2 <- "/cloud/project/Input/Data /Building_Evaluation_Sim_Raw.csv"
test_sim_data <-
  read_csv(
    file_path2,
    show_col_types = FALSE
  )

### Test Cleaned Data ###

# Test 1: I expect my decade column to have 16 unique decades 

unique_values <- unique(test_c_data$Decade)
num_unique_values <- length(unique_values)
num_unique_values == 16

# Test 2: I expect to have 16 unique value in average_scores

unique_scores <- unique(test_c_data$Average_Score)
num_unique_scores <- length(unique_scores)
num_unique_scores == 16

# Test 3: I expect to have all the current_building_eval_score to be less than 100 

if (any(Test_raw$score > 100)) {
  print("There are values exceeding 100 in the current_building_eval_score column.")
} else {
  print("All values in the current_building_eval_score column are 100 or less.")
}

### Test Sim Data ###

# Test 1 Number of rows is 1760

if (nrow(test_sim_data) == 1760) {
  print("The data sim has 1760 rows.")
} else {
  print(paste("The data sim has", nrow(test_sim_data), "rows, which is not equal to 1760."))
}

# Test 2 Column name
column_names <- names(test_sim_data)
print(column_names)

# Test 3 all the columns in Parking_Area, Elevator_Maintenance, Building_Cleanliness, Windows are number between 0 to 3. 

check_values <- function(dataset, column) {
  all(dataset[[column]] %in% 0:3)
}

parking_check <- check_values(test_sim_data, "Parking_Area")
elevator_check <- check_values(test_sim_data, "Elevator_Maintenance")
cleanliness_check <- check_values(test_sim_data, "Building_Cleanliness")
windows_check <- check_values(test_sim_data, "Windows")

print(paste("Parking_Area valid:", parking_check))
print(paste("Elevator_Maintenance valid:", elevator_check))
print(paste("Building_Cleanliness valid:", cleanliness_check))
print(paste("Windows valid:", windows_check))
