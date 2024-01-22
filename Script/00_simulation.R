#### Preamble ####
# Purpose: All data except simluations are from the OpenDataToronto website, here @ https://open.toronto.ca/
# Author: Zhijun Zhong
# Date: 20 Jan 2024
# Contact: Jerryzz.zhong@mail.utoronto.ca
# Data set: From open data Toronto, url @ https://open.toronto.ca/dataset/apartment-building-evaluation/ 

### Setup ###

library(dplyr)
library(opendatatoronto)
library(knitr)
library(janitor)
library(tidyverse)
library(lubridate)

### My simulated Data expect to be ###

# 1. having 1760 rows, since there are 1790 rows in original data with 30 rows of NA values. 
# 2. having number 0 to 3 in columns: Parking_Area, Elevator_Maintenance, Building_Cleanliness, Windows
# 3. having columns: Address, Year_built, Story_Number, Unit_Number, Evaluation_Score, Parking_Area, Elevator_Maintenance, Building_Cleanliness, Windows

n <- 1760

### simulating data ###

set.seed(2345)
buildings_evaluation_sim <- data.frame(
  ## create random address by changing only the street number.
  Address = paste0(seq(100, 100 + n - 1), " Toronto Road"),
  ## create random year number from the earliest in the data set which is 1838 to today which is 2023. 
  Year_Built = sample(1838:2023, n, replace = TRUE),
  ## create random story number from lowest 3 to highest 51.
  Story_Number = sample(3:51, n, replace = TRUE),
  ## create random story number from lowest 10 to highest 719.
  Unit_Number = sample(10:719, n, replace = TRUE),
  ## create random evaluation score from lowest 0 to highest 100.
  Evaluation_Score = sample(0:100, n, replace = TRUE),
  ## create random parking area score from lowest 0 to highest 3.
  Parking_Area = sample(c(0:3), n, replace = TRUE),
  ## create random elevator maintence score from lowest 0 to highest 3.
  Elevator_Maintenance = sample(c(0:3), n, replace = TRUE),
  ## create random building cleanliness score from lowest 0 to highest 3.
  Building_Cleanliness = sample(0:3, n, replace = TRUE),
  ## create random windows score from lowest 0 to highest 3.
  Windows = sample(0:3, n, replace = TRUE)
)

head(buildings_evaluation_sim)

### Write our simulation data as a csv ###

write_csv(
  x = buildings_evaluation_sim,
  file = "Building_Evaluation_Sim_Raw.csv"
)

### Test data ###

# Test 1 Number of rows is 1760

if (nrow(buildings_evaluation_sim) == 1760) {
  print("The data sim has 1760 rows.")
} else {
  print(paste("The data sim has", nrow(buildings_evaluation_sim), "rows, which is not equal to 1760."))
}

# Test 2 Column name
column_names <- names(buildings_evaluation_sim)
print(column_names)

# Test 3 all the columns in Parking_Area, Elevator_Maintenance, Building_Cleanliness, Windows are number between 0 to 3. 

check_values <- function(dataset, column) {
  all(dataset[[column]] %in% 0:3)
}

parking_check <- check_values(buildings_evaluation_sim, "Parking_Area")
elevator_check <- check_values(buildings_evaluation_sim, "Elevator_Maintenance")
cleanliness_check <- check_values(buildings_evaluation_sim, "Building_Cleanliness")
windows_check <- check_values(buildings_evaluation_sim, "Windows")

print(paste("Parking_Area valid:", parking_check))
print(paste("Elevator_Maintenance valid:", elevator_check))
print(paste("Building_Cleanliness valid:", cleanliness_check))
print(paste("Windows valid:", windows_check))

### Draw a graph ###

library(ggplot2)

plot <- ggplot(buildings_evaluation_sim, aes(x = Year_Built, y = Evaluation_Score)) +
  geom_point() +
  labs(x = "Year", y = "Score", title = "Score over Year")

plot

