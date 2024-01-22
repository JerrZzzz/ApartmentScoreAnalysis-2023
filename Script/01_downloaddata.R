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

### Find Package  ###

packages <- list_packages()
packages

### Get resource  ###

Building_Eval <-
  list_package_resources("4ef82789-e038-44ef-a478-a8f3590c3eb1") |>
  filter(name == 
           "Apartment Building Evaluations 2023 - current.csv") |>
  get_resource()

### See what the data set looks like ###

head(Building_Eval)

### Write the data set into a csv file ###

write_csv(
  x = Building_Eval,
  file = "Building_Evaluation_Raw.csv"
)





