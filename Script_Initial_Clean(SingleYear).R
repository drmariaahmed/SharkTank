# Script for sharktank presentation 
# FSU Data Viz Workshop (CanD3)
# 8 Nov 2023

library(haven)
library(readr)
library(dplyr)
library(tidyverse)


# CLEANING INITIAL SINGLE YEAR DATASET IN R
  ## Does not deal with year - add to group by in pipe

nat <- read_dta("nat2020us_cleanedforR.dta")

# Cleaning data into new variables:
nat <- nat %>%
  mutate(csec = case_when(deliv == 2 ~ 1, # c section is indicated as 1
                           deliv == 1 ~ 0)) %>% # vaginal birth indicated as 0
  mutate(immigrant = case_when(USborn == 1 ~ "US-Born", 
                               USborn == 0 ~ "Immigrant")) %>% 
  mutate(methpay = case_when(methpay == 2 ~ "Medicaid", 
                             methpay == 1 ~ "Private Insurance", 
                             methpay == 4 ~ "Self-Pay")) %>%
  mutate(race = case_when(race5 == 1 ~ "White",
                          race5 == 4 ~ "Asian",
                          race5 == 5 ~ "Other",
                          race5 == 2 ~ "Black",
                          race5 == 3 ~ "Hispanic")) %>%
  select(c("csec", "race", "immigrant", "methpay")) %>%  # ADD YEAR VARIABLE
  mutate_if(is.character, as.factor) %>%    # CHECK CLASS OF YEAR VARIABLE
  group_by(race, immigrant, methpay) %>%    # ADD YEAR VARIABLE
  mutate(csec_sum = mean(csec, na.rm = TRUE))
  
