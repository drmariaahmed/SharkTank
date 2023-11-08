# Script for sharktank presentation 
# FSU Data Viz Workshop (CanD3)
# 8 Nov 2023

#loading dataset
nat_us <- read_dta("C:/Users/msmar/OneDrive/Desktop/SharkTank/nat2018-2021us.dta")
skim(nat_us)

#loading libraries

library(haven)
library(readr)
library(dplyr)
library(tidyverse)
library(systemfonts)
library(palmerpenguins)
library(gapminder)
library(demography)
library(cansim)
library(WDI)
library(LexisPlotR)

# Themes, Colour Schemes

library(ggthemes)
library(hrbrthemes)
library(see)
library(paletteer)
library(gglgbtq)
library(colorspace)

# Additional packages, geoms, tools

library(lemon)
library(skimr)
library(lattice)
library(ggrepel)
library(ggridges)
library(ggtext)
library(ggdist)
library(LexisPlotR)


# CLEANING INITIAL SINGLE YEAR DATASET IN R
## Does not deal with year - add to group by in pipe

# Cleaning data into new variables-----
nat <- nat_us %>%
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
  select(c("csec", "race", "immigrant", "methpay", "year")) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(year = as.factor(year)) %>% 
  group_by(race, immigrant, methpay, year) %>%    
  mutate(csec_sum = mean(csec, na.rm = TRUE))

skim(nat)

# saving new datafile----
install.packages("data.table")
library(data.table)

write.csv(nat, "nat_cleaned.csv")
