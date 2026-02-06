# Week 1: Initial Data Exploration ====
# Author: [Mia Dowdell]
# Date: [30/01/2026]

# Load packages ====
library(tidyverse)
library(here)
library(naniar)
library(janitor)
library(skimr)
# Load data ====
mosquito_egg_data <- read_csv(here("week1/data", "mosquito_egg_data.csv"),
                             name_repair = janitor::make_clean_names)

# Basic overview ====
glimpse(mosquito_egg_raw)
summary(mosquito_egg_raw)
skim(mosquito_egg_raw)

# React table====
# view interactive table of data
view(mosquito_egg_raw)


# Counts by site and treatment====

mosquito_egg_raw |> 
  group_by(site, treatment) |> 
  summarise(n = n())

# Observations ====
# Your observations (add as comments below):
# - What biological system is this?
#   Female Mosquito fecundity (how age, body mass influence egg laying and hatching rate)
# - What's being measured?
#   Egg laying and hatching success?
# - How many observations?
#   205 rows, 9 variables 
# - Anything surprising?
#   Body mass value sitting at -93. 
# - Any obvious problems?
#   Lots of N/A values throughout multiple columns, inconsistent dash work and capitalisation


### WEEK 2 - PLAN / FIXES ### 
# Date: 06/02/2026

# FIX 1: [Capitalisation/typos] ====

# Show the problem: 
# [Code to demonstrate issue exists]
mosquito_egg_data|> 
  distinct(treatment)

mosquito_egg_data|> 
  distinct(site)

# Fix it:
mosquito_egg_data_step1 <- mosquito_egg_data|>
  mutate(treatment = case_when(
    treatment == "HIGH_DOSE" ~ "high_dose",
    treatment == "MEDIUM_DOSE" ~ "medium_dose", 
    treatment == "LOW_DOSE" ~ "low_dose",
    treatment == "CONTROL" ~ "control", 
    treatment == "High_dose" ~ "high_dose", 
    treatment == "Medium_dose" ~ "medium_dose",
    treatment == "Low_dose" ~ "low_dose",
    treatment == "Control" ~ "control",
    .default = as.character(treatment)
  ))

mosquito_egg_data_step2 <- mosquito_egg_data_step1|>
  mutate(site = case_when(
    site == "Site_A" ~ "site_a",
    site == "Site A" ~ "site_a",
    site == "Site-A" ~ "site_a",
    site == "Site_B" ~ "site_b",
    site == "Site B" ~ "site_b",
    site == "Site-B" ~ "site_b",
    site == "Site_C" ~ "site_c",
    site == "Site C" ~ "site_c",
    site == "Site-C" ~ "site_C",
    .default = as.character(site)
  ))
  
  # Verify it worked: 
  # [Code to check change happened]

view(mosquito_egg_data_step2)
  
  # What changed and why it matters: 
  # [2-3 sentences explaining consequences]
  # All values within treatment now read as "low_dose, medium_dose and high_dose" as with site - "site_a, site_b, and site_c
    # this is important as this groups the data into just three groups within its column instead of multiple due to spelling/typo errors.
    # meaning that data now sits where it needs to, making visualisation in graphs/plots much easier 



# FIX 2: [Biological impossibilities]  ====

# Show the problem: 
# [Code]

mosquito_egg_data|>
  filter(body_mass_mg <= 0)

# Fix it:
# YOUR CODE
mosquito_flagged <- mosquito_egg_data_step2|>
  mutate(
    flag_impossible = case_when(
      body_mass_mg <= 0 ~ "negative_or_zero_mass", 
      TRUE ~ NA_character_
    ),
    flag_implausible = case_when(
      body_mass_mg < -93.0 ~ "impossible_weight",
      body_mass_mg < -87.2 ~ "impossible_weight",
      body_mass_mg < -56.8 ~ "impossible_weight",
      TRUE ~ NA_character_
    ), 
    flag_id_weight = case_when(
      female_id == "184" & body_mass_mg < -93.0 ~ "184_impossible_weight",  #Only flagging 98?
      female_id == "109" & body_mass_mg < -87.2 ~ "109_impossible_weight",
      female_id == "98" & body_mass_mg < -56.8 ~ "98_impossible_weight",
      TRUE ~ NA_character_
    ),
    any_flag = !is.na(flag_impossible) | !is.na(flag_implausible) |
                !is.na(flag_id_weight)
  )

  # Verify it worked: 
  # [Code]

mosquito_flagged|>
    summarise(
      n_impossible = sum(!is.na(flag_impossible)),
      n_implausible = sum(!is.na(flag_implausible)),
      n_id_weight = sum(!is.na(flag_id_weight)),
      total_flagged = sum(any_flag)
    )

  view(mosquito_flagged)

  # What changed and why it matters: 
  # [2-3 sentences]
  # The negative body weights have now been flagged in the data set as impossible values to go back and review. This could be a simple
  # typo mistake when inputting the data which ca be resolved in the excel sheet or in R (not sure of the function to do this).
  
  
