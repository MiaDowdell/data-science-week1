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
mosquito_egg_raw <- read_csv(here("week1/data", "mosquito_egg_data.csv"),
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

mosquito_egg_data_step2 <- mosquito_egg_data|>
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

mosquito_egg_data_step1 <- mosquito_egg_data_step2
  
  # Verify it worked: 
  # [Code to check change happened]

view(mosquito_egg_data_step1)

view(mosquito_egg_data_step2)
  
  # What changed and why it matters: all values changed to lowercase  
  # [2-3 sentences explaining consequences]
  # 
  
  
  # FIX 2: [Biological impossibilities]  ====

# Show the problem: 
# [Code]

# Fix it:
mosquito_egg_data_step3 <- mosquito_egg_data_step2 |>
  
  # YOUR CODE
  
  
  # Verify it worked: 
  # [Code]
  view(mosquito_egg_data)
  
  # What changed and why it matters:
  # [2-3 sentences]
  #
