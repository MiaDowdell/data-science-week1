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