# ============================================================
# 📘 Master's Thesis: Ecological Footprint
# Author: Davide Garbo

# ---- 0) Packages -------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(janitor)
library(ggplot2)
library(here)        
library(GGally)
library(ggcorrplot)
library(corrplot)
library(maps)
library(gridExtra)
library(glue)

# ---- 1) Load data ------------------------------------------------------
# Put your file at: data/final_dataset2.csv
# If your name differs, change below accordingly
raw <- read_csv(here("data", "raw_dataset.csv")) %>%
  clean_names()

glimpse(raw)
