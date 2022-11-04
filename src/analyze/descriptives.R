# Load dependencies
library(tidyverse)
library(rio)

# Import data
baseline_demographics <- read_csv("data/raw_baseline/baseline_survey.csv") %>%
  rename(Email = Email...3)

daily_diaries <- read_csv("data/cleaned_scored/daily_diaries.csv")

# Identify cisgender men
cis_men <- baseline_demographics %>%
  filter(Gender == "Cisgender man (non-transgender)") %>%
  pull(login_id)

# Remove cisgender men
baseline_demographics <- baseline_demographics %>%
  filter(!(login_id %in% cis_men))

daily_diaries <- daily_diaries %>%
  filter(!(login_id %in% cis_men))

# AOD USE -----------------------------------------------------------------

# How many people engaged in AOD use?
daily_diaries %>%
  filter(DD_aod_use == 1) %>%
  distinct(login_id) %>%
  nrow()

# Among people who used AOD, what was the average number of days AOD was used?
daily_diaries %>%
  filter(DD_aod_use == 1) %>%
  count(login_id) %>%
  summarize(
    avg_days_aod = mean(n, na.rm = TRUE),
    sd_days_aod = sd(n, na.rm = TRUE))

# ALCOHOL USE -------------------------------------------------------------

# How many people engaged in AOD use?
daily_diaries %>%
  filter(DD_alcohol_use == 1) %>%
  distinct(login_id) %>%
  nrow()

# Among people who used AOD, what was the average number of days AOD was used?
daily_diaries %>%
  filter(DD_alcohol_use == 1) %>%
  count(login_id) %>%
  summarize(
    avg_days_alcohol = mean(n, na.rm = TRUE),
    sd_days_alcohol = sd(n, na.rm = TRUE))

# DEMOGRAPHICS ------------------------------------------------------------


