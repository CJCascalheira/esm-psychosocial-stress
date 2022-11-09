# Load dependencies
library(tidyverse)

# Import data
baseline_demographics <- read_csv("data/raw_baseline/baseline_survey.csv") 

day_01 <- read_csv("data/raw_daily_diaries/day_01_survey.csv") %>%
  mutate(day = as.numeric(day))
day_02 <- read_csv("data/raw_daily_diaries/day_02_survey.csv") %>%
  mutate(day = as.numeric(day))
day_03 <- read_csv("data/raw_daily_diaries/day_03_survey.csv") %>%
  mutate(day = as.numeric(day))
day_04 <- read_csv("data/raw_daily_diaries/day_04_survey.csv") %>%
  mutate(day = as.numeric(day))
day_05 <- read_csv("data/raw_daily_diaries/day_05_survey.csv") %>%
  mutate(day = as.numeric(day))
day_06 <- read_csv("data/raw_daily_diaries/day_06_survey.csv") %>%
  mutate(day = as.numeric(day))
day_07 <- read_csv("data/raw_daily_diaries/day_07_survey.csv") %>%
  mutate(day = as.numeric(day))
day_08 <- read_csv("data/raw_daily_diaries/day_08_survey.csv") %>%
  mutate(day = as.numeric(day))
day_09 <- read_csv("data/raw_daily_diaries/day_09_survey.csv") %>%
  mutate(day = as.numeric(day))
day_10 <- read_csv("data/raw_daily_diaries/day_10_survey.csv") %>%
  mutate(day = as.numeric(day))
day_11 <- read_csv("data/raw_daily_diaries/day_11_survey.csv") %>%
  mutate(day = as.numeric(day))
day_12 <- read_csv("data/raw_daily_diaries/day_12_survey.csv") %>%
  mutate(day = as.numeric(day))
day_13 <- read_csv("data/raw_daily_diaries/day_13_survey.csv") %>%
  mutate(day = as.numeric(day))
day_14 <- read_csv("data/raw_daily_diaries/day_14_survey.csv") %>%
  mutate(day = as.numeric(day))

# Combine the daily surveys
daily_diaries <- bind_rows(day_01, day_02) %>%
  bind_rows(day_03) %>%
  bind_rows(day_04) %>%
  bind_rows(day_05) %>%
  bind_rows(day_06) %>%
  bind_rows(day_07) %>%
  bind_rows(day_08) %>%
  bind_rows(day_09) %>%
  bind_rows(day_10) %>%
  bind_rows(day_11) %>%
  bind_rows(day_12) %>%
  bind_rows(day_13) %>%
  bind_rows(day_14) %>%
  rename(marijuana = DD_SUB_1, prescriptions = DD_SUB_2, club_drugs = DD_SUB_3,
         hallucinogens = DD_SUB_4, cocaine = DD_SUB_5, opiates = DD_SUB_6,
         meth = DD_SUB_7, other_drugs = DD_SUB_8)

# Identify cisgender men
cis_men <- baseline_demographics %>%
  filter(Gender == "Cisgender man (non-transgender)" | Sex_or == "Straight or heterosexual") %>%
  pull(login_id)

daily_diaries <- daily_diaries %>%
  filter(!(login_id %in% cis_men))

# DESCRIPTIVE ANALYSIS ----------------------------------------------------

# How many SMW used alcohol + other drugs on day X
n_alc_and_drug <- daily_diaries %>%
  select(login_id, day, DD_ALC, marijuana:other_drugs) %>%
  mutate(DD_ALC = if_else(DD_ALC >= 1, 1, 0)) %>%
  mutate(across(marijuana:other_drugs, ~ if_else(. == "Yes", 1, 0))) %>%
  mutate(across(marijuana:other_drugs, ~ if_else(is.na(.), 0, .))) %>%
  unite(col = "alc_drug", DD_ALC:other_drugs, sep = "") %>%
  mutate(alc_drug = str_remove_all(alc_drug, "0")) %>%
  filter(alc_drug != "") %>%
  filter(!(alc_drug %in% c("1", "NA"))) %>%
  count(login_id) %>%
  nrow()
n_alc_and_drug

n_alc_and_drug / nrow(count(daily_diaries, login_id))

# Most commonly used drugs other than alcohol
daily_diaries %>%
  select(login_id, marijuana:other_drugs) %>%
  mutate(across(marijuana:other_drugs, ~ if_else(. == "Yes", 1, 0))) %>%
  pivot_longer(cols = marijuana:other_drugs, names_to = "drugs", values_to = "consumed") %>%
  filter(consumed == 1) %>%
  count(login_id, drugs) %>%
  count(drugs) %>%
  arrange(desc(n)) %>%
  mutate(percent = n / nrow(count(daily_diaries, login_id)))
  