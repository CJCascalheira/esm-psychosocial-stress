# Load dependencies
library(tidyverse)

# Import data
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
  select(-starts_with("RecipientEmail"))

# DAILY AFFECT ------------------------------------------------------------

# Total score for daily affect
daily_affect <- daily_diaries %>%
  # Get the variable for joining and scoring
  select(login_id, day, DD_PAN1, DD_PAN2, DD_PAN3, DD_PAN4) %>%
  # Recode to numeric
  mutate(across(DD_PAN1:DD_PAN4, ~ recode(., "Very slightly or not at all" = 1, 
                                          "A little" = 2, 
                                          "Moderately" = 3,
                                          "Quite a bit" = 4,
                                          "Extremely" = 5))) %>%
  pivot_longer(cols = DD_PAN1:DD_PAN4, names_to = "affect", values_to = "values") %>%
  group_by(login_id, day) %>%
  summarize(DD_affect_total = sum(values, na.rm = TRUE)) %>%
  ungroup()

# DAILY STRESSFUL EVENTS --------------------------------------------------

# Total score for daily general stressors
daily_general_stress <- daily_diaries %>%
  # Get the variable for joining and scoring
  select(login_id, day, DGS_1, DGS_2, DGS_3) %>%
  # Recode to numeric
  mutate(across(DGS_1:DGS_3, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DGS_1:DGS_3, names_to = "stressors", values_to = "values") %>%
  group_by(login_id, day) %>%
  summarize(DD_gen_stress_total = sum(values, na.rm = TRUE)) %>%
  ungroup()

# DAILY MINORITY STRESS ---------------------------------------------------

# Total score SO-based minority stress
daily_minority_stress <- daily_diaries %>%
  select(login_id, day, SM_Strs_1:SM_Strs_8) %>%
  mutate(across(SM_Strs_1:SM_Strs_8, ~ recode(., "Not at all" = 0,
                                              `0` = 0,
                                              `1` = 1,
                                              `2` = 2,
                                              "Somewhat" = 3,
                                              `3` = 3,
                                              `4` = 4,
                                              `5` = 5,
                                              "A lot" = 6,
                                              `6` = 6))) %>%
  pivot_longer(cols = SM_Strs_1:SM_Strs_8, names_to = "minority_stress", values_to = "values") %>%
  group_by(login_id, day) %>%
  summarize(DD_minority_stress_total = sum(values, na.rm = TRUE)) %>%
  ungroup()

# MINORITY STRESS - OTHER IDENTITIES --------------------------------------

# Compute new variables
daily_race_gender_stress <- daily_diaries %>%
  select(login_id, day, ID_strs) %>%
  mutate(
    SM_stress_mixed = if_else(ID_strs %in% c("Race or ethnicity,Gender expression",
                                             "Race or ethnicity,Gender identity,Gender expression",
                                             "Race or ethnicity,Gender identity"), 1, 0),
    SM_stress_race = if_else(ID_strs == "Race or ethnicity", 1, 0),
    SM_stress_gender = if_else(ID_strs %in% c("Gender identity",
                                              "Gender expression",
                                              "Gender identity,Gender expression"), 1, 0))

# Compute intensity variable
daily_stress_intensity <- daily_diaries %>%
  select(login_id, day, DD_stress_intensity = DD_int_1) %>%
  mutate(DD_stress_intensity = recode(DD_stress_intensity, `12` = 6, `11` = 5,
                                      `10` = 4, `9` = 3, `8` = 2, `7` = 1,
                                      "Strongly disagree" = 1, "Strongly agree" = 6))

# DAILY SUBSTANCE USE -----------------------------------------------------

# Daily substance use other than alcohol or tobacco
daily_any_drug_use <- daily_diaries %>%
  # Select variables
  select(login_id, day, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id, day) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_any_drug_use = if_else(DD_SUB_total >= 1, 1, 0)) %>%
  select(login_id, day, DD_any_drug_use) %>%
  ungroup()

# DAILY POLYSUBSTANCE USE -------------------------------------------------

# Daily substance use other than alcohol or tobacco
daily_polysubstance <- daily_diaries %>%
  # Select variables
  select(login_id, day, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id, day) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_polysubstance = if_else(DD_SUB_total >= 2, 1, 0)) %>%
  select(login_id, day, DD_polysubstance) %>%
  ungroup()

# DAILY ALCOHOL ANY OTHER DRUG USE ----------------------------------------

# Daily alcohol and any other drug (AOD) use
daily_aod_use <- daily_diaries %>%
  # Select variables
  select(login_id, day, DD_ALC, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode other drug use to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  # Recode alcohol to binary
  mutate(DD_ALC = if_else(DD_ALC > 0, 1, 0)) %>%
  pivot_longer(cols = DD_ALC:DD_SUB_8, names_to = "aod", values_to = "values") %>%
  group_by(login_id, day) %>%
  summarize(aod_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_aod_use = if_else(aod_total >= 1, 1, 0)) %>%
  select(login_id, day, DD_aod_use) %>%
  ungroup()

# DAILY ALCOHOL USE -------------------------------------------------------

# Daily alcohol and any other drug (AOD) use
daily_alcohol_use <- daily_diaries %>%
  # Select variables
  select(login_id, day, DD_ALC) %>%
  # Recode alcohol to binary
  mutate(DD_ALC = if_else(DD_ALC > 0, 1, 0)) %>%
  group_by(login_id, day) %>%
  summarize(alcohol_total = sum(DD_ALC, na.rm = TRUE)) %>%
  mutate(DD_alcohol_use = if_else(alcohol_total >= 1, 1, 0)) %>%
  select(login_id, day, DD_alcohol_use) %>%
  ungroup()

# DAILY EXP AVOIDANCE -----------------------------------------------------

# Total score
daily_avoidance <- daily_diaries %>%
  select(login_id, day, starts_with("DD_AVD")) %>%
  pivot_longer(cols = DD_AVD_1:DD_AVD_4, names_to = "exp_avoid", values_to = "values") %>%
  group_by(login_id, day) %>%
  summarize(DD_exp_avoid_total = sum(values, na.rm = TRUE)) %>%
  ungroup()

# DAILY PTSD --------------------------------------------------------------

# Total score
daily_ptsd <- daily_diaries %>%
  select(login_id, day, starts_with("DD_PTSD")) %>%
  mutate(across(DD_PTSD_1:DD_PTSD_8, ~ recode(., "Not at all" = 0,
                                              "A little bit" = 1,
                                              "Moderately" = 2,
                                              "Quite a bit" = 3,
                                              "Extremely" = 4))) %>%
  pivot_longer(DD_PTSD_1:DD_PTSD_8, names_to = "ptsd", values_to = "values") %>%
  group_by(login_id, day) %>%
  summarize(DD_ptsd_total = sum(values, na.rm = TRUE)) %>%
  ungroup()

# COMPOSITIONAL TRAUMA STRESSOR -------------------------------------------

# Compositional effect: any traumatic event over last two weeks
participants_2week_trauma <- daily_diaries %>%
  select(login_id, day, DD_PTE) %>%
  mutate(
    any_interpersonal_2week = if_else(str_detect(DD_PTE, 
                                                 regex("assault", ignore_case = TRUE)), 1, 0)
  ) %>%
  filter(any_interpersonal_2week == 1) %>%
  distinct(login_id) %>%
  pull(login_id)

# create compositional effect
comp_trauma <- daily_diaries %>%
  select(login_id, day) %>%
  mutate(
    any_interpersonal_2week = if_else(login_id %in% participants_2week_trauma, 1, 0)
  )

# How many SMW experience interpersonal trauma over last two weeks?
comp_trauma %>%
  filter(any_interpersonal_2week == 1) %>%
  distinct(login_id)

# Check work
sum(is.na(comp_trauma$any_interpersonal_2week))
comp_trauma %>% count(login_id)
comp_trauma %>% group_by(login_id) %>% count(any_interpersonal_2week)

# JOIN SCORES VARIABLES ---------------------------------------------------

# Join all daily scores
daily_diaries1 <- left_join(daily_diaries, daily_affect, by = c("login_id", "day")) %>%
  left_join(daily_general_stress, by = c("login_id", "day")) %>%
  left_join(daily_minority_stress, by = c("login_id", "day")) %>%
  left_join(daily_any_drug_use, by = c("login_id", "day")) %>%
  left_join(daily_polysubstance, by = c("login_id", "day")) %>%
  left_join(daily_aod_use, by = c("login_id", "day")) %>%
  left_join(daily_alcohol_use, by = c("login_id", "day")) %>%
  left_join(daily_race_gender_stress, by = c("login_id", "day")) %>%
  left_join(daily_stress_intensity, by = c("login_id", "day")) %>%
  left_join(daily_avoidance, by = c("login_id", "day")) %>%
  left_join(daily_ptsd, by = c("login_id", "day")) %>%
  left_join(comp_trauma, by = c("login_id", "day")) %>%
  # Select analytic columns
  select(login_id, link_id, day, DD_affect_total, DD_gen_stress_total, 
         DD_minority_stress_total, SM_stress_mixed, SM_stress_race, SM_stress_gender, 
         DD_stress_intensity, DD_any_drug_use, DD_polysubstance, DD_aod_use, DD_alcohol_use, 
         DD_exp_avoid_total, DD_ptsd_total, any_interpersonal_2week)

# Save the file
write_csv(daily_diaries1, "data/cleaned_scored/daily_diaries.csv")
