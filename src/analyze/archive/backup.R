# Load dependencies
library(tidyverse)

# Import data
day_01 <- read_csv("data/raw_daily_diaries/day_01_survey.csv") %>%
  rename(Email = RecipientEmail...14)
day_02 <- read_csv("data/raw_daily_diaries/day_02_survey.csv") %>%
  rename(Email = RecipientEmail...14)
day_03 <- read_csv("data/raw_daily_diaries/day_03_survey.csv") %>%
  rename(Email = RecipientEmail...14)
day_04 <- read_csv("data/raw_daily_diaries/day_04_survey.csv") %>%
  rename(Email = RecipientEmail...14)
day_05 <- read_csv("data/raw_daily_diaries/day_05_survey.csv") %>%
  rename(Email = RecipientEmail...14)
day_06 <- read_csv("data/raw_daily_diaries/day_06_survey.csv") %>%
  rename(Email = RecipientEmail...14)
day_07 <- read_csv("data/raw_daily_diaries/day_07_survey.csv") %>%
  rename(Email = RecipientEmail...14)
day_08 <- read_csv("data/raw_daily_diaries/day_08_survey.csv") %>%
  rename(Email = RecipientEmail...14)
day_09 <- read_csv("data/raw_daily_diaries/day_09_survey.csv") %>%
  rename(Email = RecipientEmail...14)
day_10 <- read_csv("data/raw_daily_diaries/day_10_survey.csv") %>%
  rename(Email = RecipientEmail...14)
day_11 <- read_csv("data/raw_daily_diaries/day_11_survey.csv") %>%
  rename(Email = RecipientEmail...14)
day_12 <- read_csv("data/raw_daily_diaries/day_12_survey.csv") %>%
  rename(Email = RecipientEmail...14)
day_13 <- read_csv("data/raw_daily_diaries/day_13_survey.csv") %>%
  rename(Email = RecipientEmail...14)
day_14 <- read_csv("data/raw_daily_diaries/day_14_survey.csv") %>%
  rename(Email = RecipientEmail...14)

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
  summarize(DD_PAN_total = sum(values, na.rm = TRUE)) %>%
  ungroup()




# Total score for daily affect
day_01_pan <- day_01 %>%
  # Get the variable for joining and scoring
  select(login_id, DD_PAN1, DD_PAN2, DD_PAN3, DD_PAN4) %>%
  # Recode to numeric
  mutate(across(DD_PAN1:DD_PAN4, ~ recode(., "Very slightly or not at all" = 1, 
                                          "A little" = 2, 
                                          "Moderately" = 3,
                                          "Quite a bit" = 4,
                                          "Extremely" = 5))) %>%
  pivot_longer(cols = DD_PAN1:DD_PAN4, names_to = "affect", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_PAN_total = sum(values, na.rm = TRUE))

# Total score for daily affect
day_02_pan <- day_02 %>%
  # Get the variable for joining and scoring
  select(login_id, DD_PAN1, DD_PAN2, DD_PAN3, DD_PAN4) %>%
  # Recode to numeric
  mutate(across(DD_PAN1:DD_PAN4, ~ recode(., "Very slightly or not at all" = 1, 
                                          "A little" = 2, 
                                          "Moderately" = 3,
                                          "Quite a bit" = 4,
                                          "Extremely" = 5))) %>%
  pivot_longer(cols = DD_PAN1:DD_PAN4, names_to = "affect", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_PAN_total = sum(values, na.rm = TRUE))

# Total score for daily affect
day_03_pan <- day_03 %>%
  # Get the variable for joining and scoring
  select(login_id, DD_PAN1, DD_PAN2, DD_PAN3, DD_PAN4) %>%
  # Recode to numeric
  mutate(across(DD_PAN1:DD_PAN4, ~ recode(., "Very slightly or not at all" = 1, 
                                          "A little" = 2, 
                                          "Moderately" = 3,
                                          "Quite a bit" = 4,
                                          "Extremely" = 5))) %>%
  pivot_longer(cols = DD_PAN1:DD_PAN4, names_to = "affect", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_PAN_total = sum(values, na.rm = TRUE))

# Total score for daily affect
day_04_pan <- day_04 %>%
  # Get the variable for joining and scoring
  select(login_id, DD_PAN1, DD_PAN2, DD_PAN3, DD_PAN4) %>%
  # Recode to numeric
  mutate(across(DD_PAN1:DD_PAN4, ~ recode(., "Very slightly or not at all" = 1, 
                                          "A little" = 2, 
                                          "Moderately" = 3,
                                          "Quite a bit" = 4,
                                          "Extremely" = 5))) %>%
  pivot_longer(cols = DD_PAN1:DD_PAN4, names_to = "affect", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_PAN_total = sum(values, na.rm = TRUE))

# Total score for daily affect
day_05_pan <- day_05 %>%
  # Get the variable for joining and scoring
  select(login_id, DD_PAN1, DD_PAN2, DD_PAN3, DD_PAN4) %>%
  # Recode to numeric
  mutate(across(DD_PAN1:DD_PAN4, ~ recode(., "Very slightly or not at all" = 1, 
                                          "A little" = 2, 
                                          "Moderately" = 3,
                                          "Quite a bit" = 4,
                                          "Extremely" = 5))) %>%
  pivot_longer(cols = DD_PAN1:DD_PAN4, names_to = "affect", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_PAN_total = sum(values, na.rm = TRUE))

# Total score for daily affect
day_06_pan <- day_06 %>%
  # Get the variable for joining and scoring
  select(login_id, DD_PAN1, DD_PAN2, DD_PAN3, DD_PAN4) %>%
  # Recode to numeric
  mutate(across(DD_PAN1:DD_PAN4, ~ recode(., "Very slightly or not at all" = 1, 
                                          "A little" = 2, 
                                          "Moderately" = 3,
                                          "Quite a bit" = 4,
                                          "Extremely" = 5))) %>%
  pivot_longer(cols = DD_PAN1:DD_PAN4, names_to = "affect", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_PAN_total = sum(values, na.rm = TRUE))

# Total score for daily affect
day_07_pan <- day_07 %>%
  # Get the variable for joining and scoring
  select(login_id, DD_PAN1, DD_PAN2, DD_PAN3, DD_PAN4) %>%
  # Recode to numeric
  mutate(across(DD_PAN1:DD_PAN4, ~ recode(., "Very slightly or not at all" = 1, 
                                          "A little" = 2, 
                                          "Moderately" = 3,
                                          "Quite a bit" = 4,
                                          "Extremely" = 5))) %>%
  pivot_longer(cols = DD_PAN1:DD_PAN4, names_to = "affect", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_PAN_total = sum(values, na.rm = TRUE))

# Total score for daily affect
day_08_pan <- day_08 %>%
  # Get the variable for joining and scoring
  select(login_id, DD_PAN1, DD_PAN2, DD_PAN3, DD_PAN4) %>%
  # Recode to numeric
  mutate(across(DD_PAN1:DD_PAN4, ~ recode(., "Very slightly or not at all" = 1, 
                                          "A little" = 2, 
                                          "Moderately" = 3,
                                          "Quite a bit" = 4,
                                          "Extremely" = 5))) %>%
  pivot_longer(cols = DD_PAN1:DD_PAN4, names_to = "affect", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_PAN_total = sum(values, na.rm = TRUE))

# Total score for daily affect
day_09_pan <- day_09 %>%
  # Get the variable for joining and scoring
  select(login_id, DD_PAN1, DD_PAN2, DD_PAN3, DD_PAN4) %>%
  # Recode to numeric
  mutate(across(DD_PAN1:DD_PAN4, ~ recode(., "Very slightly or not at all" = 1, 
                                          "A little" = 2, 
                                          "Moderately" = 3,
                                          "Quite a bit" = 4,
                                          "Extremely" = 5))) %>%
  pivot_longer(cols = DD_PAN1:DD_PAN4, names_to = "affect", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_PAN_total = sum(values, na.rm = TRUE))

# Total score for daily affect
day_10_pan <- day_10 %>%
  # Get the variable for joining and scoring
  select(login_id, DD_PAN1, DD_PAN2, DD_PAN3, DD_PAN4) %>%
  # Recode to numeric
  mutate(across(DD_PAN1:DD_PAN4, ~ recode(., "Very slightly or not at all" = 1, 
                                          "A little" = 2, 
                                          "Moderately" = 3,
                                          "Quite a bit" = 4,
                                          "Extremely" = 5))) %>%
  pivot_longer(cols = DD_PAN1:DD_PAN4, names_to = "affect", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_PAN_total = sum(values, na.rm = TRUE))

# Total score for daily affect
day_11_pan <- day_11 %>%
  # Get the variable for joining and scoring
  select(login_id, DD_PAN1, DD_PAN2, DD_PAN3, DD_PAN4) %>%
  # Recode to numeric
  mutate(across(DD_PAN1:DD_PAN4, ~ recode(., "Very slightly or not at all" = 1, 
                                          "A little" = 2, 
                                          "Moderately" = 3,
                                          "Quite a bit" = 4,
                                          "Extremely" = 5))) %>%
  pivot_longer(cols = DD_PAN1:DD_PAN4, names_to = "affect", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_PAN_total = sum(values, na.rm = TRUE))

# Total score for daily affect
day_12_pan <- day_12 %>%
  # Get the variable for joining and scoring
  select(login_id, DD_PAN1, DD_PAN2, DD_PAN3, DD_PAN4) %>%
  # Recode to numeric
  mutate(across(DD_PAN1:DD_PAN4, ~ recode(., "Very slightly or not at all" = 1, 
                                          "A little" = 2, 
                                          "Moderately" = 3,
                                          "Quite a bit" = 4,
                                          "Extremely" = 5))) %>%
  pivot_longer(cols = DD_PAN1:DD_PAN4, names_to = "affect", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_PAN_total = sum(values, na.rm = TRUE))

# Total score for daily affect
day_13_pan <- day_13 %>%
  # Get the variable for joining and scoring
  select(login_id, DD_PAN1, DD_PAN2, DD_PAN3, DD_PAN4) %>%
  # Recode to numeric
  mutate(across(DD_PAN1:DD_PAN4, ~ recode(., "Very slightly or not at all" = 1, 
                                          "A little" = 2, 
                                          "Moderately" = 3,
                                          "Quite a bit" = 4,
                                          "Extremely" = 5))) %>%
  pivot_longer(cols = DD_PAN1:DD_PAN4, names_to = "affect", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_PAN_total = sum(values, na.rm = TRUE))

# Total score for daily affect
day_14_pan <- day_14 %>%
  # Get the variable for joining and scoring
  select(login_id, DD_PAN1, DD_PAN2, DD_PAN3, DD_PAN4) %>%
  # Recode to numeric
  mutate(across(DD_PAN1:DD_PAN4, ~ recode(., "Very slightly or not at all" = 1, 
                                          "A little" = 2, 
                                          "Moderately" = 3,
                                          "Quite a bit" = 4,
                                          "Extremely" = 5))) %>%
  pivot_longer(cols = DD_PAN1:DD_PAN4, names_to = "affect", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_PAN_total = sum(values, na.rm = TRUE))

# DAILY STRESSFUL EVENTS --------------------------------------------------

# Total score for daily general stressors
day_01_dgs <- day_01 %>%
  # Get the variable for joining and scoring
  select(login_id, DGS_1, DGS_2, DGS_3) %>%
  # Recode to numeric
  mutate(across(DGS_1:DGS_3, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DGS_1:DGS_3, names_to = "stressors", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DGS_total = sum(values, na.rm = TRUE))

# Total score for daily general stressors
day_02_dgs <- day_02 %>%
  # Get the variable for joining and scoring
  select(login_id, DGS_1, DGS_2, DGS_3) %>%
  # Recode to numeric
  mutate(across(DGS_1:DGS_3, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DGS_1:DGS_3, names_to = "stressors", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DGS_total = sum(values, na.rm = TRUE))

# Total score for daily general stressors
day_03_dgs <- day_03 %>%
  # Get the variable for joining and scoring
  select(login_id, DGS_1, DGS_2, DGS_3) %>%
  # Recode to numeric
  mutate(across(DGS_1:DGS_3, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DGS_1:DGS_3, names_to = "stressors", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DGS_total = sum(values, na.rm = TRUE))

# Total score for daily general stressors
day_04_dgs <- day_04 %>%
  # Get the variable for joining and scoring
  select(login_id, DGS_1, DGS_2, DGS_3) %>%
  # Recode to numeric
  mutate(across(DGS_1:DGS_3, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DGS_1:DGS_3, names_to = "stressors", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DGS_total = sum(values, na.rm = TRUE))

# Total score for daily general stressors
day_05_dgs <- day_05 %>%
  # Get the variable for joining and scoring
  select(login_id, DGS_1, DGS_2, DGS_3) %>%
  # Recode to numeric
  mutate(across(DGS_1:DGS_3, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DGS_1:DGS_3, names_to = "stressors", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DGS_total = sum(values, na.rm = TRUE))

# Total score for daily general stressors
day_06_dgs <- day_06 %>%
  # Get the variable for joining and scoring
  select(login_id, DGS_1, DGS_2, DGS_3) %>%
  # Recode to numeric
  mutate(across(DGS_1:DGS_3, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DGS_1:DGS_3, names_to = "stressors", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DGS_total = sum(values, na.rm = TRUE))

# Total score for daily general stressors
day_07_dgs <- day_07 %>%
  # Get the variable for joining and scoring
  select(login_id, DGS_1, DGS_2, DGS_3) %>%
  # Recode to numeric
  mutate(across(DGS_1:DGS_3, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DGS_1:DGS_3, names_to = "stressors", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DGS_total = sum(values, na.rm = TRUE))

# Total score for daily general stressors
day_08_dgs <- day_08 %>%
  # Get the variable for joining and scoring
  select(login_id, DGS_1, DGS_2, DGS_3) %>%
  # Recode to numeric
  mutate(across(DGS_1:DGS_3, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DGS_1:DGS_3, names_to = "stressors", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DGS_total = sum(values, na.rm = TRUE))

# Total score for daily general stressors
day_09_dgs <- day_09 %>%
  # Get the variable for joining and scoring
  select(login_id, DGS_1, DGS_2, DGS_3) %>%
  # Recode to numeric
  mutate(across(DGS_1:DGS_3, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DGS_1:DGS_3, names_to = "stressors", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DGS_total = sum(values, na.rm = TRUE))

# Total score for daily general stressors
day_10_dgs <- day_10 %>%
  # Get the variable for joining and scoring
  select(login_id, DGS_1, DGS_2, DGS_3) %>%
  # Recode to numeric
  mutate(across(DGS_1:DGS_3, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DGS_1:DGS_3, names_to = "stressors", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DGS_total = sum(values, na.rm = TRUE))

# Total score for daily general stressors
day_11_dgs <- day_11 %>%
  # Get the variable for joining and scoring
  select(login_id, DGS_1, DGS_2, DGS_3) %>%
  # Recode to numeric
  mutate(across(DGS_1:DGS_3, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DGS_1:DGS_3, names_to = "stressors", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DGS_total = sum(values, na.rm = TRUE))

# Total score for daily general stressors
day_12_dgs <- day_12 %>%
  # Get the variable for joining and scoring
  select(login_id, DGS_1, DGS_2, DGS_3) %>%
  # Recode to numeric
  mutate(across(DGS_1:DGS_3, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DGS_1:DGS_3, names_to = "stressors", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DGS_total = sum(values, na.rm = TRUE))

# Total score for daily general stressors
day_13_dgs <- day_13 %>%
  # Get the variable for joining and scoring
  select(login_id, DGS_1, DGS_2, DGS_3) %>%
  # Recode to numeric
  mutate(across(DGS_1:DGS_3, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DGS_1:DGS_3, names_to = "stressors", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DGS_total = sum(values, na.rm = TRUE))

# Total score for daily general stressors
day_14_dgs <- day_14 %>%
  # Get the variable for joining and scoring
  select(login_id, DGS_1, DGS_2, DGS_3) %>%
  # Recode to numeric
  mutate(across(DGS_1:DGS_3, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DGS_1:DGS_3, names_to = "stressors", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DGS_total = sum(values, na.rm = TRUE))

# DAILY MINORITY STRESS ---------------------------------------------------

# Total score SO-based minority stress
day_01_sm_stress <- day_01 %>%
  select(login_id, SM_Strs_1:SM_Strs_8) %>%
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
  group_by(login_id) %>%
  summarize(SM_stress_total = sum(values, na.rm = TRUE))

# Total score SO-based minority stress
day_02_sm_stress <- day_02 %>%
  select(login_id, SM_Strs_1:SM_Strs_8) %>%
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
  group_by(login_id) %>%
  summarize(SM_stress_total = sum(values, na.rm = TRUE))

# Total score SO-based minority stress
day_03_sm_stress <- day_03 %>%
  select(login_id, SM_Strs_1:SM_Strs_8) %>%
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
  group_by(login_id) %>%
  summarize(SM_stress_total = sum(values, na.rm = TRUE))

# Total score SO-based minority stress
day_04_sm_stress <- day_04 %>%
  select(login_id, SM_Strs_1:SM_Strs_8) %>%
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
  group_by(login_id) %>%
  summarize(SM_stress_total = sum(values, na.rm = TRUE))

# Total score SO-based minority stress
day_05_sm_stress <- day_05 %>%
  select(login_id, SM_Strs_1:SM_Strs_8) %>%
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
  group_by(login_id) %>%
  summarize(SM_stress_total = sum(values, na.rm = TRUE))

# Total score SO-based minority stress
day_06_sm_stress <- day_06 %>%
  select(login_id, SM_Strs_1:SM_Strs_8) %>%
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
  group_by(login_id) %>%
  summarize(SM_stress_total = sum(values, na.rm = TRUE))

# Total score SO-based minority stress
day_07_sm_stress <- day_07 %>%
  select(login_id, SM_Strs_1:SM_Strs_8) %>%
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
  group_by(login_id) %>%
  summarize(SM_stress_total = sum(values, na.rm = TRUE))

# Total score SO-based minority stress
day_08_sm_stress <- day_08 %>%
  select(login_id, SM_Strs_1:SM_Strs_8) %>%
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
  group_by(login_id) %>%
  summarize(SM_stress_total = sum(values, na.rm = TRUE))

# Total score SO-based minority stress
day_09_sm_stress <- day_09 %>%
  select(login_id, SM_Strs_1:SM_Strs_8) %>%
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
  group_by(login_id) %>%
  summarize(SM_stress_total = sum(values, na.rm = TRUE))

# Total score SO-based minority stress
day_10_sm_stress <- day_10 %>%
  select(login_id, SM_Strs_1:SM_Strs_8) %>%
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
  group_by(login_id) %>%
  summarize(SM_stress_total = sum(values, na.rm = TRUE))

# Total score SO-based minority stress
day_11_sm_stress <- day_11 %>%
  select(login_id, SM_Strs_1:SM_Strs_8) %>%
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
  group_by(login_id) %>%
  summarize(SM_stress_total = sum(values, na.rm = TRUE))

# Total score SO-based minority stress
day_12_sm_stress <- day_12 %>%
  select(login_id, SM_Strs_1:SM_Strs_8) %>%
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
  group_by(login_id) %>%
  summarize(SM_stress_total = sum(values, na.rm = TRUE))

# Total score SO-based minority stress
day_13_sm_stress <- day_13 %>%
  select(login_id, SM_Strs_1:SM_Strs_8) %>%
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
  group_by(login_id) %>%
  summarize(SM_stress_total = sum(values, na.rm = TRUE))

# Total score SO-based minority stress
day_14_sm_stress <- day_14 %>%
  select(login_id, SM_Strs_1:SM_Strs_8) %>%
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
  group_by(login_id) %>%
  summarize(SM_stress_total = sum(values, na.rm = TRUE))

# MINORITY STRESS - OTHER IDENTITIES --------------------------------------

day_01

[46] "ID_strs"    "SM_strs2"           
[49] "DD_int_1" 

# DAILY SUBSTANCE USE -----------------------------------------------------

# Daily substance use other than alcohol or tobacco
day_01_any_drug_use <- day_01 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_any_drug_use = if_else(DD_SUB_total >= 1, 1, 0)) %>%
  select(login_id, DD_any_drug_use)

# Daily substance use other than alcohol or tobacco
day_02_any_drug_use <- day_02 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_any_drug_use = if_else(DD_SUB_total >= 1, 1, 0)) %>%
  select(login_id, DD_any_drug_use)

# Daily substance use other than alcohol or tobacco
day_03_any_drug_use <- day_03 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_any_drug_use = if_else(DD_SUB_total >= 1, 1, 0)) %>%
  select(login_id, DD_any_drug_use)

# Daily substance use other than alcohol or tobacco
day_04_any_drug_use <- day_04 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_any_drug_use = if_else(DD_SUB_total >= 1, 1, 0)) %>%
  select(login_id, DD_any_drug_use)

# Daily substance use other than alcohol or tobacco
day_05_any_drug_use <- day_05 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_any_drug_use = if_else(DD_SUB_total >= 1, 1, 0)) %>%
  select(login_id, DD_any_drug_use)

# Daily substance use other than alcohol or tobacco
day_06_any_drug_use <- day_06 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_any_drug_use = if_else(DD_SUB_total >= 1, 1, 0)) %>%
  select(login_id, DD_any_drug_use)

# Daily substance use other than alcohol or tobacco
day_07_any_drug_use <- day_07 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_any_drug_use = if_else(DD_SUB_total >= 1, 1, 0)) %>%
  select(login_id, DD_any_drug_use)

# Daily substance use other than alcohol or tobacco
day_08_any_drug_use <- day_08 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_any_drug_use = if_else(DD_SUB_total >= 1, 1, 0)) %>%
  select(login_id, DD_any_drug_use)

# Daily substance use other than alcohol or tobacco
day_09_any_drug_use <- day_09 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_any_drug_use = if_else(DD_SUB_total >= 1, 1, 0)) %>%
  select(login_id, DD_any_drug_use)

# Daily substance use other than alcohol or tobacco
day_10_any_drug_use <- day_10 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_any_drug_use = if_else(DD_SUB_total >= 1, 1, 0)) %>%
  select(login_id, DD_any_drug_use)

# Daily substance use other than alcohol or tobacco
day_11_any_drug_use <- day_11 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_any_drug_use = if_else(DD_SUB_total >= 1, 1, 0)) %>%
  select(login_id, DD_any_drug_use)

# Daily substance use other than alcohol or tobacco
day_12_any_drug_use <- day_12 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_any_drug_use = if_else(DD_SUB_total >= 1, 1, 0)) %>%
  select(login_id, DD_any_drug_use)

# Daily substance use other than alcohol or tobacco
day_13_any_drug_use <- day_13 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_any_drug_use = if_else(DD_SUB_total >= 1, 1, 0)) %>%
  select(login_id, DD_any_drug_use)

# Daily substance use other than alcohol or tobacco
day_14_any_drug_use <- day_14 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_any_drug_use = if_else(DD_SUB_total >= 1, 1, 0)) %>%
  select(login_id, DD_any_drug_use)

# DAILY POLYSUBSTANCE USE -------------------------------------------------

# Daily substance use other than alcohol or tobacco
day_01_polysubstance <- day_01 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_polysubstance = if_else(DD_SUB_total >= 2, 1, 0)) %>%
  select(login_id, DD_polysubstance)

# Daily substance use other than alcohol or tobacco
day_02_polysubstance <- day_02 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_polysubstance = if_else(DD_SUB_total >= 2, 1, 0)) %>%
  select(login_id, DD_polysubstance)

# Daily substance use other than alcohol or tobacco
day_03_polysubstance <- day_03 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_polysubstance = if_else(DD_SUB_total >= 2, 1, 0)) %>%
  select(login_id, DD_polysubstance)

# Daily substance use other than alcohol or tobacco
day_04_polysubstance <- day_04 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_polysubstance = if_else(DD_SUB_total >= 2, 1, 0)) %>%
  select(login_id, DD_polysubstance)

# Daily substance use other than alcohol or tobacco
day_05_polysubstance <- day_05 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_polysubstance = if_else(DD_SUB_total >= 2, 1, 0)) %>%
  select(login_id, DD_polysubstance)

# Daily substance use other than alcohol or tobacco
day_06_polysubstance <- day_06 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_polysubstance = if_else(DD_SUB_total >= 2, 1, 0)) %>%
  select(login_id, DD_polysubstance)

# Daily substance use other than alcohol or tobacco
day_07_polysubstance <- day_07 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_polysubstance = if_else(DD_SUB_total >= 2, 1, 0)) %>%
  select(login_id, DD_polysubstance)

# Daily substance use other than alcohol or tobacco
day_08_polysubstance <- day_08 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_polysubstance = if_else(DD_SUB_total >= 2, 1, 0)) %>%
  select(login_id, DD_polysubstance)

# Daily substance use other than alcohol or tobacco
day_09_polysubstance <- day_09 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_polysubstance = if_else(DD_SUB_total >= 2, 1, 0)) %>%
  select(login_id, DD_polysubstance)

# Daily substance use other than alcohol or tobacco
day_10_polysubstance <- day_10 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_polysubstance = if_else(DD_SUB_total >= 2, 1, 0)) %>%
  select(login_id, DD_polysubstance)

# Daily substance use other than alcohol or tobacco
day_11_polysubstance <- day_11 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_polysubstance = if_else(DD_SUB_total >= 2, 1, 0)) %>%
  select(login_id, DD_polysubstance)

# Daily substance use other than alcohol or tobacco
day_12_polysubstance <- day_12 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_polysubstance = if_else(DD_SUB_total >= 2, 1, 0)) %>%
  select(login_id, DD_polysubstance)

# Daily substance use other than alcohol or tobacco
day_13_polysubstance <- day_13 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_polysubstance = if_else(DD_SUB_total >= 2, 1, 0)) %>%
  select(login_id, DD_polysubstance)

# Daily substance use other than alcohol or tobacco
day_14_polysubstance <- day_14 %>%
  # Select variables
  select(login_id, starts_with("DD_SUB")) %>%
  select(-ends_with("TEXT")) %>%
  # Recode to numeric
  mutate(across(DD_SUB_1:DD_SUB_8, ~ recode(., "Yes" = 1, "No" = 0))) %>%
  pivot_longer(cols = DD_SUB_1:DD_SUB_8, names_to = "drug_use", values_to = "values") %>%
  group_by(login_id) %>%
  summarize(DD_SUB_total = sum(values, na.rm = TRUE)) %>%
  mutate(DD_polysubstance = if_else(DD_SUB_total >= 2, 1, 0)) %>%
  select(login_id, DD_polysubstance)

# JOIN SCORES VARIABLES ---------------------------------------------------

# Day 01
day_01 <- left_join(day_01, day_01_pan) %>%
  left_join(day_01_dgs) %>%
  left_join(day_01_any_drug_use) %>%
  left_join(day_01_polysubstance)

# Day 02
day_02 <- left_join(day_02, day_02_pan) %>%
  left_join(day_02_dgs) %>%
  left_join(day_02_any_drug_use) %>%
  left_join(day_02_polysubstance)

# Day 03
day_03 <- left_join(day_03, day_03_pan) %>%
  left_join(day_03_dgs) %>%
  left_join(day_03_any_drug_use) %>%
  left_join(day_03_polysubstance)

# Day 04
day_04 <- left_join(day_04, day_04_pan) %>%
  left_join(day_04_dgs) %>%
  left_join(day_04_any_drug_use) %>%
  left_join(day_04_polysubstance)

# Day 05
day_05 <- left_join(day_05, day_05_pan) %>%
  left_join(day_05_dgs) %>%
  left_join(day_05_any_drug_use) %>%
  left_join(day_05_polysubstance)

# Day 06
day_06 <- left_join(day_06, day_06_pan) %>%
  left_join(day_06_dgs) %>%
  left_join(day_06_any_drug_use) %>%
  left_join(day_06_polysubstance)

# Day 07
day_07 <- left_join(day_07, day_07_pan) %>%
  left_join(day_07_dgs) %>%
  left_join(day_07_any_drug_use) %>%
  left_join(day_07_polysubstance)

# Day 08
day_08 <- left_join(day_08, day_08_pan) %>%
  left_join(day_08_dgs) %>%
  left_join(day_08_any_drug_use) %>%
  left_join(day_08_polysubstance)

# Day 09
day_09 <- left_join(day_09, day_09_pan) %>%
  left_join(day_09_dgs) %>%
  left_join(day_09_any_drug_use) %>%
  left_join(day_09_polysubstance)

# Day 10
day_10 <- left_join(day_10, day_10_pan) %>%
  left_join(day_10_dgs) %>%
  left_join(day_10_any_drug_use) %>%
  left_join(day_10_polysubstance)

# Day 11
day_11 <- left_join(day_11, day_11_pan) %>%
  left_join(day_11_dgs) %>%
  left_join(day_11_any_drug_use) %>%
  left_join(day_11_polysubstance)

# Day 12
day_12 <- left_join(day_12, day_12_pan) %>%
  left_join(day_12_dgs) %>%
  left_join(day_12_any_drug_use) %>%
  left_join(day_12_polysubstance)

# Day 13
day_13 <- left_join(day_13, day_13_pan) %>%
  left_join(day_13_dgs) %>%
  left_join(day_13_any_drug_use) %>%
  left_join(day_13_polysubstance)

# Day 14
day_14 <- left_join(day_14, day_14_pan) %>%
  left_join(day_14_dgs) %>%
  left_join(day_14_any_drug_use) %>%
  left_join(day_14_polysubstance)
