# Load dependencies
library(rio)
library(ltm)
library(psych)
library(tidyverse)

# Import data
baseline_demographics <- read_csv("data/raw_baseline/baseline_survey.csv") 

daily_diaries <- read_csv("data/cleaned_scored/daily_diaries.csv")

baseline_survey <- import("data/cleaned_scored/baseline_survey.sav") %>%
  as_tibble()

# Identify cisgender men
cis_men <- baseline_demographics %>%
  filter(Gender == "Cisgender man (non-transgender)" | Sex_or == "Straight or heterosexual") %>%
  pull(login_id)

# Remove cisgender men
baseline_demographics <- baseline_demographics %>%
  filter(!(login_id %in% cis_men))

daily_diaries <- daily_diaries %>%
  filter(!(login_id %in% cis_men))

baseline_survey <- baseline_survey %>%
  filter(!(login_id %in% cis_men))

# Create compositional effects
comp_effects <- daily_diaries %>%
  group_by(login_id) %>%
  summarize(
    mean_general_stress = mean(DD_gen_stress_total, na.rm = TRUE),
    mean_minority_stress = mean(DD_minority_stress_total, na.rm = TRUE)
  )

# Join data
corr_matrix_data <- baseline_survey %>%
  select(login_id, EDS_total, ACES_total, IPV_total) %>%
  right_join(daily_diaries) %>%
  left_join(comp_effects) %>%
  select(-link_id, -day, -DD_affect_total, -SM_stress_mixed, -SM_stress_race, 
         -SM_stress_gender, -DD_exp_avoid_total, -DD_ptsd_total,
         -DD_any_drug_use, -DD_polysubstance, -DD_stress_intensity) %>%
  group_by(login_id) %>%
  mutate(
    mean_alcohol_use = mean(DD_alcohol_use),
    mean_aod_use = mean(DD_aod_use)
  ) %>%
  ungroup()

# INTERNAL CONSISTENCY ----------------------------------------------------

# Discrimination 
discrimination <- baseline_demographics %>%
  select(ED_1:ED_10) %>%
  mutate(across(ED_1:ED_10, ~ recode(., "Never" = 1, "Less than once a year" = 2,
                                              "A few times a year" = 3, "A few times a month" = 4,
                                              "At least once a week" = 5, "Almost everyday" = 6)))

cronbach.alpha(data = as.matrix(na.omit(discrimination)))
omega(as.matrix(na.omit(discrimination)), nfactors = 1)

# ACES
aces <- baseline_demographics %>%
  select(starts_with("ACE")) %>%
  mutate(across(ACES_1_1:ACES_2_5, ~ recode(., "Yes" = 1, "No" = 0)))

cronbach.alpha(data = as.matrix(na.omit(aces)))
omega(as.matrix(na.omit(aces)), nfactors = 1)

# IPV
ipv <- baseline_demographics %>%
  select(starts_with("PVV"), starts_with("PVP")) %>%
  mutate(across(`PVV#1_1`:`PVP#1_12`, ~ recode(., "No" = 0, "Yes, but not in the past year" = 1,
                                               "Yes, in the past year" = 2)))

cronbach.alpha(data = as.matrix(na.omit(ipv)))
omega(as.matrix(na.omit(ipv)), nfactors = 1)

# DAILY DIARY STATISTICS --------------------------------------------------

# Retention 
n_start_daily_diary <- daily_diaries %>%
  distinct(login_id) %>% 
  nrow()
n_start_daily_diary

n_start_daily_diary / nrow(baseline_survey)

# Total days
nrow(daily_diaries)

# Average number of days completed
daily_diaries %>%
  count(login_id) %>%
  summarize(
    avg_days = mean(n),
    sd_days = sd(n))

# AOD USE -----------------------------------------------------------------

# How many people engaged in AOD use?
n_aod <- daily_diaries %>%
  filter(DD_aod_use == 1) %>%
  distinct(login_id) %>%
  nrow()
n_aod

n_aod / n_start_daily_diary

# Total days of AOD use
daily_diaries %>%
  filter(DD_aod_use == 1) %>%
  nrow()

# Among people who used AOD, what was the average number of days AOD was used?
daily_diaries %>%
  filter(DD_aod_use == 1) %>%
  count(login_id) %>%
  summarize(
    avg_days_aod = mean(n, na.rm = TRUE),
    sd_days_aod = sd(n, na.rm = TRUE))

# ALCOHOL USE -------------------------------------------------------------

# How many people engaged in alcohol use?
n_alc <- daily_diaries %>%
  filter(DD_alcohol_use == 1) %>%
  distinct(login_id) %>%
  nrow()
n_alc

n_alc / n_start_daily_diary

# Total days of using alcohol
daily_diaries %>%
  filter(DD_alcohol_use == 1) %>%
  nrow()

# Among people who used alcohol, what was the average number of days alcohol was used?
daily_diaries %>%
  filter(DD_alcohol_use == 1) %>%
  count(login_id) %>%
  summarize(
    avg_days_alcohol = mean(n, na.rm = TRUE),
    sd_days_alcohol = sd(n, na.rm = TRUE))

# DEMOGRAPHICS ------------------------------------------------------------

# Age
baseline_demographics %>%
  summarize(
    m_age = mean(Age),
    sd_age = sd(Age)
  )

# Sexual identity
baseline_demographics %>%
  # If more than one identity -> "Queer"
  mutate(
    Sex_or = if_else(str_detect(Sex_or, ","), "Multiple Identities", Sex_or),
    Sex_or = recode(Sex_or, "Lesbian" = "Lesbian / Gay", "Gay" = "Lesbian / Gay", 
                    "Demisexual" = "Other", "Fluid"  = "Other", "Sexually Fluid"  = "Other",
                    "Bisexual" = "Bi/pansexual", "Pansexual" = "Bi/pansexual", "Asexual" = "Other")
  ) %>%
  count(Sex_or) %>%
  arrange(desc(n)) %>%
  mutate(perc = round(n / nrow(baseline_demographics), 3))

# Gender
baseline_demographics %>%
  # If more than one identity -> "Nonbinary"
  mutate(
    Gender = if_else(str_detect(Gender, ","), "Nonbinary", Gender),
    Gender = recode(Gender, "Transgender woman/Trans woman" = "Trans woman/feminine", 
                    "Trans feminine" = "Trans woman/feminine",
                    "Transgender man/Trans man" = "Trans man/masculine",
                    "Trans masculine" = "Trans man/masculine",
                    "Non-binary" = "Nonbinary",
                    "Gender non-conforming" = "Nonbinary",
                    "Enby" = "Nonbinary",
                    "You don’t have an option that describes my gender identity (please specify):" = "Other",
                    "Two-Spirit" = "Other",
                    "Agender" = "Other")
  ) %>%
  count(Gender) %>%
  arrange(desc(n)) %>%
  mutate(perc = round(n / nrow(baseline_demographics), 3))

# Race
baseline_demographics %>%
  # If more than one identity -> "Multiracial"
  mutate(
    Race = if_else(str_detect(Race, ","), "Multiracial", Race),
    Race = recode(Race, "Biracial or Multiracial" = "Multiracial",
                  "Native Hawaiian or other Pacific Islander" = "AAPI",
                  "Asian" = "AAPI",
                  "Middle Eastern" = "Other",
                  "American Indian or Alaska Native" = "Other")
  ) %>%
  count(Race) %>%
  arrange(desc(n)) %>%
  mutate(perc = round(n / nrow(baseline_demographics), 3))

# Income
baseline_demographics %>%
  count(Income) %>%
  arrange(desc(n)) %>%
  mutate(perc = round(n / nrow(baseline_demographics), 3))

# Region
baseline_demographics %>%
  mutate(
    region = if_else(State %in% c("Alaska", "Hawaii", "California", "Oregon", "Washing"), "Pacific", if_else(State %in% c("Arizona", "Colorado", "Idaho", "New Mexico", "Montana", "Utah", "Nevada", "Wyoming"), "Mountain", if_else(State %in% c("New Jersey", "New York", "Pennsylvania", "Vermont", "Rhode Island", "New Hampshire", "Massachusetts", "Maine", "Connecticut"), "Northeast", if_else(State %in% c("Indiana", "Illinois", "Michigan", "Ohio", "Wisconsin", "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota"), "Midwest", "South"))))
  ) %>%
  count(region) %>%
  arrange(desc(n)) %>%
  mutate(perc = round(n / nrow(baseline_demographics), 3))

# CORRELATION MATRIX ------------------------------------------------------

# Between-person correlation matrix
between_cm <- corr_matrix_data %>%
  distinct(login_id, .keep_all = TRUE) %>%
  select(EDS_total, ACES_total, IPV_total, mean_general_stress, mean_minority_stress, 
         any_interpersonal_2week, mean_aod_use, mean_alcohol_use) %>%
  corr.test()
between_cm
between_cm$stars

# Univariate descriptive statistics
corr_matrix_data %>%
  distinct(login_id, .keep_all = TRUE) %>%
  select(EDS_total, ACES_total, IPV_total, mean_general_stress, mean_minority_stress, 
         any_interpersonal_2week, mean_aod_use, mean_alcohol_use) %>%
  describe()

# Within-person correlation matrix
within_cm <- corr_matrix_data %>%
  select(starts_with("DD_")) %>%
  corr.test()
within_cm
within_cm$stars

# Univariate descriptive statistics
corr_matrix_data %>%
  select(starts_with("DD_")) %>%
  describe()
