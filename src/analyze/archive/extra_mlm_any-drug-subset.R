# Load dependencies
library(rio)
library(lme4)
library(multilevel)
library(Hmisc)
library(tidyverse)
library(misty)
library(scales)

# Import data
baseline <- import("data/cleaned_scored/baseline_survey.sav") %>%
  as_tibble()

baseline_demographics <- read_csv("data/raw_baseline/baseline_survey.csv")

daily_diaries <- read_csv("data/cleaned_scored/daily_diaries.csv")

# Combine the data
full_data <- left_join(daily_diaries, baseline, by = c("login_id", "link_id")) %>%
  arrange(login_id)

# PREPARING THE DATA ------------------------------------------------------

# Select the demographic variables to merge with other data sets
baseline_demographics1 <- baseline_demographics %>%
  select(login_id, Sex, Gender, Sex_or, Race)

# People who used at least one drug other than alcohol or tobacco
subset_ids <- daily_diaries %>%
  filter(DD_any_drug_use == 1) %>%
  distinct(login_id) %>%
  pull(login_id)

# Grand mean center age
full_data$grand_age <- center(full_data$Age, type = "CGM")

# Create compositional effects
comp_effects <- full_data %>%
  group_by(login_id) %>%
  summarize(
    mean_general_stress = mean(DD_gen_stress_total, na.rm = TRUE),
    mean_minority_stress = mean(DD_minority_stress_total, na.rm = TRUE),
    mean_ptsd = mean(DD_ptsd_total, na.rm = TRUE)
  )

# Adding and recoding variables
full_data1 <- full_data %>%
  # Remove some demographic variables
  select(-Sex, -starts_with("Sex_or"), -starts_with("Gender"), -starts_with("Race")) %>%
  left_join(baseline_demographics1) %>%
  # Create dichotomous demo variables
  mutate(
    is_transgender = if_else(Gender != "Cisgender woman (non-transgender)", 1, 0),
    is_poc = if_else(Race != "White", 1, 0),
    above_25k = if_else(Income %in% c(1, 2), 1, 0)
  ) %>%
  # Add compositional effects
  left_join(comp_effects) %>%
  # Create a weekend variable
  mutate(
    week_day = weekdays(StartDate),
    weekend = if_else(week_day %in% c("Friday", "Saturday", "Sunday"), 1, 0)) %>%
  # Rescale values
  mutate(
    grand_age = rescale(grand_age, to = c(0, 1)),
    EDS_total = rescale(EDS_total, to = c(0, 1)),
    ACES_total = rescale(ACES_total, to = c(0, 1)),
    IPV_total = rescale(IPV_total, to = c(0, 1)),
    SEX_HAR_total = rescale(SEX_HAR_total, to = c(0, 1)),
    day = rescale(day, to = c(0, 1))
  ) %>%
  # Lag of dependent variable
  mutate(
    lag_DD_drug_use = Lag(DD_any_drug_use, -1)
  ) %>%
  # Select only people who used at least one drug during the dail diary
  filter(login_id %in% subset_ids)

# NULL MODEL --------------------------------------------------------------

# Specify the null model
null_model <- glmer(
  DD_any_drug_use ~ 1 + 
    (1|login_id), # Random effect of person because time varies within person
  family = "binomial", 
  data = full_data1,
  nAGQ = 5, # number of quadratures
  glmerControl(optimizer="bobyqa")
)
summary(null_model)

# ICC
null_model_aov <- aov(DD_any_drug_use ~ as.factor(login_id), full_data1)
ICC1(null_model_aov)

#* Compute CIs AND ORs -----------------------------------------------------

# Compute the confidence intervals for the FE
CI <- confint(null_model, parm = "beta_", method = "Wald")

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(null_model), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds of using any drug on a given day 
print(ORwCI, digits=3)

# Confidence intervals for the RE - YES, significant variance in u_oj
confint(null_model, parm = "theta_", method = "profile", oldNames = F) 

# ADD DAY? ----------------------------------------------------------------

# No RE for time variable
day_no_re <- glmer(
  DD_any_drug_use ~ 1 + day +
    (1|login_id),
  family = "binomial", 
  data = full_data1,
  nAGQ = 1, # Laplace approximation
  glmerControl(optimizer="bobyqa")
)

# No RE for time variable
day_yes_re <- glmer(
  DD_any_drug_use ~ 1 + day +
    (1 + day|login_id),
  family = "binomial", 
  data = full_data1,
  nAGQ = 1, # Laplace approximation
  glmerControl(optimizer="bobyqa")
)

# Confidence interval for day
confint(day_yes_re, parm = "theta_", method = "profile", oldNames = F) 

# Results indicate that day's CI includes 0, so there is no significant variance
# in time. That is, the time varying effect of illicit drug use ("growth rate in 
# drug use") does not differ between participants.

# Should a RE for time be included in the model?
anova(day_no_re, day_yes_re)

# No evidence for including day as an L1 RE, but should definitely include day
# at level 2 because this is necessary for longitudinal MLM (Finch et al., 2019)

# LAG OF DV ---------------------------------------------------------------

# Add lag of DV to control for drug use on previous day
lag_dv_model <- glmer(
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use +
    (1|login_id),
  family = "binomial", 
  data = full_data1,
  nAGQ = 1, # Laplace approximation
  glmerControl(optimizer="bobyqa")
)
summary(lag_dv_model)

# ADD L1 STRESSORS ---------------------------------------------------------

# Model with Level 1 daily stressors
l1_stressors_a <- glmer(
  # Specify time component, control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    # L1 within-person stressors
    DD_gen_stress_total + DD_minority_stress_total +
    # Only include a random intercept (participant daily drug use varies across
    # participants when all predictors are held at 0)
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_stressors_a)

l1_stressors_b <- glmer(
  # Specify time component, control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    DD_gen_stress_total + DD_minority_stress_total +
    # Participant illicit drug use, exposure to general stressors, and exposure
    # to minority stressors varies across participants
    (1 + DD_gen_stress_total + DD_minority_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_stressors_b)

# Does the RE model fit better? Is there significant between-person variation
# in stressor exposure?
anova(l1_stressors_a, l1_stressors_b)

# Yes, participants differ in their exposure to daily stressors

# Test Individual L1 Stressors --------------------------------------------

#* General Stress CIs AND ORs ---------------------------------------------

# Individual stressor model - General Stressors
l1_stressors_gen <- glmer(
  # Specify time component, control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    DD_gen_stress_total + 
    # Random intercept and stressor differs across participants
    (1 + DD_gen_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_stressors_gen)

# Compute the confidence intervals for the FE
CI <- confint(l1_stressors_gen, parm = "beta_", method = "Wald")

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(l1_stressors_gen), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds and odds ratios
print(ORwCI, digits=3)

#* Minority Stress CIs AND ORs --------------------------------------------

# Individual stressor model - Minority Stressors
l1_stressors_minority <- glmer(
  # Specify time component, control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    DD_minority_stress_total + 
    # Random intercept and stressor differs across participants
    (1 + DD_minority_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_stressors_minority)

# Compute the confidence intervals for the FE
CI <- confint(l1_stressors_minority, parm = "beta_", method = "Wald")

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(l1_stressors_minority), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds and odds ratios
print(ORwCI, digits=3)

#* PTSD Stress CIs AND ORs ------------------------------------------------

# Individual stressor model - PTSD Stressors
l1_stressors_ptsd <- glmer(
  # Specify time component, control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    DD_ptsd_total + 
    # Random intercept differs across participants, but do not include PTSD
    # because model becomes singular
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_stressors_ptsd)

# Compute the confidence intervals for the FE
CI <- confint(l1_stressors_ptsd, parm = "beta_", method = "Wald")

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(l1_stressors_ptsd), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds and odds ratios
print(ORwCI, digits=3)

# L1 Stressors: Compositional Effects  ------------------------------------

#* General Stress CIs AND ORs ---------------------------------------------

# Individual stressor model - General Stressors
l1_stressors_gen <- glmer(
  # Specify time component, control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    DD_gen_stress_total + mean_general_stress +
    # Random intercept and stressor differs across participants
    (1 + DD_gen_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_stressors_gen)

# Compute the confidence intervals for the FE
CI <- confint(l1_stressors_gen, parm = "beta_", method = "Wald")

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(l1_stressors_gen), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds and odds ratios
print(ORwCI, digits=3)

#* Minority Stress CIs AND ORs --------------------------------------------

# Individual stressor model - Minority Stressors
l1_stressors_minority <- glmer(
  # Specify time component, control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    DD_minority_stress_total + mean_minority_stress +
    # Random intercept and stressor differs across participants
    (1 + DD_minority_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_stressors_minority)

# Compute the confidence intervals for the FE
CI <- confint(l1_stressors_minority, parm = "beta_", method = "Wald")

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(l1_stressors_minority), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds and odds ratios
print(ORwCI, digits=3)

#* PTSD Stress CIs AND ORs ------------------------------------------------

# Individual stressor model - PTSD Stressors
l1_stressors_ptsd <- glmer(
  # Specify time component, control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    DD_ptsd_total + mean_ptsd +
    # Random intercept differs across participants, but do not include PTSD
    # because model becomes singular
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_stressors_ptsd)

# Compute the confidence intervals for the FE
CI <- confint(l1_stressors_ptsd, parm = "beta_", method = "Wald")

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(l1_stressors_ptsd), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds and odds ratios
print(ORwCI, digits=3)

# ALL L1 STRESSORS --------------------------------------------------------

# Model with PTSD, general stressors, minority stressors
l1_stressors_all <- glmer(
  # Specify time component, control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    # Add all stressors
    DD_gen_stress_total + DD_minority_stress_total + DD_ptsd_total + 
    # Compositional effects
    mean_general_stress +
    # Participant illicit drug use, exposure to general stressors, and exposure
    # to minority stressors varies across participants
    (1 + DD_gen_stress_total + DD_minority_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_stressors_all)

#* Compute CIs AND ORs -----------------------------------------------------

# Compute the confidence intervals for the FE
CI <- confint(l1_stressors_all, parm = "beta_", method = "Wald")

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(l1_stressors_all), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds and odds ratios
print(ORwCI, digits = 3)

# L1 STRESSORS + COVARIATES -----------------------------------------------

# Model with Level 1 daily stressors and demographic covariates 
l1_stress_covariates <- glmer(
  # Specify time component, control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    # Stressors
    DD_gen_stress_total + DD_minority_stress_total + DD_ptsd_total +
    # Covariates
    above_25k + grand_age +
    (1 + DD_gen_stress_total + DD_minority_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_stress_covariates)

#* Compute CIs AND ORs -----------------------------------------------------

# Compute the confidence intervals for the FE
CI <- confint(l1_stress_covariates, parm = "beta_", method = "Wald")

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(l1_stress_covariates), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds and odds ratios
print(ORwCI, digits=3)

# LIFETIME STRESSORS ------------------------------------------------------

# Model with Level 2 lifetime stressors
l2_lifetime_stressors <- glmer(
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    EDS_total + ACES_total + IPV_total + SEX_HAR_total +
    # L2 demographic covariates
    above_25k + grand_age +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_lifetime_stressors)

#* Compute CIs AND ORs -----------------------------------------------------

# Compute the confidence intervals for the FE
CI <- confint(l2_lifetime_stressors, parm = "beta_", method = "Wald")

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(l2_lifetime_stressors), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds and odds ratios
print(ORwCI, digits = 5)

# BAD + EXTRA MODELS ------------------------------------------------------

#* Add L1 All Vars --------------------------------------------------------

# Model with Level 1 daily stressors, daily affect, daily avoidance
l1_all_vars_model <- glmer(
  DD_any_drug_use ~ 1 + DD_gen_stress_total + DD_minority_stress_total + 
    DD_affect_total + DD_exp_avoid_total +
    (1 + DD_gen_stress_total + DD_minority_stress_total + DD_affect_total + 
       DD_exp_avoid_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_all_vars_model)

# Model is singular, so do not use this model

#* Just L1 Aff/Avd --------------------------------------------------------

# Model with Level 1 daily affect and daily avoidance
l1_aff_avd_model <- glmer(
  DD_any_drug_use ~ 1 + DD_affect_total + DD_exp_avoid_total +
    (1 + DD_affect_total + DD_exp_avoid_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_aff_avd_model)

#* L1 Affect * Stressors ---------------------------------------------------

# Daily stressors interacting with daily affect
affect_interaction_model <- glmer(
  DD_any_drug_use ~ 1 + DD_minority_stress_total*DD_affect_total + 
    DD_gen_stress_total*DD_affect_total +
    (1 + DD_gen_stress_total + DD_minority_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)

# Model not identified

# Daily stressors interacting with daily affect
affect_interaction_model <- glmer(
  DD_any_drug_use ~ 1 + DD_minority_stress_total*DD_affect_total + 
    DD_gen_stress_total*DD_affect_total +
    (1 + DD_gen_stress_total + DD_minority_stress_total + DD_affect_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)

# Model close to singular

# Daily stressors plus daily affect
stressors_affect_model <- glmer(
  DD_any_drug_use ~ 1 + DD_minority_stress_total + DD_gen_stress_total + 
    DD_affect_total +
    (1 + DD_gen_stress_total + DD_minority_stress_total + DD_affect_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(stressors_affect_model)

# Trouble converging the model

#* Extra: Weekend Model ---------------------------------------------------

# Specify the weekend model
weekend_model <- glmer(
  DD_any_drug_use ~ 1 + weekend +
    (1|login_id),
  family = "binomial", 
  data = full_data1,
  nAGQ = 1, # Lapalce approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(weekend_model)

#* Extra Add L2 Race -------------------------------------------------------

# Add race as a L2 interaction effect
l2_race_model <- glmer(
  DD_any_drug_use ~ 1 + day + DD_minority_stress_total + DD_gen_stress_total + is_poc +
    (1 + DD_gen_stress_total + DD_minority_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_race_model)