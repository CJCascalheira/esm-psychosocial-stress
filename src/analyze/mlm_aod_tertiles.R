# Load dependencies
library(rio)
library(lme4)
library(multilevel)
library(Hmisc)
library(misty)
library(scales)
library(tidyverse)

# Set options for scientific notation
options(scipen = 100)

# Import data
baseline <- import("data/cleaned_scored/baseline_survey.sav") %>%
  as_tibble()

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

baseline <- baseline %>%
  filter(!(login_id %in% cis_men))

# Combine the data
full_data <- left_join(daily_diaries, baseline, by = c("login_id", "link_id")) %>%
  arrange(login_id)

# PREPARING THE DATA ------------------------------------------------------

# Select the demographic variables to merge with other data sets
baseline_demographics1 <- baseline_demographics %>%
  select(login_id, Sex, Gender, Sex_or, Race, Income) %>%
  # If more than one identity -> "Nonbinary"
  mutate(
    Gender = if_else(str_detect(Gender, ","), "Nonbinary", Gender),
    Gender = recode(Gender, "Transgender woman/Trans woman" = "Binary",
                    "Trans feminine" = "Binary",
                    "Transgender man/Trans man" = "Binary",
                    "Trans masculine" = "Binary",
                    "Non-binary" = "Nonbinary",
                    "Gender non-conforming" = "Nonbinary",
                    "Enby" = "Nonbinary",
                    "You don’t have an option that describes my gender identity (please specify):" = "Other",
                    "Two-Spirit" = "Nonbinary",
                    "Agender" = "Other",
                    "Cisgender woman (non-transgender)" = "Binary",
                    "Genderqueer" = "Other"),
    Gender = if_else(Gender == "Binary", "Binary", "NonbinaryOther")
  ) %>%
  # If more than one identity -> "Multiracial"
  mutate(
    Race = if_else(str_detect(Race, ","), "Multiracial", Race),
    Race = recode(Race, "Biracial or Multiracial" = "Multiracial",
                  "American Indian or Alaska Native" = "Other",
                  "Middle Eastern" = "Other",
                  "Asian" = "AAPI",
                  "Native Hawaiian or other Pacific Islander" = "AAPI",
                  "Black or African American" = "Black",
                  "Latina/Latino/Latinx or Hispanic" = "Latinx")
  ) %>%
  # If more than one identity -> "Queer"
  mutate(
    Sex_or = if_else(str_detect(Sex_or, ","), "Multiple Identities", Sex_or),
    Sex_or = recode(Sex_or, "Lesbian" = "Lesbian / Gay", "Gay" = "Lesbian / Gay", 
                    "Demisexual" = "Other", "Fluid"  = "Plurisexual", "Sexually Fluid"  = "Plurisexual",
                    "Bisexual" = "Plurisexual", "Pansexual" = "Plurisexual", "Asexual" = "Other",
                    "Straight or heterosexual" = "Monosexual")
  ) %>%
  # Create sexual orientation moderator
  mutate(
    sex_orient = if_else(Sex_or %in% c("Lesbian / Gay", "Same-gender loving"), "Monosexual", Sex_or),
    sex_orient = if_else(sex_orient %in% c("Multiple Identities", "Other", "Queer"), "Other", sex_orient)
  ) %>%
  # Set reference group
  mutate(
    Gender = relevel(factor(Gender), ref = "Binary"),
    Sex_or = relevel(factor(Sex_or), ref = "Lesbian / Gay"),
    Race = relevel(factor(Race), ref = "White"),
    Income = relevel(factor(Income), ref = "$25,000-$49,999"),
    sex_orient = relevel(factor(sex_orient), ref = "Monosexual"),
  ) 

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

# Create interpersonal trauma covariate
criterion_a_event <- full_data %>%
  select(login_id, link_id, day, PTE_1_2, PTE_1_3, PTE_1_5) %>%
  mutate(
    criterion_a = if_else(PTE_1_2 == 1 | PTE_1_3 == 1 | PTE_1_5 == 1, 1, 0)
  ) %>%
  select(-starts_with("PTE"))

# Adding and recoding variables
full_data1 <- left_join(full_data, criterion_a_event) %>%
  # Remove some demographic variables
  select(-Sex, -starts_with("Sex_or"), -starts_with("Gender"), -starts_with("Race"), -Income) %>%
  left_join(baseline_demographics1) %>%
  # Create dichotomous demo variables
  mutate(
    is_poc = if_else(Race != "White", 1, 0),
    above_25k = if_else(Income %in% c("Under $9,000", "$10,000-$24,999"), 1, 0)
  ) %>%
  # Add compositional effects
  left_join(comp_effects) %>%
  # Center day at 0
  mutate(day = day - 1) %>%
  # Rescale values
  mutate(
    grand_age = rescale(grand_age, to = c(0, 1)),
    EDS_total = rescale(EDS_total, to = c(0, 1)),
    ACES_total = rescale(ACES_total, to = c(0, 1)),
    IPV_total = rescale(IPV_total, to = c(0, 1)),
    day = rescale(day, to = c(0, 1)),
    DD_gen_stress_total = rescale(DD_gen_stress_total, to = c(0, 1)),
    DD_minority_stress_total = rescale(DD_minority_stress_total, to = c(0, 1)),
    mean_general_stress = rescale(mean_general_stress, to = c(0, 1)),
    mean_minority_stress = rescale(mean_minority_stress, to = c(0, 1))
  ) %>%
  # Lag of dependent variable
  mutate(
    lag_DD_aod_use = Lag(DD_aod_use, -1),
    lag_DD_aod_use = rescale(lag_DD_aod_use, to = c(0, 1))
  ) %>%
  # https://stackoverflow.com/questions/62574146/how-to-create-tertile-in-r
  # Create tertiles for each lifetime stressor
  mutate(
    EDS_tertile = as.factor(ntile(EDS_total, 3)),
    ACES_tertile = as.factor(ntile(ACES_total, 3)),
    IPV_tertile = as.factor(ntile(IPV_total, 3))
  )

# BIVARIATE CORRELATION MATRIX --------------------------------------------

# Aggregate daily AOD
mean_drug_df <- full_data1 %>%
  group_by(login_id) %>%
  summarize(mean_aod = mean(DD_aod_use))

# Select the main variables: between-person (baseline, compositional)
corr_df_between <- full_data1 %>%
  select(login_id, mean_general_stress, mean_minority_stress, EDS_total, ACES_total, 
         IPV_total, IH_mean, TR_CEN_mean) %>%
  distinct(login_id, .keep_all = TRUE) %>%
  left_join(mean_drug_df)

# Select the main variables: within-person level (daily)
corr_df_within <- full_data1 %>%
  select(login_id, DD_aod_use, DD_gen_stress_total, DD_minority_stress_total)

# Between person correlations
rcorr(as.matrix(corr_df_between[, -1]))

# Within-person correlations
rcorr(as.matrix(corr_df_within[, -1]))

# NULL MODEL --------------------------------------------------------------

# Specify the null model
null_model <- glmer(
  DD_aod_use ~ 1 + 
    (1|login_id), # Random effect of person because time varies within person
  family = "binomial", 
  data = full_data1,
  nAGQ = 5, # number of quadratures
  glmerControl(optimizer="bobyqa")
)
summary(null_model)

# ICC
null_model_aov <- aov(DD_aod_use ~ as.factor(login_id), full_data1)
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
  DD_aod_use ~ 1 + day +
    (1|login_id),
  family = "binomial", 
  data = full_data1,
  nAGQ = 1, # Laplace approximation
  glmerControl(optimizer="bobyqa")
)

# No RE for time variable
day_yes_re <- glmer(
  DD_aod_use ~ 1 + day +
    (1 + day|login_id),
  family = "binomial", 
  data = full_data1,
  nAGQ = 1, # Laplace approximation
  glmerControl(optimizer="bobyqa")
)

# Confidence interval for day
confint(day_yes_re, parm = "theta_", method = "profile", oldNames = F) 

# Results indicate that day's CI includes 0, so there is no significant variance
# in time. That is, the time varying effect of AOD use ("growth rate in 
# AOD use") does not differ between participants.

# Should a RE for time be included in the model?
anova(day_no_re, day_yes_re)

# No evidence for including day as an L1 RE. No evidence of linear trend, but 
# including the time variable is necessary according to Finch et al. (2019)

# LAG OF DV? ---------------------------------------------------------------

# Add lag of DV to control for AOD use on previous day
lag_dv_model1 <- glmer(
  DD_aod_use ~ 1 + day + lag_DD_aod_use +
    (1|login_id),
  family = "binomial", 
  data = full_data1,
  nAGQ = 1, # Laplace approximation
  glmerControl(optimizer="bobyqa")
)
summary(lag_dv_model1)

# Add lag of DV to control for AOD use on previous day
lag_dv_model2 <- glmer(
  DD_aod_use ~ 1 + day + lag_DD_aod_use +
    (1 + lag_DD_aod_use|login_id),
  family = "binomial", 
  data = full_data1,
  nAGQ = 1, # Laplace approximation
  glmerControl(optimizer="bobyqa")
)
summary(lag_dv_model2)

# Should we include a random effect for yesterday's daily AOD use?
anova(lag_dv_model1, lag_dv_model2)

# No, do not let the lag of daily AOD use vary between person because no
# significant improvement in fit based on LR test

# LIFETIME STRESSORS ------------------------------------------------------

# 1) Distal Stressors -----------------------------------------------------

# ...1.1) EDS -----------------------------------------------------------

# Model with Level 2 lifetime distal stressor
l2_lifetime_eds <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age +
    # Add distal stressor
    EDS_tertile +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_lifetime_eds)

# Compute the confidence intervals for the FE
CI <- confint(l2_lifetime_eds, parm = "beta_", method = "Wald", level = .95)

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(l2_lifetime_eds), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds and odds ratios
print(ORwCI, digits = 5)

# ...1.2) ACES -----------------------------------------------------------

# Model with Level 2 lifetime distal stressor
l2_lifetime_aces <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add distal stressor
    ACES_tertile +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_lifetime_aces)

# Compute the confidence intervals for the FE
CI <- confint(l2_lifetime_aces, parm = "beta_", method = "Wald", level = .95)

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(l2_lifetime_aces), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds and odds ratios
print(ORwCI, digits = 5)

# ...1.3) IPV -----------------------------------------------------------

# Model with Level 2 lifetime distal stressor
l2_lifetime_ipv <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add distal stressor
    IPV_tertile +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_lifetime_ipv)

# Compute the confidence intervals for the FE
CI <- confint(l2_lifetime_ipv, parm = "beta_", method = "Wald", level = .95)

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(l2_lifetime_ipv), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds and odds ratios
print(ORwCI, digits = 5)

# ...1.4) Correct p Values -------------------------------------------------

# get the p values of interest
eds_p <- as.data.frame(summary(l2_lifetime_eds)$coefficients[c(7, 8), 4])
aces_p <- as.data.frame(summary(l2_lifetime_aces)$coefficients[c(7, 8), 4])
ipv_p <- as.data.frame(summary(l2_lifetime_ipv)$coefficients[c(7, 8), 4])

# Change column names
names(eds_p) <- "p_value"
names(aces_p) <- "p_value"
names(ipv_p) <- "p_value"

# Bind the rows
all_p_values <- bind_rows(eds_p, aces_p) %>%
  bind_rows(ipv_p)
all_p_values

# Benjamini-Hochberg procedures
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/p.adjust.html
p.adjust(p = all_p_values$p_value,
         method = "BH")

# 2) Stressor Interactions ---------------------------------------------------

# ...2.1) EDS ----------------------------------------------------------------

# Discrimination * gender
l2_eds_gender <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add distal stressor
    EDS_tertile*Gender +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_eds_gender)

# Gender is NOT significant

# Discrimination * sexual orientation
l2_eds_sex_or <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add distal stressor
    EDS_tertile*sex_orient +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_eds_sex_or)

# Sexual orientation is NOT significant

# ...2.2) ACES ---------------------------------------------------------------

# ACES * gender
l2_ACES_gender <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add distal stressor
    ACES_tertile*Gender +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_ACES_gender)

# Gender is not significant

# ACES * sexual orientation
l2_ACES_sex_or <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add distal stressor
    ACES_tertile*sex_orient +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_ACES_sex_or)

# Sexual orientation is not significant

# ...2.3) IPV ---------------------------------------------------------------

# IPV * gender
l2_IPV_gender <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add distal stressor
    IPV_tertile*Gender +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_IPV_gender)

# Gender is NOT significant

# IPV * sexual orientation
l2_IPV_sex_or <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add distal stressor
    IPV_tertile*sex_orient +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_IPV_sex_or)

# Sexual orientation is significant
CI <- confint(l2_IPV_sex_or, parm = "beta_", method = "Wald", level = .95)
logodds <- cbind(OR = fixef(l2_IPV_sex_or), CI)
ORwCI <- exp(logodds)
print(ORwCI, digits = 5)

# DAILY STRESSORS ---------------------------------------------------------

# 1) Test Individual L1 Stressors --------------------------------------------

# ...1.1) General Stress CIs AND ORs ---------------------------------------------

# Individual stressor model - General Stressors - no RE in stressor
l1_stressors_gen <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add stressor
    DD_gen_stress_total + 
    # Random intercept and random effect for DV
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_stressors_gen)

# Individual stressor model - General Stressors - with RE in stressor
l1_stressors_gen_re <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add stressor
    DD_gen_stress_total + 
    # Random intercept and RE for DV and stressor
    (1 + DD_gen_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_stressors_gen_re)

# Does the model with the RE for stressor fit significantly better?
anova(l1_stressors_gen, l1_stressors_gen_re)

# YES! Therefore, retain model with RE for general perceived stress

# Individual stressor model - General Stressors + compositional effect
l1_gen_comp <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add stressors
    DD_gen_stress_total + mean_general_stress +
    # Random effects
    (1 + DD_gen_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_gen_comp)

# Does the model with the compositional effects fit significantly better?
anova(l1_stressors_gen_re, l1_gen_comp)

# YES! Therefore, retain the model with compositional effects.

# Compute the confidence intervals and aORs
CI <- confint(l1_gen_comp, parm = "beta_", method = "Wald")
logodds <- cbind(OR = fixef(l1_gen_comp), CI)
ORwCI <- exp(logodds)
print(ORwCI, digits=3)

# ...1.2) Minority Stress CIs AND ORs --------------------------------------------

# Individual stressor model - Minority Stressors - no RE for stressor
l1_stressors_minority <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add stressor 
    DD_minority_stress_total + 
    # Random intercept and random effects for DV
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_stressors_minority)

# Individual stressor model - Minority Stressors - with RE for stressor
l1_stressors_minority_re <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add stressor 
    DD_minority_stress_total + 
    # Random effects
    (1 + DD_minority_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_stressors_minority_re)

# Do participants differ in their exposure to minority stressors?
anova(l1_stressors_minority, l1_stressors_minority_re)

# No significant difference between the models, so use the simpler model for the
# sake of parsimony

# Individual stressor model - Minority Stressors - compositional effetcs
l1_minority_comp <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add stressor
    mean_minority_stress + DD_minority_stress_total +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_minority_comp)

# Does the model with the compositional effect fit better?
anova(l1_stressors_minority, l1_minority_comp)

# No, but retain the more complex model to control for compositional effect

# Compute the confidence intervals and aORs
CI <- confint(l1_minority_comp, parm = "beta_", method = "Wald")
logodds <- cbind(OR = fixef(l1_minority_comp), CI)
ORwCI <- exp(logodds)
print(ORwCI, digits=3)

# ...1.3) Trauma Stress CIs AND ORs --------------------------------------------

# Individual stressor model - trauma Stressors
l1_stressors_trauma <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add stressors
    any_interpersonal_2week + 
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_stressors_trauma)

# Compute the confidence intervals and aORs
CI <- confint(l1_stressors_trauma, parm = "beta_", method = "Wald")
logodds <- cbind(OR = fixef(l1_stressors_trauma), CI)
ORwCI <- exp(logodds)
print(ORwCI, digits = 3)

# 2) Correct p Values -----------------------------------------------------

# Get the p values
gen_p <- as.data.frame(summary(l1_gen_comp)$coefficients[c(7, 8), 4])
minority_p <- as.data.frame(summary(l1_minority_comp)$coefficients[c(7, 8), 4])
trauma_p <- as.data.frame(summary(l1_stressors_trauma)$coefficients[7, 4])

# Change column names
names(gen_p) <- "p_value"
names(minority_p) <- "p_value"
names(trauma_p) <- "p_value"

# Bind the rows
all_p_values <- bind_rows(gen_p, minority_p) %>%
  bind_rows(trauma_p)
all_p_values

# Benjamini-Hochberg procedures
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/p.adjust.html
p.adjust(p = all_p_values$p_value,
         method = "BH")

# 3) Demographic Moderators --------------------------------------------------

# ...3.1) General Stress ---------------------------------------------

# General stressors + gender
l1_gen_gender <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add stressors
    DD_gen_stress_total*Gender + mean_general_stress +
    # Random effects
    (1 + DD_gen_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_gen_gender)

# Gender is not significant

# General stressors + sexual orientation
l1_gen_sex_or <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add stressors
    DD_gen_stress_total*sex_orient + mean_general_stress +
    # Random effects
    (1 + DD_gen_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_gen_sex_or)

# Sexual orientation is not significant 

# ...3.2) Minority Stress ------------------------------------------

# Minority Stressors * gender
l1_minority_gender <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add stressor
    DD_minority_stress_total*Gender + mean_minority_stress +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_minority_gender)

# Gender is NOT significant

# Minority Stressors * sexual orientation
l1_minority_sex_or <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add stressor
    DD_minority_stress_total*sex_orient + mean_minority_stress +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_minority_sex_or)

# Sexual orientation is significant
CI <- confint(l1_minority_sex_or, parm = "beta_", method = "Wald")
logodds <- cbind(OR = fixef(l1_minority_sex_or), CI)
ORwCI <- exp(logodds)
print(ORwCI, digits = 3)

# ...3.3) Trauma Stress --------------------------------------------

# Trauma Stressors * gender
l1_trauma_gender <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add stressors
    any_interpersonal_2week*Gender + 
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_trauma_gender)

# Gender is NOT significant

# Trauma Stressors * sexual orientation
l1_trauma_sex_or <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add stressors
    any_interpersonal_2week*sex_orient + 
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_trauma_sex_or)

# Sexual orientation is NOT significant

# SENSITIVITY ANALYSIS ----------------------------------------------------

# ...1) Minority Stress ---------------------------------------------------

# Control perceived stress
l1_sens_m <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add stressor
    DD_gen_stress_total + mean_general_stress + 
    DD_minority_stress_total + mean_minority_stress +
    # Random intercept and random effect for DV
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_sens_m)

# Control perceived stress - with RE
l1_sens_m_re <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add stressor
    DD_gen_stress_total + mean_general_stress + 
    DD_minority_stress_total + mean_minority_stress + 
    # Random intercept and random effect for DV
    (1 + DD_gen_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_sens_m_re)

# Likelihood ratio test
anova(l1_sens_m, l1_sens_m_re)

# No significant difference, so retain parsimonious model
CI <- confint(l1_sens_m, parm = "beta_", method = "Wald")
logodds <- cbind(OR = fixef(l1_sens_m), CI)
ORwCI <- exp(logodds)
print(ORwCI, digits = 3)

# ...2) Traumatic Stress --------------------------------------------------

# Control perceived stress
l1_sens_t <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add stressor
    DD_gen_stress_total + mean_general_stress + 
    any_interpersonal_2week +
    # Random intercept and random effect for DV
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_sens_t)

# Show aOR
CI <- confint(l1_sens_t, parm = "beta_", method = "Wald")
logodds <- cbind(OR = fixef(l1_sens_t), CI)
ORwCI <- exp(logodds)
print(ORwCI, digits = 3)

# ...3) Combined Model ----------------------------------------------------

# Control perceived stress
l1_sens_all <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add stressor
    DD_gen_stress_total + mean_general_stress + 
    DD_minority_stress_total + mean_minority_stress + 
    any_interpersonal_2week +
    # Random intercept and random effect for DV
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_sens_all)

# Control perceived stress
l1_sens_all_re <- glmer(
  # control for previous day's AOD use
  DD_aod_use ~ 1 + day + lag_DD_aod_use + 
    # Add covariates
    is_poc + above_25k + grand_age + 
    # Add stressor
    DD_gen_stress_total + mean_general_stress + 
    DD_minority_stress_total + mean_minority_stress + 
    any_interpersonal_2week +
    # Random intercept and random effect for DV
    (1 + DD_gen_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_sens_all_re)

# Likelihood ratio test
anova(l1_sens_all, l1_sens_all_re)

# Show aOR - retain model without RE for daily perceived stress
CI <- confint(l1_sens_all, parm = "beta_", method = "Wald")
logodds <- cbind(OR = fixef(l1_sens_all), CI)
ORwCI <- exp(logodds)
print(ORwCI, digits = 3)
