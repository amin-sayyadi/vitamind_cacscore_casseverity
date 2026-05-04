##########################################################################################################
########################################### Introduction #################################################
##########################################################################################################

library(foreign)       
library(tidyverse)     
library(ResourceSelection) 
library(writexl)       

data <- read.spss("File Address",
                  to.data.frame = TRUE, 
                  use.value.labels = TRUE) 

##########################################################################################################
########################################## Data Preparation ##############################################
##########################################################################################################

### depicting levels
levels(data$CAC_SCORE_LEV_4)
levels(data$CAS_LEV_4)
levels(data$VITAMIN_D_LEV_2)
levels(data$CAC_SCORE_LEV_2)
levels(data$CAS_LEV_2)

### adjusting levels
data$SEX <- relevel(data$SEX, ref = "FEMALE")
data$PHYSICAL_ACTIVITY <- relevel(data$PHYSICAL_ACTIVITY, ref = "YES")
data$VITAMIN_D_LEV_2 <- relevel(data$VITAMIN_D_LEV_2, ref = "SUFFICIENT")
data$CAC_SCORE_LEV_2 <- relevel(data$CAC_SCORE_LEV_2, ref = "NONE")
data$CAS_LEV_2 <- relevel(data$CAS_LEV_2, ref = "NONSIGNIFICANT")

### depicting new levels
levels(data$SEX)
levels(data$PHYSICAL_ACTIVITY)
levels(data$VITAMIN_D_LEV_2)
levels(data$CAC_SCORE_LEV_2)
levels(data$CAS_LEV_2)
levels(data$HYPERTENSION)
levels(data$CIGARETTE_SMOKING)
levels(data$OBESITY)
levels(data$FAMILY_HISTORY)
levels(data$ANXIETY_DISORDER)

##########################################################################################################
############################## Descriptive Statistics for the Whole Population ###########################
##########################################################################################################

### Sahpiro-Wilk Test for normality
shapiro_results <- data.frame(
  Variable = c("AGE", "VITAMIN_D"),
  W_statistic = c(
    round(shapiro.test(data$AGE)$statistic, 3),
    round(shapiro.test(data$VITAMIN_D)$statistic, 3)
  ),
  P_value = c(
    round(shapiro.test(data$AGE)$p.value, 3),
    round(shapiro.test(data$VITAMIN_D)$p.value, 3)
  )
)

### Continuous variables (Age) 
continuous_desc <- data.frame(
  Variable = c("AGE", "VITAMIN_D"),
  Median = c(median(data$AGE, na.rm = TRUE),median(data$VITAMIN_D, na.rm = TRUE)),
  IQR = c(IQR(data$AGE, na.rm = TRUE),
    IQR(data$VITAMIN_D, na.rm = TRUE)))

### Categorical variables 
categorical_desc <- data.frame(
  Variable = c("SEX", "SEX",
    "HYPERTENSION", "HYPERTENSION",
    "CIGARETTE_SMOKING", "CIGARETTE_SMOKING",
    "OBESITY", "OBESITY",
    "PHYSICAL_ACTIVITY", "PHYSICAL_ACTIVITY",
    "FAMILY_HISTORY", "FAMILY_HISTORY",
    "ANXIETY_DISORDER", "ANXIETY_DISORDER",
    "VITAMIN_D_LEV_2", "VITAMIN_D_LEV_2",
    "CAC_SCORE_LEV_4", "CAC_SCORE_LEV_4", "CAC_SCORE_LEV_4", "CAC_SCORE_LEV_4",
    "CAS_LEV_4", "CAS_LEV_4", "CAS_LEV_4", "CAS_LEV_4"),
  Category = c("FEMALE", "MALE",
    "NO", "YES",
    "NO", "YES",
    "NO", "YES",
    "YES", "NO",
    "NO", "YES",
    "NO", "YES",
    "SUFFICIENT", "INSUFFICIENT",
    "NONE", "MILD", "MODERATE", "SEVERE",
    "NONE", "MILD", "MODERATE", "SEVERE"),
  Frequency = c(sum(data$SEX == "FEMALE"), sum(data$SEX == "MALE"),
    sum(data$HYPERTENSION == "NO"), sum(data$HYPERTENSION == "YES"),
    sum(data$CIGARETTE_SMOKING == "NO"), sum(data$CIGARETTE_SMOKING == "YES"),
    sum(data$OBESITY == "NO"), sum(data$OBESITY == "YES"),
    sum(data$PHYSICAL_ACTIVITY == "YES"), sum(data$PHYSICAL_ACTIVITY == "NO"),
    sum(data$FAMILY_HISTORY == "NO"), sum(data$FAMILY_HISTORY == "YES"),
    sum(data$ANXIETY_DISORDER == "NO"), sum(data$ANXIETY_DISORDER == "YES"),
    sum(data$VITAMIN_D_LEV_2 == "SUFFICIENT"), sum(data$VITAMIN_D_LEV_2 == "INSUFFICIENT"),
    sum(data$CAC_SCORE_LEV_4 == "NONE"), sum(data$CAC_SCORE_LEV_4 == "MILD"),
    sum(data$CAC_SCORE_LEV_4 == "MODERATE"), sum(data$CAC_SCORE_LEV_4 == "SEVERE"),
    sum(data$CAS_LEV_4 == "NONE"), sum(data$CAS_LEV_4 == "MILD"),
    sum(data$CAS_LEV_4 == "MODERATE"), sum(data$CAS_LEV_4 == "SEVERE")))
categorical_desc$Percentage <- round(categorical_desc$Frequency / nrow(data) * 100, 1)

##########################################################################################################
################# Descriptive Statistics & Inferential Comparison Based on Vitamin D Levels ##############
##########################################################################################################

### Continuous variables (Age) 
age_test <- wilcox.test(AGE ~ VITAMIN_D_LEV_2, data = data)
age_comparison <- data.frame(
  Variable = "AGE",
  Category = "-",
  SUFFICIENT_Median = median(data$AGE[data$VITAMIN_D_LEV_2 == "SUFFICIENT"], na.rm = TRUE),
  SUFFICIENT_IQR = IQR(data$AGE[data$VITAMIN_D_LEV_2 == "SUFFICIENT"], na.rm = TRUE, type = 2),
  INSUFFICIENT_Median = median(data$AGE[data$VITAMIN_D_LEV_2 == "INSUFFICIENT"], na.rm = TRUE),
  INSUFFICIENT_IQR = IQR(data$AGE[data$VITAMIN_D_LEV_2 == "INSUFFICIENT"], na.rm = TRUE, type = 2),
  Test = "Mann-Whitney U",
  P_value = round(age_test$p.value, 3))

### Categorical variables 

## Checking whether to use Chi-square OR Fisher's exact test
chisq.test(table(data$SEX, data$VITAMIN_D_LEV_2))$expected
chisq.test(table(data$HYPERTENSION, data$VITAMIN_D_LEV_2))$expected
chisq.test(table(data$CIGARETTE_SMOKING, data$VITAMIN_D_LEV_2))$expected
chisq.test(table(data$OBESITY, data$VITAMIN_D_LEV_2))$expected
chisq.test(table(data$PHYSICAL_ACTIVITY, data$VITAMIN_D_LEV_2))$expected
chisq.test(table(data$FAMILY_HISTORY, data$VITAMIN_D_LEV_2))$expected
chisq.test(table(data$ANXIETY_DISORDER, data$VITAMIN_D_LEV_2))$expected
chisq.test(table(data$CAC_SCORE_LEV_4, data$VITAMIN_D_LEV_2))$expected
chisq.test(table(data$CAS_LEV_4, data$VITAMIN_D_LEV_2))$expected

## Running analysis
sex_test <- chisq.test(table(data$SEX, data$VITAMIN_D_LEV_2))
hypertension_test <- chisq.test(table(data$HYPERTENSION, data$VITAMIN_D_LEV_2))
smoking_test <- chisq.test(table(data$CIGARETTE_SMOKING, data$VITAMIN_D_LEV_2))
obesity_test <- chisq.test(table(data$OBESITY, data$VITAMIN_D_LEV_2))
physical_test <- fisher.test(table(data$PHYSICAL_ACTIVITY, data$VITAMIN_D_LEV_2))
family_test <- chisq.test(table(data$FAMILY_HISTORY, data$VITAMIN_D_LEV_2))
anxiety_test <- chisq.test(table(data$ANXIETY_DISORDER, data$VITAMIN_D_LEV_2))
cac4_test <- fisher.test(table(data$CAC_SCORE_LEV_4, data$VITAMIN_D_LEV_2))
cas4_test <- fisher.test(table(data$CAS_LEV_4, data$VITAMIN_D_LEV_2))

## Building data frame
categorical_comparison <- data.frame(
  Variable = c(
    "SEX", "SEX",
    "HYPERTENSION", "HYPERTENSION",
    "CIGARETTE_SMOKING", "CIGARETTE_SMOKING",
    "OBESITY", "OBESITY",
    "PHYSICAL_ACTIVITY", "PHYSICAL_ACTIVITY",
    "FAMILY_HISTORY", "FAMILY_HISTORY",
    "ANXIETY_DISORDER", "ANXIETY_DISORDER",
    "CAC_SCORE_LEV_4", "CAC_SCORE_LEV_4", "CAC_SCORE_LEV_4", "CAC_SCORE_LEV_4",
    "CAS_LEV_4", "CAS_LEV_4", "CAS_LEV_4", "CAS_LEV_4"),
  Category = c(
    "FEMALE", "MALE",
    "NO", "YES",
    "NO", "YES",
    "NO", "YES",
    "YES", "NO",
    "NO", "YES",
    "NO", "YES",
    "NONE", "MILD", "MODERATE", "SEVERE",
    "NONE", "MILD", "MODERATE", "SEVERE"),
  SUFFICIENT_n = c(
    sum(data$SEX == "FEMALE" & data$VITAMIN_D_LEV_2 == "SUFFICIENT"),
    sum(data$SEX == "MALE" & data$VITAMIN_D_LEV_2 == "SUFFICIENT"),
    sum(data$HYPERTENSION == "NO" & data$VITAMIN_D_LEV_2 == "SUFFICIENT"),
    sum(data$HYPERTENSION == "YES" & data$VITAMIN_D_LEV_2 == "SUFFICIENT"),
    sum(data$CIGARETTE_SMOKING == "NO" & data$VITAMIN_D_LEV_2 == "SUFFICIENT"),
    sum(data$CIGARETTE_SMOKING == "YES" & data$VITAMIN_D_LEV_2 == "SUFFICIENT"),
    sum(data$OBESITY == "NO" & data$VITAMIN_D_LEV_2 == "SUFFICIENT"),
    sum(data$OBESITY == "YES" & data$VITAMIN_D_LEV_2 == "SUFFICIENT"),
    sum(data$PHYSICAL_ACTIVITY == "YES" & data$VITAMIN_D_LEV_2 == "SUFFICIENT"),
    sum(data$PHYSICAL_ACTIVITY == "NO" & data$VITAMIN_D_LEV_2 == "SUFFICIENT"),
    sum(data$FAMILY_HISTORY == "NO" & data$VITAMIN_D_LEV_2 == "SUFFICIENT"),
    sum(data$FAMILY_HISTORY == "YES" & data$VITAMIN_D_LEV_2 == "SUFFICIENT"),
    sum(data$ANXIETY_DISORDER == "NO" & data$VITAMIN_D_LEV_2 == "SUFFICIENT"),
    sum(data$ANXIETY_DISORDER == "YES" & data$VITAMIN_D_LEV_2 == "SUFFICIENT"),
    sum(data$CAC_SCORE_LEV_4 == "NONE" & data$VITAMIN_D_LEV_2 == "SUFFICIENT"),
    sum(data$CAC_SCORE_LEV_4 == "MILD" & data$VITAMIN_D_LEV_2 == "SUFFICIENT"),
    sum(data$CAC_SCORE_LEV_4 == "MODERATE" & data$VITAMIN_D_LEV_2 == "SUFFICIENT"),
    sum(data$CAC_SCORE_LEV_4 == "SEVERE" & data$VITAMIN_D_LEV_2 == "SUFFICIENT"),
    sum(data$CAS_LEV_4 == "NONE" & data$VITAMIN_D_LEV_2 == "SUFFICIENT"),
    sum(data$CAS_LEV_4 == "MILD" & data$VITAMIN_D_LEV_2 == "SUFFICIENT"),
    sum(data$CAS_LEV_4 == "MODERATE" & data$VITAMIN_D_LEV_2 == "SUFFICIENT"),
    sum(data$CAS_LEV_4 == "SEVERE" & data$VITAMIN_D_LEV_2 == "SUFFICIENT")),
  INSUFFICIENT_n = c(
    sum(data$SEX == "FEMALE" & data$VITAMIN_D_LEV_2 == "INSUFFICIENT"),
    sum(data$SEX == "MALE" & data$VITAMIN_D_LEV_2 == "INSUFFICIENT"),
    sum(data$HYPERTENSION == "NO" & data$VITAMIN_D_LEV_2 == "INSUFFICIENT"),
    sum(data$HYPERTENSION == "YES" & data$VITAMIN_D_LEV_2 == "INSUFFICIENT"),
    sum(data$CIGARETTE_SMOKING == "NO" & data$VITAMIN_D_LEV_2 == "INSUFFICIENT"),
    sum(data$CIGARETTE_SMOKING == "YES" & data$VITAMIN_D_LEV_2 == "INSUFFICIENT"),
    sum(data$OBESITY == "NO" & data$VITAMIN_D_LEV_2 == "INSUFFICIENT"),
    sum(data$OBESITY == "YES" & data$VITAMIN_D_LEV_2 == "INSUFFICIENT"),
    sum(data$PHYSICAL_ACTIVITY == "YES" & data$VITAMIN_D_LEV_2 == "INSUFFICIENT"),
    sum(data$PHYSICAL_ACTIVITY == "NO" & data$VITAMIN_D_LEV_2 == "INSUFFICIENT"),
    sum(data$FAMILY_HISTORY == "NO" & data$VITAMIN_D_LEV_2 == "INSUFFICIENT"),
    sum(data$FAMILY_HISTORY == "YES" & data$VITAMIN_D_LEV_2 == "INSUFFICIENT"),
    sum(data$ANXIETY_DISORDER == "NO" & data$VITAMIN_D_LEV_2 == "INSUFFICIENT"),
    sum(data$ANXIETY_DISORDER == "YES" & data$VITAMIN_D_LEV_2 == "INSUFFICIENT"),
    sum(data$CAC_SCORE_LEV_4 == "NONE" & data$VITAMIN_D_LEV_2 == "INSUFFICIENT"),
    sum(data$CAC_SCORE_LEV_4 == "MILD" & data$VITAMIN_D_LEV_2 == "INSUFFICIENT"),
    sum(data$CAC_SCORE_LEV_4 == "MODERATE" & data$VITAMIN_D_LEV_2 == "INSUFFICIENT"),
    sum(data$CAC_SCORE_LEV_4 == "SEVERE" & data$VITAMIN_D_LEV_2 == "INSUFFICIENT"),
    sum(data$CAS_LEV_4 == "NONE" & data$VITAMIN_D_LEV_2 == "INSUFFICIENT"),
    sum(data$CAS_LEV_4 == "MILD" & data$VITAMIN_D_LEV_2 == "INSUFFICIENT"),
    sum(data$CAS_LEV_4 == "MODERATE" & data$VITAMIN_D_LEV_2 == "INSUFFICIENT"),
    sum(data$CAS_LEV_4 == "SEVERE" & data$VITAMIN_D_LEV_2 == "INSUFFICIENT")),
  Test = c(
    "Chi-square", "Chi-square",
    "Chi-square", "Chi-square",
    "Chi-square", "Chi-square",
    "Chi-square", "Chi-square",
    "Fisher's exact", "Fisher's exact",
    "Chi-square", "Chi-square",
    "Chi-square", "Chi-square",
    "Fisher's exact", "Fisher's exact", "Fisher's exact", "Fisher's exact",
    "Fisher's exact", "Fisher's exact", "Fisher's exact", "Fisher's exact"),
  P_value = c(
    round(sex_test$p.value, 3), NA,
    round(hypertension_test$p.value, 3), NA,
    round(smoking_test$p.value, 3), NA,
    round(obesity_test$p.value, 3), NA,
    round(physical_test$p.value, 3), NA,
    round(family_test$p.value, 3), NA,
    round(anxiety_test$p.value, 3), NA,
    round(cac4_test$p.value, 3), NA, NA, NA,
    round(cas4_test$p.value, 3), NA, NA, NA))

## Adding percentage columns
categorical_comparison$SUFFICIENT_pct <- round(
  categorical_comparison$SUFFICIENT_n / 203 * 100, 1)
categorical_comparison$INSUFFICIENT_pct <- round(
  categorical_comparison$INSUFFICIENT_n / 203 * 100, 1)

##########################################################################################################
################################ Binary Logistic Regression for CAC Scores ###############################
##########################################################################################################

# Crude model including only Vitamin D
cac_model1 <- glm(
  CAC_SCORE_LEV_2 ~ VITAMIN_D,
  data = data,
  family = binomial)

# Partially adjusted model including Vitamin D & Age & Obesity
cac_model2 <- glm(
  CAC_SCORE_LEV_2 ~ VITAMIN_D + AGE + OBESITY,
  data = data,
  family = binomial)

# Fully adjusted model 
cac_model3 <- glm(
  CAC_SCORE_LEV_2 ~ VITAMIN_D + AGE + OBESITY + SEX +
    HYPERTENSION + CIGARETTE_SMOKING +
    PHYSICAL_ACTIVITY + FAMILY_HISTORY + ANXIETY_DISORDER,
  data = data,
  family = binomial)

# Building data frame for results
cac_results <- data.frame(
  Model = c("Crude", "Partially Adjusted", "Fully Adjusted"),
  B = c(
    coef(cac_model1)["VITAMIN_D"],
    coef(cac_model2)["VITAMIN_D"],
    coef(cac_model3)["VITAMIN_D"]),
  OR = c(
    exp(coef(cac_model1)["VITAMIN_D"]),
    exp(coef(cac_model2)["VITAMIN_D"]),
    exp(coef(cac_model3)["VITAMIN_D"])),
  CI_lower = c(
    exp(confint(cac_model1)["VITAMIN_D", 1]),
    exp(confint(cac_model2)["VITAMIN_D", 1]),
    exp(confint(cac_model3)["VITAMIN_D", 1])),
  CI_upper = c(
    exp(confint(cac_model1)["VITAMIN_D", 2]),
    exp(confint(cac_model2)["VITAMIN_D", 2]),
    exp(confint(cac_model3)["VITAMIN_D", 2])),
  P_value = c(
    summary(cac_model1)$coefficients["VITAMIN_D", 4],
    summary(cac_model2)$coefficients["VITAMIN_D", 4],
    summary(cac_model3)$coefficients["VITAMIN_D", 4]))

# Round values
cac_results$B <- round(cac_results$B, 3)
cac_results$OR <- round(cac_results$OR, 3)
cac_results$CI_lower <- round(cac_results$CI_lower, 3)
cac_results$CI_upper <- round(cac_results$CI_upper, 3)
cac_results$P_value <- round(cac_results$P_value, 3)

# Hosmer-Lemeshow
hl_cac_model1 <- hoslem.test(
  as.numeric(data$CAC_SCORE_LEV_2) - 1,
  fitted(cac_model1), g = 10)
hl_cac_model2 <- hoslem.test(
  as.numeric(data$CAC_SCORE_LEV_2) - 1,
  fitted(cac_model2), g = 10)
hl_cac_model3 <- hoslem.test(
  as.numeric(data$CAC_SCORE_LEV_2) - 1,
  fitted(cac_model3), g = 10)

cac_results$Hosmer_Lemeshow_P <- c(
  round(hl_cac_model1$p.value, 3),
  round(hl_cac_model2$p.value, 3),
  round(hl_cac_model3$p.value, 3))

##########################################################################################################
################################### Binary Logistic Regression for CAS ###################################
##########################################################################################################

# Crude model including only Vitamin D
cas_model1 <- glm(
  CAS_LEV_2 ~ VITAMIN_D,
  data = data,
  family = binomial)

# Partially adjusted model including Vitamin D & Age & Obesity
cas_model2 <- glm(
  CAS_LEV_2 ~ VITAMIN_D + AGE + OBESITY,
  data = data,
  family = binomial)

# Fully adjusted model 
cas_model3 <- glm(
  CAS_LEV_2 ~ VITAMIN_D + AGE + OBESITY + SEX +
    HYPERTENSION + CIGARETTE_SMOKING +
    PHYSICAL_ACTIVITY + FAMILY_HISTORY + ANXIETY_DISORDER,
  data = data,
  family = binomial)

# Building data frame for results
cas_results <- data.frame(
  Model = c("Crude", "Partially Adjusted", "Fully Adjusted"),
  B = c(
    coef(cas_model1)["VITAMIN_D"],
    coef(cas_model2)["VITAMIN_D"],
    coef(cas_model3)["VITAMIN_D"]),
  OR = c(
    exp(coef(cas_model1)["VITAMIN_D"]),
    exp(coef(cas_model2)["VITAMIN_D"]),
    exp(coef(cas_model3)["VITAMIN_D"])),
  CI_lower = c(
    exp(confint(cas_model1)["VITAMIN_D", 1]),
    exp(confint(cas_model2)["VITAMIN_D", 1]),
    exp(confint(cas_model3)["VITAMIN_D", 1])),
  CI_upper = c(
    exp(confint(cas_model1)["VITAMIN_D", 2]),
    exp(confint(cas_model2)["VITAMIN_D", 2]),
    exp(confint(cas_model3)["VITAMIN_D", 2])),
  P_value = c(
    summary(cas_model1)$coefficients["VITAMIN_D", 4],
    summary(cas_model2)$coefficients["VITAMIN_D", 4],
    summary(cas_model3)$coefficients["VITAMIN_D", 4]))

# Round values
cas_results$B <- round(cas_results$B, 3)
cas_results$OR <- round(cas_results$OR, 3)
cas_results$CI_lower <- round(cas_results$CI_lower, 3)
cas_results$CI_upper <- round(cas_results$CI_upper, 3)
cas_results$P_value <- round(cas_results$P_value, 3)

# Hosmer-Lemeshow
hl_cas_model1 <- hoslem.test(
  as.numeric(data$CAS_LEV_2) - 1,
  fitted(cas_model1), g = 10)
hl_cas_model2 <- hoslem.test(
  as.numeric(data$CAS_LEV_2) - 1,
  fitted(cas_model2), g = 10)
hl_cas_model3 <- hoslem.test(
  as.numeric(data$CAS_LEV_2) - 1,
  fitted(cas_model3), g = 10)

cas_results$Hosmer_Lemeshow_P <- c(
  round(hl_cas_model1$p.value, 3),
  round(hl_cas_model2$p.value, 3),
  round(hl_cas_model3$p.value, 3))

##########################################################################################################
######################################## Confounding Assessment ##########################################
##########################################################################################################

### CAC Score

# Model A - vitamin D + age only
model_A_cac <- glm(CAC_SCORE_LEV_2 ~ VITAMIN_D + AGE,
                   data = data, family = binomial)

# Model B - vitamin D + obesity only  
model_B_cac <- glm(CAC_SCORE_LEV_2 ~ VITAMIN_D + OBESITY,
                   data = data, family = binomial)

# Model C - vitamin D + age + obesity
model_C_cac <- glm(CAC_SCORE_LEV_2 ~ VITAMIN_D + AGE + OBESITY,
                   data = data, family = binomial)

# Building data frame for results
conf_cac_results <- data.frame(
  Model = c("Age", "Obesity", "Age + Obesity"),
  B = c(
    coef(model_A_cac)["VITAMIN_D"],
    coef(model_B_cac)["VITAMIN_D"],
    coef(model_C_cac)["VITAMIN_D"]),
  OR = c(
    exp(coef(model_A_cac)["VITAMIN_D"]),
    exp(coef(model_B_cac)["VITAMIN_D"]),
    exp(coef(model_C_cac)["VITAMIN_D"])),
  CI_lower = c(
    exp(confint(model_A_cac)["VITAMIN_D", 1]),
    exp(confint(model_B_cac)["VITAMIN_D", 1]),
    exp(confint(model_C_cac)["VITAMIN_D", 1])),
  CI_upper = c(
    exp(confint(model_A_cac)["VITAMIN_D", 2]),
    exp(confint(model_B_cac)["VITAMIN_D", 2]),
    exp(confint(model_C_cac)["VITAMIN_D", 2])),
  P_value = c(
    summary(model_A_cac)$coefficients["VITAMIN_D", 4],
    summary(model_B_cac)$coefficients["VITAMIN_D", 4],
    summary(model_C_cac)$coefficients["VITAMIN_D", 4]))

# Round values
conf_cac_results$B <- round(conf_cac_results$B, 3)
conf_cac_results$OR <- round(conf_cac_results$OR, 3)
conf_cac_results$CI_lower <- round(conf_cac_results$CI_lower, 3)
conf_cac_results$CI_upper <- round(conf_cac_results$CI_upper, 3)
conf_cac_results$P_value <- round(conf_cac_results$P_value, 3)

# Hosmer-Lemeshow
hl_cac_modelA <- hoslem.test(
  as.numeric(data$CAC_SCORE_LEV_2) - 1,
  fitted(model_A_cac), g = 10)
hl_cac_modelB <- hoslem.test(
  as.numeric(data$CAC_SCORE_LEV_2) - 1,
  fitted(model_B_cac), g = 10)
hl_cac_modelC <- hoslem.test(
  as.numeric(data$CAC_SCORE_LEV_2) - 1,
  fitted(model_C_cac), g = 10)

conf_cac_results$Hosmer_Lemeshow_P <- c(
  round(hl_cac_modelA$p.value, 3),
  round(hl_cac_modelB$p.value, 3),
  round(hl_cac_modelC$p.value, 3))

### CAS Severity

# Model A - vitamin D + age only
model_A_cas <- glm(CAS_LEV_2 ~ VITAMIN_D + AGE,
                   data = data, family = binomial)

# Model B - vitamin D + obesity only  
model_B_cas <- glm(CAS_LEV_2 ~ VITAMIN_D + OBESITY,
                   data = data, family = binomial)

# Model C - vitamin D + age + obesity
model_C_cas <- glm(CAS_LEV_2 ~ VITAMIN_D + AGE + OBESITY,
                   data = data, family = binomial)

# Building data frame for results
conf_cas_results <- data.frame(
  Model = c("Age", "Obesity", "Age + Obesity"),
  B = c(
    coef(model_A_cas)["VITAMIN_D"],
    coef(model_B_cas)["VITAMIN_D"],
    coef(model_C_cas)["VITAMIN_D"]),
  OR = c(
    exp(coef(model_A_cas)["VITAMIN_D"]),
    exp(coef(model_B_cas)["VITAMIN_D"]),
    exp(coef(model_C_cas)["VITAMIN_D"])),
  CI_lower = c(
    exp(confint(model_A_cas)["VITAMIN_D", 1]),
    exp(confint(model_B_cas)["VITAMIN_D", 1]),
    exp(confint(model_C_cas)["VITAMIN_D", 1])),
  CI_upper = c(
    exp(confint(model_A_cas)["VITAMIN_D", 2]),
    exp(confint(model_B_cas)["VITAMIN_D", 2]),
    exp(confint(model_C_cas)["VITAMIN_D", 2])),
  P_value = c(
    summary(model_A_cas)$coefficients["VITAMIN_D", 4],
    summary(model_B_cas)$coefficients["VITAMIN_D", 4],
    summary(model_C_cas)$coefficients["VITAMIN_D", 4]))

# Round values
conf_cas_results$B <- round(conf_cas_results$B, 3)
conf_cas_results$OR <- round(conf_cas_results$OR, 3)
conf_cas_results$CI_lower <- round(conf_cas_results$CI_lower, 3)
conf_cas_results$CI_upper <- round(conf_cas_results$CI_upper, 3)
conf_cas_results$P_value <- round(conf_cas_results$P_value, 3)

# Hosmer-Lemeshow
hl_cas_modelA <- hoslem.test(
  as.numeric(data$CAS_LEV_2) - 1,
  fitted(model_A_cas), g = 10)
hl_cas_modelB <- hoslem.test(
  as.numeric(data$CAS_LEV_2) - 1,
  fitted(model_B_cas), g = 10)
hl_cas_modelC <- hoslem.test(
  as.numeric(data$CAS_LEV_2) - 1,
  fitted(model_C_cas), g = 10)

conf_cas_results$Hosmer_Lemeshow_P <- c(
  round(hl_cas_modelA$p.value, 3),
  round(hl_cas_modelB$p.value, 3),
  round(hl_cas_modelC$p.value, 3))

##########################################################################################################
########################################## Exporting Results #############################################
##########################################################################################################

library(writexl)

write_xlsx(
  list(
    "Shapiro_Wilk" = shapiro_results,
    "Descriptive_Continuous" = continuous_desc,
    "Descriptive_Categorical" = categorical_desc,
    "Table1_Age" = age_comparison,
    "Table1_Categorical" = categorical_comparison,
    "Regression_CAC" = cac_results,
    "Regression_Stenosis" = cas_results,
    "CAC Score Confounding" = conf_cac_results,
    "CAS Confounding" = conf_cas_results),
  path = "File Address")