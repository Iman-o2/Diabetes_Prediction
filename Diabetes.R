
Sys.setenv(EPICONDUCTOR_CONTAINER_DB = "postgres",
           EPICONDUCTOR_DB_SERVER = "www.isid.ac.in")

nhanesOptions(log.access = TRUE)
nhanesOptions(use.db = TRUE)

nhanesOptions()
#Code for Checking Variable Description:
cb <- nhanesCodebook("GLU_H")

#Constructing Response Variable Diabetes

glycohaemoglobin_17 <- nhanes("GHB_J")
glycohaemoglobin_15 <- nhanes("GHB_I")
glycohaemoglobin_13 <- nhanes("GHB_H")
fastingglucose_17 <- nhanes("GLU_J")
fastingglucose_15 <- nhanes("GLU_I")
fastinggglucose_13 <- nhanes("GLU_H")



glycohaemoglobin <- bind_rows(glycohaemoglobin_13,glycohaemoglobin_15,glycohaemoglobin_17)
fastinglucose <- bind_rows(fastinggglucose_13,fastingglucose_15,fastingglucose_17)



response_table <- diabetes_questionaire %>%
  select(SEQN, DIQ010) %>%
  left_join(
    glycohaemoglobin %>% select(SEQN, LBXGH), 
    by = "SEQN"
  ) %>%
  left_join(
    fastinglucose %>% select(SEQN, LBXGLU),
    by = "SEQN"
  )

# Discarding those obs for which both the lab results are missing.
response_table <- response_table %>% filter(!is.na(LBXGH) | !is.na(LBXGLU))


response_table <- response_table %>%
  mutate(
    diabetes = case_when(
      # Rule 1: Use both tests if available (original logic)
      LBXGH > 6.4 | LBXGLU > 125 ~ 2,  # Diabetes
      (LBXGH < 5.7 & LBXGLU >= 100 & LBXGLU <= 125) |
        (LBXGH >= 5.7 & LBXGH <= 6.4 & LBXGLU < 100) |
        (LBXGH >= 5.7 & LBXGH <= 6.4 & LBXGLU >= 100 & LBXGLU <= 125) ~ 1,  # Prediabetes
      LBXGH < 5.7 & LBXGLU < 100 ~ 0,  # Normal
      
      # Rule 2: If LBXGH is missing, use LBXGLU alone
      is.na(LBXGH) & LBXGLU > 125 ~ 2,
      is.na(LBXGH) & LBXGLU >= 100 & LBXGLU <= 125 ~ 1,
      is.na(LBXGH) & LBXGLU < 100 ~ 0,
      
      # Rule 3: If LBXGLU is missing, use LBXGH alone
      is.na(LBXGLU) & LBXGH > 6.4 ~ 2,
      is.na(LBXGLU) & LBXGH >= 5.7 & LBXGH <= 6.4 ~ 1,
      is.na(LBXGLU) & LBXGH < 5.7 ~ 0,
      
      # Default: If both are missing (unlikely due to earlier filtering) or other edge cases
      TRUE ~ NA_real_
    )
  )

#Choosing Covariates:
  
#Demographics
demo_13 <- nhanes("DEMO_H")
demo_15 <- nhanes("DEMO_I")
demo_17 <- nhanes("DEMO_J")

demographics <- bind_rows(demo_13, demo_15, demo_17)
physicalactivity <- bind_rows(paq_13, paq_15, paq_17)

covariates_diabetes <- demographics %>% select(SEQN, RIDAGEYR,RIAGENDR,RIDRETH3,INDFMPIR,DMDHHSIZ) %>% 
  rename(age = RIDAGEYR,gender = RIAGENDR, ethnicity = RIDRETH3, 
         income_poverty_ratio = INDFMPIR, household_size = DMDHHSIZ)

# Physical activity
paq_13 <- nhanes("PAQ_H")
paq_15 <- nhanes("PAQ_I")
paq_17 <- nhanes("PAQ_J")


covariates_diabetes <- covariates_diabetes %>% 
  left_join(physicalactivity %>% 
              select(SEQN,PAQ605, PAQ620, PAD680, PAQ650, PAQ665), 
            by = "SEQN")

#Creating activity and recreational activity variables:
covariates_diabetes <- covariates_diabetes  %>% mutate(
  activity = case_when(
      PAQ605 == "Yes" & PAQ620 == "No" ~ 2,
      PAQ605 == "No" & PAQ620 == "Yes" ~ 1,
      PAQ605 == "No" & PAQ620 == "No" ~ 0,
      TRUE ~ NA_integer_ 
    ),
    recreational_activity = case_when(
      PAQ650 == "Yes" & PAQ665 == "No" ~ 2,
      PAQ650 == "No" & PAQ665 == "Yes" ~ 1,
      PAQ650 == "No" & PAQ665 == "No" ~ 0,
      TRUE ~ NA_integer_ 
    )
  ) %>%
  rename(sedentary_minutes = PAD680) %>%
  select(-PAQ605,-PAQ620,-PAQ650,-PAQ665)

#Diet Data

diet_17 <- nhanes("DR1TOT_J")
diet_15 <- nhanes("DR1TOT_I")
diet_13 <- nhanes("DR1TOT_H")                                                        

diet <- bind_rows(
  diet_13 %>% mutate(DRQSDT5 = as.character(DRQSDT5)),
  diet_15 %>% mutate(DRQSDT5 = as.character(DRQSDT5)),
  diet_17 %>% mutate(DRQSDT5 = as.character(DRQSDT5))
)                                                         

covariates_diabetes <- covariates_diabetes %>% left_join(diet %>% 
                                                           select(SEQN,DR1TKCAL,DR1TCARB,DR1TSUGR,DR1TFIBE,
                                                                  DR1TSFAT,DR1TTFAT,DR1TPROT,DR1TPFAT,DR1TVD,
                                                                  DR1TMAGN,DR1TSODI),by = "SEQN") %>% 
  rename(calorie = DR1TKCAL, carbs = DR1TCARB, sugar = DR1TSUGR, fibre = DR1TFIBE, saturated_fat = DR1TSFAT,
         trans_fat = DR1TTFAT, polyunsaturated_fat = DR1TPFAT, protein = DR1TPROT, vitamin_D = DR1TVD,
         magnesium = DR1TMAGN, sodium = DR1TSODI)

#Alcohol Consumption 

alcohol_13 <- nhanes("ALQ_H")
alcohol_15 <- nhanes("ALQ_I")
alcohol_17 <- nhanes("ALQ_J")
                                  
alcohol <- bind_rows(alcohol_13,alcohol_15,alcohol_17)

#Creating Alcohol Status

covariates_diabetes <- covariates_diabetes %>%
  left_join(alcohol %>% select(SEQN, ALQ101, ALQ110, ALQ130), by = "SEQN") %>%
  mutate(
    alcohol_status = case_when(
      ALQ101 == "No" ~ "Never",
      ALQ110 == "No" & ALQ101 == "Yes" ~ "Former",
      ALQ130 <= 1 ~ "Light",
      ALQ130 <= 3 & ALQ130 > 1 ~ "Moderate",
      ALQ130 > 3 ~ "Heavy",
      TRUE ~ NA_character_
    )) %>%
  select(-ALQ101,-ALQ110,-ALQ130)

#Smoking

smoking_13 <- nhanes("SMQ_H")
smoking_15 <- nhanes("SMQ_I")
smoking_17 <- nhanes("SMQ_J")

smoking <- bind_rows(smoking_13,smoking_15,smoking_17)                                


#Laboratory Data:

#Fasting glucose, insulin and C-peptide
ins_13 <- nhanes("INS_H")
ins_15 <- nhanes("INS_I")
ins_17 <- nhanes("INS_J")

#Triglycerides
trigly_13 <- nhanes("TRIGLY_H")
trigly_15 <- nhanes("TRIGLY_I")
trigly_17 <- nhanes("TRIGLY_J")

#Total Cholestrol
tchol_13 <- nhanes("TCHOL_H")
tchol_15 <- nhanes("TCHOL_I")
tchol_17 <- nhanes("TCHOL_J")

#LDL Cholestrol
hdlchol_13 <- nhanes("HDL_H")
hdlchol_15 <- nhanes("HDL_I")
hdlchol_17 <- nhanes("HDL_J")

#Serum Creatinine
serumcreat_13 <- nhanes("BIOPRO_H")
serumcreat_15 <- nhanes("BIOPRO_I")
serumcreat_17 <- nhanes("BIOPRO_J")

#Binding 
insulin_data <- bind_rows(ins_13,ins_15,ins_17)
trigly_data <- bind_rows(trigly_13,trigly_15,trigly_17)
tchol_data <- bind_rows(tchol_13,tchol_15,tchol_17)
hdlchol_data <- bind_rows(hdlchol_13,hdlchol_15,hdlchol_17)
serumcreat_data <- bind_rows(serumcreat_13,serumcreat_15,serumcreat_17)


covariates_diabetes <- covariates_diabetes %>% 
  left_join(trigly_data %>% select(SEQN, LBXTR, LBDLDL), by = "SEQN") %>% 
  rename(trigly = LBXTR, ldl_chol = LBDLDL) %>%
  
  left_join(insulin_data %>% select(SEQN, LBDINSI), by = "SEQN") %>% 
  rename(insulin = LBDINSI) %>%
  
  left_join(tchol_data %>% select(SEQN, LBXTC), by = "SEQN") %>% 
  rename(total_chol = LBXTC) %>%
  
  left_join(hdlchol_data %>% select(SEQN, LBDHDD), by = "SEQN") %>% 
  rename(hdl_chol = LBDHDD) %>%
  
  left_join(serumcreat_data %>% select(SEQN, LBXSCR, LBXSAL), by = "SEQN") %>% 
  rename(creatinine = LBXSCR, albumin = LBXSAL)

#Examination Data:

# Body measures (BMI, waist)
bmx_13 <- nhanes("BMX_H")
bmx_15 <- nhanes("BMX_I")
bmx_17 <- nhanes("BMX_J")

body_measures <- bind_rows(bmx_13,bmx_15,bmx_17)

covariates_diabetes <- covariates_diabetes %>%
  left_join(body_measures %>% select(SEQN, BMXBMI,BMXWAIST), by = "SEQN") %>%
  rename(bmi = BMXBMI, waist = BMXWAIST)


# Blood pressure
bpx_13 <- nhanes("BPX_H")
bpx_15 <- nhanes("BPX_I")
bpx_17 <- nhanes("BPX_J")

blood_pressure <- bind_rows(bpx_13,bpx_15,bpx_17)

covariates_diabetes <- covariates_diabetes %>%
  left_join(blood_pressure %>% select(SEQN, BPXSY1, BPXDI1), by = "SEQN") %>%
  rename(systolic_bp = BPXSY1,
         diastolic_bp = BPXDI1,
  )

#Family History

mcq_13<- nhanes("MCQ_H") 
mcq_15 <- nhanes("MCQ_I") 
mcq_17 <- nhanes("MCQ_J")

mcq <- bind_rows(
  mcq_13 %>% select(SEQN, MCQ300C),
  mcq_15 %>% select(SEQN, MCQ300C),
  mcq_17 %>% select(SEQN, MCQ300C)
  )

covariates_diabetes <- covariates_diabetes %>%
  left_join(mcq, by = "SEQN") %>%
  rename(family_history = MCQ300C)

#Final Dataframe:
covariates_diabetes <- covariates_diabetes %>% inner_join(response_table %>% select(SEQN,diabetes),by="SEQN")
diabetes_data <- covariates_diabetes %>% inner_join(response_table %>% select(SEQN,diabetes),by="SEQN")

#Dealing with null values:

install.packages("naniar")
library(naniar)

# Visualize missing data
gg_miss_var(covariates_diabetes)


diabetes_data %>% count(is.na(total_chol))

#Median Imputation for income_poverty_ratio:

diabetes_data <- diabetes_data %>%
  mutate(
    age_group = cut(
      age,
      breaks = c(0, 18, 35, 50, 65, Inf),
      labels = c("0-18", "19-35", "36-50", "51-65", "65+")
    )
  )

diabetes_data <- diabetes_data %>%
  group_by(household_size, ethnicity, age_group) %>%
  mutate(
    income_poverty_ratio = ifelse(
      is.na(income_poverty_ratio),
      median(income_poverty_ratio, na.rm = TRUE),
      income_poverty_ratio
    )
  ) %>%
  ungroup() %>% select(-age_group)

#Median Imputation for sedentary_minutes:

diabetes_data <- diabetes_data %>%
  mutate(
    
    age_group = cut(
      age,
      breaks = c(0, 18, 35, 50, 65, Inf),
      labels = c("0-18", "19-35", "36-50", "51-65", "65+")
    ),
    
   
    bmi_category = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi >= 18.5 & bmi < 25 ~ "Normal",
      bmi >= 25 & bmi < 30 ~ "Overweight",
      bmi >= 30 ~ "Obese",
      TRUE ~ NA_character_
    )
  )

diabetes_data <- diabetes_data %>%
  group_by(age_group, bmi_category, activity) %>%
  mutate(
    sedentary_minutes = ifelse(
      is.na(sedentary_minutes),
      median(sedentary_minutes, na.rm = TRUE),
      sedentary_minutes
    )
  ) %>%
  ungroup()

#Mode imputation of activity:

# Function to calculate mode
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}


diabetes_data <- diabetes_data %>%
  group_by(age_group, gender, bmi_category) %>%
  mutate(
    activity = ifelse(
      is.na(activity),
      get_mode(activity),
      activity
    )
  ) %>%
  ungroup()

#Mode imputation of recreational activity:

diabetes_data <- diabetes_data %>%
  group_by(age_group, bmi_category) %>%
  mutate(
    recreational_activity = ifelse(
      is.na(recreational_activity),
      get_mode(recreational_activity),
      recreational_activity
    )
  ) %>%
  ungroup()

#Median imputation of diet variables:

diet_vars <- c("calorie", "carbs", "sugar", "fibre", "saturated_fat", 
               "trans_fat", "protein", "polyunsaturated_fat", "vitamin_D", 
               "magnesium", "sodium")

diabetes_data <- diabetes_data %>%
  group_by(age_group, bmi_category, diabetes) %>%
  mutate(across(all_of(diet_vars), 
                ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  ungroup()

#Mode imputation of Alcohol Consumption

diabetes_data <- diabetes_data %>%
  mutate(
    age_group = cut(age, breaks = c(0, 30, 50, 70, Inf), 
                    age_group = factor(age_group, labels = c("<30", "30-50", "50-70", "70+"))
    ))

diabetes_data <- diabetes_data %>%
  group_by(age_group, gender, diabetes) %>%
  mutate(
    alcohol_status = ifelse(
      is.na(alcohol_status),
      get_mode(alcohol_status),
      alcohol_status
    )
  ) %>%
  ungroup()    

#Imputing total_chol and hdl_chol:

diabetes_data <- diabetes_data %>%
  mutate(
    lipid_group = cut(
      age,
      breaks = c(0, 40, 60, Inf),
      labels = c("<40", "40-60", "60+")
    )
  ) %>%
  group_by(lipid_group, gender, bmi_category) %>%
  mutate(
    total_chol = ifelse(
      is.na(total_chol),
      median(total_chol, na.rm = TRUE),
      total_chol
    ),
    hdl_chol = ifelse(
      is.na(hdl_chol),
      median(hdl_chol, na.rm = TRUE),
      hdl_chol
    )
  ) %>%
  ungroup()

#Imputing trigly and ldl_chol:

diabetes_data <- diabetes_data %>%
  mutate(
    lipid_group = case_when(
      total_chol < 200 & hdl_chol >= 40 ~ "Normal",
      total_chol >= 200 | hdl_chol < 40 ~ "High-risk",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(lipid_group, diabetes) %>%
  mutate(
    trigly = ifelse(is.na(trigly), median(trigly, na.rm = TRUE), trigly),
    ldl_chol = ifelse(
      is.na(ldl_chol),
      median(ldl_chol, na.rm = TRUE),
      ldl_chol
    )
  ) %>%
  ungroup()

#Imputing Diastolic and Systolic BP:

diabetes_data <- diabetes_data %>%
  mutate(
    bp_risk_group = case_when(
      diabetes == 2 ~ "Diabetic",
      bmi >= 30 ~ "Obese",
      TRUE ~ "General"
    ),
    age_decade = cut(age, breaks = seq(0, 100, by = 10))
  ) %>%
  group_by(age_decade, gender, bp_risk_group) %>%
  mutate(
    systolic_bp = ifelse(
      is.na(systolic_bp),
      median(systolic_bp, na.rm = TRUE),
      systolic_bp
    ),
    diastolic_bp = ifelse(
      is.na(diastolic_bp),
      median(diastolic_bp, na.rm = TRUE),
      diastolic_bp
    )
  ) %>%
  ungroup()

#Imputing Creatinine and albumin:

diabetes_data <- diabetes_data %>%
  mutate(
    # Log-transformtion of creatinine (skewed)
    log_creat = log(creatinine),
    
    kidney_risk = case_when(
      diabetes == 2 ~ "Diabetic",
      systolic_bp >= 140 ~ "Hypertensive",
      TRUE ~ "Low-Risk"
    )
  ) %>%
  group_by(gender, age_group = cut(age, c(0, 50, 70, Inf)), kidney_risk) %>%
  mutate(
    creatinine = ifelse(
      is.na(creatinine),
      exp(median(log_creat, na.rm = TRUE)),  # Back-transform median(log)
      creatinine
    ),
    albumin = ifelse(
      is.na(albumin),
      median(albumin, na.rm = TRUE),
      albumin
    )
  ) %>%
  ungroup()

#Median Imputation of BMI and Waist

diabetes_data <- diabetes_data %>%
  mutate(
    bodycomp_group = case_when(
      diabetes == 2 ~ "Diabetic",
      gender == "Male" & age >= 50 ~ "Older_Male",
      gender == "Female" & age >= 50 ~ "Older_Female",
      TRUE ~ "General"
    )
  ) %>%
  group_by(bodycomp_group) %>%
  mutate(
    bmi = ifelse(is.na(bmi), median(bmi, na.rm = TRUE), bmi),
    waist = ifelse(is.na(waist), median(waist, na.rm = TRUE), waist)
  ) %>%
  ungroup()

#Imputing Family History

diabetes_data <- diabetes_data %>%
  mutate(
    age_group = cut(age, breaks = c(0, 40, 60, Inf), 
                    labels = c("<40", "40-60", "60+")),
    family_history = factor(family_history, levels = c("No", "Yes"))
  ) %>%
  group_by(age_group, ethnicity) %>%
  mutate(
    family_history = ifelse(
      is.na(family_history),
      get_mode(family_history),
      family_history
    )
  ) %>%
  ungroup()

#Handling Insulin and deleting a few rows:

diabetes_data <- diabetes_data %>% select(-insulin,) %>% filter(!is.na(carbs)) %>% 
  filter(!is.na(sedentary_minutes))

diabetes_data$diabetes <- factor(diabetes_data$diabetes)
diabetes_data$gender <- factor(diabetes_data$gender)
diabetes_data$ethnicity <- factor(diabetes_data$ethnicity)
diabetes_data$household_size <- factor(diabetes_data$household_size)
diabetes_data$activity <- factor(diabetes_data$activity)
diabetes_data$recreational_activity <- factor(diabetes_data$recreational_activity)
diabetes_data$alcohol_status <- factor(diabetes_data$alcohol_status)
diabetes_data$family_history <- factor(diabetes_data$family_history)


#Correlation Analysis:

numeric_data_diabetes <- diabetes_data[sapply(diabetes_data, is.numeric)]
cor_matrix_diabetes <- cor(numeric_data_diabetes, use = "complete.obs")
corrplot(cor_matrix_diabetes, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)

#Linearity Check (Box Plots)

# Create a list to store all boxplots
boxplot_list <- list()

# Define color palette

status_colors <- c("0" = "#4CAF50",  # Green
                   "1" = "#FFC107",  # Amber
                   "2" = "#F44336")  # Red

# Loop through all covariates (excluding response)
covariates <- setdiff(names(diabetes_data), "diabetes")

# Create and store boxplots
boxplot_list <- map(covariates, ~{
  if(is.numeric(diabetes_data[[.x]])) {
    # For continuous variables: Boxplot of values by diabetes status
    ggplot(diabetes_data, aes(x = diabetes, y = .data[[.x]], fill = diabetes)) +
      geom_boxplot(alpha = 0.8, outlier.shape = 1) +
      scale_fill_manual(values = status_colors) +
      labs(title = paste("Distribution of", .x, "by Diabetes Status"),
           x = "Diabetes Status",
           y = .x) +
      theme_minimal() +
      theme(legend.position = "none")
  }
}) %>% set_names(covariates)

#Assessing Normality

# Load required libraries
library(MASS)
library(bestNormalize)
# Load required packages
library(bestNormalize)

# Define variable names
var_names <- c(
  "income_poverty_ratio", "carbs", "saturated_fat", "protein", "fibre", "trans_fat",
  "polyunsaturated_fat", "vitamin_D", "magnesium", "sodium", "trigly", "ldl_chol",
  "total_chol", "hdl_chol", "creatinine", "bmi", "waist"
)

library(bestNormalize)

# Function to apply transformations and plot QQ plots
plot_transformations <- function(var, varname) {
  par(mfrow = c(3, 2))  # Layout for 6 plots
  
  # Original
  qqnorm(var, main = paste(varname))
  qqline(var, col = "red")
  
  # Log Transformation
  log_var <- log(var + 1)
  qqnorm(log_var, main = paste(varname, "- Log Transformation"))
  qqline(log_var, col = "red")
  
  # Inverse Transformation
  inv_var <- 1 / (var + 1)
  qqnorm(inv_var, main = paste(varname, "- Inverse Transformation"))
  qqline(inv_var, col = "red")
  
  # Box-Cox Transformation (shift if necessary)
  if (any(var <= 0)) {
    shift <- abs(min(var)) + 1
    var_for_bc <- var + shift
  } else {
    var_for_bc <- var
  }
  bc_obj <- boxcox(var_for_bc)
  bc_var <- predict(bc_obj)
  qqnorm(bc_var, main = paste(varname, "- Box Cox Transformation"))
  qqline(bc_var, col = "red")
  
  # Yeo-Johnson Transformation
  yj_obj <- yeojohnson(var)
  yj_var <- predict(yj_obj)
  qqnorm(yj_var, main = paste(varname, "- Yeo Johnson Transformation"))
  qqline(yj_var, col = "red")
}
for (var in var_names) {
  cat("\nPlotting:", var, "\n")
  plot_transformations(diabetes_data[[var]], var)
  readline(prompt = "Press [Enter] to continue to the next variable...")
}

# Loop over each variable and plot
for (var in var_names) {
  cat("\nPlotting:", var, "\n")
  plot_transformations(diabetes_data[[var]], var)
  readline(prompt = "Press [Enter] to continue to the next variable...")
}

#Variable Selection using Lasso:

library(glmnetcr)


y <- ordered(diabetes_data$diabetes)
X <- model.matrix(~ . - diabetes, data = diabetes_data)[,-1]
X_scaled <- scale(X)


set.seed(123)

lasso_fit_diabetes <- glmnetcr(X_scaled, y,
                      method = "backward", maxit=1000,pmax=100)

plot(lasso_fit_diabetes, xvar = "lambda", label = TRUE)

#Selecting Optimal Lambda:

bic.step<-select.glmnetcr(lasso_fit_diabetes)
coef(lasso_fit_diabetes,s=bic.step)

aic.step<-select.glmnetcr(lasso_fit_diabetes, which="AIC")
coef(lasso_fit_diabetes,s=aic.step)

plot(lasso_fit_diabetes, xvar = "step", type="bic")

#Splines

continuous_vars <- c("age", "income_poverty_ratio", "sedentary_minutes", 
                     "calorie", "carbs", "sugar", "fibre", "saturated_fat",
                     "trans_fat", "protein", "polyunsaturated_fat", "vitamin_D",
                     "magnesium", "sodium", "trigly", "ldl_chol", "total_chol",
                     "hdl_chol", "creatinine", "albumin", "bmi", "waist",
                     "systolic_bp", "diastolic_bp")

categorical_vars <- c("gender", "ethnicity", "household_size", "activity",
                      "recreational_activity", "alcohol_status", "family_history")


formula <- as.formula(
  paste("~", 
        paste0("bs(", continuous_vars, ", df=4)", collapse = " + "), 
        "+",
        paste(categorical_vars, collapse = " + "),
        "- 1"
  )
)

X_splines <- model.matrix(formula, data = diabetes_data)

X_scaled_splines <- scale(X_splines)


set.seed(123)

lasso_fit_diabetes_splines <- glmnetcr(X_scaled_splines, y,
                                       method = "backward", 
                                       maxit = 3000, 
                                       pmax = 150)


plot(lasso_fit_diabetes_splines, xvar = "lambda", label = TRUE)


bic.step_spline <- select.glmnetcr(lasso_fit_diabetes_splines)
aic.step_spline <- select.glmnetcr(lasso_fit_diabetes_splines, which="AIC")

coef(lasso_fit_diabetes_splines, s = bic.step_spline)
coef(lasso_fit_diabetes_splines, s = aic.step_spline)

plot(lasso_fit_diabetes_splines, xvar = "step", type="bic")

#Dicarding Variables:

diabetes_clean <- diabetes_data %>%
  select(-household_size, 
         -sedentary_minutes,
         -activity,
         -fibre,
         -protein,
         -vitamin_D,
         -magnesium,
         -sodium,
         -total_chol,
         -hdl_chol,
         -creatinine,
         -diastolic_bp)


diabetes_clean <- diabetes_clean %>%
  mutate(recreational_activity = case_when(
    recreational_activity == 1 ~ 0,         
    recreational_activity == 2 ~ 2,         
    recreational_activity == 0 ~ 0,         
    TRUE ~ NA_real_                          
  ))

diabetes_clean <- diabetes_clean %>%
  mutate(family_history = case_when(
    family_history == 1 ~ 0,         
    family_history == 2 ~ 1,
    TRUE ~ NA_real_                          
  ))

diabetes_clean <- diabetes_clean %>% select(-SEQN)

#Equality of Covariance matrices:

library(tidyverse)
library(dplyr)

numeric_covs<-diabetes_clean %>%
  select(-gender, -ethnicity, -recreational_activity, -alcohol_status, -family_history, -diabetes)


#permutation test
# --- Load required libraries ---
library(tidyverse)

# --- Prepare your data ---
# Keep only numeric predictors (exclude diabetes)
X <- as.matrix(numeric_covs)
labels <- diabetes_clean$diabetes
groups <- unique(labels)
P <- 1000  # Number of permutations

# --- Frobenius norm-based statistic function ---
frobenius_stat <- function(data, group_labels) {
  groups <- unique(group_labels)
  cov_mats <- lapply(groups, function(g) cov(data[group_labels == g, ]))
  total <- 0
  for (i in 1:(length(groups)-1)) {
    for (j in (i+1):length(groups)) {
      diff <- cov_mats[[i]] - cov_mats[[j]]
      total <- total + sum(diff^2)
    }
  }
  total
}

# --- Compute observed statistic ---
T_obs <- frobenius_stat(X, labels)

# --- Permutation test ---
T_perm <- replicate(10000, frobenius_stat(X, sample(labels)))


# --- Compute p-value ---
p_value <- mean(T_perm >= T_obs)

# --- Output results ---
cat("Observed statistic:", T_obs, "\n")
cat("Permutation p-value:", p_value, "\n")

# --- Plot the permutation distribution ---
hist(T_perm, breaks=30, main="Permutation Null Distribution",
     xlab="Test Statistic", col="lightblue", border="white")
abline(v=T_obs, col="red", lwd=2, lty=2)
legend("topright", legend=c("Observed Stat"), col="red", lty=2, lwd=2)

library(biotools)
# Make sure diabetes is a factor
diabetes <- as.factor()

# Run Box's M test
boxM_result <- boxM(numeric_covs, diabetes_clean$diabetes)

# View results
print(boxM_result)



#LDA and QDA:

library(caret)
library(pROC)


# Get class distribution
class_counts <- table(diabetes_clean$diabetes)
print(class_counts)

# Calculate inverse-frequency priors
prior_probs <- 1 / class_counts
prior_probs <- prior_probs / sum(prior_probs)  # Normalize
print(prior_probs)

#Splitting into training and testing data sets
set.seed(123)  
index <- createDataPartition(diabetes_clean$diabetes, p = 0.7, list = FALSE)
train_data <- diabetes_clean[index, ]
test_data <- diabetes_clean[-index, ]

#Performing QDA
qda_fit_diabetes <- qda(diabetes ~ ., data = train_data, prior = prior_probs)
qda_pred <- predict(qda_fit_diabetes, newdata = test_data)
cm_qda <- confusionMatrix(qda_pred$class, test_data$diabetes)

library(pROC)

#Creating the ROC curves
# For class "0"
roc_0_qda <- roc(response = as.numeric(test_data$diabetes == "0"),
             predictor = qda_pred$posterior[, "0"],
             levels = c(0, 1))

# For class "1"
roc_1_qda <- roc(response = as.numeric(test_data$diabetes == "1"),
             predictor = qda_pred$posterior[, "1"],
             levels = c(0, 1))

# For class "2"
roc_2_qda <- roc(response = as.numeric(test_data$diabetes == "2"),
             predictor = qda_pred$posterior[, "2"],
             levels = c(0, 1))

plot(roc_0_qda, col = "blue", lwd = 2, main = "Multiclass ROC Curve - QDA")
plot(roc_1_qda, col = "red", lwd = 2, add = TRUE)
plot(roc_2_qda, col = "darkgreen", lwd = 2, add = TRUE)
legend("bottomright", legend = c("Class 0", "Class 1", "Class 2"),
       col = c("blue", "red", "darkgreen"), lwd = 2)

#Printing the AUC values of each class
cat("AUC for Class 0 vs Rest:", auc(roc_0_qda), "\n")
cat("AUC for Class 1 vs Rest:", auc(roc_1_qda), "\n")
cat("AUC for Class 2 vs Rest:", auc(roc_2_qda), "\n")

#Performing LDA
lda_fit_diabetes <- lda(diabetes ~ ., data = train_data)
lda_pred <- predict(lda_fit_diabetes, newdata = test_data)
cm_lda <- confusionMatrix(lda_pred$class, test_data$diabetes)

#Creating the ROC curves
# For class "0"
roc_lda_0 <- roc(response = as.numeric(test_data$diabetes == "0"),
                 predictor = lda_pred$posterior[, "0"],
                 levels = c(0, 1))

# For class "1"
roc_lda_1 <- roc(response = as.numeric(test_data$diabetes == "1"),
                 predictor = lda_pred$posterior[, "1"],
                 levels = c(0, 1))

# For class "2"
roc_lda_2 <- roc(response = as.numeric(test_data$diabetes == "2"),
                 predictor = lda_pred$posterior[, "2"],
                 levels = c(0, 1))

plot(roc_lda_0, col = "blue", lwd = 2, main = "Multiclass ROC Curve - LDA")
plot(roc_lda_1, col = "red", lwd = 2, add = TRUE)
plot(roc_lda_2, col = "darkgreen", lwd = 2, add = TRUE)
legend("bottomright", legend = c("Class 0", "Class 1", "Class 2"),
       col = c("blue", "red", "darkgreen"), lwd = 2)

#Printing the AUC values for each class
cat("AUC for Class 0 vs Rest:", auc(roc_lda_0), "\n")
cat("AUC for Class 1 vs Rest:", auc(roc_lda_1), "\n")
cat("AUC for Class 2 vs Rest:", auc(roc_lda_2), "\n")

library(PRROC)

# Function to compute PR curves for multiclass
compute_pr_curves <- function(true_classes, posterior_probs) {
  classes <- levels(true_classes)
  pr_curves <- list()
  
  for (class in classes) {
    # Binary labels for current class
    binary_true <- as.numeric(true_classes == class)
    
    # Scores for current class
    class_scores <- posterior_probs[, class]
    
    # PR curve
    pr_curve <- pr.curve(
      scores.class0 = class_scores[binary_true == 1],
      scores.class1 = class_scores[binary_true == 0],
      curve = TRUE
    )
    
    pr_curves[[class]] <- pr_curve
  }
  return(pr_curves)
}

# Compute PR curves for QDA
pr_qda <- compute_pr_curves(test_data$diabetes, qda_pred$posterior)

# Plot QDA PR curves
plot(pr_qda[["0"]]$curve[,1], pr_qda[["0"]]$curve[,2], 
     type = "l", col = "blue", lwd = 2,
     xlab = "Recall", ylab = "Precision",
     main = "Precision-Recall Curves - QDA",
     xlim = c(0,1), ylim = c(0,1))
lines(pr_qda[["1"]]$curve[,1], pr_qda[["1"]]$curve[,2], 
      col = "red", lwd = 2)
lines(pr_qda[["2"]]$curve[,1], pr_qda[["2"]]$curve[,2], 
      col = "darkgreen", lwd = 2)
legend("topright", legend = c("Class 0", "Class 1", "Class 2"),
       col = c("blue", "red", "darkgreen"), lwd = 2)

# Print QDA AUPRC values
cat("\nQDA AUPRC Values:\n")
cat("Class 0:", pr_qda[["0"]]$auc.integral, "\n")
cat("Class 1:", pr_qda[["1"]]$auc.integral, "\n")
cat("Class 2:", pr_qda[["2"]]$auc.integral, "\n")

# Compute PR curves for LDA
pr_lda <- compute_pr_curves(test_data$diabetes, lda_pred$posterior)

# Plot LDA PR curves
plot(pr_lda[["0"]]$curve[,1], pr_lda[["0"]]$curve[,2], 
     type = "l", col = "blue", lwd = 2,
     xlab = "Recall", ylab = "Precision",
     main = "Precision-Recall Curves - LDA",
     xlim = c(0,1), ylim = c(0,1))
lines(pr_lda[["1"]]$curve[,1], pr_lda[["1"]]$curve[,2], 
      col = "red", lwd = 2)
lines(pr_lda[["2"]]$curve[,1], pr_lda[["2"]]$curve[,2], 
      col = "darkgreen", lwd = 2)
legend("topright", legend = c("Class 0", "Class 1", "Class 2"),
       col = c("blue", "red", "darkgreen"), lwd = 2)

# Print LDA AUPRC values
cat("\nLDA AUPRC Values:\n")
cat("Class 0:", pr_lda[["0"]]$auc.integral, "\n")
cat("Class 1:", pr_lda[["1"]]$auc.integral, "\n")
cat("Class 2:", pr_lda[["2"]]$auc.integral, "\n")

#Cross-Validation:

# Create 10 stratified folds
set.seed(123)
folds <- createFolds(diabetes_clean$diabetes, k = 10, list = TRUE, returnTrain = FALSE)

# Initialize storage for predictions - use NULL instead of empty factors/dataframes
actual_all <- NULL
predicted_lda_all <- NULL
predicted_qda_all <- NULL
lda_posterior_all <- NULL
qda_posterior_all <- NULL

for(i in 1:length(folds)) {
  train_data <- diabetes_clean[-folds[[i]], ]
  test_data <- diabetes_clean[folds[[i]], ]
  
  # Fit LDA with adjusted priors
  lda_model <- lda(diabetes ~ ., data = train_data)
  
  # Fit QDA with adjusted priors
  qda_model <- qda(diabetes ~ ., data = train_data)
  
  # Predictions
  lda_pred <- predict(lda_model, newdata = test_data)
  qda_pred <- predict(qda_model, newdata = test_data)
  
  # Store results - no need to remove initial values later
  actual_all <- c(actual_all, as.character(test_data$diabetes))
  predicted_lda_all <- c(predicted_lda_all, as.character(lda_pred$class))
  predicted_qda_all <- c(predicted_qda_all, as.character(qda_pred$class))
  
  # Store posterior probabilities
  if(is.null(lda_posterior_all)) {
    lda_posterior_all <- lda_pred$posterior
    qda_posterior_all <- qda_pred$posterior
  } else {
    lda_posterior_all <- rbind(lda_posterior_all, lda_pred$posterior)
    qda_posterior_all <- rbind(qda_posterior_all, qda_pred$posterior)
  }
}

# Convert to factors with consistent levels
actual_all <- factor(actual_all, levels = c("0", "1", "2"))
predicted_lda_all <- factor(predicted_lda_all, levels = c("0", "1", "2"))
predicted_qda_all <- factor(predicted_qda_all, levels = c("0", "1", "2"))

# Combined Confusion Matrices
lda_cm_cv <- confusionMatrix(predicted_lda_all, actual_all)
qda_cm_cv <- confusionMatrix(predicted_qda_all, actual_all)

# ROC and PR Curve Functions (unchanged)
plot_multiclass_roc <- function(true_classes, posterior_probs) {
  # ROC for each class
  roc_0 <- roc(response = as.numeric(true_classes == "0"),
               predictor = posterior_probs[, "0"],
               levels = c(0, 1))
  roc_1 <- roc(response = as.numeric(true_classes == "1"),
               predictor = posterior_probs[, "1"],
               levels = c(0, 1))
  roc_2 <- roc(response = as.numeric(true_classes == "2"),
               predictor = posterior_probs[, "2"],
               levels = c(0, 1))
  
  # Plot
  plot(roc_0, col = "blue", lwd = 2, main = "Multiclass ROC Curve")
  plot(roc_1, col = "red", lwd = 2, add = TRUE)
  plot(roc_2, col = "darkgreen", lwd = 2, add = TRUE)
  legend("bottomright", legend = c("Class 0", "Class 1", "Class 2"),
         col = c("blue", "red", "darkgreen"), lwd = 2)
  
  # Return AUC values
  return(list(
    auc_0 = auc(roc_0),
    auc_1 = auc(roc_1),
    auc_2 = auc(roc_2)
  ))
}

compute_pr_curves <- function(true_classes, posterior_probs) {
  classes <- levels(true_classes)
  pr_curves <- list()
  
  for (class in classes) {
    binary_true <- as.numeric(true_classes == class)
    class_scores <- posterior_probs[, class]
    
    pr_curve <- pr.curve(
      scores.class0 = class_scores[binary_true == 1],
      scores.class1 = class_scores[binary_true == 0],
      curve = TRUE
    )
    
    pr_curves[[class]] <- pr_curve
  }
  return(pr_curves)
}

# Plot ROC and PR curves for LDA
lda_roc_cv<- plot_multiclass_roc(actual_all, lda_posterior_all)


pr_lda_cv <- compute_pr_curves(actual_all, lda_posterior_all)
plot(pr_lda[["0"]]$curve[,1], pr_lda[["0"]]$curve[,2], 
     type = "l", col = "blue", lwd = 2,
     xlab = "Recall", ylab = "Precision",
     main = "LDA PR Curves (Cross-Validated)",
     xlim = c(0,1), ylim = c(0,1))
lines(pr_lda[["1"]]$curve[,1], pr_lda[["1"]]$curve[,2], 
      col = "red", lwd = 2)
lines(pr_lda[["2"]]$curve[,1], pr_lda[["2"]]$curve[,2], 
      col = "darkgreen", lwd = 2)
legend("topright", legend = c("Class 0", "Class 1", "Class 2"),
       col = c("blue", "red", "darkgreen"), lwd = 2)

# Plot ROC and PR curves for QDA

qda_roc_cv <- plot_multiclass_roc(actual_all, qda_posterior_all)
title("QDA ROC Curves (Cross-Validated)")

pr_qda_cv <- compute_pr_curves(actual_all, qda_posterior_all)
plot(pr_qda_cv[["0"]]$curve[,1], pr_qda_cv[["0"]]$curve[,2], 
     type = "l", col = "blue", lwd = 2,
     xlab = "Recall", ylab = "Precision",
     main = "QDA PR Curves (Cross-Validated)",
     xlim = c(0,1), ylim = c(0,1))
lines(pr_qda_cv[["1"]]$curve[,1], pr_qda_cv[["1"]]$curve[,2], 
      col = "red", lwd = 2)
lines(pr_qda_cv[["2"]]$curve[,1], pr_qda_cv[["2"]]$curve[,2], 
      col = "darkgreen", lwd = 2)
legend("topright", legend = c("Class 0", "Class 1", "Class 2"),
       col = c("blue", "red", "darkgreen"), lwd = 2)

# Print metrics
cat("LDA Performance (Cross-Validated):\n")
cat("ROC AUC - Class 0:", lda_roc_cv$auc_0, "\n")
cat("ROC AUC - Class 1:", lda_roc_cv$auc_1, "\n")
cat("ROC AUC - Class 2:", lda_roc_cv$auc_2, "\n")
cat("PR AUC - Class 0:", pr_lda_cv[["0"]]$auc.integral, "\n")
cat("PR AUC - Class 1:", pr_lda_cv[["1"]]$auc.integral, "\n")
cat("PR AUC - Class 2:", pr_lda_cv[["2"]]$auc.integral, "\n\n")

cat("QDA Performance (Cross-Validated):\n")
cat("ROC AUC - Class 0:", qda_roc_cv$auc_0, "\n")
cat("ROC AUC - Class 1:", qda_roc_cv$auc_1, "\n")
cat("ROC AUC - Class 2:", qda_roc_cv$auc_2, "\n")
cat("PR AUC - Class 0:", pr_qda_cv[["0"]]$auc.integral, "\n")
cat("PR AUC - Class 1:", pr_qda_cv[["1"]]$auc.integral, "\n")
cat("PR AUC - Class 2:", pr_qda_cv[["2"]]$auc.integral, "\n")


#Visualization with Fisher's Discriminant analysis:
library(MASS)

model_matrix <- model.matrix(diabetes ~ ., data = diabetes_clean)[,-1]  # Remove intercept
y <- diabetes_clean$diabetes

# Now run LDA on the encoded data
lda_fit <- lda(model_matrix, y)

lda_fit_diabetes <- lda(diabetes ~ ., data = diabetes_clean)

# Get group means (now properly aligned with dummy encoding)
group_means <- lda_fit$means
n_obs <- table(y)
global_mean <- colMeans(group_means)

# Initialize matrices
p <- ncol(model_matrix)
Sw <- matrix(0, p, p)
Sb <- matrix(0, p, p)

# Compute Sw and Sb
for (i in rownames(group_means)) {
  # Subset encoded data
  group_mask <- y == i
  X_group <- model_matrix[group_mask, ]
  
  # Center within group
  X_centered <- scale(X_group, center = group_means[i, ], scale = FALSE)
  
  # Update Sw
  Sw <- Sw + t(X_centered) %*% X_centered
  
  # Update Sb
  diff <- group_means[i, ] - global_mean
  Sb <- Sb + n_obs[i] * (diff %*% t(diff))
}

eig <- eigen(solve(Sw) %*% Sb)

# Extract top 2 discriminant directions
discriminant_directions <- Re(eig$vectors[, 1:2]) 
colnames(discriminant_directions) <- c("LD1", "LD2")

projected_data <- model_matrix %*% discriminant_directions
projected_df <- data.frame(projected_data, Diabetes = diabetes_clean$diabetes)

ggplot(projected_df, aes(x = LD1, y = LD2, color = Diabetes)) +
  geom_point(alpha = 0.6) +
  stat_ellipse(level = 0.95) +
  labs(title = "Fisher's LDA Projection", 
       x = "First Discriminant Direction (LD1)",
       y = "Second Discriminant Direction (LD2)") +
  theme_minimal()

#3d Plot:

discriminant_directions_3d <- Re(eig$vectors[, 1:3])
colnames(discriminant_directions_3d) <- c("LD1", "LD2", "LD3")


projected_data_3d <- model_matrix %*% discriminant_directions_3d
projected_df_3d <- data.frame(projected_data_3d, Diabetes = as.factor(y))

# Create 3D scatter plot
plot_ly(projected_df_3d, 
        x = ~LD1, y = ~LD2, z = ~LD3, 
        color = ~Diabetes, colors = c('#E41A1C', '#4DAF4A', '#377EB8'),
        type = "scatter3d", mode = "markers",
        marker = list(size = 4, opacity = 0.7)) %>%
  layout(title = "3D Fisher's LDA Projection",
         scene = list(
           xaxis = list(title = "LD1"),
           yaxis = list(title = "LD2"),
           zaxis = list(title = "LD3")
         ))


# 1. Data Preparation
model_matrix <- model.matrix(diabetes ~ ., data = train_data)[,-1]  # Remove intercept
y <- train_data$diabetes
groups <- c(0,1,2)  # Use actual group labels from your data

# 2. Single LDA Fit
lda_fit <- lda(model_matrix, y)
group_means <- lda_fit$means

# 3. Calculate Scatter Matrices
n_obs <- table(y)
global_mean <- colMeans(group_means)

p <- ncol(model_matrix)
Sw <- Sb <- matrix(0, p, p)

for (grp in rownames(group_means)) {
  # Within-class scatter
  X_grp <- model_matrix[y == grp, ]
  X_centered <- scale(X_grp, center = group_means[grp, ], scale = FALSE)
  Sw <- Sw + t(X_centered) %*% X_centered
  
  # Between-class scatter
  diff <- group_means[grp, ] - global_mean
  Sb <- Sb + n_obs[grp] * (diff %*% t(diff))
}

# 4. Eigen decomposition
eig <- eigen(solve(Sw) %*% Sb)
discriminant_directions <- eig$vectors[, 1:2]  # Top 2 directions
colnames(discriminant_directions) <- c("LD1", "LD2")
test_matrix <- model.matrix(diabetes ~ ., data = test_data)[,-1]  # Remove intercept
# 5. Projections
projected_data <- test_matrix %*% discriminant_directions
projected_means <- group_means %*% discriminant_directions

# 6. Distance calculation (corrected)
distances <- matrix(NA, nrow(test_matrix), length(groups))
for (i in seq_along(groups)) {
  diff <- sweep(projected_data, 2, projected_means[i, ], "-")
  distances[, i] <- sqrt(rowSums(diff^2))  # Correct Euclidean distance
}
colnames(distances) <- c(0,1,2)

# 7. Classification
predicted_class <- groups[apply(distances, 1, which.min)]
y_test<- test_data$diabetes

#cross validation for fisher's lda


library(MASS)
library(pROC)
library(ggplot2)

set.seed(123)
K <- 10
folds <- sample(rep(1:K, length.out = nrow(diabetes_clean)))

all_probs <- list()
all_true <- list()

for (k in 1:K) {
  train_data <- diabetes_clean[folds != k, ]
  test_data  <- diabetes_clean[folds == k, ]
  
  # Design matrices
  model_matrix <- model.matrix(diabetes ~ ., data = train_data)[, -1]
  test_matrix  <- model.matrix(diabetes ~ ., data = test_data)[, -1]
  y <- train_data$diabetes
  y_test <- test_data$diabetes
  groups <- sort(unique(y))  # Assuming 0, 1, 2
  
  # LDA Fit
  lda_fit <- lda(model_matrix, y)
  group_means <- lda_fit$means
  n_obs <- table(y)
  global_mean <- colMeans(group_means)
  p <- ncol(model_matrix)
  Sw <- Sb <- matrix(0, p, p)
  
  for (grp in rownames(group_means)) {
    X_grp <- model_matrix[y == grp, ]
    X_centered <- scale(X_grp, center = group_means[grp, ], scale = FALSE)
    Sw <- Sw + t(X_centered) %*% X_centered
    diff <- group_means[grp, ] - global_mean
    Sb <- Sb + n_obs[grp] * (diff %*% t(diff))
  }
  
  eig <- eigen(solve(Sw) %*% Sb)
  W <- eig$vectors[, 1:2]  # Top 2 linear discriminants
  projected_data <- test_matrix %*% W
  projected_means <- group_means %*% W
  
  # Distances to class means in LD space
  distances <- matrix(NA, nrow(test_matrix), length(groups))
  for (i in seq_along(groups)) {
    diff <- sweep(projected_data, 2, projected_means[i, ], "-")
    distances[, i] <- sqrt(rowSums(diff^2))
  }
  
  # Convert distances to probabilities
  probs <- 1 / (1 + distances)
  probs <- probs / rowSums(probs)
  colnames(probs) <- paste0("prob_", groups)
  
  # Save fold results
  all_probs[[k]] <- probs
  all_true[[k]] <- as.character(y_test)  # Ensure character for ROC
}

# Combine predictions from all folds
probs_all <- do.call(rbind, all_probs)
true_all <- unlist(all_true)
classes <- as.character(sort(unique(true_all)))

# ROC and AUC for each class
roc_list <- list()
for (cls in classes) {
  roc_list[[paste0("Class_", cls)]] <- roc(
    response = (true_all == cls),
    predictor = as.numeric(probs_all[, paste0("prob_", cls)]),
    levels = c(FALSE, TRUE)
  )
}

# Plot ROC curves
ggroc(roc_list, legacy.axes = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = paste(K, "Fold Cross-Validated ROC Curves"),
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red", "green")) +
  theme(legend.position = "bottom")

# Print AUCs
for (cls in classes) {
  cat("AUC for Class", cls, "vs Rest:", auc(roc_list[[paste0("Class_", cls)]]), "\n")
}



#Fitting a KNN Classifier:
library(caret)

#Scaling the continuous variables and factoring the categorical variables:

categorical_vars <- c("gender","ethnicity","recreational_activity","alcohol_status","family_history")
continuous_vars <- c("age","income_poverty_ratio","calorie","carbs","sugar", 
                     "saturated_fat","trans_fat","polyunsaturated_fat","trigly","ldl_chol","albumin",
                     "bmi","waist","systolic_bp")


diabetes_clean$gender <- as.factor(diabetes_clean$gender)
diabetes_clean$ethnicity <- as.factor(diabetes_clean$ethnicity)
diabetes_clean$recreational_activity <- factor(diabetes_clean$recreational_activity, ordered = TRUE)
diabetes_clean$alcohol_status <- factor(diabetes_clean$alcohol_status)
diabetes_clean$family_history <- factor(diabetes_clean$family_history)

diabetes_clean$diabetes <- factor(diabetes_clean$diabetes, ordered = TRUE)

preProc <- preProcess(diabetes_clean[,continuous_vars], 
                      method = c("center", "scale"))


diabetes_clean_std <- predict(preProc, diabetes_clean)



library(class)

set.seed(123)
trainIndex <- createDataPartition(diabetes_clean_std$diabetes, p = 0.8, list = FALSE)
trainData <- diabetes_clean_std[trainIndex, ]
testData <- diabetes_clean_std[-trainIndex, ]

dummies <- dummyVars(~ ., data = trainData[, categorical_vars])
trainX <- predict(dummies, trainData)
testX <- predict(dummies, testData)


knn_pred_diabetes <- knn(train = trainX, 
                test = testX, 
                cl = trainData$diabetes, 
                k = 19)

confusionMatrix(knn_pred_diabetes, testData$diabetes)


#Tuning Hyperparameter K:

ctrl <- trainControl(method = "cv", number = 20, classProbs = TRUE)

trainData$diabetes <- factor(make.names(trainData$diabetes))
testData$diabetes <- factor(make.names(testData$diabetes))

knnFit <- train(x = trainX,
                y = trainData$diabetes,
                method = "knn",
                trControl = ctrl,
                tuneLength = 20) 

plot(knnFit)
knnFit$bestTune
#19

