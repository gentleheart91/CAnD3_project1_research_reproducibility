#Load libraries
library(dplyr)
#Load data
census_data <- read.csv("C:/Users/MCS/Downloads/CAND3 Data/GSS 2017 - Family, Cycle 31/gss-12M0025-E-2017-c-31_F1.csv")
head(census_data)
names(census_data)

# Select variables of interest; age group,  marital status and sex
cen_data <- census_data %>%
  select("AGEGR10" ,"SEX" , "MARSTAT"  )

# Exclude participants with missing values on MARSTAT (98 don't know, 98 = Refusal )
unique(cen_data$MARSTAT)
str(cen_data)
cen_data <- cen_data %>%
  filter(!MARSTAT %in% c(98, 97 ) )
unique(cen_data$MARSTAT)

# Re-code sex variables, 1 = male and 2 = female
cen_data <- cen_data %>%
  mutate(SEX = case_when(
    SEX == 1 ~ "Male",
    SEX == 2 ~ " Female"
  ))

# Re-code Age Group variables, 1 = "15 - 24" and 2 = "25 - 34", 3 = "35 - 44", 4 = "45 - 54", 5 =  "55 - 64", 6 = "65 - 74" and  7 = "Age 75+
cen_data <- cen_data %>%
  mutate( Age_group = case_when(
    AGEGR10 == 1 ~ "15 - 24",
    AGEGR10 == 2 ~ "25 - 34",
    AGEGR10 == 3 ~ "35 - 44",
    AGEGR10 == 4 ~ "45 - 54",
    AGEGR10 == 5 ~ "55 - 64",
    AGEGR10 == 6 ~ "65 - 74",
    AGEGR10 == 7 ~ "Age 75+"
  ))
# Check unique levels of marital status.
unique(cen_data$MARSTAT)
#Exclude 3 = widowed and 6 = single never married
cen_data <- cen_data %>%
  filter(!MARSTAT %in% c(3,6) )
#Recheck unique levels of marital status
unique(cen_data$MARSTAT)
# Re-group marital status into : married/living with common law partner and divorced/separated. where 1 =  married, 2 = living with common law partner,4 = separated and 5 = divorced.
cen_data <- cen_data %>%
  mutate( Marital_status = case_when(
    MARSTAT %in% c(1,2)  ~ "Married/Living with a common law partner",
    MARSTAT %in% c(4,5) ~ " Divorced/Separated",
  ))
unique(cen_data$Marital_status)
str(cen_data)
