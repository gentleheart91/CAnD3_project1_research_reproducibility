
library(knitr)
library(kableExtra)
library(webshot2)
library(gt)
library(tidyr)
library(ggplot2)
library(dplyr)
library(broom)
#Load data
census_data <- read.csv("C:/Users/MCS/Downloads/CAND3 Data/GSS 2017 - Family, Cycle 31/gss-12M0025-E-2017-c-31_F1.csv")
names(census_data)
unique(census_data$MARSTAT)
#Recode variable MARSTAT and name the new variable MARTSTAT2
census_data <- census_data %>%
  mutate(MARSTAT2 = case_when (
        MARSTAT == 1 ~ "married/common-law",
         MARSTAT == 2 ~ "married/common-law",
         MARSTAT == 3 ~ "single now/previously married",
         MARSTAT == 4 ~ "single now/previously married",
         MARSTAT == 5 ~ "single now/previously married",
         MARSTAT == 6 ~  "never married",
         TRUE ~ NA_character_
  ))
#Drop “NA” from MARSTAT
census_data <- census_data %>%
  filter(!is.na(MARSTAT2) )
unique(census_data$MARSTAT2)

#Create a table for the new recoded variable MARTSTAT21
MARTSAT2_tabl <- table(census_data$MARSTAT2)

# Convert to a data frame and save as CSV
marstat2_df <- as.data.frame(MARTSAT2_tabl)
unique(census_data$AGEC)
#  save as as csv
write.csv(marstat2_df, "C:/Users/MCS/Documents/GitHub/CAnD3/CAnD3_project1_research_reproducibility/Reproducableresearch1/R/Output/MARSTAT2_frequency_table.csv", row.names = FALSE)

#Create regression analysis
reg <- lm(AGEC ~ SEX + MARSTAT2, data = census_data )
summary(reg)
# Capture the summary of the regression model
reg_summary <- summary(reg)

# Save the summary to a text file
capture.output(reg_summary, file = "C:/Users/MCS/Documents/GitHub/CAnD3/CAnD3_project1_research_reproducibility/Reproducableresearch1/R/Output/regression_summary.txt")

# Extract the coefficients and confidence intervals from the model
tidy_reg <- tidy(reg, conf.int = TRUE)

# Plot the coefficients with 95% confidence intervals
reg_plot <- ggplot(tidy_reg, aes(x = term, y = estimate)) +
  geom_point(size = 3) +  # Points for the estimates
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # Error bars for CI
  labs(title = "Regression Coefficients with 95% Confidence Intervals",
       x = "Predictor",
       y = "Coefficient Estimate") +
  theme_minimal() +
  coord_flip()  # Flip for horizontal view, if preferred
# Save the plot
ggsave("C:/Users/MCS/Documents/GitHub/CAnD3/CAnD3_project1_research_reproducibility/Reproducableresearch1/R/Output/reg_plot.png", plot = reg_plot, width = 10, height = 6, dpi = 300)
