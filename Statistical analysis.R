#install and load packages
install.packages("webshot")
webshot::install_phantomjs()
library(knitr)
library(kableExtra)
library(webshot)
library(gt)
library(tidyr)
library(ggplot2)

# Descriptive statistics
#Group data by marital_status, sex and age group
plot_data <- cen_data %>%
  group_by(Marital_status, SEX, Age_group) %>%
  summarise(count = n()) %>%
  ungroup()

# Pivot the data to spread it by Marital_status and SEX
pivot_data <- plot_data %>%
  pivot_wider(names_from = c(Marital_status, SEX), values_from = count, values_fill = 0)

# Calculate row totals
pivot_data <- pivot_data %>%
  mutate(Total = rowSums(select(., -Age_group)))

# Calculate column totals
col_totals <- pivot_data %>%
  summarise(across(-Age_group, sum)) %>%
  mutate(Age_group = "Total")

# Combine the data with totals
final_data <- bind_rows(pivot_data, col_totals)

# Create table
descriptive_table <- kable(final_data, format = "html", caption = "Marital Status, Sex, and Age Group Distribution") %>%
  kable_styling(full_width = FALSE, position = "left", html_font = "Arial") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#0073e6") %>%  # Header styling
  column_spec(1, bold = TRUE) %>%  # Make the first column bold
  add_header_above(c(" " = 1, "Married/Living with a common law partner" = 2, "Divorced/Separated" = 2, "Total" = 1)) %>%
  row_spec(1:nrow(final_data), extra_css = "border: 1px solid black;") %>%
  row_spec(nrow(final_data), bold = TRUE, color = "white", background = "#0073e6")  # Style the total row

# Save the table as an HTML file
save_kable(descriptive_table, file = "C:/Users/MCS/Documents/GitHub/CAnD3/CAnD3_project1_research_reproducibility/Reproducableresearch1/R/Output/descriptive_table.html")

# Logistic regression Analysis --------------------------------------------

# Re-code Marital_status as binary variables in a new column "Divorced_Seperated"
cen_data <- cen_data %>%
  mutate(Divorced_Separated = case_when(
    Marital_status == " Divorced/Separated" ~ 1,
    Marital_status == "Married/Living with a common law partner" ~ 0))

#Confirm unique levels of Divorced_Seperated
unique(cen_data$Divorced_Separated)
# Logistic regression model
names(cen_data)
model <- glm(Divorced_Separated ~ SEX + Age_group, data = cen_data, family = binomial)

# Summary of the model
summary(model)
exp(coef(model))

#create a function to extract model result
odds_data <- function(model) {
  coef_table <- summary(model)$coefficients
  odds <- exp(coef_table[-1, "Estimate"])  # Exclude intercept
  ci_lower <- exp(coef_table[-1, "Estimate"] - 1.96 * coef_table[-1, "Std. Error"])
  ci_upper <- exp(coef_table[-1, "Estimate"] + 1.96 * coef_table[-1, "Std. Error"])
  p_values <- coef_table[-1, "Pr(>|z|)"]

  data.frame(
    Variable = rownames(coef_table)[-1],
    OR = odds,
    CI_lower = ci_lower,
    CI_upper = ci_upper,
    p_value = p_values
  )
}

# Use the odds_data function to get the data from the logistic model
model_data <- odds_data(model)

#Plot model result using ggplot

model_plot <- ggplot(model_data, aes(y = Variable, x = OR)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  scale_x_log10() +
  labs(title = "Divorce/Separation\nOdds Ratios and 95% Confidence Intervals by Sex",
       x = "Odds Ratio (log scale)",
       y = "") +
  theme(
    panel.background = element_blank(),   # Remove the panel background
    plot.background = element_blank(),     # Remove the plot background
    panel.grid.major = element_blank(),    # Remove major grid lines
    panel.grid.minor = element_blank(),    # Remove minor grid lines
    axis.line = element_line(color = "black"), # Add axis lines if desired
    plot.title = element_text(hjust = 0.5), # Center the title
    plot.subtitle = element_text(hjust = 0.5) # Center the subtitle
  )+
  geom_text(aes(label = sprintf("%.2f (%.2f-%.2f)", OR, CI_lower, CI_upper)),
            hjust = -0.2, vjust = 0.5, size = 3)


# Save the plot
ggsave("C:/Users/MCS/Documents/GitHub/CAnD3/CAnD3_project1_research_reproducibility/Reproducableresearch1/R/Output/odds_ratio_plot.png", plot = model_plot, width = 10, height = 6, dpi = 300)


 # The result suggests that being male is associated with a 37% decrease in the odds of being divorced/separated compared to being married/living with a common law partner.
