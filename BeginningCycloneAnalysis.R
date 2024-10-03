# Data manipulation & wrangling
library(dplyr)      # Data manipulation
library(tidyr)      # Data reshaping
library(data.table) # Fast manipulation & merging

# Reading Stata data
library(haven)      # Import .dta files
library(labelled)   # Use Stata metadata

# Data visualization
library(ggplot2)    # Graphs & plots
library(ggpubr)     # Publication-ready plots

# Stats & models
library(stats)      # Basic stats
library(broom)      # Tidy model outputs

# Data merging
library(dplyr)      # Also for dataset joins

# Summary tables
library(janitor)    # Crosstabs & cleaning
library(gtsummary)  # Summary tables

# Date & string handling
library(lubridate)  # Dates & times
library(stringr)    # String manipulation

# Exporting
library(writexl)    # Export to Excel
library(readr)      # Read/write CSV

# Reporting
library(rmarkdown)  # Dynamic reports

# Advanced visualization
library(patchwork)  # Combine ggplots

# Machine learning (optional)
library(caret)      # ML models
library(randomForest) # Random forests

pre_cyclone <- read_dta("C:/Users/abdul/Desktop/ID Assistantship/ID Assistantship/pre_cyclone_baseline.dta")
# pre-cyclone does indeed have 8,909 observations
# 23 columns (variables)

str(pre_cyclone)
# Several values are categorical but have been encoded as numeric with labels such as
# pre_sex, pre_Electricity, etc.: these must be interpreted properly when analyzing
# Sampling weights, such as pre_WR and pre_WT, give different weights to different individuals
# Each variable has name, type, and attributes - detailed data written in master Word file

View(pre_cyclone)
# caseid variable uniquely identifies an individual in both data sets

unique(pre_cyclone$pre_outcome)
# "Completed interview", "Partially complete (no callback)", "Incomplete (callback)"

duplicates <- pre_cyclone$caseid[duplicated(pre_cyclone$caseid)]
pre_cyclone[pre_cyclone$caseid %in% duplicates, ]
# no duplicates in caseid

outcome_counts <- pre_cyclone %>%
  count(pre_outcome)
print(outcome_counts)
# completed interview: 8,796
# Incomplete (callback): 1
# Partially complete (no callback): 112

# Since incomplete (callback) is only one individual, we remove that row
pre_cyclone <- pre_cyclone %>%
  filter(pre_outcome != "Incomplete (callback)")

# Checking if the above code snippet worked
outcome_counts <- pre_cyclone %>%
  count(pre_outcome)
print(outcome_counts)

# Exploring completed vs. partially completed individuals to see if there are any demographic or 
# household characteristics associated with incomplete interviews

# 1. Filter out relevant cases (Completed and Partially complete)
compare_data <- pre_cyclone %>%
  filter(pre_outcome %in% c("Completed interview", "Partially complete (no callback)"))

# 2. Create a new column to label 'Completed' vs 'Partially complete (no callback)'
compare_data <- compare_data %>%
  mutate(outcome_group = ifelse(pre_outcome == "Completed interview", "Completed", "Partially complete"))

# 3. Summarize demographic characteristics (sex, age, education, etc.) by outcome group
# Example 1: Summarize categorical data (sex)
table_sex <- compare_data %>%
  group_by(outcome_group, pre_sex) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Print the summary
print(table_sex)

# Example 2: Summarize numeric data (age)
summary_age <- compare_data %>%
  group_by(outcome_group) %>%
  summarise(mean_age = mean(pre_age, na.rm = TRUE),
            median_age = median(pre_age, na.rm = TRUE),
            sd_age = sd(pre_age, na.rm = TRUE))

# Print the summary
print(summary_age)

# In the completed group, the gender split is balanced, but in the partially complete group,
# females are much more likely to have incomplete interviews (78.6% female vs. 21.4% male)
# Possible reasons for this?

partially_complete_rows <- pre_cyclone %>%
  filter(pre_outcome == "Partially complete (no callback)")

# View the filtered rows
View(partially_complete_rows)

# Partially complete rows have data in all variables










post_cyclone = read_dta("C:/Users/abdul/Desktop/ID Assistantship/ID Assistantship/post_cyclone_followup.dta")
str(post_cyclone)
# 5,218 rows (survey responses) and 584 columns (variables/questions)
# chr: character strings
# num: numbers
# POSIXct: date-time
# Other attributes in the data listed in the master word document

# for post-cyclone
# Add a column to check if the interview was completed based on key variables
post_cyclone <- post_cyclone %>%
  mutate(interview_completed = ifelse(
    is.na(starttime) | is.na(endtime) | is.na(submissiondate), "Incomplete", "Completed"))

# Check the distribution of completed vs. incomplete interviews
table(post_cyclone$interview_completed)

# 5,218 completed interviews, 5,218 obs. in post_cyclone






# Creating a data set containing individuals who were interviewed before and after
# Inner join based on caseid
interviewed_before_and_after <- pre_cyclone %>%
  inner_join(post_cyclone, by = "caseid")

# Check the number of observations
nrow(interviewed_before_and_after)
# This will return the number of rows where 'caseid' exists in both data sets







# Creating new variable to indicate if reinterviewed
# Left join to keep all individuals from pre-cyclone data
merged_data <- pre_cyclone %>%
  left_join(post_cyclone, by = "caseid")

# Check the number of observations
nrow(merged_data)

# Create a new variable 'reinterviewed' based on non-missing values in 'starttime', 'endtime', and 'submissiondate'
merged_data <- merged_data %>%
  mutate(reinterviewed = ifelse(!is.na(starttime) & !is.na(endtime) & !is.na(submissiondate), 
                                "Reinterviewed", "Not Reinterviewed"))

# Check the distribution of reinterviewed vs not reinterviewed
table(merged_data$reinterviewed)
# Not Reinterviewed: 3,690
# Reinterviewed: 5,218
# Reinterviewed number matches number of rows in post-cyclone dataset 


# Check if any reinterviewed individuals have missing values for 'starttime', 'endtime', or 'submissiondate'
reinterviewed_check <- merged_data %>%
  filter(reinterviewed == "Reinterviewed") %>%
  summarise(missing_starttime = sum(is.na(starttime)),
            missing_endtime = sum(is.na(endtime)),
            missing_submissiondate = sum(is.na(submissiondate)))


# Add both text and binary versions of the 'reinterviewed' variable
merged_data <- merged_data %>%
  mutate(reinterviewed_text = ifelse(!is.na(starttime) & !is.na(endtime) & !is.na(submissiondate), 
                                     "Reinterviewed", "Not Reinterviewed"),
         reinterviewed_binary = ifelse(!is.na(starttime) & !is.na(endtime) & !is.na(submissiondate), 1, 0))

# Check the distribution
table(merged_data$reinterviewed_binary)
# 1 for reinterviewed, 0 for not reinterviewed

merged_data <- merged_data %>%
  select(-reinterviewed)

table(merged_data$pre_GO)



# Create custom numeric age categories
merged_data <- merged_data %>%
  mutate(age_category_numeric = cut(pre_age, 
                                    breaks = c(18, 24, 34, 44, 54, 64, Inf), 
                                    labels = c(1, 2, 3, 4, 5, 6),  # Numeric labels
                                    right = FALSE))

# Check the distribution of the numeric age categories
table(merged_data$age_category_numeric)
# age_category_numeric contains the age ranges
# 1 = 18-24, 2 = 25-34, 3 = 35-44, 4 = 45-54, 5 = 55-64, 6 = 65+

# Visualizing 
# Create a bar plot to visualize the age distribution by reinterview status
ggplot(merged_data, aes(x = factor(age_category_numeric), fill = factor(reinterviewed_binary))) +
  geom_bar(position = "dodge") +  # Side-by-side bars for each category
  labs(title = "Age Distribution by Reinterview Status",
       x = "Age Category",
       y = "Count",
       fill = "Reinterviewed (1 = Yes, 0 = No)") +
  scale_x_discrete(labels = c("1" = "18-24", "2" = "25-34", "3" = "35-44", "4" = "45-54", "5" = "55-64", "6" = "65+")) +
  theme_minimal() +  # Clean plot theme
  theme(plot.title = element_text(hjust = 0.5))  # Center the title




# Step 1: Summarize the age distribution by reinterview status
age_summary <- merged_data %>%
  group_by(reinterviewed_binary, age_category_numeric) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Print the summarized age distribution
print(age_summary)



# Step 2: Create a contingency table for the chi-square test
age_table <- table(merged_data$age_category_numeric, merged_data$reinterviewed_binary)

# Perform Chi-square test to compare age distribution across reinterview status
chi_square_test <- chisq.test(age_table)

# Print the result of the chi-square test
print(chi_square_test)

# P-value is extremely small (essentially zero) thus we reject the null hyp
# Thus, the age distribution is significantly different between those who
# were re-interviewed, and those who were not 





