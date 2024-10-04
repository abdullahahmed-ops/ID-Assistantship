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
library(ggmosaic)

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
chi_square_test_age <- chisq.test(age_table)

# Print the result of the chi-square test
print(chi_square_test_age)

# P-value is extremely small (essentially zero) thus we reject the null hyp
# Thus, the age distribution is significantly different between those who
# were re-interviewed, and those who were not 




# Analysis now for sex of the respondent
look_for(merged_data, "sex")
# Step 1: Summarize the distribution of sex by reinterview status
sex_summary <- merged_data %>%
  group_by(reinterviewed_binary, pre_sex) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Print the summarized sex distribution
print(sex_summary)


# Step 2: Create a contingency table for the chi-square test
sex_table <- table(merged_data$pre_sex, merged_data$reinterviewed_binary)

# Perform Chi-square test to compare sex distribution across reinterview status
chi_square_test_sex <- chisq.test(sex_table)

# Print the result of the chi-square test
print(chi_square_test_sex)

# The chi-square test (X-squared = 23.989, p-value = 9.686e-07) shows a statistically
# significant difference in the distribution of sex between those reinterviewed and
# those not reinterviewed





# Electricity
look_for(merged_data, "pre_Electricity")





# Step 1: Summarize the distribution of electricity access by reinterview status
electricity_summary <- merged_data %>%
  group_by(reinterviewed_binary, pre_Electricity) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Print the summarized electricity distribution
print(electricity_summary)

# Step 2: Create a contingency table for the chi-square test
electricity_table <- table(merged_data$pre_Electricity, merged_data$reinterviewed_binary)

# Perform Chi-square test to compare electricity access across reinterview status
chi_square_test_electricity <- chisq.test(electricity_table)

# Print the result of the chi-square test
print(chi_square_test_electricity)

# Step 3: Visualizing the electricity distribution by reinterview status
ggplot(merged_data, aes(x = factor(pre_Electricity), fill = factor(reinterviewed_binary))) +
  geom_bar(position = "dodge") +  # Side-by-side bars for each electricity status
  labs(title = "Electricity Access by Reinterview Status",
       x = "Electricity Access",
       y = "Count",
       fill = "Reinterviewed (1 = Yes, 0 = No)") +
  scale_x_discrete(labels = c("1" = "Yes", "2" = "No")) +
  theme_minimal() +  # Clean plot theme
  theme(plot.title = element_text(hjust = 0.5))  # Center the title


# Added plots thus far for electricity access by reinterview status and
# age distribution by reinterview status

# chi_square_test_electricity results:
### X-squared = 29.728, df = 1, and p-value = 4.971e-08.
# The p-value is extremely small, indicating that there is a statistically 
# significant difference in electricity access between individuals who were
# reinterviewed and those who were not.

# Thus, those without electricity are more likely to have been reinterviewed
# compared to those with electricity
# ?



# Water
look_for(merged_data, "pre_Water")
# 1 = unprotected source
# 2 = protected source

# Step 1: Summarize the distribution of water source by reinterview status
water_summary <- merged_data %>%
  group_by(reinterviewed_binary, pre_Water) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Print the summarized water source distribution
print(water_summary)

# Step 2: Create a contingency table for the chi-square test
water_table <- table(merged_data$pre_Water, merged_data$reinterviewed_binary)

# Perform Chi-square test to compare water source across reinterview status
chi_square_test_water <- chisq.test(water_table)

# Print the result of the chi-square test
print(chi_square_test_water)

# Step 3: Visualizing the water source distribution by reinterview status
ggplot(merged_data, aes(x = factor(pre_Water), fill = factor(reinterviewed_binary))) +
  geom_bar(position = "dodge") +  # Side-by-side bars for each water source
  labs(title = "Water Source by Reinterview Status",
       x = "Water Source",
       y = "Count",
       fill = "Reinterviewed (1 = Yes, 0 = No)") +
  scale_x_discrete(labels = c("1" = "Unprotected", "2" = "Protected")) +
  theme_minimal() +  # Clean plot theme
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

# Simple stacked bar plot
ggplot(merged_data, aes(x = factor(reinterviewed_binary), fill = factor(pre_Water))) +
  geom_bar(position = "stack") +  # Standard stacked bar plot
  labs(title = "Water Source by Reinterview Status",
       x = "Reinterviewed Status (1 = Yes, 0 = No)",
       y = "Count",
       fill = "Water Source") +
  scale_fill_manual(values = c("#2b83ba", "#d7191c"),  # Simple color palette
                    labels = c("Unprotected", "Protected")) +
  theme_minimal() +  # Clean and minimalistic theme
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))  # Centered title






# Chi-square Test Results:
# X-squared = 10.927, df = 1, and p-value = 0.0009475.
# The small p-value indicates a statistically significant difference in the 
# distribution of water source between those reinterviewed and those not reinterviewed

# Thus, households with protected water sources were significantly more likely
# to be reinterviewed than those with unprotected sources


look_for(merged_data, "pre_UR")
# 1 = urban
# 2 = rural

# Step 1: Summarize the distribution of place of residence by reinterview status
residence_summary <- merged_data %>%
  group_by(reinterviewed_binary, pre_UR) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Print the summarized place of residence distribution
print(residence_summary)

# Step 2: Create a contingency table for the chi-square test
residence_table <- table(merged_data$pre_UR, merged_data$reinterviewed_binary)

# Perform Chi-square test to compare place of residence across reinterview status
chi_square_test_residence <- chisq.test(residence_table)

# Print the result of the chi-square test
print(chi_square_test_residence)

# Step 3: Visualizing the place of residence distribution by reinterview status
ggplot(merged_data, aes(x = factor(pre_UR), fill = factor(reinterviewed_binary))) +
  geom_bar(position = "dodge") +  # Side-by-side bars for each residence type
  labs(title = "Place of Residence by Reinterview Status",
       x = "Place of Residence",
       y = "Count",
       fill = "Reinterviewed (1 = Yes, 0 = No)") +
  scale_x_discrete(labels = c("1" = "Urban", "2" = "Rural")) +
  theme_minimal() +  # Clean plot theme
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

# p-value is very high - no statistically significant difference
# in the place of residence between
# those who were reinterviewed and those who were not


look_for(merged_data, "pre_household")


# 1 = 0-4 members
# 2 = 5-8 members
# 3 = 9+ members

# Step 1: Summarize the distribution of household size by reinterview status
household_summary <- merged_data %>%
  group_by(reinterviewed_binary, pre_household) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Print the summarized household size distribution
print(household_summary)

# Step 2: Create a contingency table for the chi-square test
household_table <- table(merged_data$pre_household, merged_data$reinterviewed_binary)

# Perform Chi-square test to compare household size across reinterview status
chi_square_test_household <- chisq.test(household_table)

# Print the result of the chi-square test
print(chi_square_test_household)

# Updated visualization to show percentages
ggplot(merged_data, aes(x = factor(pre_household), fill = factor(reinterviewed_binary))) +
  geom_bar(position = "fill") +  # This will scale the bars to show proportions (percentages)
  labs(title = "Household Size by Reinterview Status (Percentages)",
       x = "Household Size",
       y = "Proportion",
       fill = "Reinterviewed (1 = Yes, 0 = No)") +
  scale_y_continuous(labels = scales::percent_format()) +  # Convert y-axis to percentages
  scale_x_discrete(labels = c("1" = "0-4 members", "2" = "5-8 members", "3" = "9+ members")) +
  theme_minimal() +  # Clean plot theme
  theme(plot.title = element_text(hjust = 0.5))  # Center the title


# Household size seems to have a statistically significant impact on whether
# individuals were reinterviewed. In particular, households with 5-8 members were
# more likely to be reinterviewed compared to the other categories

