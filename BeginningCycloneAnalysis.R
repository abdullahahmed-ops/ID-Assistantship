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

#
#
#

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

#
#
#
#

str(post_cyclone)

# Inner join based on caseid
merged_data <- pre_cyclone %>%
  inner_join(post_cyclone, by = "caseid")

# Check the number of observations
nrow(merged_data)
# This will return the number of rows where 'caseid' exists in both data sets


