
# Load  packages
library(shiny)
library(tidyr)
library(ggplot2)

# Import files 
Combined_comorbidity_age_region <- read_excel("Documents/GitHub/summer_thesis/data/Combined_comorbidity_age_region.xlsx")
bmi_cat <- read_excel("Documents/GitHub/summer_thesis/data/bmi_cat.xlsx")
ethnicity <- read_excel("Documents/GitHub/summer_thesis/data/ethnicity.xlsx")
sm_cat <- read_excel("Documents/GitHub/summer_thesis/data/sm_cat.xlsx")
# Rename file 
data <- Combined_comorbidity_age_region

# Run saved script in showcase mode
runApp("~/Documents/GitHub/summer_thesis/scripts", display.mode = "showcase")

# Summarise 'data' 
summary(data)

# Change comorbidity_yes column from character to numeric 
# Some values (<5) have been converted to NA
data$comorbidity_yes <- as.numeric(data$comorbidity_yes)

# Create histogram for morbidities with age group 
p1 <- ggplot(data, aes(x="age_group", y="comorbidity_yes", colour = 'region_cat')) +
  geom_histogram() +
  ggtitle("Morbidity Prevalance by Age Group")
# Print graph 
p1


# Checking number of categories 
table(data$age_group)


