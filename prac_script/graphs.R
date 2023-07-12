
# Load  packages
library(shiny)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(readxl)


# Check local working directory
getwd()

# Import files 
Combined_comorbidity_age_region <- read_excel("../Data/Combined_comorbidity_age_region.xlsx")
bmi_cat <- read_excel("../Data/bmi_cat.xlsx")
ethnicity <- read_excel("../Data/ethnicity.xlsx")
sm_cat <- read_excel("../Data/sm_cat.xlsx")

# Rename file 
data <- Combined_comorbidity_age_region

# Run saved script in showcase mode
#runApp("~/Documents/GitHub/summer_thesis/shiny_scripts", display.mode = "showcase")

# Summarise data
summary(data)

# Convert age_group and region_cat from 'character' to factor column type 
data$age_group <- as.factor(data$age_group)

# Fig 2 first panel plots cancer prevalence by age in 2019
## Filter data to relevant subset
subset <- data %>%
  filter(year==2019 & comorbidity=="cancerlastyr" &
           region_cat %in% c("Scotland", "Wales", "Northern Ireland", "West Midlands"))
# West Midlands included as combined 'England' data not extracted in current data

## Generate plot
g1 = ggplot(subset, aes(x=age_group, y=comorbidity_prop, colour = region_cat, group = region_cat)) + # Covariates cannot be in " " in this line!
  geom_line() +
  ggtitle("Cancer (previous 1y)") + 
  theme_bw() +
  xlab("Age (years) : 2-9 yrs; 5 year age bands; 90-99 yrs") +
  ylab("Prevalence/100,000") +
  labs(colour="") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
g1
# Print graph 


# Change comorbidity_yes column from character to numeric 
# Some values (<5) have been converted to NA
data$comorbidity_yes <- as.numeric(data$comorbidity_yes)
data$region_cat <- as.factor(data$region_cat)
data$comorbidity <- as.factor(data$comorbidity)

# Checking number of categories 
table(data$age_group)
table(data$region_cat)
table(data$comorbidity)
# Found two bmi categories in comorbidities: 'bmi_40' and 'bmi40'

# Changed all 'bmi_40' to 'bmi40' to merge into one category 
data$comorbidity[data$comorbidity == 'bmi_40'] <- 'bmi40'

# Create histogram for morbidities with age group 
ggplot(data, aes(x="age_group", y="comorbidity_yes")) +
  geom_boxplot() +
  ggtitle("Morbidity Prevalance by Age Group")
# Print graph 
p1


# Create histogram for morbidities with age group 
p2 <- ggplot(ethnicity, aes(x="eth5_cat")) +
  geom_histogram()
# Print graph 
p2



################################################################
################################################################
 
# Example Plots 

# First test 
p1 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet, group=Chick)) +
  geom_line() +
  ggtitle("Growth curve for individual chicks")
p1

# Second test
p2 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet)) +
  geom_point(alpha=.3) +
  geom_smooth(alpha=.2, size=1) +
  ggtitle("Fitted growth curve per diet")
p2

# Third test
p3 <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, colour=Diet)) +
  geom_density() +
  ggtitle("Final weight, by diet")
p3

# Fourth test
p4 <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, fill=Diet)) +
  geom_histogram(colour="black", binwidth=50) +
  facet_grid(Diet ~ .) +
  ggtitle("Final weight, by diet") +
  theme(legend.position="none")    
p4

# Plot all graphs at once
multiplot(p1, p2, p3, p4, cols=2)


# Create multiplot function 
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


