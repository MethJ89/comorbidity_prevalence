
################  INSTALL AND LOAD PACKAGES ################  

# Load  packages
library(shiny)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(readxl)

################  IMPORT & CLEAN DATA ################  

# Import files 
Combined_comorbidity_age_region <- read_excel("~/Documents/GitHub/summer_thesis/data/Combined_comorbidity_age_region.xlsx")
bmi_cat <- read_excel("~/Documents/GitHub/summer_thesis/data/bmi_cat.xlsx")
ethnicity <- read_excel("~/Documents/GitHub/summer_thesis/data/ethnicity.xlsx")
sm_cat <- read_excel("~/Documents/GitHub/summer_thesis/data/sm_cat.xlsx")
# Rename file 
data <- Combined_comorbidity_age_region

# Run saved script in showcase mode
# runApp("~/Documents/GitHub/summer_thesis/shiny_scripts", display.mode = "showcase")

# Summarise 'data' 
summary(data)

# Change comorbidity_yes column from character to numeric 
# Some values (<5) have been converted to NA
data$comorbidity_yes <- as.numeric(data$comorbidity_yes)

# Convert age_group and region_cat from 'character' to factor column type 
data$age_group <- as.factor(data$age_group)
data$region_cat <- as.factor(data$region_cat)
data$comorbidity <- as.factor(data$comorbidity)

# Checking number of categories 
table(data$age_group)
table(data$region_cat)
table(data$comorbidity)
# Found two bmi categories in comorbidities: 'bmi_40' and 'bmi40'

# Changed all 'bmi_40' to 'bmi40' to merge into one category 
data$comorbidity[data$comorbidity == 'bmi_40'] <- 'bmi40'


################ CREATE GRAPHS ################  

######## Fig 2 Plots: Comorbidity Prevalence by Age in 2019 ######## 

######## Cancer (previous 1 yr) ########

## Filter data to relevant subset 
subset_can1y <- data %>%
  filter(year==2019 & comorbidity=="cancerlastyr" &
           region_cat %in% c("Scotland", "Wales", "Northern Ireland", "West Midlands"))
# West Midlands included as combined 'England' data not extracted in current data

## Generate plot
can1y = ggplot(subset_can1y, aes(x=age_group, y=comorbidity_prop, colour = region_cat, group = region_cat)) + 
  geom_line() +
  ggtitle("Cancer (previous 1y)") + 
  theme_bw() +
  xlab("Age (years) : 2-9 yrs; 5 year age bands; 90-99 yrs") +
  ylab("Prevalence/100,000") +
  labs(colour="") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
# Print graph 
can1y

######## Cancer (previous 5 yr) ########

## Filter data to relevant subset 
subset_can5y <- data %>%
  filter(year==2019 & comorbidity=="cancerlast5yrs" &
           region_cat %in% c("Scotland", "Wales", "Northern Ireland", "West Midlands"))
# West Midlands included as combined 'England' data not extracted in current data

## Generate plot
can5y = ggplot(subset_can5y, aes(x=age_group, y=comorbidity_prop, colour = region_cat, group = region_cat)) + 
  geom_line() +
  ggtitle("Cancer (previous 5y)") + 
  theme_bw() +
  xlab("Age (years) : 2-9 yrs; 5 year age bands; 90-99 yrs") +
  ylab("Prevalence/100,000") +
  labs(colour="") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
# Print graph 
can5y


######## Chronic Heart Disease ########

## Filter data to relevant subset 
subset_heart <- data %>%
  filter(year==2019 & comorbidity=="heart" &
           region_cat %in% c("Scotland", "Wales", "Northern Ireland", "West Midlands"))
# West Midlands included as combined 'England' data not extracted in current data

## Generate plot
heart = ggplot(subset_heart, aes(x=age_group, y=comorbidity_prop, colour = region_cat, group = region_cat)) + 
  geom_line() +
  ggtitle("Chronic Heart Disease") + 
  theme_bw() +
  xlab("Age (years) : 2-9 yrs; 5 year age bands; 90-99 yrs") +
  ylab("Prevalence/100,000") +
  labs(colour="") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
# Print graph 
heart


######## Current Asthma ########

## Filter data to relevant subset 
subset_asthma <- data %>%
  filter(year==2019 & comorbidity=="asthma_ever" &
           region_cat %in% c("Scotland", "Wales", "Northern Ireland", "West Midlands"))
# West Midlands included as combined 'England' data not extracted in current data

## Generate plot
asthma = ggplot(subset_asthma, aes(x=age_group, y=comorbidity_prop, colour = region_cat, group = region_cat)) + 
  geom_line() +
  ggtitle("Asthma Ever") + 
  theme_bw() +
  xlab("Age (years) : 2-9 yrs; 5 year age bands; 90-99 yrs") +
  ylab("Prevalence/100,000") +
  labs(colour="") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
# Print graph 
asthma



######## Diabetes Mellitus ########

## Filter data to relevant subset 
subset_diab <- data %>%
  filter(year==2019 & comorbidity=="diabetes" &
           region_cat %in% c("Scotland", "Wales", "Northern Ireland", "West Midlands"))
# West Midlands included as combined 'England' data not extracted in current data

## Generate plot
diabetes = ggplot(subset_diab, aes(x=age_group, y=comorbidity_prop, colour = region_cat, group = region_cat)) + 
  geom_line() +
  ggtitle("Diabetes Mellitus") + 
  theme_bw() +
  xlab("Age (years) : 2-9 yrs; 5 year age bands; 90-99 yrs") +
  ylab("Prevalence/100,000") +
  labs(colour="") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
# Print graph 
diabetes


######## Chronic Kidney Disease ########

## Filter data to relevant subset 
subset_ckd <- data %>%
  filter(year==2019 & comorbidity=="ckd" &
           region_cat %in% c("Scotland", "Wales", "Northern Ireland", "West Midlands"))
# West Midlands included as combined 'England' data not extracted in current data

## Generate plot
ckd = ggplot(subset_ckd, aes(x=age_group, y=comorbidity_prop, colour = region_cat, group = region_cat)) + 
  geom_line() +
  ggtitle("Chronic Kidney Disease") + 
  theme_bw() +
  xlab("Age (years) : 2-9 yrs; 5 year age bands; 90-99 yrs") +
  ylab("Prevalence/100,000") +
  labs(colour="") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
# Print graph 
ckd


######## Multimorbidity (excluding cancer & obesity) ########

## Filter data to relevant subset 
subset_multi <- data %>%
  filter(year==2019 & comorbidity=="multi_prev" &
           region_cat %in% c("Scotland", "Wales", "Northern Ireland", "West Midlands"))
# West Midlands included as combined 'England' data not extracted in current data

## Generate plot
multi = ggplot(subset_multi, aes(x=age_group, y=comorbidity_prop, colour = region_cat, group = region_cat)) + 
  geom_line() +
  ggtitle("Multimorbidity") + 
  theme_bw() +
  xlab("Age (years) : 2-9 yrs; 5 year age bands; 90-99 yrs") +
  ylab("Prevalence/100,000") +
  labs(colour="") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
# Print graph 
multi


######## Any condition or severe obesity) ########

## Filter data to relevant subset 
subset_any <- data %>%
  filter(year==2019 & comorbidity %in% c('nyonecond_prev' , 'anyonecond_prevbmi' , 'asthma_ever' , 'asthma_specific' , 'bmi_40' , 'cancerever' ,
                                         'cancerlast5yrs' , 'cancerlastyr' ,  'ckd' , 'diabetes','heart' , 'immuno' , 'immuno_no_si_sp' ,
                                         'liver' , 'lung'  , 'multi_prev' , 'neuro' , 'organ_tx' , 'si_sp') &
           region_cat %in% c("Scotland", "Wales", "Northern Ireland", "West Midlands"))
# West Midlands included as combined 'England' data not extracted in current data

## Generate plot
any = ggplot(subset_multi, aes(x=age_group, y=comorbidity_prop, colour = region_cat, group = region_cat)) + 
  geom_line() +
  ggtitle("Any Condition") + 
  theme_bw() +
  xlab("Age (years) : 2-9 yrs; 5 year age bands; 90-99 yrs") +
  ylab("Prevalence/100,000") +
  labs(colour="") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
# Print graph 
any


## Plot all graphs at once
multiplot(asthma, can1y, can5y, ckd, diabetes, heart, multi, any, cols = 4)


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


######## Pie Chart ########

# Create Data
prac_data <- data.frame(
  group=LETTERS[1:5],
  value=c(13,7,9,21,2)
)

# Compute the position of labels
prac_data <- prac_data %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(prac_data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Basic piechart
ggplot(prac_data, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  
  geom_text(aes(y = ypos, label = group), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")




subset_pie <- data %>%
  filter(year==2019 & comorbidity=="heart" &
           region_cat %in% c("Scotland", "Wales", "Northern Ireland", "England"))


ggplot(data, aes(x="", y=comorbidity_yes, fill=comorbidity)) +
  geom_bar(stat="identity",width=1) +
  coord_polar("y", start=0)
























