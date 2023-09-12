
################  INSTALL AND LOAD PACKAGES ################  

devtools::install_github("ricardo-bion/ggradar", 
                         dependencies = TRUE)

install.packages("devtools")
install.packages("ggradar")
install.packages("tidyverse")
install.packages("maps")
install.packages("readxl")
install.packages("scales")
install.packages("maps")

library(ggplot2)
library(devtools)
library(ggradar)
library(tidyverse)
library(readxl)
library(scales)
library(maps)
library(dplyr)

################  IMPORT DATA ################  

# Import files 
Combined_comorbidity_age_region <- read_excel("~/Documents/GitHub/summer_thesis/data/Combined_comorbidity_age_region.xlsx")
bmi_cat <- read_excel("~/Documents/GitHub/summer_thesis/data/bmi_cat.xlsx")
ethnicity <- read_excel("~/Documents/GitHub/summer_thesis/data/ethnicity.xlsx")
sm_cat <- read_excel("~/Documents/GitHub/summer_thesis/data/sm_cat.xlsx")
# Rename file 
data <- Combined_comorbidity_age_region


################  CREATE GRAPHS ################  

####### Create Spider Graph called Data Radar ########  

data_radar <- data %>% 
  # Create new column for region and group morbidities by the regions
  mutate(group=region_cat) %>%
  group_by(group,comorbidity) %>%
  # Create new column calculating morbidity from proportion and denominator given 
  mutate(comorbidity_yes=comorbidity_prop*comorbidity_tot_non_miss) %>%
  # Summarise columns and ignore na values
  summarise(sum_comorbidity_yes = sum(comorbidity_yes, na.rm=TRUE), 
            sum_comorbidity_tot_non_miss = sum(comorbidity_tot_non_miss)) %>%
  # Calculate proportion for each morbidity
  mutate(prop = sum_comorbidity_yes/sum_comorbidity_tot_non_miss) %>%
  select(group,comorbidity,prop) %>%
  
  pivot_wider(names_from=comorbidity,values_from=prop) %>%
  ungroup() %>%
  select_if(~ !any(is.na(.))) %>%
  mutate(across(-1,rescale))

# Print graph
data_radar %>%
  head() %>%
  ggradar()

####### Create Scatter Plot ########  

data %>%
  group_by(region_cat,comorbidity, age_group) %>%
  mutate(comorbidity_yes=comorbidity_prop*comorbidity_tot_non_miss) %>%
  summarise(sum_comorbidity_yes = sum(comorbidity_yes), 
            sum_comorbidity_tot_non_miss = sum(comorbidity_tot_non_miss)) %>%
  mutate(prop = sum_comorbidity_yes/sum_comorbidity_tot_non_miss) %>%
  ggplot(aes(x=age_group,y=prop,size=sum_comorbidity_tot_non_miss,colour=region_cat)) +
  geom_point()


####### Create Stacked Bar Graph ########  

data %>%
  group_by(region_cat,comorbidity, age_group) %>%
  mutate(comorbidity_yes=comorbidity_prop*comorbidity_tot_non_miss) %>%
  summarise(sum_comorbidity_yes = sum(comorbidity_yes), 
            sum_comorbidity_tot_non_miss = sum(comorbidity_tot_non_miss)) %>%
  mutate(prop = sum_comorbidity_yes/sum_comorbidity_tot_non_miss) %>%
  ggplot(aes(fill=comorbidity, y=prop, x=region_cat)) + 
  geom_bar(position="stack", stat="identity") +
  coord_flip() 



####### Create Heat Map ########  

worldmap = map_data('world')
ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, 
                   y = lat, 
                   group = group)) + 
  coord_fixed(ratio = 1.3, 
              xlim = c(-10,3), 
              ylim = c(50, 59))


g = data %>% ggplot(aes(y=comorbidity_prop, x=reorder(comorbidity, comorbidity_prop), fill = region_cat)) + 
  geom_bar(stat="identity" , position = position_dodge()) 
g



gg = data %>%
  filter(region == "National") %>%
  group_by(region_cat,comorbidity) %>%
  mutate(comorbidity_yes=comorbidity_prop*comorbidity_tot_non_miss) %>%
  summarise(sum_comorbidity_yes = sum(comorbidity_yes), 
            sum_comorbidity_tot_non_miss = sum(comorbidity_tot_non_miss)) %>%
  mutate(prop = sum_comorbidity_yes/sum_comorbidity_tot_non_miss) %>%
  ggplot(aes(y=prop, x=reorder(comorbidity, prop), fill = region_cat)) + 
  geom_bar(stat="identity" , position = position_dodge()) 
gg




ggg = data %>%
    group_by(region_cat,comorbidity) %>%
  ggplot(aes(y=comorbidity_prop, x=reorder(comorbidity, comorbidity_prop), fill = region_cat)) + 
  geom_bar(stat="identity" , position = position_dodge()) 
ggg




g2 = ggplot(data, aes(x = region_cat, y = comorbidity_prop, fill = comorbidity)) +
  geom_bar(stat = "identity") +
  labs(title = "Comorbidity Prevalence by Region",
       x = "Region",
       y = "Comorbidity Prevalence") +
  theme_minimal()
g2


comorbidity_data <- data %>%
  filter(comorbidity == "Liver") %>%
  select(region_cat, comorbidity_prop)

heatmap_data <- pivot_wider(comorbidity_data, id_cols = region_cat, names_from = region_cat, values_from = comorbidity_prop)

ggplot(heatmap_data, aes(x = region_cat, y = region_cat, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +  # Adjust color scale
  labs(title = "Comorbidity Prevalence Heatmap by Region",
       x = "Region",
       y = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability



ggplot(data, aes(x = region_cat, y = comorbidity, fill = comorbidity_prop)) +
  geom_tile() +
  labs(title = "Comorbidity Prevalence by Region",
       x = "Region",
       y = "Comorbidity") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




