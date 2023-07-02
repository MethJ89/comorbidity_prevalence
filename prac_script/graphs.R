library(shiny)
runApp("~/Documents/GitHub/summer_thesis/scripts", display.mode = "showcase")

# runExample("01_hello")      # a histogram
# runExample("02_text")       # tables and data frames
# runExample("03_reactivity") # a reactive expression
# runExample("04_mpg")        # global variables
# runExample("05_sliders")    # slider bars
# runExample("06_tabsets")    # tabbed panels
# runExample("07_widgets")    # help text and submit buttons
# runExample("08_html")       # Shiny app built from HTML
# runExample("09_upload")     # file upload wizard
# runExample("10_download")   # file download wizard
# runExample("11_timer")      # an automated timer



library(tidyr)
library(ggplot2)

p1 <- ggplot(data, aes(x="age_group", y="comorbidity_prop", colour = 'region_cat')) +
  geom_line() +
  ggtitle("Morbidity Prevalance by Age Group")

p1
