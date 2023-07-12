
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


# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/


library(shiny)

# Define UI ----
ui <- fluidPage(
    titlePanel("Morbidity Prevalance Graphs"),

    sidebarLayout(
        sidebarPanel(
            helpText("Create demographic maps with 
               information from the 2010 US Census."),
            
            selectInput("var", 
                        label = "Choose a year to plot",
                        choices = list("2014", 
                                       "2019"),
                        selected = "2014"),
            
             checkboxGroupInput("checkGroup", 
                                h3("Regions"), 
                                choices = list("England" = 1, 
                                               "Scotland" = 2,
                                               "Wales" = 3,
                                               "N.Ireland" = 4),
                                selected = 1),
            
            sliderInput("range",
                        label = "Age Range",
                        min = 0, max = 100, value = c(0, 100))
        ),
        
        mainPanel(
            img(src = "LSHTM.jpeg", height = 120, width = 300))
    )
)

# Define server logic ----
server <- function(input, output) {

}

# Run the app ----
shinyApp(ui = ui, server = server)


