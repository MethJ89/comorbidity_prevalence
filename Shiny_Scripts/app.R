
### Load  packages
library(shiny)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(shinythemes)
library(plotly)


### Load data
Combined_comorbidity_age_region <- read_excel("../Data/Combined_comorbidity_age_region.xlsx")
bmi_cat <- read_excel("../Data/bmi_cat.xlsx")
ethnicity <- read_excel("../Data/ethnicity.xlsx")
sm_cat <- read_excel("../Data/sm_cat.xlsx")

# Rename file 
data <- Combined_comorbidity_age_region

# Create comorbidity list 
# comorbidity_list = as.list(unique(data$comorbidity))
# comorbidity_list <- list('nyonecond_prev' , 'anyonecond_prevbmi' , 'asthma_ever' , 'asthma_specific' , 'bmi_40' , 'cancerever' , 'cancerlast5yrs' , 'si_sp' ,
#                          'cancerlastyr' , 'ckd' , 'diabetes', 'heart' , 'immuno' , 'immuno_no_si_sp' , 'liver' , 'lung' , 'multi_prev' , 'neuro' , 'organ_tx') 

# Define UI ----
ui <- navbarPage(windowTitle = "Window title",
                 
                 title = "Dashboard",
                 
                 #### MAIN PAGE ####
                 
                 ## Create main page to briefly describe dashboard and its content 
                 tabPanel("Home",
                          h2("Main Page Content"),
                          p("Welcome to Main Page"),
                          icon = icon("dashbaord"),
                          img(src = "https://via.placeholder.com/150", 
                              style = "float: right; margin-top: 10px; margin-right: 5px;"),
                    
                          fluidPage(
                            h2("Tab 1 Content"),
                            
                            
                            
                            # fluidRow(
                            #   column(8,
                            #          p("This is the content for Tab 1.")
                            #   ),
                            #   column(4,
                            #          img(src = "https://via.placeholder.com/150", 
                            #              style = "float: right;")
                            #   )
                            # )
                          ),
                          
                          style = "background-color:#e1f5fe;"
                 ),
                 
                 
                 
                 #### FIRST TAB ####
                 
                 ## First tab with first graph 
                 tabPanel(title = "Graph 1",
                          icon = icon("chart-line"),
                          
                          
                          sidebarLayout(
                            
                            ## Side bar panel for parameters
                            sidebarPanel(
                              width = 3,
                              ## Create radio button for year
                              radioButtons("input_year", label = h3("Select year"),
                                           choices = list("2014", "2019"), 
                                           selected = 2014),
                              
                              ## Create selectInput for comorbidity
                              selectInput("input_morbidity", label = h3("Select Comorbditity"), 
                                          choices = list(
                                            # Input requires a list where the label is on the left and the 'value' held on selection is on the right
                                            # This list could be created as an object in the introductory code
                                            "Liver" = "liver",
                                            "Heart" = "heart",
                                            "Lung" = "lung",
                                            "Diabetes" = "diabetes",
                                            "Chronic Kidney Disease" = "ckd",
                                            "Immunosuppression" = "immuno",
                                            "Chronic neurological disease" = "neuro",
                                            "Any history of asthma" = "asthma_ever",
                                            "Current asthma (no COPD)" = "asthma_specific",
                                            "Severe obesity" = "bmi_40",
                                            "Any history of cancer" = "cancerever",
                                            "Cancer (last 5 years)" = "cancerlast5yrs",
                                            "Cancer (last year)" = "cancerlastyr",
                                            "Cancer (last 6 months)" = "cancerlast6months",
                                            "Dysplenia (including sickle cell disease)" = "si_sp",
                                            "Any 'higher risk' health condition" = "nyonecond_prev",
                                            "Any 'higher risk' risk factor" = "anyonecond_prevbmi",
                                            "Immunosuppression excluding dysplenia" = "immuno_no_si_sp",
                                            "Organ transplant recipient" = "organ_tx",
                                            "Multimorbidity" = "multi_prev"),
                                          selected = "liver"),
                              
                              ## Create selectInput for region
                              selectInput("input_region" , label = h3("Select Region"),
                                          choices = list("National" = "National",
                                                         "Local" = "Local"),
                                          selected = "national")
                              
                            ),
                         
                            ## Create main panel for plots
                            mainPanel(
                              h4("Add plot heading here"),
                              tags$br(),
                              "Graph 1",
                              tags$br(),tags$br(),
                                 div(style = "border: 2px solid #333;",
                                   plotlyOutput("interactive_fig2", height="300px", width="650px")),

                            )
                            
                          ),  
                          
                          ## Add some background colour to tab
                          style = "background-color: #e8f5e9;"
                          
                 ),
                 
                 #### SECOND TAB ####
                 
                 ## Second tab with second graph 
                 tabPanel("Graph 2",
                          icon = icon("chart-line"),
                          
                          sidebarLayout(
                            
                            ## Side bar panel for parameters
                            sidebarPanel(
                              width = 3,
                              ## Create radio button for year
                              radioButtons("input_year", label = h3("Select year"),
                                           choices = list("2014", "2019"), 
                                           selected = 2014),
                              
                              ## Create selectInput for region
                              selectInput("input_region" , label = h3("Select Region"),
                                          choices = list("National" = "National",
                                                         "Local" = "Local"),
                                          selected = "national")
                            ),
                            
                            ## Create main panel for plots
                            mainPanel(
                              h4("Scatter Plot"),
                              tags$br(),
                              "Graph 2",
                              tags$br(),tags$br(),
                              div(style = "border: 2px solid #333;",
                              plotlyOutput("interactive_fig3", height="300px", width="650px")),
                              
                            )
                            
                          ),  
                          
                          ## Add some background colour to tab
                          style = "background-color: #e8f5e9;"
                 ),

                 
                 #### THIRD TAB ####
                 
                 tabPanel("Graph 3",
                          icon = icon("chart-line"),
                          
                          sidebarLayout(
                            
                            ## Side bar panel for parameters
                            sidebarPanel(
                              width = 3,

                            ),
                            
                            ## Create main panel for plots
                            mainPanel(
                              h4("Stacked Bar Graph"),
                              tags$br(),
                              "Graph 3",
                              tags$br(),tags$br(),
                              div(style = "border: 2px solid #333;",
                              plotlyOutput("interactive_fig4", height="300px", width="650px")),
                              
                            )
                            
                          ),
                          
                          ## Add some background colour to tab
                          style = "background-color: #e8f5e9;"
                 ),
                 
)

# Define server logic ----
server <- function(input, output) {
  
      data_subset = reactive({ 
        subset(data, year==input$input_year & comorbidity==input$input_morbidity & region==input$input_region)
    })
    
    
    ###### PLOT 1 ######

      # Regional prevalence plot
    output$interactive_fig2 <- renderPlotly({
        ## Read in output from reactive statement above
        plot_data = data_subset()
                # Static version for developing plot code
        # plot_data = subset(data, year==2014 & comorbidity=="lung")
        
        g1 = ggplot(plot_data, aes(x=age_group, y=comorbidity_prop, colour = region_cat, group = region_cat,
                                   # Specify hover info here: this can be as simple/complex as you like 
                                   text=paste0("Hover info:\n",round(comorbidity_prop,1)))) +
            geom_line() +
            theme_bw() +
            xlab("Age (years) : 2-9 yrs; 5 year age bands; 90-99 yrs") +
            ylab("Prevalence/100,000") +
            labs(colour="") +
            theme(text = element_text(size=12), axis.text.x=element_text(angle = 45, hjust = 1))
        
        ggplotly(g1, tooltip = 'text')
    })
    
    ###### PLOT 2 ######

      # Scatter plot
    output$interactive_fig3 <- renderPlotly({
    
     g2 = data %>%
        group_by(region_cat,comorbidity, age_group) %>%
        mutate(comorbidity_yes=comorbidity_prop*comorbidity_tot_non_miss) %>%
        summarise(sum_comorbidity_yes = sum(comorbidity_yes), 
                sum_comorbidity_tot_non_miss = sum(comorbidity_tot_non_miss)) %>%
        mutate(prop = sum_comorbidity_yes/sum_comorbidity_tot_non_miss) %>%
        ggplot(aes(x=age_group,y=prop,size=sum_comorbidity_tot_non_miss,colour=region_cat)) +
        xlab("Age Group") +
        ylab("Comorbidity Prevalance") +
        geom_point()
    
     ggplotly(g2, tooltip = 'text')
     
    })
    
    
    ###### PLOT 3 ######
  
    output$interactive_fig4 <- renderPlotly({ 
      
      g3 = data %>%
        group_by(region_cat,comorbidity, age_group) %>%
        mutate(comorbidity_yes=comorbidity_prop*comorbidity_tot_non_miss) %>%
        summarise(sum_comorbidity_yes = sum(comorbidity_yes), 
                sum_comorbidity_tot_non_miss = sum(comorbidity_tot_non_miss)) %>%
        mutate(prop = sum_comorbidity_yes/sum_comorbidity_tot_non_miss) %>%
        ggplot(aes(fill=comorbidity, y=prop, x=region_cat)) + 
        geom_bar(position="stack", stat="identity") +
        coord_flip() 
      
      ggplotly(g3, tooltip = 'text')
      
    })
    
     
    
}

# Run the app ----
shinyApp(ui = ui, server = server)

