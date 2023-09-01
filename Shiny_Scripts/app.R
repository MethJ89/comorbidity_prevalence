
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
                 theme = shinytheme("lumen"),
                 title = "LSHTM",
                 
                 #### HOME PAGE ####
                 
                 ## Create main page to briefly describe dashboard and its content 
                 tabPanel("Home",
                          icon = icon("home"),
                          # h2("Main Page Content"),
                          # p("Welcome to Main Page"),
                          # icon = icon("dashbaord"),
                          # img(src = "https://via.placeholder.com/150", 
                          #     style = "float: right; margin-top: 10px; margin-right: 5px;"),
                    
                          fluidRow(
                            column(8,
                                   wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 500px;",
                                             fluidRow(style = "margin-top: 5px;",
                                                      h2("R Shiny Dashboard"),
                                                      p("Dashboard based on the UK prevalence of underlying conditions which increase the risk of severe COVID-19 disease: a point prevalence study using electronic health records ")
                                   ))),
                            
                            
                            column(4,
                                  wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 200px;",
                                            fluidRow(style = "margin-top: 1px;",
                                                     div(style = "float: right; margin-top: 5px; margin-right: 5px;",
                                                         img(src = "https://www.lshtm.ac.uk/sites/default/files/LSHTM-logo-bw.jpg", 
                                                             height = "150", width = "250",
                                                             alt = "Logo")
                                            ))),
                            
                            )
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
                              h4("Line Graph for Comorbidity Prevalance by Age-Group"),
                              tags$br(),
                              "Age distribution of at-risk population and underlying health conditions, N = 2,706,053",
                              tags$br(),
                              "Filter by Year, Comorbidity and Region Type",
                              tags$br(),tags$br(),
                                 div(style = "border: 2px solid #333;",
                                   plotlyOutput("interactive_fig1", height="300px", width="650px")),
                              p("Further information can be found in here: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7948667/table/Tab2/")

                            )
                            
                          ),  
                          
                          ## Add some background colour to tab
                          style = "background-color: #e8f5e9;"
                          
                 ),
                 
                 
                 #### SECOND TAB ####
                 
                 tabPanel("Graph 2",
                          icon = icon("bar-chart"),
                          
                          sidebarLayout(
                            
                            ## Side bar panel for parameters
                            sidebarPanel(
                              width = 3,
                              
                              radioButtons("input_year2", label = h3("Select year"),
                                           choices = list("2014", "2019"), 
                                           selected = 2014),
                              
                              ## EP: line below needs to be updated to avoid duplication of 'input_region' input
                              selectInput("input_region" , label = h3("Select Region"),
                                          choices = list("National" = "National",
                                                         "Local" = "Local"),
                                          selected = "national"),

                            ),
                            
                            ## Create main panel for plots
                            mainPanel(
                              h4("Stacked Bar Graph"),
                              tags$br(),
                              "Graph 2",
                              tags$br(),tags$br(),
                              div(style = "border: 2px solid #333;",
                              plotlyOutput("interactive_fig2", height="300px", width="650px")),
                              
                            )
                            
                          ),
                          
                          ## Add some background colour to tab
                          style = "background-color: #e8f5e9;"
                 ),
                 
                 
                 #### THIRD TAB ####
                 
                 ## Third tab with third graph 
                 tabPanel("Table",
                          icon = icon("table"),
                          
                        
                          ## Add some background colour to tab
                          style = "background-color: #e8f5e9;"
                 ),
                 
)

# Define server logic ----
server <- function(input, output) {

    ## Create data subset for plot 1 ##
      data_subset_one = reactive({ 
        subset(data, year==input$input_year & comorbidity==input$input_morbidity & region==input$input_region)
    })
    
      ## Create data subset for plot 2 ##
      ## EP: reactive component below needs to be updated to look at the relevant inputs - input_year2 & input_region2
      ## Do you want to add an input_morbidity2 dropdown?
      data_subset_two = reactive({
        subset(data, year==input$input_year & comorbidity==input$input_morbidity & region==input$input_region)
      })

    
    ###### PLOT 1 ######

      # Regional prevalence plot
    output$interactive_fig1 <- renderPlotly({
        ## Read in output from reactive statement above
        plot_one = data_subset_one()
                # Static version for developing plot code
        
        
        g1 = ggplot(plot_one, aes(x=age_group, y=comorbidity_prop, colour = region_cat, group = region_cat,
                                   # Specify hover info here: this can be as simple/complex as you like 
                                   text=paste0("Prevalance ", round(comorbidity_prop,1), "\n",
                                               "Region: ", region_cat, "\n",
                                               "Age Group: ", age_group))) +
            geom_line() +
            theme_bw() +
            xlab("Age (years) : 2-9 yrs; 5 year age bands; 90-99 yrs") +
            ylab("Prevalence/100,000") +
            labs(colour="") +
            theme(text = element_text(size=12), axis.text.x=element_text(angle = 45, hjust = 1))
        
        ggplotly(g1, tooltip = 'text')
    })
    
    
    ###### PLOT 2 ######
  
      output$interactive_fig2 <- renderPlotly({

         plot_two = data_subset_two()
        
        g2 = ggplot(plot_two, aes(y=comorbidity_prop, x=region_cat, fill=comorbidity,
                                  text=paste0("Prevalance ", round(comorbidity_prop,1), "\n",
                                              "Region: ", region_cat))) +
          geom_bar(position="stack", stat="identity") +
          coord_flip() +
          xlab("Region") +
          ylab("Prevalence/100,000") 
        
        ## EP comment: Need to render object g2 using 'ggplotly' (as above) to enable interaction
         
        
      })
      
    # output$interactive_fig2 <- renderPlotly({
    # 
    #   g2 = data %>%
    #     group_by(region_cat,comorbidity, age_group) %>%
    #     mutate(comorbidity_yes=comorbidity_prop*comorbidity_tot_non_miss) %>%
    #     summarise(sum_comorbidity_yes = sum(comorbidity_yes),
    #             sum_comorbidity_tot_non_miss = sum(comorbidity_tot_non_miss)) %>%
    #     mutate(prop = sum_comorbidity_yes/sum_comorbidity_tot_non_miss) %>%
    # 
    #      ggplot(aes(fill=comorbidity, y=prop, x=region_cat,
    #                 text=paste0("Hover info:\n",round(prop,1)))) +
    # 
    #     geom_bar(position="stack", stat="identity") +
    #     coord_flip()
    # 
    #   ggplotly(g2, tooltip = 'text')
    # 
    # })
    
    ###### PLOT 3 ######
    
   
    
}

# Run the app ----
shinyApp(ui = ui, server = server)

