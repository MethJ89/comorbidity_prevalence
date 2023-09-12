
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

# Define UI ----
ui <- fluidPage(

  
  navbarPage(windowTitle = "Window title",
                 theme = shinytheme("flatly"),
                 title = "LSHTM",
                 
                 #### HOME PAGE ####
                 
                 ## Create main page to briefly describe dashboard and its content 
                 tabPanel("Home",
                          icon = icon("home"),
                   
                          fluidRow(
                            column(8,
                                   wellPanel(style = "background-color: #f0f0f0; border-color: #2c3e50; height: 900px;",
                                             fluidRow(style = "margin-top: 0px;",
                                                      h2("COVID-19 Comorbidity Prevalance Dashboard"),
                                                      p("Dashboard based on study which looked at the UK prevalence of underlying conditions which increase the risk of severe COVID-19 disease: a point prevalence study using electronic health records"),
                                                      tags$br(),
                                                      h4("Summary"),
                                                      h5("During the start of the COVID-19 pandemic, characterizing the size and distribution of the population who were at severe risk of catching the disease was vitally important for effective policy and planning. The study aimed to describe this at-risk population"),
                                                      h4("Aims"),
                                                      h5("The main aim of the study was to identify the UK prevalence of underlying conditions which increase the risk of severe COVID-19 disease (1)."),
                                                      
                                                      h4("Methods"),
                                                      h5("The study was carried out using anonymised electronic health records from the Clinical Practice Datalink GOLD (3). This data was used to estimate the point prevalence of the at-risk population on 5 March 2019, in line with national guidance. Prevalence for any risk condition and for each individual condition is given overall and stratified by age and region with binomial exact confidence intervals. The analysis was then repeated on 5 March 2014 in order to achieve full regional representation (1)."),
                                                      
                                                      h4("Findings"),
                                                      h5("The study found that on 5 March 2019, 24.4% of the UK population were at risk of COVID-19 due to having a recording of at least one underlying health condition as per national guidance. This included 8.3% of school-aged children, 19.6% of working-aged adults, and 66.2% of individuals aged 70 years or more. The study also determined that 7.1% of the population had the co-occurrence of at least two underlying chronic health conditions (2). Despite some changes, overall, the size of the at-risk population was stable when comparing 2014 to 2019."),
                                                      h5("The study concludes that the population at risk of severe COVID-19 comprises 18.5 million individuals in the UK – defined as either aged over 70 years, or under 70 but with an underlying health condition. The national estimates from the study broadly support the use of Global Burden of Disease modelled estimates in other countries. "),
                                                      
                                                      h4("Data Source"),
                                                      h5("The Clinical Practice Research Datalink (CPRD) GOLD is a comprehensive electronic health records database that contains anonymized medical information from general practices across the United Kingdom, encompassing data from approximately 11 million patients. Fully-coded patient electronic health records data is collected directly from GP practices. CPRD GOLD contains data contributed by practices using Vision® software"),
                                                      
                                   ))),
                            
                            
                            column(4,
                                  wellPanel(style = "background-color: #fff; width:50; height: 200px;",
                                            fluidRow(style = "margin-top: 1px;",
                                                     div(style = "float: right; margin-top: 5px; margin-right: 5px;",
                                                         img(src = "https://www.lshtm.ac.uk/sites/default/files/LSHTM-logo-bw.jpg", 
                                                             height = "150", width = "250",
                                                             alt = "Logo",
                                                             ),
                                                         
                                            ))
                                            
                                            ),
                                  wellPanel(style = "background-color: #f0f0f0; border-color: #2c3e50",
                                            h4("Dashboard Summary"),
                                            p("Graph 1: Line Graph Illustrating Comorbidity Prevalance by Age-Group"),
                                            p("Graph 2: Bar Graph Illustrating Comorbidity Prevalance Grouped by Region"),
                                            p("Graph 3: Bar Graph Illustrating Comorbidity Prevalance by Specific Region"),
                                            p(" "),
                                                                      ),
                                  
                                  wellPanel(style = "background-color: #f0f0f0; border-color: #2c3e50",
                                            h4("Dashboard Guidance"),
                                            p("Each tab contains a different "),
                                            
                                  )
                            
                            ),
                            
                          ),
                          
                          style = "background-color:#fff;"
                 ),
                 
                 
                 
                 #### FIRST TAB ####
                 
                 ## First tab with first graph 
                 tabPanel(title = "Graph 1",
                          icon = icon("chart-line"),
                          
                          
                          sidebarLayout(
                            
                            ## Side bar panel for parameters
                            sidebarPanel(
                              width = 3,
                              h3("Select Filters:"),
                              # Create radio button for year
                              radioButtons("input_year", label = h4("Year"),
                                           choices = list("2014", "2019"),
                                           selected = 2014),
                     
                              ## Create selectInput for comorbidity
                              selectInput("input_morbidity", label = h4("Comorbidity"), 
                                          choices = list(
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
                              selectInput("input_region" , label = h4("Region Type"),
                                          choices = list("National" = "National",
                                                         "Local" = "Local"),
                                          selected = "national")
                              
                            ),
                         
                            ## Create main panel for plots
                            mainPanel(
                              h4("Figure 1: Line Graph Illustrating Comorbidity Prevalance by Age-Group"),
                              tags$br(),
                              p("This graph displays the age distribution of the at-risk population for different underlying health conditions"),
                              tags$br(),
                              "Use the drop downs and buttons on the left to select the year, individual comorbidity and region type between national and local regions.",
                              tags$br(),tags$br(),
                                 div(style = "border: 2px solid #333;",
                                   plotlyOutput("interactive_fig1", height="400px", width="750px")),
                              
                              p("Hover over a point in the graph to get additional information regarding prevalance estimates "),
                              p("Further information can be found below:"),
                              a("Additional Data" , href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7948667/table/Tab2/", target = "_blank")

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
                              
                              selectInput("input_region2" , label = h3("Select Region"),
                                          choices = list("National" = "National",
                                                         "Local" = "Local"),
                                          selected = "national"),
                              
                              selectInput("input_morbidity2", label = h3("Select Comorbditity"), 
                                          choices = list(
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
                              
                            ),
                            
                            ## Use the drop downs and buttons on the left to select the year, individual comorbidity and region type between national and local regions. 
                            
                            ## Create main panel for plot
                            mainPanel(
                              h4("Figure 2: Bar Graph Illustrating Comorbidity Prevalance by Region"),
                              tags$br(),
                              p("This graph displays the different comorbidity prevalance for the at-risj population for different regions"),
                              tags$br(),
                              "Use the drop downs and buttons on the left to select the year, individual comorbidity and region type between national and local regions. 
 .",
                              tags$br(),tags$br(),
                              div(style = "border: 2px solid #333;",
                              plotlyOutput("interactive_fig2", height="400px", width="750px")),
                              
                              p("Hover over a point in the graph to get additional information regarding prevalance estimates "),
                              
                            )
                            
                          ),
                          
                          ## Add some background colour to tab
                          style = "background-color: #e8f5e9;"
                 ),
                 
                 
                 #### THIRD TAB ####
                 
            
                 ## Third tab with third graph 
                 tabPanel("Graph 3",
                          icon = icon("bar-chart"),
                          
                          ## Side bar panel for parameters
                          sidebarPanel(
                            width = 3,
                            
                            radioButtons("input_year3", label = h3("Select year"),
                                         choices = list("2014", "2019"), 
                                         selected = 2014),
                            
                            selectInput("input_region3" , label = h3("Select Region"),
                                        choices = list("National" = "National",
                                                       "Local" = "Local"),
                                        selected = "national"),
                            
                            selectInput("input_morbidity3", label = h3("Select Comorbditity"), 
                                        choices = list(
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
                            
                          ),
                          
                          
                          ## Create main panel for plot
                          mainPanel(
                            h4("Graph 3 text"),
                            tags$br(),
                            p("Graph 3 explanation"),
                            tags$br(),
                            "Use the filters..., .",
                            
                            tags$br(),tags$br(),
                            div(style = "border: 2px solid #333;",
                                plotlyOutput("interactive_fig3", height="300px", width="650px")),
                            
                            p("Hover over a point in the graph to get additional information regarding prevalance estimates "),
                            
                          ),
                          
                          
                          ## Add some background colour to tab
                          style = "background-color: #e8f5e9;"
                 ),
                 
             
             
             
                #### HELP TAB ####
             navbarMenu("Help",
                        icon = icon("search"),
                        # First drop down in Help tab (information about graphs, morbidities, prevalance etc.)
                        tabPanel("About", fluid = TRUE,
                                 fluidRow(
                                   column(6,
                                          h4(p("Dashboard Info")),
                                          h5(p("Test"),
                                             p("test"),
                                          ))
                                 )),
                        # Second drop down in Help tab (any further information)
                        tabPanel("References", fluid = TRUE,
                                 fluidRow(
                                   column(6,
                                          h4(p("References")),
                                          h5(p("(1) Walker JL, Grint DJ, Strongman H, Eggo RM, Peppa M, Minassian C, et al. UK prevalence of underlying conditions which increase the risk of severe COVID-19 disease: a point prevalence study using electronic health records. BMC Public Health. 2021 Mar 11;21(1):484.")),
                                          h5(p("(2) Skou ST, Mair FS, Fortin M, Guthrie B, Nunes BP, Miranda JJ, et al. Multimorbidity. Nat Rev Dis Primer. 2022 Jul 14;8(1):48.")),
                                          h5(p("(3) https://cprd.com/primary-care-data-public-health-research")),
                                          )
                                 ))
                        
             ),
             
             
             
))

# Define server logic ----
server <- function(input, output, session) {

  ###### CREATE GRAPH SUBSETS ######
  
    #### Create data subset for plot 1 ####
      data_subset_one = reactive({ 
        subset(data, year==input$input_year & comorbidity==input$input_morbidity & region==input$input_region)
    })
    
    #### Create data subset for plot 2 ####
      data_subset_two = reactive({
        subset(data, year==input$input_year2 & comorbidity==input$input_morbidity2 & region==input$input_region2, -age_group)
      })
      #plot_two = subset(data, year=="2014" & comorbidity=="lung" & region=="National")
      
      
    #### Create data subset for plot 3 ####
      data_subset_three = reactive({
        subset(data, year==input$input_year3 & comorbidity==input$input_morbidity3 & region==input$input_region3)
      })
      
      
    
    ###### PLOT 1 ######

      # Regional prevalence plot
    output$interactive_fig1 <- renderPlotly({
        ## Read in output from reactive statement above
        plot_one = data_subset_one()
                # Static version for developing plot code
        
        
        g1 = ggplot(plot_one, aes(x=age_group, y=comorbidity_prop, colour = region_cat, group = region_cat,
                                   text=paste0("Comorbidity: ", comorbidity, "\n",
                                               "Prevalance ", round(comorbidity_prop,1), " per 100,000", "\n",
                                               "Region: ", region_cat, "\n",
                                               "Age Group: ", age_group))) +
            geom_line() +
            theme_bw() +
            xlab("Age Bands (years)") +
            ylab("Prevalence (per 100,000 people)") +
            labs(colour="Region") +
            theme(text = element_text(size=12), axis.text.x=element_text(angle = 45, hjust = 1))
        
        ggplotly(g1, tooltip = 'text')
    })
    
    
    ###### PLOT 2 ######
  
      output$interactive_fig2 <- renderPlotly({

         plot_two = data_subset_two()

        g2 = ggplot(plot_two, aes(y=comorbidity_prop, x=region_cat, fill=region_cat,
                                  text=paste0("Comorbidity: ", comorbidity, "\n",
                                              "Prevalance ", round(comorbidity_prop,1), "\n",
                                              "Region: ", region_cat))) +
          geom_bar(stat="identity") +
          coord_flip() +
          xlab("Region") +
          ylab("Prevalence/100,000") +
          labs(fill="Region") 
    
        
        ggplotly(g2, tooltip = 'text')
         
      })
      
      # g2 = ggplot(data, aes(x = region_cat, y = comorbidity_prop, fill = comorbidity)) +
      #   geom_bar(stat = "identity") +
      #   labs(title = "Comorbidity Prevalence by Region",
      #        x = "Region",
      #        y = "Comorbidity Prevalence") +
      #   theme_minimal()
      # 
      

    ###### PLOT 3 ######
    
      output$interactive_fig3 <- renderPlotly({
        
        plot_three = data_subset_three()
        
        g3 = ggplot(plot_three, aes(y=comorbidity_prop, x=reorder(comorbidity, comorbidity_prop), fill = region_cat)) +
          geom_bar(stat="identity" , position = position_dodge) +
          xlab("Comorbidity") +
          ylab("Prevalance") +
          
          ggplotly(g3, tooltip = 'text')
        
      })
        
        # g3 = data %>%
        #   group_by(region_cat,comorbidity) %>%
        #   ggplot(aes(y=prop, x=reorder(comorbidity, prop), fill = region_cat)) +
        #   geom_bar(stat="identity" , position = position_dodge())

        
        
       
 
      

}

# Run the app ----
shinyApp(ui = ui, server = server)

