
### Load  packages
library(shiny)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(shinythemes)
library(plotly)
library(shinydashboard)
library(DT)
library(scales)


### Load data
Combined_comorbidity_age_region <- read_excel("../Data/Combined_comorbidity_age_region.xlsx")
# Combined_comorbidity_region <- read_excel("../Data/Combined_comorbidity_region.xlsx")
bmi_cat <- read_excel("../Data/bmi_cat.xlsx")
ethnicity <- read_excel("../Data/ethnicity.xlsx")
sm_cat <- read_excel("../Data/sm_cat.xlsx")

# Rename file 
data <- Combined_comorbidity_age_region

# Changed all 'bmi40' to 'bmi_40' to merge into one category 
data$comorbidity[data$comorbidity == 'bmi40'] <- 'bmi_40'

# 
data$comorbidity_yes[data$comorbidity_yes == '<5'] <- '0'
data$comorbidity_no[data$comorbidity_no == 'suppressed'] <- '0'

# Create copy of 'data' dataframe without age_group column
data2 <- data %>% select (-c(age_group))

# Convert to numeric so that data can be aggregated 
data2$comorbidity_yes <- as.numeric(data$comorbidity_yes)
data2$comorbidity_no <- as.numeric(data$comorbidity_no)
 
# Aggregate data2
data2 <- data2 %>%
  group_by (region_cat, region, comorbidity, year) %>%
  summarise (status = mean(status),
             comorbidity_yes = sum(comorbidity_yes),
             comorbidity_no = sum(comorbidity_no) ,
             comorbidity_tot_non_miss = sum(comorbidity_tot_non_miss),
             comorbidity_prop = sum(comorbidity_prop),
             comorbidity_lb = sum(comorbidity_lb),
             comorbidity_ub = sum(comorbidity_ub)) 


# Calculate new prevalence based on total comorbidity over total population 
data2 <- data2 %>% mutate(comorbidity_prop_new = comorbidity_yes/comorbidity_tot_non_miss*100000)


# Aggregate data3 with new column name
data3 <- data2 %>% mutate (comorbidity_name = comorbidity)

# Change values to contain full name of comorbidities 
data3 <- data3 %>%
  mutate(
    comorbidity_name = case_when(
      comorbidity_name == "liver" ~ "Chronic Liver Disease", 
      comorbidity_name == "heart" ~ "Chronic Heart Disease",
      comorbidity_name == "lung" ~ "Chronic Lung Disease",
      comorbidity_name == "diabetes" ~ "Diabetes" ,
      comorbidity_name == "ckd" ~ "Chronic Kidney Disease",
      comorbidity_name == "immuno" ~ "Immunosuppression",
      comorbidity_name == "neuro" ~ "Chronic neurological disease",
      comorbidity_name == "asthma_ever" ~ "Any history of asthma",
      comorbidity_name == "asthma_specific" ~ "Current asthma (no COPD)",
      comorbidity_name == "bmi_40" ~ "Severe obesity",
      comorbidity_name == "cancerever" ~ "Any history of cancer",
      comorbidity_name == "cancerlast5yrs" ~ "Cancer (last 5 years)",
      comorbidity_name == "cancerlastyr" ~ "Cancer (last year)",
      comorbidity_name == "cancerlast6months" ~ "Cancer (last 6 months)",
      comorbidity_name == "si_sp"~ "Dysplenia (including sickle cell disease)",
      comorbidity_name == "anyonecond_prev" ~ "Any 'higher risk' health condition",
      comorbidity_name == "anyonecond_prevbmi" ~ "Any 'higher risk' risk factor",
      comorbidity_name == "immuno_no_si_sp" ~ "Immunosuppression excluding dysplenia",
      comorbidity_name == "organ_tx" ~ "Organ transplant recipient",
      comorbidity_name == "multi_prev" ~ "Multimorbidity",
      TRUE ~ comorbidity_name
    )
  )


# Define UI ----
ui <- navbarPage(windowTitle = "Window title",
                 theme = shinytheme("flatly"),
                 title = "LSHTM",
                 
                 #### HOME PAGE ####
                 
                 ## Create main page to briefly describe dashboard and its content 
                 tabPanel("Home",
                          icon = icon("home"),
                   
                          fluidRow(
                            column(8,
                                   wellPanel(style = "background-color: #f0f0f0; border-color: #2c3e50; height: 700px;",
                                             fluidRow(style = "margin-top: 0px;",
                                                      h2("COVID-19 Comorbidity Prevalence Dashboard"),
                                                      p("Dashboard based on study which looked at the UK prevalence of underlying conditions which increase the risk of severe COVID-19 disease: a point prevalence study using electronic health records. The original study paper can be found here:"),
                                                      a("Original Study" , href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7948667/#MOESM1", target = "_blank"),
                                                      
                                                      h5("This dashboard was developed for a thesis which is part of a Health Data Science MSc at the London School and Hygiene and Tropical Medicine. Ethical approval for this follow-up study was obtained from the LSHTM Research Ethics Committee. All data are openly available via LSHTM Data Compass:"),
                                                      a("LSHTM Data Compass" , href = "https://datacompass.lshtm.ac.uk/id/eprint/1833/", target = "_blank"),
                                                
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
                                                      h5("The Clinical Practice Research Datalink (CPRD) GOLD is a comprehensive electronic health records database that contains anonymized medical information from general practices across the United Kingdom, encompassing data from approximately 11 million patients. Fully coded patient electronic health records data is collected directly from GP practices. CPRD GOLD contains data contributed by practices using Vision® software"),
                                                      
                                   )),
                                   
                                   wellPanel(style = "background-color: #f0f0f0; border-color: #2c3e50; height: 150px;",
                                             fluidRow(style = "margin-top: 0px;",
                                                      h5("References"),
                                                      h6("(1) Walker JL, Grint DJ, Strongman H, Eggo RM, Peppa M, Minassian C, et al. UK prevalence of underlying conditions which increase the risk of severe COVID-19 disease: a point prevalence study using electronic health records. BMC Public Health. 2021 Mar 11;21(1):484."),
                                                      h6("(2) Skou ST, Mair FS, Fortin M, Guthrie B, Nunes BP, Miranda JJ, et al. Multimorbidity. Nat Rev Dis Primer. 2022 Jul 14;8(1):48."),
                                                      
                                                      h6("(3) https://cprd.com/primary-care-data-public-health-research"),
                                                      
                                             )),
                                   ),
                            
                            
                            
                            column(4,
                                  wellPanel(style = "background-color: #fff; width:50; height: 200px;",
                                            fluidRow(style = "margin-top: 1px;",
                                                     div(style = "float: right; margin-top: 5px; margin-right: 5px;",
                                                         img(src = "https://www.lshtm.ac.uk/sites/default/files/LSHTM-logo-bw.jpg", 
                                                             height = "150", width = "270",
                                                             alt = "Logo",
                                                             ),
                                            ))
                                            
                                            ),
                                  wellPanel(style = "background-color: #f0f0f0; border-color: #2c3e50",
                                            h4("Dashboard Summary"),
                                            p("Graph 1: Line Graph Illustrating Comorbidity Prevalence by Age-Group"),
                                            p("Graph 2: Bar Graph Illustrating Comorbidity Prevalence Grouped by Region"),
                                            p("Graph 3: Bar Graph Illustrating Comorbidity Prevalence by Specific Region"),
                                            p(" "),
                                                                      ),
                                  
                                  wellPanel(style = "background-color: #f0f0f0; border-color: #2c3e50",
                                            h4("Dashboard Guidance"),
                                            p("Each tab can be accessed using the navigation bar at the top of the dashboard. All graphs use CRPD GOLD data to create a different interactive visualisation."),
                                  ),
                                  
                                  wellPanel(style = "background-color: #f0f0f0; border-color: #2c3e50",
                                            h4("GitHub"),
                                            p("The underlying code for the app can be found on GitHub using the following link:"),
                                            a("GitHub Code" , href = "https://github.com/MethJ89/summer_thesis-", target = "_blank"),
                                            
                                  ),
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
                                            "Chronic Liver Disease" = "liver",
                                            "Chronic Heart Disease" = "heart",
                                            "Chronic Lung Disease" = "lung",
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
                                            "Any 'higher risk' health condition" = "anyonecond_prev",
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
                              h4("Graph 1: Line Graph Illustrating Comorbidity Prevalence by Age-Group"),
                              tags$br(),
                              p("This graph displays the age distribution of the at-risk population for different underlying health conditions"),
                              tags$br(),
                              "Use the drop downs and buttons on the left to select the year, individual comorbidity and region type between national and local regions.",
                              tags$br(),tags$br(),
                                 div(style = "border: 2px solid #333;",
                                   plotlyOutput("interactive_fig1", height="400px", width="750px")),
                              
                              p("Hover over a point in the graph to get additional information regarding prevalence estimates "),
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
                              h3("Select Filters:"),
                              
                              radioButtons("input_year2", label = h4("Select year"),
                                           choices = list("2014", "2019"), 
                                           selected = 2014),
                              
                              selectInput("input_region2" , label = h4("Select Region Type"),
                                          choices = list("National" = "National",
                                                         "Local" = "Local"),
                                          selected = "national"),
                              
                              selectInput("input_morbidity2", label = h4("Select Comorbditity"), 
                                          choices = list(
                                            "Chronic Liver Disease" = "liver",
                                            "Chronic Heart Disease" = "heart",
                                            "Chronic Lung Disease" = "lung",
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
                                            "Any 'higher risk' health condition" = "anyonecond_prev",
                                            "Any 'higher risk' risk factor" = "anyonecond_prevbmi",
                                            "Immunosuppression excluding dysplenia" = "immuno_no_si_sp",
                                            "Organ transplant recipient" = "organ_tx",
                                            "Multimorbidity" = "multi_prev"),
                                          selected = "liver"),
                              
                            ),
                            
                            
                            ## Use the drop downs and buttons on the left to select the year, individual comorbidity and region type between national and local regions. 
                            
                            ## Create main panel for plot
                            mainPanel(
                              h4("Graph 2: Bar Graph Illustrating Comorbidity Prevalence by Region"),
                              tags$br(),
                              p("This graph displays the different comorbidity prevalence for the at-risj population for different regions"),
                              tags$br(),
                              "Use the drop downs and buttons on the left to select the year, individual comorbidity and region type between national and local regions.",
                              tags$br(),tags$br(),
                              div(style = "border: 2px solid #333;",
                              plotlyOutput("interactive_fig2", height="400px", width="750px")),
                              
                              p("Hover over a point in the graph to get additional information regarding prevalence estimates "),
                              
                            )
                          ),
                          
                          ## Add some background colour to tab
                          style = "background-color: #e8f5e9;"
                 ),
                 
                 
                 #### THIRD TAB ####
                 
                 ## Third tab with third graph 
                 tabPanel("Graph 3",
                          icon = icon("bar-chart"),
                          
                          sidebarLayout(
                          
                          ## Side bar panel for parameters
                          sidebarPanel(
                            width = 3,
                            h3("Select Filters:"),
                            
                            radioButtons("input_year3", label = h4("Select year"),
                                         choices = list("2014", "2019"), 
                                         selected = 2014),
                            
                            selectInput("input_region3" , label = h4("Select Region Type"),
                                        choices = list("National" = "National",
                                                       "Local" = "Local"),
                                        selected = "National"),
                            
                            selectInput("input_region_cat3", label = h4("Select Region"), 
                                        choices = NULL)
                            
                          ),
                          
                          ## Create main panel for plot
                          mainPanel(
                            h4("Graph 3: Bar Graph Illustrating Comorbidity Prevalence by Individual Region"),
                            tags$br(),
                            p("This graph illustrates the prevalence of comorbidities for each specific region, giving a greater level of granularity for each region by comorbidity"),
                            tags$br(),
                            "Use the drop down and button on the left to select which region to view and for which specific year.",
                            
                            tags$br(),tags$br(),
                            div(style = "border: 2px solid #333;",
                                plotlyOutput("interactive_fig3", height="450px", width="750px")),
                            
                            p("Hover over a point in the graph to get additional information regarding prevalence estimates "),
                            p("Further information about the comorbidities and their corresponding descriptions can be found in the Comorbidity Table under the Help tab"),
                          ),
                          
                          ),
                          
                          ## Add some background colour to tab
                          style = "background-color: #e8f5e9;"
                 ),
                 
             
             
                #### HELP TAB ####
             navbarMenu("Help",
                        icon = icon("question"),
                        # First drop down in Help tab (information about graphs, morbidities, prevalence etc.)
                        tabPanel("Comorbidity Table", fluid = TRUE,
                                 fluidRow(
                                   column(6,
                                          h4(p("Comorbidity Table")),
                                          h5(p("Description of all morbidities in the dashboard as well as their definitions"),
                                             # Render table here
                                             dataTableOutput("myTable")
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
)


# Define server logic 
server <- function(input, output, session) {

  ###### CREATE GRAPH SUBSETS ######
  
    #### Create data subset for plot 1 ####
      data_subset_one = reactive({ 
        subset(data, year==input$input_year & comorbidity==input$input_morbidity & region==input$input_region)
    })
    
      
    #### Create data subset for plot 2 ####
      data_subset_two = reactive({
        subset(data2, year==input$input_year2 & comorbidity==input$input_morbidity2 & region==input$input_region2)
      })
      
      
    #### Create data subset for plot 3 ####
      data_subset_three = reactive({
        subset(data3, year==input$input_year3 & region==input$input_region3 & region_cat==input$input_region_cat3)
      })
      
      
    ###### PLOT 1 ######

      # Regional prevalence plot
    output$interactive_fig1 <- renderPlotly({
        ## Read in output from reactive statement above
        plot_one = data_subset_one()
                # Static version for developing plot code
        
        
        g1 = ggplot(plot_one, aes(x=age_group, y=comorbidity_prop, colour = region_cat, group = region_cat,
                                   text=paste0("Comorbidity: ", comorbidity, "\n",
                                               "Prevalence ", round(comorbidity_prop,1), " per 100,000", "\n",
                                               "Region: ", region_cat, "\n",
                                               "Age Group: ", age_group))) +
            geom_line() +
            theme_bw() +
            xlab("Age Bands (years)") +
            ylab("Prevalence / 100,000") +
            labs(colour="Region") +
            theme(text = element_text(size=12), axis.text.x=element_text(angle = 45, hjust = 1))
        
        ggplotly(g1, tooltip = 'text')
    })
    
    
    ###### PLOT 2 ######
  
      output$interactive_fig2 <- renderPlotly({

         plot_two = data_subset_two()

        g2 = ggplot(plot_two, aes(y=comorbidity_prop_new, x=region_cat, fill=region_cat,
                                  text=paste0("Comorbidity: ", comorbidity, "\n",
                                              "Prevalence ", round(comorbidity_prop_new,1), "\n",
                                              "Region: ", region_cat))) +
          geom_bar(stat="identity") +
          theme_bw() +
          coord_flip() +
          xlab("Region") +
          ylab("Prevalence / 100,000") +
          labs(fill="Region") +
          theme(text = element_text(size=12), axis.text.x=element_text(angle = 45, hjust = 1)) +
          scale_fill_manual(values = scales::hue_pal()(length(unique(plot_two$region_cat)))) 
        
        ggplotly(g2, tooltip = 'text')
         
      })
      

    ###### PLOT 3 ######
    
      output$interactive_fig3 <- renderPlotly({
        
        plot_three = data_subset_three()
        
        g3 = ggplot(plot_three, aes(y=comorbidity_prop_new, x=reorder(comorbidity_name, comorbidity_prop_new), fill = region_cat,
                                    text=paste0("Comorbidity: ", comorbidity_name, "\n",
                                                "Prevalence ", round(comorbidity_prop_new,1), "\n", 
                                                "Region: ", region_cat))) +
          geom_bar(stat="identity" , position = position_dodge()) +
          xlab("Comorbidity") +
          ylab("Prevalence  / 100,000") +
          scale_y_continuous(labels = scales::comma) +
          theme_bw() +
          labs(fill="Region") + 
          coord_flip() +
          theme(text = element_text(size=12), axis.text.x=element_text(angle = 45, hjust = 1)) +
          scale_fill_manual(values = scales::hue_pal()(length(unique(plot_three$comorbidity_name)))) 
          
          ggplotly(g3, tooltip = 'text')
        
      })
      
      # Making second drop down values dependent on what is selected in the first 
      
      observe({
        if(input$input_region3 == "National") {
          updateSelectInput(session, "input_region_cat3" , "Select Region" , choices = c("England" , "Scotland" , "Wales" , "Northern Ireland"))
        } else {
          updateSelectInput(session, "input_region_cat3" , "Select Region" , choices = c("North East" , "East Midlands", "East of England", "London", "North West", "South Central", "South East Coast", "South West", "West Midlands", "Yorkshire & The Humber"))

        }
      })
        
      
    ###### COMORBIDITY TABLE ######
      
      # Generate table with comorbidity information
      table <- data.frame(
         Comorbidity_Name = c("Chronic liver disease" , "Chronic heart disease" , "Chronic respiratory disease" , "Any history of asthma" , "Current asthma (no COPD)" , "Chronic neurological disease" , "Organ transplant recipient" , "Immunosuppression" , "Dysplenia (including sickle cell disease)" , "Immunosuppression excluding dysplenia" , "Diabetes mellitus" , "Chronic kidney disease" , "Any history of cancer" , "Cancer (last 5 years)" , "Cancer (last year)" , "Cancer (last 6 months)" , "Multimorbidity"  , "Any 'higher risk' health condition" , "Any 'higher risk' risk factor", "Severe obesity"),
         
         Study_Name = c("liver" , "heart" , "lung" , "asthma_ever" , "asthma_specific" , "neuro" , "organ_tx" , "immuno" , "si_sp" , "immuno_no_si_sp" , "diabetes" , "ckd" , "cancerever" , "cancerlast5yrs" , "cancerlastyr" , "cancerlast6months" , "multi_prev" , "anyonecond_prev" , "anyonecond_prevbmi" , "bmi40"),
         
       
         Description = c("Chronic liver disease including cirrhosis, oesophageal varices, biliary atresia and chronic hepatitis." , "Chronic heart disease likely to need follow up or medication, including ischaemic heart disease and chronic heart failure." , "Chronic respiratory disease including COPD; bronchiectasis, cystic fibrosis, interstitial lung fibrosis, pneumoconiosis and bronchopulmonary dysplasia (BPD). Excludes asthma.", "Any previous diagnosis of asthma." , "A diagnosis of asthma within the previous 3 years, excluding people ever diagnosed with COPD." , "A diagnosis of stroke, transient ischaemic attack, or conditions in which respiratory function may be compromised due to neurological disease, such as myasthenia gravis." , "Solid organ transplant recipient." , "Immunosuppression due to disease or treatment" , "Asplenia or dysplenia, including sickle cell disease. Subset of immunosuppression." , "Subset of immunosuppression, excluding asplenia/dysplenia/sickle cell disease." , "Diabetes mellitus" , "Chronic kidney disease at stage 3, 4 or 5 (based on diagnoses or eGFR estimated from the latest serum creatinine test result), chronic kidney failure, nephrotic syndrome, kidney transplantation or dialysis" , "Any previous diagnosis of a malignant cancer" , "First diagnosis of a malignant cancer within the previous 5 years" , "First diagnosis of a malignant cancer within the previous year" , "First diagnosis of a malignant cancer within the previous 6 months" , "≥2 of the following domains: chronic liver disease; chronic heart disease; asthma or chronic respiratory disease (using asthma_specific definition); chronic neurological disease; immunosuppression (including organ transplant recipient, dysplenia and sickle cell disease); diabetes mellitus; chronic kidney disease. Does not include cancer or obesity.", "Any of the following: chronic liver disease; chronic heart disease; asthma or chronic respiratory disease (using the specific asthma definition); chronic neurological disease; immunosuppression (including organ transplant recipient, dysplenia and sickle cell disease); diabetes mellitus; chronic kidney disease." , "Any 'higher risk' health condition or severe obesity (BMI≥40 kg/m2). (BMI measurements only from age 18+)" , "Adult BMI ≥40 kg/m2 based on latest recorded height and weight since aged 18 years. Prevalence estimates restricted to age ≥20 years." )
      )    
         
      # Render the table using DT package
      output$myTable <- renderDataTable({
        datatable(table)
      })
}

# Run the app ----
shinyApp(ui = ui, server = server)







