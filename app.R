#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
source('data_loader.R')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Age of Death Regression Tool (USA)"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            "Select a sex, race, and state to show the respective distribution of deaths by age and the model's predicted age of death.",
            hr(),
            selectInput('Sex', 'Sex', c('Male','Female'), selected = 'Male', multiple = FALSE,
                        selectize = TRUE, width = NULL, size = NULL),
            selectInput('Race', 'Race', c("White",
                                          "Black or African American",
                                          "Asian or Pacific Islander",
                                          "American Indian or Alaska Native"),
                        selected = 'White', multiple = FALSE,
                        selectize = TRUE, width = NULL, size = NULL),
            selectInput('State', 'State', c("Alabama"              ,
                                            "Alaska"               ,
                                            "Arizona"              ,
                                            "Arkansas"             ,            
                                            "California"           ,           
                                            "Colorado"             ,             
                                            "Connecticut"          ,          
                                            "Delaware"             ,            
                                            "District of Columbia" , 
                                            "Florida"              ,
                                            "Georgia"              ,
                                            "Hawaii"               ,
                                            "Idaho"                ,
                                            "Illinois"             ,
                                            "Indiana"              ,
                                            "Iowa"                 ,
                                            "Kansas"               ,
                                            "Kentucky"             ,
                                            "Louisiana"            ,
                                            "Maine"                ,
                                            "Maryland"             ,
                                            "Massachusetts"        ,
                                            "Michigan"             ,
                                            "Minnesota"            ,
                                            "Mississippi"          ,
                                            "Missouri"             ,
                                            "Montana"              ,
                                            "Nebraska"             ,
                                            "Nevada"               ,
                                            "New Hampshire"        ,
                                            "New Jersey"           ,
                                            "New Mexico"           ,
                                            "New York"             ,
                                            "North Carolina"       ,
                                            "North Dakota"         ,
                                            "Ohio"                 ,
                                            "Oklahoma"             ,
                                            "Oregon"               ,
                                            "Pennsylvania"         ,
                                            "Rhode Island"         ,
                                            "South Carolina"       ,
                                            "South Dakota"         ,
                                            "Tennessee"            ,
                                            "Texas"                ,
                                            "Utah"                 ,
                                            "Vermont"              ,
                                            "Virginia"             ,
                                            "Washington"           ,
                                            "West Virginia"        ,
                                            "Wisconsin"            ,
                                            "Wyoming"),
                        selected = 'Alabama', multiple = FALSE,
                        selectize = TRUE, width = NULL, size = NULL),
            actionButton("Regress", "Regress"),
            hr(),
            "Data citation: Centers for Disease Control and Prevention, National Center for Health Statistics. Underlying Cause of Death 1999-2018 on CDC WONDER Online Database, released in 2020. Data are from the Multiple Cause of Death Files, 1999-2018, as compiled from data provided by the 57 vital statistics jurisdictions through the Vital Statistics Cooperative Program. Accessed at http://wonder.cdc.gov/ucd-icd10.html on Apr 30, 2020"
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("title"),
            textOutput("sex"),
            textOutput("race"),
            textOutput("state"),
            plotlyOutput("distPlot"),
            hr(),
            textOutput("predict")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    showModal(modalDialog('Loading Data...', footer=NULL))
        load_data_results = load_data()
        model = load_data_results[[1]]
        death_table = load_data_results[[2]]
    removeModal()
    
    showModal(modalDialog('Disclaimer: The following tool was developed for a class project. It employs simple linear regression to predict age of death using three categorical metrics from data available online at https://wonder.cdc.gov/controller/datarequest/D76. Please be aware that the predictions calculated by this tool rely soley on the available regressors and DO NOT NECESSARILY IMPLY CAUSATION by said regressors. Death rate realization is a complex phenomenon which cannot be characterized in full by three metrics.'
                          , title = NULL, footer = modalButton("Dismiss"),
                size = c("m", "s", "l"), easyClose = FALSE, fade = TRUE))
    
    observeEvent(input$Regress, {
        submitted_gender = input$Sex
        submitted_state = input$State
        submitted_race = input$Race
        submitted_data = data.frame('gender' = submitted_gender, 'state' = submitted_state, 'race' = submitted_race)
        result = get_prediction_and_plot(model, death_table, submitted_data)
        prediction = result[[1]]
        p = result[[2]]
        
        
        output$'title' <- renderText({ 
            isolate('Age Distribution of Deaths for:')
        })
        output$'sex' <- renderText({ 
            isolate(paste('     Sex - ', submitted_gender, sep=''))
        })
        output$'race' <- renderText({ 
            isolate(paste('     Race - ', submitted_race, sep=''))
        })
        output$'state' <- renderText({ 
            isolate(paste('     State - ', submitted_state, sep=''))
        })
        output$distPlot <- renderPlotly({
            isolate(p)
        })
        output$'predict' <- renderText({ 
            isolate(paste('Predicted age of death from regression: ', round(prediction), ' years', sep=''))
        })
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
