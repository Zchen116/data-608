#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(rsconnect)
library(ggplot2)
library (dplyr)

data_url <- "https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv"
data <- read.csv(data_url, stringsAsFactors = FALSE)
data2010 <- subset(data, Year==2010)


cause <- unique(data2010$ICD.Chapter)
states <- unique(data2010$State)



# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Mortality Rate Across All States by Causes"),
    
    #Dropdown state and cause
    sidebarLayout(
        sidebarPanel(
            selectInput("cause_name",
                        label = "Select cause of death:",
                        choices = 
                            c("Infectious & parasitic" = "Certain infectious and parasitic diseases"    ,
                              "Neoplasms" = "Neoplasms",
                              "Blood & blood-forming & immune mechanism disorders" = "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism",
                              "Endocrine, nutritional & metabolic" = "Endocrine, nutritional and metabolic diseases",
                              "Mental & behavioural disorders" = "Mental and behavioural disorders",
                              "Nervous system" = "Diseases of the nervous system",
                              "Ear & mastoid process" = "Diseases of the ear and mastoid process",
                              "Circulatory system"  = "Diseases of the circulatory system",
                              "Respiratory system" = "Diseases of the respiratory system",
                              "Digestive system" = "Diseases of the digestive system",
                              "Skin & subcutaneous tissue" = "Diseases of the skin and subcutaneous tissue",
                              "Musculoskeletal system & connective tissue" = "Diseases of the musculoskeletal system and connective tissue",
                              "Genitourinary system" = "Diseases of the genitourinary system",
                              "Pregnancy, childbirth & the puerperium" = "Pregnancy, childbirth and the puerperium",
                              "Conditions originating in the perinatal period" = "Certain conditions originating in the perinatal period",
                              "Congenital malformations, deformations & chromosomal abnormalities" = "Congenital malformations, deformations and chromosomal abnormalities",
                              "Symptoms, signs & abnormal clinical & laboratory findings, not classified" = "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified",
                              "External causes of morbidity & mortality" = "External causes of morbidity and mortality"),
                        selected = 'Neoplasms'
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("barplot"),
            br(),br(),
            tableOutput("results")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    cause1 <- reactive({
        df <- subset(data2010, data2010$ICD.Chapter==input$cause_name) %>%
            arrange(Crude.Rate)
        
    })
    
    output$barplot <- renderPlot({
        ggplot(cause1(), aes(x = reorder(State, Crude.Rate), y = Crude.Rate, colour = Crude.Rate)) + 
            geom_point(stat="identity", position=position_dodge(), size=5) +
            coord_flip() + 
            geom_text(aes(x= State, y=0, ymax = Crude.Rate,
                          label = State, hjust = 1, vjust = 0.4)) + 
            scale_x_discrete(breaks = NULL) +
            xlab("State") +
            ylab("Crude Mortality Rate") + 
            theme(axis.text.y = element_text(angle = 90, size=8, vjust = 0.35))
        
        
    }, height = 800)
    
}
    


# Run the application 
shinyApp(ui = ui, server = server)
