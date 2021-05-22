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
library(readr)


nyc_mortality <- read.csv("C:/Users/Hugo/Desktop/data608/final/cleaned_nyc_data01.csv")

# Define UI for application that draws a histogram
ui = fluidPage(
    # Application title
    titlePanel("NYC Leading Causes of Death on 2007-2017"),
    tabsetPanel(
        
        
        
        tabPanel("Visulization 1", titlePanel(""), 
                 selectInput(inputId = "year", 
                             label = "Choose Year",
                             choices = unique(nyc_mortality$year)
                             
                 ),
                 mainPanel(
                     plotOutput(outputId = "graph3") 
                 )
        ),
        # Sidebar with a 3 inputs 
        tabPanel("Visulization 2", titlePanel(""),
                 
                     sidebarPanel(
                         sliderInput(inputId = "year",
                                     label = "Choose Year:",
                                     min = 2007, max = 2017,value=10, step=1
                         ),
                         radioButtons(inputId = "sex",
                                      label = "Sex:",
                                      choices = c(
                                          "Female" = "F",
                                          "Male" = "M"
                                      )),
                         radioButtons(inputId = "race",
                                      label = "Race/Ethnicity:",
                                      choices = unique(nyc_mortality$race_ethnicity))
                     ),
                     
                     # Show plot and table
                     mainPanel(
                         plotOutput("deathPlot"),
                         DT::dataTableOutput("deathTable")
                     )
                 ),
        
        
        tabPanel("Visulization 3", titlePanel("Trend line for the total death by sex for all years"
        ),
        selectInput(inputId = "sex", 
                      label = "Choose Sex",
                      choices = unique(nyc_mortality$sex)),
        
        mainPanel(
            plotOutput(outputId = ("graph2") 
            )
        )
        
        
    )))
    

    
    

# Define server logic required to draw a histogram

server = function(input, output) {
    output$graph2 <- renderPlot({ 
        nyc_mortality %>% group_by(year,sex) %>% summarise(Total = sum(as.numeric(deaths))) %>%
            ggplot(.,aes(year,Total)) + geom_line(aes(color = sex)) + ylab('Total Death') + facet_wrap(~sex)
    })
    
    
    
    
    output$graph3 <- renderPlot({ 
        ggplot(data = selections(), mapping = aes(x = reorder(leading_cause, -deaths), y = deaths, fill = leading_cause )) + 
            geom_bar(stat = 'identity', position = 'stack')  + coord_polar(theta = 'x')+labs( x = "Causes",
                                                                                              y = "Number of Deaths") 
        
    },height=800,width=800)
    
    selections = reactive({
        req(input$year)
        req(input$sex)
        req(input$race)
        filter(nyc_mortality, year == input$year) %>%
            filter(sex %in% input$sex) %>%
            filter(race_ethnicity %in% input$race)
        
    })
    
    output$deathPlot = renderPlot({
        ggplot(data = selections(), aes(x = reorder(leading_cause, -deaths), y = deaths)) +
            geom_bar(stat = 'identity', color = 'grey', fill = 'steelblue') +
            labs(
                title = "Top 10 Leading Causes of Death",
                x = "Causes",
                y = "Number of Deaths"
            ) +
            theme(axis.text.x = element_text(angle = 90, size=8, vjust = 0.35, hjust=1))
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)