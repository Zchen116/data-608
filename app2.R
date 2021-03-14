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
library(dplyr)

data_url <- paste0("https://raw.githubusercontent.com/fung1091/data608/master/module3/cleaned-cdc-mortality-1999-2010-2.csv")
data <- read.csv(data_url, stringsAsFactors = FALSE)

crude_rate <- 100000

nat_cause <- aggregate(cbind(Deaths, Population) ~ ICD.Chapter + Year, data, FUN = sum)

nat_cause$NatAvge <- round(nat_cause$Deaths / nat_cause$Population * crude_rate, 4)

# Define UI for application that draws a histogram
ui <- fluidPage(

    #Application title
    titlePanel("Mortality Improvement by State"),
    
    #Dropdown state and cause
    sidebarLayout(
        sidebarPanel(
            selectInput("state_name", "State:", choices = unique(data$State)),
            selectInput("causes", "Select type of disease - cause of Death:",
                         choices = unique(data$ICD.Chapter))
        ),
        # Show a plot of the generated distribution
        mainPanel(plotOutput("State_graph"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    sub_data <- reactive({subset(
        filter(data, ICD.Chapter == input$causes & State == input$state_name),
        select = c(Year, Crude.Rate, ICD.Chapter)
    )})
    
    new_df <- reactive({
        df <- sub_data()
        colnames(df) <- c("Year", "Rate_State", "ICD.Chapter")
        merge(df, nat_cause, by=c("Year", "ICD.Chapter")) %>%
            mutate(state_diff = lag(Rate_State) - Rate_State) %>%
            mutate(nat_diff = lag(NatAvge) - NatAvge) %>%
            mutate(st_nat = (state_diff - nat_diff))
    })
    
    output$State_graph <- renderPlot({
        ggplot(data=new_df(), aes(x=Year, y=st_nat)) + geom_line(col = "red", lwd = 1) + ylab("State Crude Rate")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
