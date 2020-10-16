#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(shiny)
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)

# Define UI for application that draws a histogram

initial_join

ui <- navbarPage(
    "Predicting Corruption in Latin America",
    
    tabPanel("Model",
             fluidPage(
                 selectInput("x", "Pick your x variable", choices = names(initial_join)),
                 selectInput("y", "Pick your y variable", choices = names(initial_join)), 
                 selectInput("geom", "geom", c("point", "jitter", "smooth", "col")), 
                 plotOutput("plot"))),
    
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this is where I talk about my project. 
               You can find a link to the underlying data and code here: 
               https://github.com/kendricknfoster/latin_america_corruption"),
             p("The current data is the Transparency International Corruption 
               Perceptions Index (CPI) for 2019, the most recent year for which 
               we have available data, and the World Bank's GDP per capita figures. 
               I also plan to collect data on each country's Gini index from the 
               World Bank, democratization from the Economist Intelligence Unit, 
               and infrastructure spending from the World Bank. 
               I will also see if I can find datasets on total campaign spending 
               and public contracting law, though those may be harder to find."),
             h3("About Me"),
             p("My name is Kendrick Foster, and I study History and Government. 
             You can reach me at kfoster@college.harvard.edu.")))


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    plot_geom <- reactive({
        switch(input$geom,
               point = geom_point(),
               jitter = geom_jitter(),
               col = geom_col(), 
               smooth = geom_smooth()
        )
    })
    
    output$plot <- renderPlot({
        ggplot(initial_join, aes(.data[[input$x]], .data[[input$y]])) +
            plot_geom()
    }, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)

# Run the application 
shinyApp(ui = ui, server = server)
