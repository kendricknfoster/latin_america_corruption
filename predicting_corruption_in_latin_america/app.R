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



ui <- navbarPage(
    "Final Project Title",
    
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
             p("Hello, this is where I talk about my project."),
             h3("About Me"),
             p("My name is ______ and I study ______. 
             You can reach me at ______@college.harvard.edu.")))


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
