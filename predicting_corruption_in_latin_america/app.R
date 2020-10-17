library(shiny)
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(readxl)

# Define UI for application that draws a histogram

# I don't want any spaces in the names, so I set.name_repair to Universal. I
# only want countries in Latin America, so I filter for the "AME" in the Region
# column and get rid of the United States and Canada in the dataset. I arrange
# in alphabetical order to make future data merging easier.

CPI2019 <- read_excel("raw_data/2019_CPI_FULLDATA/CPI2019.xlsx", 
                      skip = 2, .name_repair = make.names) %>%
    filter(Region == "AME", 
           !(Country == "United States of America" | Country == "Canada")) %>%
    select(Country, CPI.score.2019) %>%
    arrange(Country)

# I include X2014 to include the latest available data from Venezuela. I'm
# currently working on fixing this so I can create a column called
# "latest_available", but I have too much going on this week to include it in
# this milestone. I combine the dataset and the metadata so I can filter by
# region and alphabetize to facilitate joining.

gdp_pc_data = read_excel("raw_data/API_NY.GDP.PCAP.CD_DS2_en_excel_v2_1495414.xls", 
                         skip = 3, .name_repair = make.names) %>%
    select(Country.Name, Country.Code, X2014, X2019)

gdp_pc_meta = read_excel("raw_data/API_NY.GDP.PCAP.CD_DS2_en_excel_v2_1495414.xls", 
                         sheet = 2, .name_repair = make.names)

gdp_pc <- inner_join(gdp_pc_data, gdp_pc_meta, by = "Country.Code") %>%
    filter(Region == "Latin America & Caribbean") %>%
    select(Country.Name, X2014, X2019) %>%
    arrange(Country.Name)

# I use a left join so I can filter out the Caribbean countries I don't really
# care about, since they aren't in the Transparency International dataset in
# CPI2019.

initial_join <- left_join(CPI2019, gdp_pc, by = c("Country" = "Country.Name"))

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
