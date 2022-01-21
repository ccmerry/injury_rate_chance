#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(

    # Application title
    titlePanel("NFL Injury Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          
          selectizeInput("teamschoice",
                         "Teams:",
                         choices = team_list,
                         multiple = T),
          
          selectizeInput("injurytypes",
                         "Injury Types:",
                         choices = injury_type_list,
                         multiple = T)
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            
            tabPanel("Overview",
                     plotlyOutput("summaryPlot",height=600)
                     #tableOutput("summaryTable")
                     ),
            
            tabPanel("Weekly",
                     plotlyOutput("timePlot"),
                     plotlyOutput("corrPlot")
              
            ),
            
            tabPanel("Injury Type",
                     plotlyOutput("typePlot"),
                     plotlyOutput("upperlowerPlot")
                     
            )
            
            )
          )
        )
    )
  )
