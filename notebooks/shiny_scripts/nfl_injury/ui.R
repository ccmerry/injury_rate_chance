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
        conditionalPanel(condition="input.tabselected==1",
                         
                         selectizeInput("teamschoice",
                                        "Teams:",
                                        choices = team_list,
                                        multiple = T)
        ),
        
        conditionalPanel(condition="input.tabselected==3",
                         
                         selectizeInput("injurytypes",
                                        "Injury Types:",
                                        choices = injury_type_list,
                                        multiple = T),
                         
                         checkboxInput("corr", 
                                       label = "Correlation", 
                                       value = FALSE)
        ),
        
        conditionalPanel(condition="input.tabselected==2",
                         
                         checkboxInput("timecorr", 
                                       label = "Correlation", 
                                       value = FALSE)
                         
        ),
        conditionalPanel(condition="input.tabselected==6",
                         
                         selectizeInput("teamschoice",
                                        "Teams:",
                                        choices = team_list,
                                        multiple = T),
                         
                         selectizeInput("injurytypes",
                                        "Injury Types:",
                                        choices = injury_type_list,
                                        multiple = T)
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          
          tabPanel("Overview",
                   value=1,
                   conditionalPanel(condition = "input.choice==1"),
                   plotlyOutput("summaryPlot",height=600),
                   #tableOutput("summaryTable")
          ),
          
          tabPanel("Weekly",
                   value=2,
                   conditionalPanel(condition = "input.choice==2"),
                   plotlyOutput("timePlot"),
                   #plotlyOutput("corrPlot")
                   
          ),
          
          tabPanel("Injury Type",
                   value=3,
                   conditionalPanel(condition = "input.choice==3"),
                   plotlyOutput("typePlot"),
                   plotlyOutput("upperlowerPlot"),
                   plotlyOutput("corrtypePlot")
          ),
          
          tabPanel("Field Type",
                   value=4,
                   conditionalPanel(condition = "input.choice==4"),
                   plotlyOutput("fieldPlot"),
                   plotlyOutput("aclPlot")
          ),
          
          tabPanel("Years",
                   value=5,
                   conditionalPanel(condition = "input.choice==5"),
                   plotlyOutput("heatPlot")
          ),
          
          tabPanel("Data",
                   value=1,
                   conditionalPanel(condition = "input.choice==1"),
                   downloadButton('download',"Download the data"),
                   dataTableOutput('saveTable')),
          id = "tabselected"
        )
      )
    )
  )
)
