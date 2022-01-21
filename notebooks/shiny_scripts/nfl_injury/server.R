#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  column_filter <- c("Team","details")
  
  team_df <- injury_na_df[column_filter] %>%
    count(Team) %>%
    rename("Injuries" = "n") %>%
    arrange(desc(Team))
  
  team_df$Team <- factor(team_df$Team, levels=(unique(team_df$Team)))
  
  team_filter_df <- reactive({
    if (is_empty(input$teamschoice)) {
      team_df
    }
    else{
      team_df %>%
        filter(Team %in% input$teamschoice)
    }
  })
  
  
  output$summaryPlot <- renderPlotly({
    
    team_filter_df() %>%
      ggplot(aes(y = Team, x = Injuries)) +
      labs(title = "NFL Injury") +
      scale_y_discrete(limits = rev(levels("Teams"))) +
      geom_col() +
      ylab("Team")
  })
  
  output$timePlot <- renderPlotly({
    
    date_sort %>%
      ggplot(aes(x = week_num, y = Injuries)) +
      labs(title = "NFL Injury") +
      geom_col() +
      xlab("Week of the Season")
  })
  
  output$summaryTable <- renderTable({
    team_filter_df()
  })
  
})
