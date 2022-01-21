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
  
  
  injury_filter_list <- reactive({
    if (is_empty(input$injurytypes)) {
      date_injury_list_filter <- append(injury_type_list,"week_num")
    }
    else{
      date_injury_list_filter <- append(input$injurytypes,"week_num")
    }
  })
  
  injury_type_filter_df <- reactive({
    if (is_empty(input$injurytypes)) {
      date_injury_sum_df[injury_filter_list()] %>%
        pivot_longer(cols = -week_num, names_to = "Type", values_to = "Number") %>%
        arrange(week_num)
    }
    else{
      date_injury_sum_df[injury_filter_list()] %>%
        pivot_longer(cols = -week_num, names_to = "Type", values_to = "Number") %>%
        arrange(week_num)
    }
  })
  
  
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
      labs(title = "Total Injuries by Team") +
      scale_y_discrete(limits = rev(levels("Teams"))) +
      geom_col(fill = "darkblue") +
      theme(axis.text.x = element_text(face = "bold", size=rel(1.0)),
            axis.text.y = element_text(face = "bold", size=rel(1.0)),
            axis.title = element_text(size=rel(1.2)),
            legend.text = element_text(size=rel(.8)),
            legend.title = element_text(size=rel(.8)),
            plot.title = element_text(hjust = 0.5, size=rel(1.5))) +
      ylab("Team")
  })
  
  
  
  #Plots for the Injuries by week
  
  
  output$timePlot <- renderPlotly({
    
    date_sort %>%
      ggplot(aes(x = week_num, y = Injuries)) +
      labs(title = "NFL Injuries by Week") +
      geom_col(fill = "darkblue") +
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.5))) +
      xlab("Week of the Season")
  })
  
  output$corrPlot <- renderPlotly({
    
    corr_data %>%
      ggplot(aes(x=week_num, y=Injuries)) + 
      geom_point(color="black") +
      geom_smooth(method=lm, color="blue") +
      labs(title="Injury by Week Correlation",
         x="Week", y = "Number of Injuries")
  })
  
  
  
  
  
  output$summaryTable <- renderTable({
    team_filter_df()
  })
  
  
  
  #Plots for the type of injury
  
  output$typePlot <- renderPlotly({
    
    injury_type_filter_df() %>%
      ggplot(aes(x=week_num, y=Number, fill=Type)) +
      labs(title = "NFL Injury Type by Week") +
      geom_col(position = "dodge") +
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.5))) +
      xlab("Week of the Season") +
      ylab("Cases")
  })
  
  output$upperlowerPlot <- renderPlotly({
    
    upper_lower_df %>%
      ggplot(aes(x=week_num, y=Number, fill=Type)) +
      labs(title = "Upper Body & Lower Body Injury by Week") +
      geom_col(position = "dodge") +
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.5))) +
      xlab("Week of the Season") +
      ylab("Cases")
  })
  
  
})
