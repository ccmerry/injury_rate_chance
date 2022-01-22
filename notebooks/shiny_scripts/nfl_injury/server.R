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
  
  column_filter <- c("Team","details","Field")
  
  team_df <- injury_na_df[column_filter] %>%
    count(Team) %>%
    rename("Injuries" = "n") %>%
    arrange(desc(Team))
  
  team_df$Team <- factor(team_df$Team, levels=(unique(team_df$Team)))
  
  team_df <- merge(x=team_df, y=nfl_stadium_df, by="Team", all.x=TRUE)
  
  
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
  
  save_table_df <- reactive({
    if (is_empty(input$teamschoice)) {
      injury_na_df
    }
    else{
      injury_na_df %>%
        filter(Team %in% input$teamschoice)
    }
  })
  
  
  
  
  output$summaryPlot <- renderPlotly({
    
    team_filter_df() %>%
      ggplot(aes(y = Team, x = Injuries, fill = Field)) +
      labs(title = "Total Injuries by Team") +
      scale_y_discrete(limits = rev(levels("Teams"))) +
      geom_col() +
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
    
    if(input$timecorr){
      date_sort %>%
        filter(week_num != 18) %>%
        ggplot(aes(x = week_num, y = Injuries)) +
        labs(title = "NFL Injuries by Week") +
        geom_col(fill = "darkblue") +
        geom_smooth(method=lm, color="blue") +
        theme(plot.title = element_text(hjust = 0.5, size=rel(1.5))) +
        xlab("Week of the Season")
    }
    else{
      date_sort %>%
        ggplot(aes(x = week_num, y = Injuries)) +
        labs(title = "NFL Injuries by Week") +
        geom_col(fill = "darkblue") +
        theme(plot.title = element_text(hjust = 0.5, size=rel(1.5))) +
        xlab("Week of the Season")
    }
  })
  
  #output$corrPlot <- renderPlotly({
    
    #corr_data %>%
      #ggplot(aes(x=week_num, y=Injuries)) + 
      #geom_point(color="black") +
      #geom_smooth(method=lm, color="blue") +
      #labs(title="Injury by Week Correlation",
         #x="Week", y = "Number of Injuries")
  #})
  
  
  
  
  
  output$summaryTable <- renderTable({
    team_filter_df()
  })
  
  
  
  #Plots for the type of injury
  
  output$typePlot <- renderPlotly({
    
    if(input$corr){
      injury_type_filter_df() %>%
        filter(week_num != 18) %>%
        ggplot(aes(x=week_num, y=Number, fill=Type)) +
        labs(title = "NFL Injury Type by Week") +
        geom_col(position = "dodge") +
        geom_smooth(method=lm, color="blue") +
        theme(plot.title = element_text(hjust = 0.5, size=rel(1.5))) +
        xlab("Week of the Season") +
        ylab("Cases")
    }
    else{
      injury_type_filter_df() %>%
        ggplot(aes(x=week_num, y=Number, fill=Type)) +
        labs(title = "NFL Injury Type by Week") +
        geom_col(position = "dodge") +
        theme(plot.title = element_text(hjust = 0.5, size=rel(1.5))) +
        xlab("Week of the Season") +
        ylab("Cases")
    }
  })
  
  output$upperlowerPlot <- renderPlotly({
    
    if(input$corr){
      upper_lower_df %>%
        filter(week_num != 18) %>%
        ggplot(aes(x=week_num, y=Number, fill=Type)) +
        labs(title = "Upper Body & Lower Body Injury by Week") +
        geom_col(position = "dodge") +
        geom_smooth(method=lm, color="blue") +
        theme(plot.title = element_text(hjust = .5, size=rel(1.5))) +
        xlab("Week of the Season") +
        ylab("Cases")
    }
    else{
      upper_lower_df %>%
        ggplot(aes(x=week_num, y=Number, fill=Type)) +
        labs(title = "Upper Body & Lower Body Injury by Week") +
        geom_col(position = "dodge") +
        theme(plot.title = element_text(hjust = .5, size=rel(1.5))) +
        xlab("Week of the Season") +
        ylab("Cases")
    }
  })
  
  output$fieldPlot <- renderPlotly({
    injury_lower_turf %>%
      ggplot(aes(x= week_num, y=Type, fill=Field)) +
      geom_col(position = "dodge") +
      labs(title = "Comparing Playing Surfaces") +
      xlab("Week of the Season") +
      ylab("Cases")
  })
  
  output$aclPlot <- renderPlotly({
    injury_acl_turf %>%
      ggplot(aes(x= week_num, y=Type, fill=Field)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      xlab("Week of the Season") +
      ylab("Cases")
  })
  
  output$heatPlot <- renderPlotly({
    year_week_num_df %>%
      ggplot(aes(x=week_num, y=nfl_year, fill= count)) + 
      geom_tile() +
      scale_x_continuous(expand=c(0,0)) + 
      scale_y_continuous(expand=c(0,0)) +
      theme(legend.position="none") +
      labs(title = "Injuries through the Years") +
      xlab("Week of the Season") +
      ylab("Year")
  })
  
  #output$corrtypePlot <- renderPlotly({
    
    #injury_type_filter_df() %>%
      #ggplot(aes(x=week_num, y=Number, shape=Type)) + 
      #geom_point() +
      #geom_smooth(method=lm, color="blue") +
      #labs(title="Injury by Week Correlation",
           #x="Week", y = "Number of Injuries")
  #})
  
  output$saveTable <- renderDataTable({
    save_table_df()
    })
  
  output$download <- downloadHandler(
    filename = function(){"NFL_injury_rhsiny_data.csv"}, 
    content = function(fname){
      write.csv(save_table_df(), fname)
    }
  )
  
})
