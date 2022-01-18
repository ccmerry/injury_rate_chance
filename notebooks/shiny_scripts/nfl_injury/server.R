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
  na_df <- injury_df %>%
    filter(!is.na(details))
  
  team_df <- na_df[column_filter]
  
  count_df <- team_df %>%
    count(Team)
  
  count_df <- count_df %>% 
    rename("Injuries" = "n" )
  
  count_df <- count_df %>%
    arrange(desc(Team))
  
  #df$Species <- factor(df$Species,levels=rev(unique(df$Species)))
  count_df$Team <- factor(count_df$Team, levels=(unique(count_df$Team)))
    

    output$distPlot <- renderPlotly({
      
      
      count_df %>%
        ggplot(aes(y = Team, x = Injuries)) +
        labs(title = "NFL Injury") +
        scale_y_discrete(limits = rev(levels("Teams"))) +
        geom_col() +
        ylab("Team")
    })

})
