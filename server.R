#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(repmis)

shinyServer(function(input, output) {
  
    winnersData <- repmis::source_data("https://guscastles.github.io/developing_data_products/winners.csv")
    
    output$linePlot <- renderPlot({
        set.seed(2019-08-15)
        xMin <- input$editions[1]
        xMax <- input$editions[2]
        data <- repmis::source_data("https://guscastles.github.io/developing_data_products/tournaments.csv")
        dateFilter <- (data$Year >= xMin) & (data$Year <= xMax)
        if (input$totalGoals == "Per Tournament") {
            dt <- data[dateFilter,] %>%
                      select(Year, Goals.for, Penalty.goal) %>%
                      mutate(Goals = Penalty.goal + Goals.for)
            plotData <- aggregate(Goals ~ Year, data=dt, FUN= sum)
        } else {
            goalsByWinner <- winnersData %>%
                                 select(Year)
            dt <- data[dateFilter,] %>%
                select(Year, Goals.for, Penalty.goal) %>%
                mutate(Goals = Penalty.goal + Goals.for)
            plotData <- aggregate(Goals ~ Year, data=dt, FUN= sum)
        }
        xLab <- "Year"
        yLab <- "Goals Scored"
        mainTitle <- paste("Goals Scored", input$totalGoals)
        xLabels = seq(xMin, xMax, 4)
        g <- ggplot(plotData, aes(x=Year, y=Goals))
        g + geom_bar(stat = "identity", aes(fill = Year)) +
            geom_line(color = "red", lwd = 2) +
            xlab(xLab) +
            ylab(yLab) +
            ggtitle(mainTitle) +
            scale_x_continuous(breaks=xLabels, labels=xLabels) +
            theme(legend.position = "none") +
            geom_text(aes(label=Goals, fontface="bold"), colour = "white", nudge_y = -20)
    })
  
    output$onlyWinners <- renderPlot({
        if (input$onlyWinners) {
            winners <- winnersData %>%
                           select(Winner) %>%
                           group_by(Winner) %>%
                           count
            winners <- winners[order(winners$n, decreasing = TRUE),]
            g <- ggplot(winners, aes(x = reorder(Winner, n), y = n))
            g + geom_bar(stat = "identity", aes(fill = Winner)) +
                coord_flip() +
                xlab("Countries") +
                ylab("Number Of Wins") +
                ggtitle("Number Of Wins per Country") +
                theme(legend.position = "none")
                
        }
    })
})
