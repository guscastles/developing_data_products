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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
    output$linePlot <- renderPlot({
        set.seed(2019-08-15)
        xMin <- input$editions[1]
        xMax <- input$editions[2]
        yMin <- 10
        yMax <- 20
        xData <- seq(xMin, xMax, by=4)
        yData <- runif(length(xData), yMin, yMax)
        xLab <- input$totalGoals
        yLab <- "Goals Scored"
        g <- ggplot(data.frame(x=xData, y=yData), aes(x, y))
        g + geom_line() +
            xlim(c(1926, 2030)) +
            ylim(c(1, 30)) +
            xlab(xLab) +
            ylab(yLab)
    })
  
    output$onlyWinners <- renderPlot({
        if (input$onlyWinners) {
            data <- repmis::source_data("https://guscastles.github.io/developing_data_products/winners.csv")
            winners <- data %>%
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
