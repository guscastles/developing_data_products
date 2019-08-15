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
            g <- ggplot(data.frame(x=1:10), aes(x))
            g + geom_bar() +
                coord_flip()
        }
    })
})
