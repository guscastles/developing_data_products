#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(
    titlePanel("The History Of FIFA World Cup, In Goals"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("editions", "Editions:", value = c(2002, 2018),
                        min = 1930, max = 2026, step = 4),
            radioButtons("totalGoals", label = "Total Goals",
                         choices = c("Per tournament", "Per winner"),
                         choiceValues = c("tournament", "winner")
            ),
            checkboxInput("onlyWinners", "Total wins per country")
        ),
        mainPanel(
            plotOutput("linePlot"),
            textOutput("totalGoals"),
            plotOutput("onlyWinners")
        )
    )
))
