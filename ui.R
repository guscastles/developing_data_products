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
            sliderInput("editions", "Editions", value = c(2002, 2018),
                        min = 1930, max = 2026, step = 4),
            radioButtons("totalGoals", label = "Total Goals",
                         choices = c("Per tournament", "Per winner")),
            checkboxInput("onlyWinners", "Wins per country"),
            h3("How To Use The App"),
            h4("Editions"),
            div("The slide bar gives the years of each edition."),
            div("Choose any period of time to display the correspondent data."),
            h4("Total Goals"),
            div("Choose between total goals from the tournament or just from the winners."),
            h4("Wins Per Country"),
            div("If checked, displays the total wins per country."),
            div("Only winners are diplayed.")
        ),
        mainPanel(
            plotOutput("linePlot"),
            textOutput("totalGoals"),
            plotOutput("onlyWinners")
        )
    )
))
