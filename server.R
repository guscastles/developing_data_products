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
library(rpart)

shinyServer(function(input, output) {

    winnersData <- repmis::source_data("https://guscastles.github.io/developing_data_products/winners.csv")
    
    predictNextEditions <- function(data, xMin, xMax) {
        
        predictedTeam <- function(data) {
            names(data)[which(data == max(data))]
        }
        
        predictedGoals <- function(data, team) {
            data %>% filter(Winner == team) %>% select(Goals) %>% sapply(median) %>% as.integer
        }
        
        if (input$totalGoals == "Per Tournament") {
            model <- lm(Goals ~ Year, data = data)
            prediction <- predict(model, data.frame(Year=c(2022, 2026))) %>% as.integer
            rbind(data, data.frame(Year=c(2022, 2026), Goals=prediction)) %>%
                filter(Year >= xMin, Year <= xMax)
        } else {
            winnersModel <- rpart(Winner ~ Year + Goals, data = data, method = "class")
            goals <- data$Goals %>% median
            prediction <- predict(winnersModel, data.frame(Year=c(2022, 2026), Goals=c(goals, goals)))
            team1 <- predictedTeam(prediction[1,])[1]
            team2 <- predictedTeam(prediction[2,])[2]
            goals1 <- predictedGoals(data, team1)
            goals2 <- predictedGoals(data, team2)
            rbind(data, data.frame(Year=c(2022, 2026), Winner=c(team1, team2), Goals=c(goals1, goals2))) %>%
                filter(Year >= xMin, Year <= xMax)
        }
    }
    
    perTournamentData <- function(data) {
        dt <- data %>% select(Year, Goals.for, Penalty.goal) %>% mutate(Goals = Goals.for)
        list(data=aggregate(Goals ~ Year, data=dt, FUN= sum), nudge_y=-20, colour="white")
    }
    
    perWinnerData <- function(data, winners) {
        plotData <- merge(data, winners, by="Year") %>% select(Team, Year, Winner, Goals.for, Penalty.goal) %>%
            mutate(Goals = Goals.for) %>% filter(Team == Winner) %>% select(Year, Winner, Goals)
        list(data=plotData, nudge_y=1.5, colour="darkblue")
    }
    
    fetchData <- function() {
        repmis::source_data("https://guscastles.github.io/developing_data_products/tournaments.csv")
    }
    
    output$linePlot <- renderPlot({
        set.seed(2019-08-15)
        xMin <- input$editions[1]
        xMax <- input$editions[2]
        data <- fetchData()
        dataByDates <- data[(data$Year >= xMin) & (data$Year <= xMax),]
        plotData <- if (input$totalGoals == "Per Tournament") perTournamentData(data) else perWinnerData(data, winnersData)
        plotData[["predicted"]] <- plotData[["data"]] %>% predictNextEditions(xMin, xMax) %>%
                                       filter(Year >= xMin, Year <= xMax)
        xLab <- "Year"
        yLab <- "Goals Scored"
        mainTitle <- paste("Goals Scored", input$totalGoals)
        xLabels = seq(xMin, xMax, 4)
        g <- ggplot(plotData[["predicted"]], aes(Year, Goals)) +
                geom_bar(stat = "identity", aes(fill = Year)) +
                geom_line(color = "red", lwd = 2) + xlab(xLab) + ylab(yLab) + ggtitle(mainTitle) +
                scale_x_continuous(breaks=xLabels, labels=xLabels) + theme(legend.position = "none") +
                geom_text(aes(label=Goals, fontface="bold"), colour = plotData[["colour"]],
                          nudge_y = plotData[["nudge_y"]])
        if (input$totalGoals == "Per Winner") {
            g <- g + geom_text(aes(label=Winner, fontface="bold", angle=90), colour = "white", nudge_y = -3)
        }
        g
    })
  
    output$onlyWinners <- renderPlot({
        if (input$onlyWinners) {
            winners <- winnersData %>% select(Winner) %>% group_by(Winner) %>% count
            winners <- winners[order(winners$n, decreasing = TRUE),]
            g <- ggplot(winners, aes(x = reorder(Winner, n), y = n))
            g + geom_bar(stat = "identity", aes(fill = Winner)) +
                coord_flip() + xlab("Countries") + ylab("Number Of Wins") +
                ggtitle("Wins per Country") + theme(legend.position = "none")
                
        }
    })
})
