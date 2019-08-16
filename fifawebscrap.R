library(rvest)
library(stringr)

tournaments <- function(url) {
    read_html(url) %>%
        html_nodes("li.comp-item > a") %>%
        html_attr("href") %>%
        (function(href) {paste0(main_website, href)})
}

changeGermany <- function(team) {
    if(team == "Germany FR") "Germany" else as.character(team)
}

hostYear <- function(edition) {
    host <- sub("\\d+", "", edition)
    year <- as.numeric(sub("\\D+", "", edition))
    list(host, year)
}

tournamentData <- function(dataset, edition) {
    host_year <- hostYear(edition)
    nRows <- dim(dataset)[1]
    data.frame(Host=rep(host_year[[1]], nRows),
               Year=rep(as.numeric(host_year[[2]]), nRows),
               Team=dataset["Team"],
               Goals.for=as.numeric(unlist(dataset["Goals for"])),
               Penalty.goal=as.numeric(unlist(dataset["Penalty goal"]))
    )
}

pastStats <- function(url) {
    stats_url <- gsub("index.html", "statistics/teams/goal-scored.html", url)
    edition <- str_match(url, "archive/(.+)/index")[2]
    root <- read_html(stats_url)
    stats <- html_nodes(root, "table.tbl-statistics > tbody > tr > td") %>%
        sapply(html_text)
    header <- html_nodes(root, "table.tbl-statistics > thead > tr > th > span.th-text") %>%
        html_text %>%
        str_trim %>%
        sapply(function(text) {substring(text, 1, str_length(text) - 2)})
    df <- as.data.frame(matrix(stats, ncol = length(header), byrow = TRUE))
    colnames(df) <- header
    df$Team <- df$Teams %>% sapply(changeGermany)
    tournamentData(df[,-2], edition)
}

lastStats <- function(url) {
    root <- read_html(url)
    stats <- html_nodes(root, "table#goal-scored > tbody > tr > td") %>%
        sapply(html_text)
    header <- html_nodes(root, "table#goal-scored > thead > tr > th") %>%
        html_text %>% cleanText
    df <- as.data.frame(matrix(stats, ncol = length(header), byrow = TRUE))
    colnames(df) <- c("Position", "Team", header[-c(1, 2)])
    df$Team <- df$Team %>% cleanText %>% sapply(changeGermany)
    tournamentData(df, "russia2018")
}

cleanText <- function(text) {
    text %>%
        str_trim %>%
        (function(text) {gsub("\r.+", "", text)})
}

pastWinners <- function(url) {
    xPath <- "//h3[contains(text(), 'Winner')]/following-sibling::div[@class='t-n']/span[@class='t-nText ']/a"
    urls <- tournaments(url) %>%
        lapply(function(text) {gsub("/index", "/awards/index", text)})
    winners <- data.frame(Edition=c(), Winner=c())
    for (url in urls) {
        finalists <- read_html(url) %>% html_nodes(xpath = xPath) %>% html_text
        edition <- str_match(url, "/(\\w+\\d{4})/")[2]
        winners <- rbind(winners, data.frame(Edition=edition, Winner=finalists[1]))
    }
    winners$Winner <- sapply(winners$Winner, changeGermany)
    winners
}

lastWinner <- function(url) {
    xPath <- "//span[@class='fi-t__nText ']"
    finalists <- read_html(url) %>% html_nodes(xpath = xPath) %>% html_text
    data.frame(Edition="russia2018", Winner=finalists[1])
}
