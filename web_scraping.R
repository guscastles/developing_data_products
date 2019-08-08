library(rvest)
library(dplyr)

url <- "https://en.wikipedia.org/wiki/List_of_national_parks_of_Australia"

fetch_parks <- function(url) {
    webpage <- read_html(url)
    state_sections <- html_nodes(webpage, 'h2 > span.mw-headline')
    list(states=html_text(state_sections[1:9]),
         parks=html_nodes(webpage, 'table.wikitable.sortable')[1:9])
}

get_coordinates <- function(record) {
    park_coordinates <- gsub("[\n ]+", "", tail(strsplit(record, "/")[[1]], 1))
    coordinates <- strsplit(park_coordinates, ";")
    latitude <- as.numeric(coordinates[[1]][1])
    longitude <- as.numeric(coordinates[[1]][2])
    data.frame(list("latitude"=latitude,
                    "longitude"=longitude))
}

create_empty_dataframe <- function() {
    data.frame("division"=character(),
               "name"=character(),
               "coordinates"=character(),
               stringsAsFactors = FALSE)
}

new_row <- function(division, name, coordinates) {
      data.frame("division"=division,
                 "name"=name,
                 "coordinates"=coordinates,
                 stringsAsFactors = FALSE)
}

build_dataframe <- function(nat_parks, divisions) {
    dataframe <- create_empty_dataframe()
    for (park in nat_parks) {
        div <- divisions[1]
        divisions <- divisions[-1]
        park_info <- html_nodes(park, 'tbody > tr')
        for (record in park_info[-1]) {
            info <- html_nodes(record, 'td')
            pk_name <- gsub("\n|\\[[0-9]+\\]","", html_text(info[1]))
            coord <- gsub("^ +|\n","", html_text(info[2]))
            dataframe <- rbind(dataframe, new_row(div, pk_name, coord))
        }
    }
    dataframe %>% filter(coordinates != "")
}

parks_dataset <- function(parks) {
    divisions <- parks[['states']]
    nat_parks <- parks[['parks']]
    dataframe <- build_dataframe(nat_parks, divisions)
    dataframe['division'] <- as.factor(dataframe[, 'division'])
    dataframe
}

add_coordinates <- function(dataframe) {
    coordinates <- lapply(dataframe[,'coordinates'], get_coordinates)
    dataframe['latitude'] <- sapply(coordinates, function(x) {c(x$latitude)})
    dataframe['longitude'] <- sapply(coordinates, function(x) {x$longitude})
    dataframe[, c(-3)]
}

get_parks <- function() {
    url %>%
         fetch_parks %>%
         parks_dataset %>%
         add_coordinates
}