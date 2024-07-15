
library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(here)

# use to generate the necessary files...one time only
source(here("code/get_hucs_cntys.R"))

library(DBI)
library(RSQLite)
db <- dbConnect(RSQLite::SQLite(), here("h12_spp/data/cemap_terrestrial.sqlite"))
dbListTables(db_conn)

# Connect and Inquire -----------------------------------------------------

# to query, we can write code to filter/select THEN collect at the end
sites <- tbl(db_conn, "site_locations_combined") |> collect()


fluidPage(
    titlePanel("CA Counties"),
    sidebarLayout(
        sidebarPanel(
            sliderInput('years', 'Year Range',sep = "",
                        min = 2021, max = 2023,
                        value = c(2021, 2023)),
            selectInput('cnty', 'County',
                        ca_cnty$name,
                        multiple = FALSE)),
            #selectInput('huc8', 'HUC8',
            #            hucs$huc8$name)),
        mainPanel(
            leafletOutput(outputId = 'map'),
            br(),br(),br(),
            textOutput('text')
        )
    )
)
