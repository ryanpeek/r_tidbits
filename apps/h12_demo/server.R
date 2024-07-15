
library(shiny)
library(dplyr)

function(input, output, session){
    cnty_df <- reactive({
        ca_cnty |> filter(name == input$cnty)
    })
    #huc12_df <- reactive({
    #    st_filter(hucs$huc12, ca_cnty[ca_cnty$name==input$cnty,])
    #})
    output$map <- renderLeaflet({
        leaflet()  |>
            addTiles()  |>
            addPolygons(data = cnty_df(),group = "County",
                             color = "royalblue")# |>
            #addPolygons(data = huc12_df(),group = "HUC12",
            #        color = "orange", fill = FALSE, opacity = 0.9,
            #        popup = ~paste0(huc12_df()$huc, "<br>",
            #                        huc12_df()$name))
    })

    output$text <- renderText(
        if(DBI::dbIsValid(db)){
        glue("Database is connected!")
    } else({ "Uh oh...nope!"})
    )
}

