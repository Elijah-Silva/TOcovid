#================
# LOAD PACKAGES #
#================
library(shiny)
library(leaflet)
library(RColorBrewer)
library(shinythemes)
library(ggplot2)
library(lubridate)
library(dplyr)
library(shinyWidgets)
library(tigris)
library(DT)
library(rgdal)
library(dplyr)
library(countrycode)
library(shinycssloaders)
library(data.table)
library(plotly)
library(shinyalert)
library(sqldf)
library(tidyr)
library(gsheet)

#=====
# GOOGLE SHEET
#=====
google <- gsheet2tbl('https://docs.google.com/spreadsheets/d/11KF1DuN5tntugNc10ogQDzFnW05ruzLH/edit#gid=1644462752')

#==============
# IMPORT DATA #
#==============
data <-
    read.csv(
        "https://ckan0.cf.opendata.inter.prod-toronto.ca/download_resource/e5bf35bc-e681-43da-b2ce-0242d00922ad?format=csv",
        header = TRUE
    )

data <- data %>%
    select("Neighbourhood.Name", "Reported.Date", "Outcome")

startdate = min(as.Date((data$Reported.Date)))
lastdate = max(as.Date((data$Reported.Date)))
data$Reported.Date <-
    as.Date(data$Reported.Date, format = "%Y-%m-%d")

data <-
    data[order(data$Neighbourhood.Name, data$Reported.Date),] #Order DF by neighborhood
data <-
    data[!(is.na(data$Neighbourhood.Name) |
               data$Neighbourhood.Name == ""), ] #Delete rows with no neighorhood

data <- data %>%
    rename(location = Neighbourhood.Name,
           date = Reported.Date,
           outcome = Outcome)

data <- data %>% #Add blank dates
    group_by(location) %>%
    do(date = seq(from = startdate, to = lastdate, by = 1)) %>%
    unnest(cols = c(date)) %>%
    left_join(data, by = c("location", "date")) %>%
    replace_na(list(dummy = 0)) %>%
    select(colnames(data))

data$new_deaths = ifelse(grepl("FATAL", data$outcome), "1", 0) #new_deaths
data$new_recovery = ifelse(grepl("RESOLVED", data$outcome), "1", 0) #new_recovery
data$new_cases = ifelse(grepl("(RESOLVED|ACTIVE)", data$outcome), "1", 0) #new_cases

#Begin here
data <- data %>%
    select("location", "date", "new_cases", "new_deaths", "new_recovery")

data$new_cases <- as.numeric(data$new_cases)
data$new_deaths <- as.numeric(data$new_deaths)
data$new_recovery <- as.numeric(data$new_recovery)

data <- data %>%
    group_by(location, date) %>%
    mutate(new_cases = sum(new_cases)) %>%
    mutate(new_deaths = sum(new_deaths)) %>%
    mutate(new_recovery = sum(new_recovery)) %>%
    distinct(location, .keep_all = TRUE)

data <-
    data %>% group_by(location) %>% mutate(total_cases = cumsum(new_cases)) #Total-cases
data <-
    data %>% group_by(location) %>% mutate(total_deaths = cumsum(new_deaths)) #Total-deathhs
data <-
    data %>% group_by(location) %>% mutate(total_recovery = cumsum(new_recovery)) #Total-recovery

data <- data %>%
    select(
        location,
        date,
        new_cases,
        total_cases,
        new_deaths,
        total_deaths,
        new_recovery,
        total_recovery
    ) #Re-organize

#===================
# IMPORT SHAPEFILE #
#===================
dataToday <-
    subset(data, date == lastdate) #Subset for last updated date

toronto_shp <- readOGR(
    #import spatial files
    dsn = paste0(getwd(), "/toneighshape/") ,
    layer = "Neighbourhoods v2_region",
    verbose = FALSE
)
toronto_shp <- spTransform(toronto_shp, CRS("+init=epsg:4326"))

#=======================
# RENAME NEIGHBORHOODS #
#=======================
toronto_shp@data$NAME[toronto_shp@data$NAME == "Centenial Scarborough"] = "Centennial Scarborough"
toronto_shp@data$NAME[toronto_shp@data$NAME == "L'Amoureaux"] = "L'Amoreaux"
toronto_shp@data$NAME[toronto_shp@data$NAME == "Wexford/Maryville"] = "Wexford/Maryvale"
toronto_shp@data$NAME[toronto_shp@data$NAME == "Brichcliffe-Cliffside"] = "Birchcliffe-Cliffside"
toronto_shp@data$NAME[toronto_shp@data$NAME == "East End Danforth"] = "East End-Danforth"
toronto_shp@data$NAME[toronto_shp@data$NAME == "O'Conner-Parkview"] = "O'Connor-Parkview"
toronto_shp@data$NAME[toronto_shp@data$NAME == "Flemington Park"] = "Flemingdon Park"
toronto_shp@data$NAME[toronto_shp@data$NAME == "Bridle Path-Sunnybrooke-York Mills"] = "Bridle Path-Sunnybrook-York Mills"
toronto_shp@data$NAME[toronto_shp@data$NAME == "Danforth Village East York"] = "Danforth-East York"
toronto_shp@data$NAME[toronto_shp@data$NAME == "Danforth Village Toronto"] = "Danforth"
toronto_shp@data$NAME[toronto_shp@data$NAME == "Cabbagetown-South St.Jamestown"] = "Cabbagetown-South St. James Town"
toronto_shp@data$NAME[toronto_shp@data$NAME == "North St.Jamestown"] = "North St. James Town"
toronto_shp@data$NAME[toronto_shp@data$NAME == "Oakwood-Vaughan"] = "Oakwood Village"
toronto_shp@data$NAME[toronto_shp@data$NAME == "Caledonia - Fairbanks"] = "Caledonia-Fairbank"
toronto_shp@data$NAME[toronto_shp@data$NAME == "Corsa Italia-Davenport"] = "Corso Italia-Davenport"
toronto_shp@data$NAME[toronto_shp@data$NAME == "Junction"] = "Junction Area"
toronto_shp@data$NAME[toronto_shp@data$NAME == "Rockliffe-Smythe"] = "Rockcliffe-Smythe"
toronto_shp@data$NAME[toronto_shp@data$NAME == "Lambton-Baby Point"] = "Lambton Baby Point"
toronto_shp@data$NAME[toronto_shp@data$NAME == "Dowsnview-Roding-CFB"] = "Downsview-Roding-CFB"
toronto_shp@data$NAME[toronto_shp@data$NAME == "Mimco"] = "Mimico (includes Humber Bay Shores)"
toronto_shp@data$NAME[toronto_shp@data$NAME == "Markland Woods"] = "Markland Wood"
toronto_shp@data$NAME[toronto_shp@data$NAME == "Crescent Town"] = "Taylor-Massey"

#==================
# MERGE SHAPEFILE #
#==================
toronto_merge <-
    geo_join(toronto_shp, dataToday, "NAME", "location")

#========================
# CREATE LEAFLET POPUPS #
#========================
popup_sb <-
    paste0(
        "<center>",
        "<strong>",
        "<code>",
        toronto_merge$location,
        "</code>",
        "</strong>",
        "</center>",
        br(),
        "Total Cases: ",
        "<strong>",
        format(
            toronto_merge$total_cases,
            big.mark = ",",
            scientific = FALSE
        ),
        "</strong>",
        "<br/>",
        "Total Deaths: ",
        "<strong>",
        format(
            toronto_merge$total_deaths,
            big.mark = ",",
            scientific = FALSE
        ),
        "</strong>",
        "<br/>",
        "New Cases: ",
        "<strong>",
        format(
            toronto_merge$new_cases,
            big.mark = ",",
            scientific = FALSE
        ),
        "</strong>",
        "<br/>",
        "New Deaths: ",
        "<strong>",
        format(
            toronto_merge$new_deaths,
            big.mark = ",",
            scientific = FALSE
        ),
        "</strong>"
    )

#=======================
# COLOR PALETTE GGPLOT #
#=======================
palC <-
    colorNumeric("YlOrBr",
                 domain = toronto_merge@data$total_cases,
                 na.color = "transparent")
palD <-
    colorNumeric("YlOrBr",
                 domain = toronto_merge@data$total_deaths,
                 na.color = "transparent")

palNC <-
    colorNumeric("YlOrBr",
                 domain = toronto_merge@data$new_cases,
                 na.color = "transparent")

palND <-
    colorNumeric("YlOrBr",
                 domain = toronto_merge@data$new_deaths,
                 na.color = "transparent")

#==============
# GGPLOT TEXT #
#==============
themeGG <- theme(
    plot.background = element_rect(fill = "white"),
    plot.title = element_text(
        size = 25,
        family = "Avenir",
        color = 'black',
        hjust = 0.5,
        lineheight = 1.2
    ),
    axis.title.y = element_text(
        size = 15,
        family = "Avenir",
        color = "black"
    ),
    axis.title.x = element_text(
        size = 15,
        color = "black",
        family = "Avenir"
    ),
    axis.text.y = element_text(
        size = 10,
        family = "Avenir",
        color = "black"
    )
)

#=======================
# BEGIN USER INTERFACE #
#=======================
ui <- shinyUI(navbarPage(
    theme = shinytheme("flatly"),
    "Toronto COVID-19 Tracker",
    tabPanel("Data Explorer",
             div(class = "outer",
                 fluidPage(fluidRow(
                     column(12,
                            align = 'center',
                            br(),
                            fluidRow(
                                column(
                                    2,
                                    "",
                                    fluidRow(column(
                                        12,
                                        h5("Total Cases:", align = "center"),
                                        h1(textOutput("worldCase"),
                                           style = "color:darkblue",
                                           align = "center"),
                                    )),
                                    fluidRow(column(
                                        12,
                                        div(style = 'height: 220px; overflow-y: scroll',
                                            dataTableOutput("totalCasesTable"))
                                    )),
                                    fluidRow(
                                        column(
                                            12,
                                            hr(),
                                            useShinyalert(),
                                            selectInput(
                                                "area",
                                                "Neighborhood:",
                                                choices =
                                                    dataToday$location,
                                                selected = "Willowdale",
                                            ),
                                            radioButtons(
                                                "radio",
                                                label = "Statistic:",
                                                inline = TRUE,
                                                selected = "Cases",
                                                choices = c(
                                                    "Cases",
                                                    "Deaths",
                                                    "New Cases",
                                                    "Cases per Million",
                                                    "Deaths per Million"
                                                ),
                                            ),
                                            actionButton("help", "Quick Tip!", style =
                                                             'padding:4px; font-size:95%'),
                                            hr(),
                                        )
                                    ),
                                    fluidRow(column(
                                        12,
                                        h6("Last Updated:", lastdate),
                                        h6(
                                            "Source:",
                                            tags$a(href = "https://open.toronto.ca/dataset/covid-19-cases-in-toronto/", "Toronto Public Health")
                                        ),
                                        tags$h6(
                                            "Created by Elijah Silva",
                                            br(),
                                            a(href = "https://www.linkedin.com/in/elijahsilva/", "LinkedIn"),
                                            " | ",
                                            a(href = "https://github.com/Elijah-Silva/covid", "Github"),
                                            align = "center"
                                        )
                                    ))
                                ),
                                column(6,
                                       withSpinner(leafletOutput("map",
                                                                 height = 711), type =
                                                       6)),
                                column(4,
                                       fluidRow(
                                           tags$head(tags$script('!function(d,s,id){var js,fjs=d.getElementsByTagName(s)    
                                                                 [0],p=/^http:/.test(d.location)?\'http\':\'https\';if(!d.getElementById(id)){js=d.createElement(s);js.
                                                                 id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,
                                                                 "script","twitter-wjs");')),
                                                   a("@TOPublicHealth",class="twitter-timeline",height="300",href="https://twitter.com/TOPublicHealth", "data-widget-id"=553910391399383040)     

                                       ),
                                       fluidRow(
                                           column(
                                               12,
                                               withSpinner(plotlyOutput("plot1"), type =
                                                               6),
                                               dateRangeInput(
                                                   "dateRange",
                                                   label = "Filter Date Range:",
                                                   min = min(data$date),
                                                   max = max(data$date),
                                                   start = startdate,
                                                   end = max(data$date)
                                               )
                                           )
                                       ))
                                
                            ))
                 ))))
))

################
# START SERVER #
################
server <- function(input, output) {
    location <- data$location
    dates <- format(data$date, format = "%Y-%m-%d")
    total_cases <- data$total_cases
    total_deaths <- data$total_deaths
    new_cases <- data$new_cases
    
    df <-
        data.frame(dates,
                   location,
                   total_cases,
                   total_deaths,
                   new_cases)
    
    df_subset <- reactive({
        subset(
            df,
            location == input$area &
                as.Date(dates) >= input$dateRange[1] &
                as.Date(dates) <= input$dateRange[2]
        )
    })
    
    observe({
        if (input$radio == "Cases") {
            output$plot1 <- renderPlotly({
                ggplotly({
                    ggCases <- ggplot(data = df_subset(), aes(x = as.Date(dates))) +
                        geom_line(aes(y = total_cases),
                                  color = 'darkred',
                                  group = 5) +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                        scale_x_date() +
                        themeGG +
                        labs(title = "Number of Cases",
                             y = "Number of Cases",
                             x = "Date")
                    ggCases
                })
            })
        }
        
        if (input$radio == "Deaths") {
            output$plot1 <- renderPlotly({
                ggplotly({
                    ggDeaths <- ggplot(data = df_subset(), aes(x = as.Date(dates))) +
                        geom_line(aes(y = total_deaths),
                                  color = 'steelblue',
                                  group = 5) +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                        scale_x_date() +
                        labs(title = "Number of Deaths",
                             y = "Number of Deaths",
                             x = "Date") +
                        themeGG
                    
                    ggDeaths
                })
                
            })
        }
        if (input$radio == "New Cases") {
            output$plot1 <- renderPlotly({
                ggplotly({
                    ggNew <-
                        ggplot(data = df_subset(), aes(x = as.Date(dates), y = new_cases)) +
                        geom_bar(stat = "identity") +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                        scale_x_date() +
                        labs(title = "New Cases (last 24 hours)",
                             y = "Number of New Cases",
                             x = "Date") +
                        themeGG
                    
                    ggNew
                })
            })
        }
    })
    
    #==========
    # LEAFLET #
    #==========
    
    observe({
        if (input$radio == "Cases") {
            output$map <- renderLeaflet({
                leaflet(toronto_merge) %>%
                    addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
                    setView(-79.3832, 43.6532, zoom = 10) %>%
                    addPolygons(
                        data = toronto_merge,
                        fillColor = ~ palC(toronto_merge$total_cases),
                        fillOpacity = 0.7,
                        weight = 0.2,
                        smoothFactor = 0.2,
                        popup = ~ popup_sb
                    ) %>%
                    addLegend(
                        pal = palC,
                        values = toronto_merge$total_cases,
                        position = "bottomright",
                        title = "Total Cases"
                    )
            })
        }
        if (input$radio == "Deaths") {
            output$map <- renderLeaflet({
                leaflet() %>%
                    leaflet(toronto_merge) %>%
                    addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
                    setView(-79.3832, 43.6532, zoom = 10) %>%
                    addPolygons(
                        data = toronto_merge ,
                        fillColor = ~ palD(toronto_merge$total_deaths),
                        fillOpacity = 0.7,
                        weight = 0.2,
                        smoothFactor = 0.2,
                        popup = ~ popup_sb
                    ) %>%
                    addLegend(
                        pal = palD,
                        values = toronto_merge$total_deaths,
                        position = "bottomright",
                        title = "Total Deaths"
                    )
            })
        }
        if (input$radio == "New Cases") {
            output$map <- renderLeaflet({
                leaflet(toronto_merge) %>%
                    addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
                    setView(-79.3832, 43.6532, zoom = 10) %>%
                    addPolygons(
                        data = toronto_merge ,
                        fillColor = ~ palNC(toronto_merge$new_cases),
                        fillOpacity = 0.7,
                        weight = 0.2,
                        smoothFactor = 0.2,
                        popup = ~ popup_sb
                    ) %>%
                    addLegend(
                        pal = palNC,
                        values = toronto_merge$new_cases,
                        position = "bottomright",
                        title = "New Cases"
                    )
            })
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
