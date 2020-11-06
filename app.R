library(leaflet)
library(readr)
library(dplyr)
library(shiny)
library(shiny.semantic)
options(semantic.themes=T)

# Subset of ship data with only pertinent columns for quicker loading
ships <- read_csv("ship_data.csv")

moduleServer <- function(id, module) {
  callModule(module, id)
}

# Dropdown input modules & server functionality

menuInput <- function(id) {
  dropdown_input(id, levels(as.factor(ships$ship_type)),
                 default_text="Filter by ship type")
}

menuInput2 <- function(id) {
  dropdown_input(id,levels(as.factor(ships$SHIPNAME)),default_text="Select ship name")
}

menuServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      renderText(input[[id]])
    }
  )
}

ui <- semanticPage(
  title="Ship Dashboard for Appsilon",
  div(class="ui grid",style="padding:5%",
      div(class="two column row",
          div(class="column",
              segment(
              h4(class = "ui dividing header", "Ship Lookup"),
                field(menuInput("menu1")),
                field(menuInput2("menu2"))),
              segment(
                htmlOutput("note")
                )
              ),
          div(class="column",
              segment(
              form(leafletOutput("shipmap"))),
              segment(
                field(checkboxInput("travel_path","Show travel paths",value = T)),
                field(checkboxInput("highlight","Highlight longest distance traveled",value = T)),
                div(class="ui divider"),
                field(
                  div(style="display: flex;flex-direction: column;align-items: center;justify-content: center;",
                      actionButton("zoom",class="ui labeled icon button", "Zoom to highlight",
                             icon = icon("search location")
                             )
                      )
                  )
                )
              )
          )
      )
)

server <- function(input, output, session) {
  
  
  # Ship lookup menus
  menuServer("menu1")
  menuServer("menu2")
  
  # Store data on selected ship in reactiveValues to call in multiple output funtions
  maxdist_data <- reactiveValues(lon=c(0,0),lat=c(0,0),time=c(0,0),std_dist=0,mean_dist=0)
  
  # Math helper functions to convert lon/lat data to distances
  getrad <- function(deg) {(deg * pi) / (180)}
  
  haversine <- function(lat1,lat2,lon1,lon2){
    a <- ((sin(getrad(lat2-lat1)/2))^2) + 
      (cos(getrad(lat1))*cos(getrad(lat2))*(sin((getrad(lon2-lon1))/2))^2)
    c <- 2*atan2(sqrt(a),sqrt(1-a))
    return(6371000*c)
  }
  
  # Observe functions:
  # 1. Update second menu (names) depending on selection in first(types)
  observeEvent(input$menu1, {
    update_dropdown_input(session,"menu2",
                          choices=levels(as.factor(filter(ships,ship_type==input$menu1)$SHIPNAME))
    )
  },ignoreInit=T)
  
  # 2. Update reactive values when a ship name is selected
  observeEvent(input$menu2, {
    # Create temporary dataframe to filter and arrange chronologically, since not all observations are in chronological order
    shipdata <- arrange(filter(ships,SHIPNAME==input$menu2),DATETIME) 
    # Initialize columns
    shipdata$travel_dist <- rep(0,nrow(shipdata))
    shipdata$obs_time <- rep(0,nrow(shipdata))
    shipdata$std_dist <- rep(0,nrow(shipdata))
    # Iterate through temporary dataframe to calculate distance traveled and duration of observation
    for(i in 1:(nrow(shipdata)-1)){
      shipdata$travel_dist[i+1] <- haversine(shipdata$LAT[i],shipdata$LAT[i+1],shipdata$LON[i],shipdata$LON[i+1])
      shipdata$obs_time[i+1] <- as.numeric(shipdata$DATETIME[i+1]-shipdata$DATETIME[i],units="secs")
    }
    # Standardise distances by computing distance per 30 sec, in order to account for inconsistent measurement (i.e. some much longer observations) 
    shipdata <- shipdata %>% 
      mutate(std_dist=travel_dist/(obs_time/30)) %>% 
      filter(obs_time >= 30)
    # Get index for observation in which ship travels the furthest
    maxrow <- max(which(shipdata$std_dist==max(shipdata$std_dist,na.rm=T)))
   # Update reactive values with values associated with maximum observation, plus the mean standardized distance (for comparison)
    maxdist_data$lon <- c(shipdata[[maxrow-1,"LON"]],shipdata[[maxrow,"LON"]])
    maxdist_data$lat <- c(shipdata[[maxrow-1,"LAT"]],shipdata[[maxrow,"LAT"]])
    maxdist_data$time <- c(shipdata[[maxrow-1,"DATETIME"]],shipdata[[maxrow,"DATETIME"]])
    maxdist_data$std_dist <- shipdata[[maxrow,"std_dist"]]
    maxdist_data$mean_dist <- mean(filter(shipdata,!is.na(std_dist))$std_dist)
  }, ignoreInit=T)
  
  # Render base map
  # Map tiles by Stamen Design, http://creativecommons.org/licenses/by/3.0 CC BY 3.0 -- Map data copyright OpenStreetMap contributors
  output$shipmap <- renderLeaflet({
    map_out <- leaflet(options = leafletOptions(attributionControl=FALSE)) %>%
      addProviderTiles(providers$Stamen.Toner,options = providerTileOptions(noWrap = TRUE)) %>%
      fitBounds(min(ships$LON),min(ships$LAT),max(ships$LON),max(ships$LAT)) 
    
    map_out
  })
  
  # Generate map to be re-rendered depending on user input
  updateMap <- function(){
    if(input$menu2!=""){
      geodata <- arrange(filter(ships,SHIPNAME==input$menu2),DATETIME)
      
      updated_map <-leafletProxy("shipmap") %>%
        clearGroup(group = "one") %>%
        fitBounds(min(geodata$LON),min(geodata$LAT),max(geodata$LON),max(geodata$LAT))
      
      if(input$travel_path){
        updated_map <- updated_map %>%
          addPolylines(lng=geodata$LON,
                       lat=geodata$LAT,color="red",weight=1.5,opacity=1,group="one")}
      if(input$highlight){
        updated_map <- updated_map %>%
          addPolylines(lng=maxdist_data$lon,
                       lat=maxdist_data$lat,color="white",weight=2,opacity=1,group="one") %>%
          addCircleMarkers(lng=maxdist_data$lon,
                           lat=maxdist_data$lat,color="white",radius=3,group="one",fillOpacity = 1,stroke=F)
      }
      return(updated_map)
    }
  }
  
  # Re-render map upon user input
  observeEvent(c(input$menu1,input$menu2,input$travel_path,input$highlight),{
      updateMap()
  }

  )
  
  # Re-render map and adjust limits when zoom button is pressed
  observeEvent(input$zoom,{
    if(!is.null(maxdist_data$std_dist)){
    updateMap() %>%
      fitBounds(min(maxdist_data$lon),min(maxdist_data$lat),max(maxdist_data$lon),max(maxdist_data$lat))
    }
  })

  # HTML output detailing the observation of interest
  output$note <- renderUI({
    if(input$menu2!="")
      {out <- paste0(
      "<p><b>",input$menu2,"</b>",
      " traveled the greatest standardized distance* (",
      round(maxdist_data$std_dist,2),
      " meters) ",
      " in the observation from ",
      as.character(maxdist_data$time[1]),
      " to ",
      as.character(maxdist_data$time[2]),
      ".</p><p>Over this interval,  <b>",input$menu2,"</b> traveled ",
      round(maxdist_data$std_dist-maxdist_data$mean_dist,2),
      " meters further (per 30 seconds) than it did in the average observation.</p><hr><p>*As observations are not measured over constant intervals of time, distances for each observation have been standardized by calculating the average distance traveled per 30 seconds.</p>"
      )} else{out <- "<p>Select a ship for more information.</p>"}
    HTML(out)
  })

}

shinyApp(ui, server)
