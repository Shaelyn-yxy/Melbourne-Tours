# GEOM90007 Group 12 Assignment 3: Melb Tour 
# Author: 
#   Haolin Weng 1286881
#   Jiqiang Wang 1165452
#   Xinyi Yuan 906737
#   Yuchen Lin 1274762
#
#  Please run our interface as follows to get a better experience:
#  
#  1. Open RStudio
#  2. Change the working directory to our project's directory
#  3. Click 'Run App'
#  4. Click 'Open in Browser', and view in full screen


if (!require(shiny)) install.packages("shiny")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(plotly)) install.packages("plotly")
if (!require(leaflet)) install.packages("leaflet")
if (!require(leaflet.extras)) install.packages("leaflet.extras")
if (!require(dplyr)) install.packages("dplyr")
if (!require(highcharter)) install.packages("highcharter")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(shinyWidgets)) install.packages("shinyWidgets")
if (!require(DT)) install.packages("DT")

library(shiny)
library(ggplot2)
library(leaflet.extras)
library(plotly)
library(leaflet)
library(dplyr)
library(highcharter)
library(shinydashboard)
library(shinyWidgets)
library(DT)

# Read the data
# import data and merge two dataset together to get the final hotel data
hotelData1 <- read.csv("Data/airBNB/listings2.csv")
hotelData2 <- read.csv("Data/airBNB/listings.csv")
hotelData <- merge(hotelData2, hotelData1, by.x = "id", by.y = "id")
data <- read.csv("Data/cleaned_yelp.csv", header = TRUE)
au_map <- read.csv("Data/clearned_Landmarks.csv")



################################################################################
#                              USER INTERFACE                                  #
################################################################################

ui <- dashboardPage(
  # Set the skin of the shiny dashboard
  skin = "black",

  # Add the Dashboard title
  dashboardHeader(
    title = "Melb Tours"
  ),

  # Function for the sidebar
  dashboardSidebar(
    collapsed = FALSE,
    sidebarMenu(
      # ADD MENU ICON HERE
      menuItem("Introduction", tabName = "Intro", icon = icon("house")),
      menuItem("Transport", tabName = "trans", icon = icon("location-dot")),
      menuItem("Attraction", tabName = "POIs", icon = icon("star")),
      menuItem("Restaurant", tabName = "Restaurants", icon = icon("utensils")),
      menuItem("Hotel", tabName = "Hotel", icon = icon("bed"))
    )
  ),

  # Function for the body
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia";
        font-weight: bold;
        font-size: 26px;
      }
    '))),
    tabItems(

      # ************************************************************************
      # ******************************** [Intro] *******************************
      # ************************************************************************
      # By Yuchen
      tabItem(
        # Define the tab name
        tabName = "Intro",

        # Define the tab content
        fluidPage(
          htmlOutput("intropage", width = "1920px", height = "1080px")
        )
      ),


      # ************************************************************************
      # ****************************** [Transport] *****************************
      # ************************************************************************
      # By Yuchen
      tabItem(
        tabName = "trans",
        fluidPage(
          htmlOutput("transportPage", width = "1920px", height = "1080px")
        )
      ),

      # ************************************************************************
      # ********************************* [Hotel] ******************************
      # ************************************************************************
      # wirtten by Haolin's UI
      tabItem(
        # Define the tab name
        tabName = "Hotel",
        # Define the tab content
        fluidRow(
          # First box is the filter box which can filter the number of people to live in and budget price range, and room type
          box(
            title = "Filter", solidHeader = TRUE,
            width = 3, height = "450px",
            "Please enter your requirment to filter the option",
            sliderInput("numberOfHoterPeople", "Number of people:", 1, 16, 1, step = 1, animate = TRUE),
            sliderInput("hotelPriceRange", "Price Range", min = 0, max = 15000, value = c(200, 3000), step = 400, pre = "$", sep = ",", animate = TRUE),
            selectInput(
              "roomType",
              label = "Room type",
              choices = c(
                "All Type",
                "Entire home/apt",
                "Private room",
                "Shared room",
                "Hotel room"
              ),
              selected = "All Type"
            ),
          ),
          # the second box is the box to display the price density, which is a interative graph
          box(
            width = 7,height = "450px",
            title = "Price Density Analysis", solidHeader = TRUE,
            plotlyOutput("hotel_priceCount"),
          ),
          # the third box is the tab box that contain the rate table and map to locate the hotel
          tabBox(
            title = NULL, width = 10,
            id = "map", height = "600px",
            tabPanel("AirBNB Map", icon = icon("location-dot"), leafletOutput("hotelMap", height = "540px")),
            tabPanel("Top 3 AirBNB", icon = icon("circle-info"), dataTableOutput("hotelRate"))
          ),
          # the fourth box is the box to display the information for hourse when user click the map marker or table content
          box(
            title = "Room Info", solidHeader = TRUE, width = 5,
            htmlOutput("hotelRoomInfo", width = 10)
          ),
          # the fiveth box is the box to display the information for host when user click the map marker or table content
          box(
            title = "Host Info", solidHeader = TRUE, width = 5,
            htmlOutput("hotelHostInfo", width = 10)
          ),
        )
      ),
      
      # ************************************************************************
      # ******************************* [Restaurants] **************************
      # ************************************************************************
      # By xinyi
      tabItem(
        # Define the tab name
        tabName = "Restaurants",
        
        # row 1
        fluidRow(
          height="600px",
          # Filter
          box(
            title = "Filter", solidHeader = TRUE,

            "Please Filter the Option Below As Required",
            br(),
            width = 3, height = "400px",
            br(),
            sliderInput("range",
                        "Rating of Restaurants",
                        min = 0, max = 5,
                        value = c(3, 5)
            ),
            br(),
            pickerInput(
              "category", "Category of Restaurants: ",
              choices = c("All", unique(data$my_category)),
              selected = "All"
             
            )
          ),
          # map &table
          tabBox(
            title = NULL, width = 9,height = "690px",
            tabPanel("Location", icon = icon("location-dot"), leafletOutput("location_map", height = "610px")),
            tabPanel("Restaurant Info", icon = icon("circle-info"), DT::dataTableOutput("table", height = "610px"))
          )
        ),
        # row 2
        fluidRow(
          height="650px",width = 12,
          tabBox(
            width = 6,height = "600px",
            tabPanel("Hottest Category", icon = icon("fire"), includeHTML("./HTML/bubble.html"))
          ),
          tabBox(height = "600px", width = 6, tabPanel("What to Eat Today?", icon = icon("spinner"), includeHTML("./HTML/wheel.html")))
        )
      ),
      
      
      
      # ************************************************************************
      # ****************************** [Attractions] ***************************
      # ************************************************************************      
      tabItem(
        tabName = "POIs",
        fluidRow(
          box(
            title = "Theme Filter", width = 3, solidHeader = TRUE,
            selectInput(
              "theme",
              label = "theme",
              choices = c("All theme", sort(unique(au_map$Theme))),
              selected = "All theme"
            ),
            selectInput(
              "subtheme",
              label = "",
              choices = c("All sub theme",sort(unique(au_map$Sub.Theme))),
              selected = "All sub theme"
            )
          ),
          tabBox(
            title = 'POIs of melbourne',width=9,
            id = "map", 
            tabPanel("map", icon = icon("location-dot"), leafletOutput("melb_map", height = "540px"))
          )
        ),
        
        fluidRow(
          tabBox(
            width = 6,
            height = "500px",
            tabPanel("Top 10 Attractions in Melb", icon = icon("fire"), includeHTML("./HTML/tree_map.html")) 
          ),
          tabBox(height = "500px", width = 6, tabPanel("Popular things to do in Melb", icon = icon("spinner"), includeHTML("./HTML/popular_things.html")))
        )
      )
    )
  ),
)


################################################################################
#                                 SHINY SERVER                                 #
################################################################################

au_map$popup <- paste0( au_map$Sub.Theme, '<br>',
                        'name: ', au_map$Feature.Name, '<br>',
                        "<b><a href=", au_map$URL, "target = '_blank'",">", au_map$Feature.Name, "</a></b>")


server <- function(input, output, session) {
  
  
  # ************************************************************************
  # *********************** [Intro and Transport] **************************
  # ************************************************************************  
  output$intropage <- renderUI({
    includeHTML("./HTML/intro.html")
  })

  output$transportPage <- renderUI({
    includeHTML("./HTML/googlemap.html")
  })
  
  
  # ************************************************************************
  # ****************************** [Restaurants] ***************************
  # ************************************************************************

  ## map
  map_filter <- reactive({
    filter(
      data,
      if (input$category == "All") TRUE else my_category== input$category,
      rating >= c(input$range)[1] & rating <= c(input$range)[2]
    )
  })
  mybins <- seq(0, 5, by = 1)
  mypalette <- colorBin(palette = "YlOrBr", domain = map_filter, na.color = "transparent", bins = mybins)
  # the map shows the location of each restaurants with their rating in yelp
  output$location_map <- renderLeaflet(
    leaflet(map_filter()) %>%
      addProviderTiles(providers$CartoDB, group = "Simple Map") %>%
      setView(lat = -37.81, lng = 144.96, zoom = 14) %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        fillColor = ~ mypalette(rating), fillOpacity = 0.7, color = "white", radius = 4, stroke = FALSE,
        popup = ~ paste(
          "<b><a href=", url,"target='_blank'",">", name, "</a></b>",
          "<br>Category: ", my_category,
          "<br>Rating : ", rating, "/ 5",
          "<br>Reviews Counts: ", review_count,
          "<br>Contact Number: ", phone,
          "<br>Address: ", address
        )
      ) %>%
      addLegend(pal = mypalette, values = ~rating, opacity = 0.9, title = "Rating in Yelp!", position = "bottomright")
    # set the parameter of legend
  )
  
  ## table
  # control the showing limits of rows 
  options(DT.options = list(pageLength = 3))
  output$table <- DT::renderDataTable(
    # filter the data
    head(
      select(arrange(map_filter(), desc(rating)), name, my_category, rating, review_count, phone, address, url),
      10
    ),
    option = list(
      scrollY = TRUE,
      scrollX = TRUE ## enable scrolling on X axis
    ),
    selection = "single"
  )

  
  # ************************************************************************
  # ********************************* [Hotels] *****************************
  # ************************************************************************
  

  # inital the reactive value
  hotelIDrecord <- reactiveValues(x = 0)

  # set the hotel table to row of 3
  options(DT.options = list(pageLength = 3))


  # genereate the filtering hotel data
  hotelDataFiltering <- reactive({
    filter(
      hotelData,
      if (input$roomType == "All Type") TRUE else room_type.x == input$roomType,
      input$hotelPriceRange[1] <= price.y & input$hotelPriceRange[2] >= price.y,
      input$numberOfHoterPeople >= (accommodates - 1) & input$numberOfHoterPeople <= (accommodates + 1)
    )
  })

  # create the render text to count the number of house founded
  output$hotelCount <- renderText({
    hotelFilterData <- hotelDataFiltering()
    sumResult <- hotelFilterData %>%
      # count the number of cases happen in each state and drop other data
      summarise(count = n(), .groups = "drop")
    paste("Find ", as.character(sumResult), " number of result")
  })


  # output the hotel price density chart
  output$hotel_priceCount <- renderPlotly({
    # get the filtering data
    hotelFilterData <- hotelDataFiltering()
    # if input no data, return 0
    if (nrow(hotelFilterData) == 0) {
      return()
    }
    hotelGroupData <- hotelFilterData %>%
      # group by price and room type
      group_by(price.y, room_type.x) %>%
      # count the number of houses base on price and room type and drop the other data
      summarise(count = n(), .groups = "drop") %>%
      ungroup()

    # draw the plot by ggplot and set price as x and fill the house type as the filter to seperate color
    p <- ggplot(hotelGroupData, aes(price.y, fill = room_type.x)) +
      geom_density(alpha = 0.8) +
      # the layoout at the up right side
      scale_fill_viridis_d("Room Type", option = "inferno", begin = 0.5, end = 0.9) +
      # setting the sentense for the graph by indentify data is enough to build graph or not
      # and generaete the pop up sentense
      labs(
        title = if (nrow(hotelGroupData) == 0) "Sorry, lack of data, please find other option" else ("Melbourne AirBNB house density by price"), x = "accommodates",
        y = paste0("Price Density for ", gsub("X", "", input$numberOfHoterPeople), " people"), fill = "Room Type"
      ) +
      guides(fill = guide_legend(reverse = T))
    # create graph
    ggplotly(p, source = "hotel_priceCount")
  })

  # output the map to contain the house location as marker
  output$hotelMap <- renderLeaflet({
    # get the filtering data
    hotelFilterData <- hotelDataFiltering()
    # set the label when mouth go to the label
    labels <- sprintf(
      "<strong>%s</strong><br/>host name: %s <br/>accommodates: %g <br/>bedroom: %g <br/>bed: %g <br/>prices: %g  ",
      hotelFilterData$name.x, hotelFilterData$host_name.x, hotelFilterData$accommodates, hotelFilterData$bedrooms, hotelFilterData$beds, hotelFilterData$price.y
    ) %>% lapply(htmltools::HTML)
    # guard the programe when input no data
    if (nrow(hotelFilterData) == 0) {
      return()
    }
    # use leaflet to generate map
    leaflet(hotelFilterData) %>% # create base map and set the group so that can make different layer
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$CartoDB, group = "Simple Map") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Net Geo World Map") %>%
      # add maker for showing the house, make the group so that can show or hide by the layer
      addAwesomeMarkers(~longitude.x, ~latitude.x,
        layerId = ~id, group = "House",
        icon = ~ awesomeIcons(
          icon = "ios-home",
          library = "ion",
          markerColor = "darkred",
          iconColor = "#fff"
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        clusterOptions = markerClusterOptions()
      ) %>%
      # set the layer management
      addLayersControl(
        baseGroups = c("Toner Lite (default)", "Toner", "Simple Map", "Net Geo World Map"),
        overlayGroups = c("House"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  # use proxy to listen to the hotel click marker event
  hotelLeafletProxy <- leafletProxy(mapId = "hotelMap", session)
  # when mouth click the marker in map, pass the id to reative value
  observeEvent(input$hotelMap_marker_click, {
    clicked_point <- input$hotelMap_marker_click
    hotelIDrecord$x <- clicked_point$id
  })

  # use obsever event to listen to the table click event
  observeEvent(input$hotelRate_rows_selected, {
    # get the clicked row number
    clicked_point <- input$hotelRate_rows_selected
    # guard programe
    if (is.null(clicked_point)) {
      return()
    }
    # set the row's house id to reactive value
    hotelRatedData <- head(select(arrange(hotelDataFiltering(), desc(reviews_per_month.y)), id, name.x, price.x, reviews_per_month.y), 3)
    hotelIDrecord$x <- hotelRatedData$id[clicked_point]
  })


  # use DT to listen the event and render the data table
  output$hotelRate <- DT::renderDataTable(
    # filter the data
    head(
      select(arrange(hotelDataFiltering(), desc(reviews_per_month.y)), id, name.x, price.x, reviews_per_month.y),
      3
    ),
    selection = "single"
  )

  # use HTML to wirte the house infomation, and get the information detail by read the reactive value
  output$hotelRoomInfo <- renderText({
    hotelRoomData <- hotelData %>% filter(id == hotelIDrecord$x)
    # if no data input, let user select in the map or table
    if (nrow(hotelRoomData) == 0) {
      paste("Please select the house from map by clicking the marker or select in table")
    } else {
      paste("
      <h2>House Image</h2>
<img src= ", hotelRoomData$picture_url, " width='500' height='333'>
<h4><a href=", hotelRoomData$listing_url, "><b> Click here to view in AirBNB <b></a></h4>
      <table class='table'>
  <thead>
    <tr>
      <th scope='col'>#</th>
      <th scope='col'>Title</th>
      <th scope='col'>Detail</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th scope='row'>1</th>
      <td>Name </td>
      <td><b>", hotelRoomData$name.x, "<b></td>
    </tr>
    <tr>
      <th scope='row'>2</th>
      <td>Price </td>
      <td><b>", hotelRoomData$price.x, "<b></td>
    </tr>
    <tr>
      <th scope='row'>3</th>
      <td>Room Type</td>
      <td><b>", hotelRoomData$room_type.x, "<b></td>
    </tr>
    <tr>
      <th scope='row'>4</th>
      <td>Accommodates</td>
      <td><b>", hotelRoomData$accommodates, "<b></td>
    </tr>
        <tr>
      <th scope='row'>5</th>
      <td>Bathrooms</td>
      <td><b>", hotelRoomData$bathrooms_text, "<b></td>
    </tr>
        <tr>
      <th scope='row'>6</th>
      <td>Bedrooms</td>
      <td><b>", hotelRoomData$bedrooms, "<b></td>
    </tr>
        <tr>
      <th scope='row'>7</th>
      <td>Bed</td>
      <td><b>", hotelRoomData$beds, "<b></td>
    </tr>
        <tr>
      <th scope='row'>8</th>
      <td>Service</td>
      <td><b>", paste(hotelRoomData$amenities, collapse = ", "), "<b></td>
    </tr>
        <tr>
      <th scope='row'>9</th>
      <td>Description</td>
      <td><b>", hotelRoomData$description, "<b></td>
    </tr>
  </tbody>
</table>")
    }
  })
  # use HTML to wirte the host infomation, and get the information detail by read the reactive value
  output$hotelHostInfo <- renderText({
    hotelRoomData <- hotelData %>% filter(id == hotelIDrecord$x)
    # if no data input, let user select in the map or table
    if (nrow(hotelRoomData) == 0) {
      paste("Please select the house from map by clicking the marker")
    } else {
      paste("
      <h2>Host Image</h2>
<img src= ", hotelRoomData$host_picture_url, " width='500' height='333'>
<h4><a href=", hotelRoomData$host_url, "><b> Click here to view in AirBNB <b></a></h4>
      <table class='table'>
  <thead>
    <tr>
      <th scope='col'>#</th>
      <th scope='col'>Title</th>
      <th scope='col'>Detail</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th scope='row'>1</th>
      <td>Name </td>
      <td><b>", hotelRoomData$host_name.x, "<b></td>
    </tr>
    <tr>
      <th scope='row'>2</th>
      <td>Join AirBNB at </td>
      <td><b>", hotelRoomData$host_since, "<b></td>
    </tr>
    <tr>
      <th scope='row'>3</th>
      <td>Introduce</td>
      <td><b>", hotelRoomData$host_about, "<b></td>
    </tr>
    <tr>
      <th scope='row'>4</th>
      <td>Acceptance Rate</td>
      <td><b>", hotelRoomData$host_acceptance_rate, "<b></td>
    </tr>
        <tr>
      <th scope='row'>5</th>
      <td>Verification</td>
      <td><b>", paste(hotelRoomData$host_verifications, collapse = ", "), "<b></td>
    </tr>
  </tbody>
</table>")
    }
  })
  
  
  # ************************************************************************
  # ****************************** [Attractions] ***************************
  # ************************************************************************   
  observe({
    x=input$theme
    if(x == "Transport") {
      y = c("Railway Station",
            "Transport Terminal",
            "Marina",
            "Bridge")
    } else if (x == "Mixed Use"){
      y = c("Retail/Office/Carpark",
            "Retail/Office",
            "Retail/Office/Residential/Carpark",
            "Retail/Residential")
    } else if (x == "Place of Assembly"){
      y = c("Art Gallery/Museum",
            "Function/Conference/Exhibition Centre",
            "Library",
            "Theatre Live")
    } else if (x == "Leisure/Recreation"){
      y = c("Informal Outdoor Facility (Park/Garden/Reserve)",
            "Major Sports & Recreation Facility",
            "Private Sports Club/Facility",
            "Outdoor Recreation Facility (Zoo, Golf Course)",
            "Observation Tower/Wheel",
            "Indoor Recreation Facility",
            "Gymnasium/Health Club")
    } else if (x == "Health Services"){
      y = c("Private Hospital",
            "Public Hospital",
            "Medical Services")
    } else if (x == "Community Use"){
      y = c("Police Station",
            "Visitor Centre",
            "Cemetery",
            "Public Buildings",
            "Government Building",
            "Fire Station"
      )
    } else if (x == "Retail"){
      y = c("Retail",
            "Department Store")
    } else if (x == "Education Centre"){
      y = c("Primary Schools",
            "Secondary Schools",
            "School - Primary and Secondary Education",
            "Tertiary (University)",
            "Further Education",
            "Tertiary (University)")
    } else if (x == "Office") {
      y = c("Office")
    } else if (x == "Place of Worship"){
      y = c("Church",
            "Synagogue")
    } else if (x == "Purpose Built"){
      y = c("Casino",
            "Film & RV Studio",
            "Aquarium")
    } else if (x == "Vacant Land"){
      y = c('Vacant Land - Undeveloped Site',
            "Current Construction Site",
            "Current Construction Site - Commercial")
    } else if (x =="Specialist Residential Accommodation"){
      y = c("Hostel")
    } else if (x == "Residential Accommodation"){
      y = c("Dwelling (House)")
    } else if (x == "Industrial"){
      y = c("Industrial (Manufacturing)")
    } else if (x == "Warehouse/Store"){
      y = c("Store Yard")
    }
    
    if (x != "All theme"){
      updateSelectInput(session,"subtheme", label = "subtheme", choices = y)
    } else if (x == "All theme"){
      updateSelectInput(session,"subtheme", label ="subtheme", choices = c("All sub theme",sort(unique(au_map$Sub.Theme))))
    }
  })
  
  ## fitlter data
  filterdData <- reactive(
    filter(au_map,
           
           if(input$theme == "All theme") TRUE else Theme == input$theme,
           if(input$subtheme == "All sub theme") TRUE else Sub.Theme == input$subtheme)
  )
  
  ### generate map
  output$melb_map <-renderLeaflet(
    leaflet(filterdData()) %>%
      setView(lng = 144.946457, lat =  -37.840935, zoom = 12)%>%
      addProviderTiles(providers$CartoDB.Voyager)%>%
      addAwesomeMarkers(~longitude, ~latitude,
                        icon =~awesomeIcons(icon = case_when(Sub.Theme == "Railway Station" ~ "fa-subway",
                                                             Sub.Theme == "Church" ~ "fa-plus-square",
                                                             Sub.Theme == "Library" ~ "fa-book",
                                                             Sub.Theme == "Private Hospital" ~ "fa-hospital-o",
                                                             Sub.Theme == "Public Hospital" ~ "fa-hospital-o",
                                                             Sub.Theme == "Transport Terminal" ~ "fa-ship",
                                                             Sub.Theme == "Retail" ~ "fa-shopping-bag",
                                                             Sub.Theme == "Tertiary (University)" ~ "fa-university",
                                                             Sub.Theme == "Office" ~ "fa-building",
                                                             Sub.Theme == "Hostel" ~ "fa-h-square",
                                                             Sub.Theme == "Cinema" ~ "fa-film",
                                                             Sub.Theme == "Public Buildings" ~ "fa-building-o",
                                                             TRUE ~ "fa-info-circle"),
                                            library = "ion",
                                            markerColor = case_when(Theme == "Transport" ~ "#blue",
                                                                    Theme == "Mixed Use" ~ "lightbrown",
                                                                    Theme == "Place of Assembly" ~ "darkgrey",
                                                                    Theme == "Leisure/Recreation" ~ "lightorange",
                                                                    Theme == "Health Services" ~ "lightred",
                                                                    Theme == "Community Use" ~"pink",
                                                                    Theme == "Retail" ~ "lightgreen",
                                                                    Theme == "Education Centre" ~"lightblue",
                                                                    Theme == "Office" ~ "darkblue",
                                                                    Theme == "Place of Worship" ~ "red",
                                                                    Theme == "Purpose Built" ~ "brown",
                                                                    Theme == "Vacant Land" ~ "darkbrown",
                                                                    Theme == "Specialist Residential Accommodation" ~ "darkyellow",
                                                                    Theme == "Residential Accommodation" ~ "yellow",
                                                                    Theme == "Industrial" ~ "grey",
                                                                    Theme == "Warehouse/Store" ~ "black"),
                                            iconColor = '#FFFFFF'),
                        label = ~Sub.Theme,clusterOptions = markerClusterOptions(), popup=~popup)
    
  )
}

################################################################################
#                                  RUN SHINY                                   #
################################################################################


shinyApp(ui, server)
