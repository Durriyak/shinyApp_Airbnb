## global
library(readr)
library(shiny)
library(leaflet)
library(dplyr)
library(plotly)
library(readr)
library(fastDummies)
library(readxl)

data1 <- read_excel("Airbnb_Boston.xlsx")
str(data1)
airbnb_data <- na.omit(data1)
airbnb_data
Bostonmap <- read_csv("Bostonmap.csv")
month <- read_csv("month.csv")
review <- read_csv("review.csv")

# variables
neighbourhood_cleansed <- c("East Boston","Roxbury", "Beacon Hill","Downtown","Back Bay",
                            "North End",
                            "Dorchester",
                            "South End",
                            "Charlestown",
                            "Jamaica Plain",
                            "South Boston",
                            "Bay Village",
                            "Brighton",
                            "West Roxbury",
                            "Roslindale",
                            "Mission Hill",
                            "Fenway",
                            "Allston",
                            "Hyde Park",
                            "West End",
                            "Mattapan",
                            "South Boston Waterfront",
                            "Chinatown",
                            "Longwood Medical Area",
                            "Leather District")

room_type <- c("Entire home/apt", "Private room", "Shared room")

#groupColors <- colorFactor(c("#e01149", "#f6ff00","#2c07b2"),
groupColors <- colorFactor(c("#e01149", "#f6ff00","#2c07b2"),
                           domain = c("Entire home/apt", "Private room","Shared room"))






a<-Bostonmap %>% select(neighbourhood_cleansed, review_scores_rating) %>%
  filter(review_scores_rating!="NA") %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(avg_review=mean(review_scores_rating)) %>%
  arrange(desc(avg_review))

write_csv(a,"review.csv")



popular_neighbourhood <- c("Dorchester","Roxbury","Jamaica Plain","Downtown","South End")


####### regression ######
top6areas <- as.data.frame(airbnb_data %>% 
                             count(neighbourhood_cleansed,sort = TRUE) %>%
                             top_n(6,n))
top6areas
areas_top6 <- c("Dorchester","Roxbury","Jamaica Plain","Downtown","South End","Allston")
airbnb_data_top6areas <- as.data.frame(airbnb_data %>% 
                                         filter(neighbourhood_cleansed %in% areas_top6 ))
airbnb_data_top6areas

dum_1<- dummy_cols(airbnb_data_top6areas, select_columns = c('neighbourhood_cleansed','room_type'))

dum_data1 <- dum_1 %>% select(21,24,54:64)
correlation <- cor(dum_data1)
reg_price2 <- lm(dum_data1$price ~ dum_data1$bedrooms+dum_data1$`room_type_Entire home/apt`+dum_data1$`room_type_Hotel room`+
                   dum_data1$`room_type_Shared room`+dum_data1$`room_type_Private room`+dum_data1$neighbourhood_cleansed_Dorchester+
                   dum_data1$neighbourhood_cleansed_Roxbury+dum_data1$`neighbourhood_cleansed_Jamaica Plain`+dum_data1$neighbourhood_cleansed_Downtown+
                   dum_data1$`neighbourhood_cleansed_South End`, data = dum_data1)
coefficients <- as.data.frame(summary(reg_price2)$coefficients)
coefficients_1 <- coefficients[-1,1]
intercept <- coefficients[1,1]







server <- function(input, output, session) {
  
  ##### Boston Interactive Map ############
  # reactivate map info
  mapdf <- reactive({
    Bostonmap %>%
      filter(neighbourhood_cleansed %in% input$select_neighbourhood & 
               room_type %in% input$select_room & 
               price >= input$slider_price[1] &
               price <= input$slider_price[2] &
               number_of_reviews >= input$slider_review[1] &
               number_of_reviews <= input$slider_review[2] &
               review_scores_rating >= input$slider_rating[1] &
               review_scores_rating <= input$slider_rating[2]) 
  })
  
  # create the map
  output$map <- renderLeaflet({
    leaflet() %>% 
      setView(lng = -71.0589, lat = 42.3601, zoom = 13) %>%
      addTiles()
  
  })
  
  # observe an event
  observe({ #require a trigger to call the observe function
    proxy <- leafletProxy("map",data = mapdf()) %>%
      clearMarkerClusters() %>% 
      clearMarkers() %>%
      # circle
      addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 2, color = ~groupColors(room_type),
                       group = "CIRCLE",
                       popup = ~paste('<b><font color="Black">','Listing Information','</font></b><br/>',
                                      'Room Type:', room_type,'<br/>',
                                      'Price:', price,'<br/>',
                                      'Rating Score:', review_scores_rating, '<br/>',
                                      'Number of Reviews:', number_of_reviews,'<br/>')) %>% 
      # cluster
      addCircleMarkers(lng = ~longitude, lat = ~latitude, clusterOptions = markerClusterOptions(),
                       group = "CLUSTER",
                       popup = ~paste('<b><font color="Black">','Listing Information','</font></b><br/>',
                                      'Room Type: ', room_type, '<br/>',
                                      'Price:', price,'<br/>',
                                      'Rating Score:', review_scores_rating, '<br/>',
                                      'Number of Reviews:', number_of_reviews,'<br/>')) %>% 
      # circle/ cluster panel
      addLayersControl(
        baseGroups = c("CIRCLE","CLUSTER"),
        options = layersControlOptions(collapsed = FALSE)
      ) 
  })
  
  ## reactivate count dataframe for map graph1 
  countdf <- reactive({
    mapdf() %>%
      group_by(., room_type) %>%
      summarise(., count_type = n())
  })
  
  #map graph1 
  output$count_room <- renderPlotly({
    plot_ly(data = countdf(), x = ~room_type, y = ~count_type, type = "bar", color = ~room_type,
            colors = c("#e01149", "#f6ff00","#2c07b2"),
            hoverinfo = 'text',
            text = ~count_type) %>%
      layout(xaxis = list(title = "", showticklabels = FALSE),
             yaxis = list(title = "count"), showlegend = FALSE,
             annotations = list(x = ~room_type, y = ~count_type, text = ~paste(round(count_type/sum(count_type),2)*100,'%'),
                                xanchor = 'center', yanchor = 'bottom',
                                showarrow = FALSE)) %>% 
      config(displayModeBar = FALSE)
  })
  
  ## reactivate price dataframe for map graph2
  pricedf <- reactive({
    mapdf() %>% 
      group_by(., room_type) %>% 
      summarise(., avg_price = round(mean(price),2))
  })
  
  #map graph2 avgprice
  output$avgprice <- renderPlotly({
    plot_ly(data = pricedf(), x = ~room_type, y = ~avg_price, type = "bar", color = ~room_type,
            colors = c("#e01149", "#f6ff00","#2c07b2"),
            hoverinfo = 'text',
            text = ~avg_price) %>% 
      layout(xaxis = list(title = "", showticklabels = FALSE), 
             yaxis = list(title = "price"), showlegend = FALSE,
             annotations = list(x = ~room_type, y = ~avg_price, text = ~paste('$',avg_price),
                                xanchor = 'center', yanchor = 'bottom', 
                                showarrow = FALSE)) %>% 
      config(displayModeBar = FALSE)
  })
  
    
  
  ##### Neighbours and Price Changes #######################
  ## reactivate dataframe for listings grapgh
  graph1df <- reactive({
    Bostonmap %>%
      select(neighbourhood_cleansed,room_type,price,review_scores_rating) %>% 
      filter(price >= input$tab2_price[1] &
               price <= input$tab2_price[2] &
               review_scores_rating >= input$tab2_rating[1] &
               review_scores_rating <= input$tab2_rating[2]) %>% 
      group_by(neighbourhood_cleansed,room_type) %>% 
      summarise(n=n()) %>%
      arrange(desc(n))
  })
  
  
  # listings graph
  output$graph1 <- renderPlotly({
    t <- list(size = 9)
    plot_ly(data = graph1df(), x = ~n, y = ~room_type, type = "bar", color = ~neighbourhood_cleansed,
            colors = c("#E52B50", "#00C4B0", "#FFBF00", "#34B334", "#FF9899","#9966CC",
                       "#008000", "#FF9966", "#007FFF", "#F4C2C2", "#89CFF0", "#FFE135",
                       "#848482", "#F28E1C", "#8F5973", "#D1001C", "#0093AF", "#FADA5E",
                       "#059033", "#FF6600", "#00B9FB", "#8A2BE2", "#DA70D6", "#414A4C",
                       "#DDE26A", "#9BC4E2", "#DDADAF", "#DA8A67", "#DCD0FF", "#F984E5",
                       "#96DED1", "#78184A", "#009B7D", "#50C878", "#AEC6CF", "#FF6961",
                       "#D1E231"),
            orientation = 'h', showlegend = TRUE) %>%
      layout(xaxis = list(title = "count"),
             yaxis = list(title = ""), 
             barmode = 'group', font = t)
  })
  
  # price change graph (month)
  output$tab_price <- renderPlotly({
    plot_ly(data = month, x = ~month, y = ~avg_pricemo, type= 'scatter', mode = 'markers+lines', color = "Set9",
            text = ~paste('Price: $', avg_pricemo)) %>%
      layout(xaxis = list(title = "month", type = 'category'),
             yaxis = list(title = "price"))
  })
  
  
  ##########overall rating scores by neighborhoods##########
  review$neighbourhood_cleansed <- factor(review$neighbourhood_cleansed, 
                                          levels = c("Diamond Heights", "Presidio", 
                                                     "Castro/Upper Market", "Twin Peaks", 
                                                     "Potrero Hill", "Presidio Heights", 
                                                     "West of Twin Peaks", "Noe Valley", 
                                                     "Treasure Island/YBI", "Outer Mission", 
                                                     "Seacliff", "Mission", "Bernal Heights",
                                                     "Western Addition", "Glen Park", 
                                                     "Pacific Heights", "Marina", 
                                                     "Inner Sunset", "Russian Hill", 
                                                     "South of Market", "Outer Richmond", 
                                                     "Haight Ashbury", "Visitacion Valley", 
                                                     "Inner Richmond", "Lakeshore", 
                                                     "Financial District", "North Beach", 
                                                     "Outer Sunset", "Parkside", "Nob Hill", 
                                                     "Ocean View", "Excelsior", "Golden Gate Park", 
                                                     "Downtown/Civic Center", "Bayview", 
                                                     "Chinatown", "Crocker Amazon"))
  output$review1 <- renderPlotly({
    plot_ly(data = review, x = ~ neighbourhood_cleansed, y = ~ avg_review-90, type= 'bar', color=I("pink")) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "average review rating"),
             paper_bgcolor = 'rgba(245, 246, 249, 1)',
             plot_bgcolor = 'rgba(245, 246, 249, 1)')
  })
  
  
  
  
  #########price and review##################
  p_r<-Bostonmap %>% select(review_scores_rating,price) %>%
    filter(review_scores_rating!="NA",price!="NA") %>%
    group_by(review_scores_rating) %>%
    summarise(avg_price=mean(price))
  
  output$pr <- renderPlotly({
    plot_ly(data = p_r, x = ~ review_scores_rating, y = ~ avg_price, type= 'scatter', mode = 'markers+lines', color=I("grey")) %>%
      layout(xaxis = list(title = "review rating"),
             yaxis = list(title = "average price"))
  })


  output$graph2 <- renderPlot({
  price_region <- as.data.frame(airbnb_data %>%
                                  group_by(neighbourhood_cleansed) %>%
                                  summarize(average_price=mean(price)))
  ggplot(data=price_region,aes(x=average_price,y=neighbourhood_cleansed,size =4, color = "orange"))+
    geom_point()+
    labs(x="Average Price",y="Region",title = "Average Price by Region")
})
  
  output$popular_table <- renderPlot({
    popular_room <- as.data.frame(airbnb_data %>%
                  group_by(bedrooms,room_type) %>%
                  summarize(available_days=mean(availability_365)))
    ggplot(data=popular_room,aes(x=bedrooms,y=available_days))+
      geom_boxplot(aes(color=room_type))+
      facet_wrap( ~room_type )+
      xlim(c(1,15))+
      labs(x="bedroom numbers",y="unoccupied days per year",title = "cumtomer preference")
  })
  

  
  ############### Prediction of Price for an Airbnb ##########
  
  observeEvent(input$go7,{
  output$pred_price <- renderPrint(
    
    if (input$neighbourhood == "Dorchester" ){
      if(input$room_type== "Entire home/apt"){
        price <- intercept + coefficients_1[6] + coefficients_1[2] + input$bedroom_no*coefficients_1[1]
        round(price)
      }
      else if(input$room_type== "Private room"){
        price <- intercept + coefficients_1[6] + coefficients_1[5]+ input$bedroom_no*coefficients_1[1]
        round(price)
      }
      else {
        price <- intercept + coefficients_1[6] + coefficients_1[4]+ input$bedroom_no*coefficients_1[1]
        round(price)
      }}
    
    
      else if(input$neighbourhood == "Roxbury" ){
        if(input$room_type== "Entire home/apt"){
          price <- intercept + coefficients_1[7] + coefficients_1[2]+ input$bedroom_no*coefficients_1[1]
          round(price)
        }
        else if(input$room_type== "Private room"){
          price <- intercept + coefficients_1[7] + coefficients_1[5]+ input$bedroom_no*coefficients_1[1]
          round(price)
        }
        else {
          price <- intercept + coefficients_1[7] + coefficients_1[4]+ input$bedroom_no*coefficients_1[1]
          round(price)
        }}
    
    
      else if(input$neighbourhood == "Jamaica Plain" ){
        if(input$room_type== "Entire home/apt"){
          price <- intercept + coefficients_1[8] + coefficients_1[2]+ input$bedroom_no*coefficients_1[1]
          round(price)
        }
        else if(input$room_type== "Private room"){
          price <- intercept + coefficients_1[8] + coefficients_1[5]+ input$bedroom_no*coefficients_1[1]
          round(price)
        }
        else {
          price <- intercept + coefficients_1[8] + coefficients_1[4]+ input$bedroom_no*coefficients_1[1]
          round(price)
        }}

    
        else if(input$neighbourhood == "Downtown" ){
        
        if(input$room_type== "Entire home/apt"){
          price <- intercept + coefficients_1[9] + coefficients_1[2]+ input$bedroom_no*coefficients_1[1]
          round(price)
        }
        else if(input$room_type== "Private room"){
          price <- intercept + coefficients_1[9] + coefficients_1[5]+ input$bedroom_no*coefficients_1[1]
          round(price)
        }
        else {
          price <- intercept + coefficients_1[9] + coefficients_1[4]+ input$bedroom_no*coefficients_1[1]
          round(price)
        }}
    
    
      else {
        if(input$room_type== "Entire home/apt"){
          price <- intercept + coefficients_1[10] + coefficients_1[2]+ input$bedroom_no*coefficients_1[1]
          round(price)
        }
        else if(input$room_type== "Private room"){
          price <- intercept + coefficients_1[10] + coefficients_1[5]+ input$bedroom_no*coefficients_1[1]
          round(price)
        }
        else {
          price <- intercept + coefficients_1[10] + coefficients_1[4]
          round(price)
        }}
    
      
  )})
  
  
  ########## Neighbourhood Analytics ##############
str(airbnb_data)
  # Scatter Plot
  output$insights <- renderPlot ({
    data_sub <- airbnb_data %>% dplyr::filter(neighbourhood_cleansed == input$neighbourhood,
                                           bedrooms == input$rooms,
                                           accommodates == input$accommodates,
                                           price <= 800)
    
    ggplot(data_sub, aes(x = review_scores_rating,
                         y = price)) +
      geom_point(size = 3,
                 alpha = 0.8) +
      theme(panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, 
                                            linetype = "solid"),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(size = .1, 
                                              color = "black",
                                              linetype = "dashed")) +
      labs(x = "Review Score Rating (0-5)",
           y = "Price (GBP)",
           color = "Bathrooms",
           title = "Price and Ratings of Listings based on all Input Filters",
           subtitle = "Orange Dashed Lines are Average Values based on Input Features") +
      geom_hline(yintercept = mean(data_sub$price),
                 colour = "#F7965C",
                 linetype = "dashed") +
      geom_vline(xintercept = mean(data_sub$review_scores_rating, na.rm = TRUE),
                 colour = "#F7965C",
                 linetype = "dashed")
  })
  
  # Histogram
  output$histogram <- renderPlot ({
    
    data_sub2 <- airbnb_data %>% dplyr::filter(neighbourhood_cleansed == input$neighbourhood,
                                            bedrooms == input$rooms,
                                            accommodates == input$accommodates,
                                            price <= 800)
    
    ggplot(data_sub2, aes(x = price,)) +
      geom_histogram(fill = "#434343") +
      theme(panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, 
                                            linetype = "solid"),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(size = .1, 
                                              color = "black",
                                              linetype = "dashed")) +
      labs(x = "Price (GBP)",
           y = "Number of Listings",
           title = "Histogram of Airbnb Prices based on all Input Filters",
           subtitle = "Orange Dashed Lines are Average Values based on Input Features") +
      geom_vline(xintercept = mean(data_sub2$price, na.rm = TRUE),
                 colour = "#F7965C",
                 linetype = "dashed")
  })
  
  # Listing Breakdown
  output$listingsBreakdown <- renderPlot ({
    data_sub10 <- airbnb_data %>% dplyr::filter(price <= 800,
                                             neighbourhood_cleansed == input$neighbourhood,
                                             accommodates <= 10)
    
    data_sub10 %>%
      select(room_type, accommodates) %>%
      group_by(room_type, accommodates) %>%
      count() %>%
      ggplot(aes(fill=room_type, y=n, x=factor(accommodates))) +
      geom_bar(position ="stack", stat="identity", alpha=0.7) +
      scale_fill_manual(name = "Room Type", 
                        values = c("red", "lightblue", "orange", "grey", "blue")) +
      labs(x= "Number of Guests Listing can Accommodate", y = "Count of Units",
           title = "Number of Listings by Maximum Pax and Room Type") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, 
                                            linetype = "solid"),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(size = .1, 
                                              color = "black",
                                              linetype = "dashed"),
            panel.grid.minor = element_blank(),
            legend.position = c(.95, .95),
            legend.justification = c("right", "top"),
            legend.box.just = "right",
            legend.margin = margin(6, 6, 6, 6),
            plot.title = element_text(size = 12, margin = margin(b = 10)))
    
  })
  
  
  # Price Breakdown
  output$priceBreakdown <- renderPlot ({
    data_sub11 <- airbnb_data%>% 
      dplyr::filter(price <= 800,
                    neighbourhood_cleansed == input$neighbourhood,
                    accommodates <= 10) %>%
      dplyr::select(accommodates, room_type, price)
    
    
    data_sub11 %>% 
      ggplot(aes(x = factor(accommodates), y = price)) +
      geom_boxplot(alpha = 0.2) +
      facet_wrap(~room_type) + 
      labs(x= "Number of Guests Listing can Accommodate", y = "Price (GBP)",
           title = "Price by Maximum Pax and Room Type") +
      theme(panel.spacing = unit(1, "lines"))
  })
  
}

library(shinythemes)

ui <- fluidPage(
  navbarPage(title = "Airbnb in Boston", 
             #id ="nav",
             
             theme = shinytheme("united"), #https://rstudio.github.io/shinythemes/
             
             ##### Map ########      
             tabPanel("Map",
                      div(class="outer",
                           leafletOutput(outputId = "map", width = 1375, height =780 ),
                          
                          # Panel options: neighborhood, Room Type, Price, Rating, Reviews
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE, 
                                        top = 70, left = "auto", right = 20, bottom = 200,
                                        width = 350, height = "auto",
                                        h2("Airbnb in Boston"),
                                        selectInput(inputId = "select_neighbourhood",
                                                    label = "neighbourhood:",
                                                    choices = neighbourhood_cleansed,
                                                    selected = neighbourhood_cleansed, 
                                                    multiple = TRUE)),
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE, draggable = TRUE, 
                                        top = 20, left = 20, right = "auto" , bottom = 200,
                                        width = 250, height = "auto",
                                        checkboxGroupInput(inputId = "select_room", label = h4("Room Type"), 
                                                           choices = room_type, selected = room_type),
                                        sliderInput(inputId = "slider_price", label = h4("Price"), min = 0, max = 1000, step = 50,
                                                    pre = "$", sep = ",", value = c(100, 500)),
                                        sliderInput(inputId = "slider_rating", label = h4("Rating Score"), min = 0, max = 5, step = 1,
                                                    value = c(0, 5)),
                                        sliderInput(inputId = "slider_review", label = h4("Number of Reviews"), min = 0, max = 400, step = 50,
                                                    value = c(10, 350)),
                                       
                          ),
                          
                          # Results: count_room, avgprice
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE, draggable = TRUE, 
                                        top = 570, left = 150, right = "auto" , bottom = "auto",
                                        width = 280, height = "auto",
                                        plotlyOutput(outputId = "avgprice", height = 150),
                                        plotlyOutput(outputId = "count_room", height = 130)
                                       )
                      )),
             
             ######## Average Price ########## 
             
             tabPanel("Average Price",    
                      fluidRow(
                       h3(""),
                               plotOutput("graph2",width=1300, height =740)
                      )
             ),
            
             
             
         
             ######## Neighbourhood Analytics ########## 
             tabPanel("Neighbourhood Analytics",
                      sidebarPanel(
                        selectInput(inputId = "neighbourhood",
                                    label = "Neighbourhood:",
                                    choices = neighbourhood_cleansed,
                                    selected = neighbourhood_cleansed, 
                                    multiple = TRUE),
                        sliderInput("rooms",
                                    "Number of Rooms",
                                    min = 1,
                                    max = 10,
                                    value = 1),
                        sliderInput("accommodates",
                                    "Number of Guests",
                                    min = 1,
                                    max = 10,
                                    value = 2),
                        selectInput(inputId = "room_types_input", 
                                    label = "Room Type",
                                    choices = c("All types",
                                                "Entire home/apt",
                                                "Private room",
                                                "Shared room",
                                                "Hotel room"
                                    )), width = 4
                      ),
                      
                      mainPanel( 
                        h4("Neighbourhood Analytics"),
                        p("Find out more about the supply and prices of listings in your neighbourhood"),
                        br(),
                        plotOutput("insights"),
                        br(),
                        plotOutput("histogram"),
                        br(),
                        plotOutput("listingsBreakdown"),
                        br(),
                        plotOutput('priceBreakdown'),
                        br())),
                        
             
             ######## Listings ##########               
             tabPanel("Price and Review Ratings",    
                      fluidRow(
                        column(3,
                               h3("Prices and Review Ratings by Neighbourhood and Room Type"),
                               br(),
                               br(),
                               sliderInput(inputId = "tab2_price", h4("Price/Night"), min = 10, max = 1000, value = c(10, 1000)),
                               sliderInput(inputId = "tab2_rating", h4("Rating Score"), min = 0, max = 5, value = c(0,5))
                        ),
                        column(9,
                               h3(""),
                               plotlyOutput(outputId = "graph1", width=900, height =1200))
                      )
             ),
             
             ######## Predict Price ########## 
             tabPanel("Predict Price",
                      sidebarPanel(
                        sliderInput(inputId = "bedroom_no", h4("Choose number of bedrooms"), min = 1, max = 12, value = 3),
                        selectInput('neighbourhood', label = "Choose neighbourhood", popular_neighbourhood),
                        selectInput('room_type', label = "Choose Room Type", room_type),
                        actionButton(inputId = "go7", label = "Predict Price")),
                      
                      mainPanel( 
                        h3("Predicted Price"),
                        verbatimTextOutput("pred_price"),
                      )),
             
             
                  
                       
  ))
shinyApp(ui = ui, server = server)
