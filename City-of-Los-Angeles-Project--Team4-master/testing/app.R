#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("crime"),
   
     sidebarLayout(
       sidebarPanel(
         selectInput(inputId = "dataset",
                     label = "DATASETS",
                     choices = list("By Community", "By Tract"),
                     selected = "By Community"),
         selectInput("District",
                     "Click on District",
                     choices = cd_crime$CD)
       ),
       mainPanel(leafletOutput("crime_map"),
                 plotOutput("hour_crime"))
     )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
   
  datasets <- observe({
    if(input$crime_dataset == "By Community"){
      
      
      renderTable ({cd_crime = data_crime %>%
        left_join(data_census17_tract,
                  by = c("CT10" = "tract")) %>%
        group_by(CD) %>%
        summarise(Number = n())
      
      cd_crime$CD = as.character(cd_crime$CD)
      crime_combine = left_join(community_map, cd_crime,
                                by = c("id" = "CD"))
      })
      
      
      crime1 = distinct(crime_combine, id, Number)
      crimeid = crime1$id
      
      crimepolygons<-lapply(crimeid, function(x) polyFunc(x, dat=crime_combine))
      
      crimesp.polygon<-SpatialPolygons(crimepolygons)
      crimedf.polygon<-SpatialPolygonsDataFrame(crimesp.polygon,
                                                data=data.frame(row.names=crimeid, crime1))
      
      pal <- colorNumeric(
        palette = "Blues",
        domain = crimedf.polygon$Number
      )
      
      output$crime_map <- renderLeaflet({
        leaflet()%>%
          addTiles() %>%
          addPolygons(data = crimedf.polygon,
                      fillColor = ~pal(Number),
                      color = "#5297A8", 
                      fillOpacity = 0.8, 
                      weight = 1, 
                      smoothFactor = 1)%>%
          addLegend(pal = pal, 
                    values = crimedf.polygon$Number, 
                    position = "bottomright", 
                    title = "Number of Crime by District")
      })
    }
    
    else{
      
      crime_tract = data_crime %>%
        group_by(CT10) %>%
        summarise(Number = n())
      
      total_crime = tract_map %>%
        left_join(crime_tract,
                  by = c("id"="CT10"))
      
      crime_map = distinct(total_crime, id,  Number)
      crime_map = crime_map %>%
        filter(Number != "NA")
      crimemapid <- crime_map$id
      
      crimepolygons<-lapply(crimemapid, function(x) polyFunc(x, dat=total_crime))
      
      sp.polygon_crime<-SpatialPolygons(crimepolygons)
      df.polygon_crime<-SpatialPolygonsDataFrame(sp.polygon_crime,
                                                 data=data.frame(row.names=crimemapid, crime_map))
      pal <- colorNumeric(
        palette = "Blues",
        domain = df.polygon_crime$Number
      )
      
      output$crime_map <- renderLeaflet({
        leaflet()%>%
          addTiles() %>%
          addPolygons(data = df.polygon_crime,
                      fillColor = ~pal(Number),
                      color = "#5297A8", 
                      fillOpacity = 0.5, 
                      weight = 1, 
                      smoothFactor = 1)%>%
          addLegend(pal = pal, 
                    values = df.polygon_crime$Number, 
                    position = "bottomright", 
                    title = "Number of Crime by Tract")
      })
      
    }
  })
  
  
  ###Crime
  
  data_crime$TIME.OCCURRED = 
    format(strptime(substr(as.POSIXct(sprintf("%04.0f", data_crime$TIME.OCCURRED), 
                                      format="%H%M"), 12, 16), '%H:%M'), '%I:%M %p')
  
  data_crime$TIME.OCCURRED = strptime(data_crime$TIME.OCCURRED, "%I:%M %p")
  
  data_crime$TIME.OCCURRED = 
    str_sub(string = data_crime$TIME.OCCURRED, start = 12, end = 13)
  
  
  crime_sub<- reactive({
    
    crimesub <- data_crime %>%
      left_join(data_census17_tract,
                by = c("CT10" = "tract")) %>%
      filter(CD = input$District)
  
  
  output$hour_crime = renderPlot({
     crimesub%>%
      group_by(TIME.OCCURRED) %>%
      summarise(number_occured = n()) %>%
      ggplot(aes(x = TIME.OCCURRED, y = number_occured))+
      geom_bar(stat = "identity", fill = "light blue")+
      labs(y = "The Number of Crime", x = "Hour", title = "The Number of Crime Reported by Hour")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background= element_blank(), axis.line = element_line(colour = "black"))+
      theme(plot.title = element_text(face = "bold",size = 18))+
      theme(axis.title = element_text(size = 14))+
      coord_fixed(ratio = 1/10)
  }) 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

