library(shiny)
library(shinythemes)

ui <-navbarPage(
  theme = shinytheme("journal"),
    title = "LA City Project",

                tabPanel("Introduction",
                         fluidPage(
                           br(),
                           br(),
                           tags$blockquote("Overviews, Goals, and Data Sets"),
                           #text in paragraph
                           p(tags$b("Overviews:"),"In 2017, there are more than ", tags$em("50,000", style = "color:blue"),
                             "on the street, and about", tags$em("75%", style = "color:blue"),"of homeless people are unsheltered.

                             "),
                           img(src = "homeless1.jpg", aligh = "left"),
                           p(tags$b("Project Goals:"), "Our goal is to measure the homeless density in the greater Los Angeles Area,
                             identify potential risks and health issues towards homeless people,
                             figure up the services provided by the governments and instituions,
                             and provide implantable recommendations for the city of Los Angeles on the homeless issues."),

                           
                           img(src='homeless.jpg', width = 500, height = 300, align = "left"),
                           #leave some room here
                           br(),
                           tags$blockquote("Data Sets"),
                           theme = "bootstrap.css",
                           
                           
                           fluidRow(
                             column(6, h3("Title in one row")),
                             column(6)
                           
                         ))),
  
                navbarMenu("Data Analysis",
                           
                           tabPanel("Homeless Data", fluidPage(
                             
                           )),
                           
                           tabPanel("311 Call Data"),
                           
                           tabPanel("Crime Data"),
                           
                           tabPanel("Shelter Data")),
                tabPanel("Risks"),
  
                tabPanel("Recommendations"),
  position = "fixed-top",
  header = "City of Los Angeles Homeless Project"
  
)


server <- function(input, output){
  
  
}

shinyApp(ui,server)