library(shiny)

shinyUI(fluidPage(
  fluidRow(
    column(2,
           "sidebar"
    ),
    column(10,
           "main"
    )
  )
))

ui <-navbarPage((title = "City fo Los Angeles Homeless Project"),
                tabPanel("Overview"),
                navbarMenu("Data Analysis",
                         tabPanel("Shelter Analysis"),
                         tabPanel("Crime Analysis"),
                         tabPanel("311call Analysis")),
                tabPanel("Recommendations")
)


server <- function(input, output){
  
  
}

shinyApp(ui,server)
