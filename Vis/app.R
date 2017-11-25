library(shiny)

ui <-navbarPage((title = "City fo Los Angeles Homeless Project"),
                tabPanel("Introduction",
                         plotOutput("trend")),
                tabPanel("Data Analysis"),
                tabPanel("Overview"),
                tabPanel("Recommendations")
)


server <- function(input, output){
  
  
}

shinyApp(ui,server)