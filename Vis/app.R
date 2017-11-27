library(shiny)
library(shinythemes)

ui <-navbarPage(
  theme = shinytheme("paper"),
    title = "City fo Los Angeles Homeless Project",

                tabPanel("Introduction",
                         fluidPage(
                           h1("City of Los Angeles Homeless Overview"),
                           tags$blockquote("123"),
                           #plain text in paragraph
                           p("This is the body."),
                           #Single line break
                           br(),
                           #bold text in paragraph
                           tags$b("This is the bold text"),
                           div("div creates segments of text with a similar style. 
                               This division of text is all blue because I passed the argument 'style = color:blue' to div",
                               style = "color:blue"),
                           
                           img(src='homeless.jpg', align = "right"),
                           
                           
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
  
                tabPanel("Recommendations")
)


server <- function(input, output){
  
  
}

shinyApp(ui,server)