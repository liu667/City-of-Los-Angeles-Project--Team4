






tabPanel("Homeless Data",
         div(class="outer",
             
             tags$head(
               # Include our custom CSS
               includeCSS("styles.css"),
               leafletOutput("homelessmap")
             ))),

# If not using custom CSS, set height of leafletOutp,




shinytheme("paper"),