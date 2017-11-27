library(shiny)
library(shinythemes)

ui <-navbarPage(
  theme = shinytheme("journal"),
    title = "LA City Project",

                tabPanel("Introduction",
                         fluidPage(
                           br(),
                           br(),
                           h3("Overviews and Goals"),
                           #text in paragraph
                           fluidRow(
                             column(4,
                               p(tags$b("Overviews:"),"In 2017, there are more than ", tags$em("50,000", style = "color:blue"),
                             "on the street in the LA County, and about", tags$em("75%", style = "color:blue"),"of homeless people are unsheltered.", 
                             tags$em("2645", style = "color:blue"), "crimes was committed towards homeless people from Auguest 2016 to Auguest 2017, 
                             and about", tags$em("30%", style = "color:blue"), "of the crimes are assulting with deadly weapon. 
                             The increasing serverity of homeless density in the Los Angeles is partly because the increasing rents. In 2017, 
                             the median price of 1B1B apartment is $1,997. The median rental price incresed 30% between 2000 and 2015, but the 
                             median income did not change during this time."

                             )),
                             column(8,img(src = "homeless1.png", width = 600, height = 400, aligh = "left"))),
                           br(),
                           p(tags$b("Project Goals:"), "Our goal is to measure the",tags$em("homeless density",style = "color:blue"),"in the greater Los Angeles Area,
                             identify potential", tags$em("risks and health issues",style = "color:blue"), "towards homeless people,
                             figure up the", tags$em("services", style = "color:blue"), "provided by the governments and instituions,
                             and provide implantable", tags$em("recommendations", style = "color:blue"), "for the city of Los Angeles on the homeless issues."),
                           

                           fluidRow(
                             column(7,br(),img(src='homeless.png', width = 520, height = 380, align = "left")),
                             column(5, h3("Data Sets"), p("The data sets are provided by the city of Los Angeles,
                                                          and the data is collected in the Greater Los Angeles area.
                                                          The data included the 311 Calls, Crime Data, Shelter Data, Homeless Counts, and Tract maps."),
                                    p(tags$b("311 Calls:"), "Received 311 Calls from various Apps and Devices: Locations and Times."),
                                    p(tags$b("Crime Data:"), "Reported crimes towards homeless people in the Greater Los Angeles Area: Locations, Times and Crime Types"),
                                    p(tags$b("Shelter Data:"), "Shelter in Greater Los Angeles Area: Location, Types and Times "),
                                    p(tags$b("Homeless Count Data"), "Homeless count by tract and community: Location, Times, Family Numbers, with/without Youth, and Sheltered/Unsheltered ")))
                             
                           
                           
                           
                           )),
  
                navbarMenu("Data Analysis",
                           
                           tabPanel("Homeless Data", fluidPage(
                             
                           )),
                           
                           tabPanel("311 Call Data"),
                           
                           tabPanel("Crime Data"),
                           
                           tabPanel("Shelter Data")),
                tabPanel("Risks"),
  
                tabPanel("Recommendations",
                         fluidPage(
                           br(),
                           br(),
                           h3("Causes of Homeless:"),
                           p(tags$ul(
                             tags$li("povery: lack of affordable housing"), 
                             tags$li("mental illness:discharged from mental hospitals"), 
                             tags$li("released from the criminal justice system"),
                             tags$li("domestic violence"))
                           ),
                           h3("Recommendations:"),
                           p("1. Build Affordable Housing:", tags$ul(tags$li("in high homeless density area"), tags$li("at publicly owned vacant properties"),
                                                                     tags$li("build more “tiny houses” for the homeless"))), 
                           p("2. Build more shelters/ homeless services", tags$ul(tags$li("similar to current shelters/ services"), tags$li("imitate the operations/services/ success shelters 
                                                                                                                                            and open branches in severe areas 
                                                                                                                                            (identified by the data -- 
                                                                                                                                            those area with greatest decrease 
                                                                                                                                            in severity and look at their shelter services):focus on career training, job seeking, and mental issue consulting"))),
                           p("3. Easier access to shelters and services", tags$ul(tags$li("only 16% of homeless people seek local services"),
                                                                                  tags$li("call center points at local stores (7-11,Walgreens, CVS, etc.)", tags$ul(tags$li("provide
                                                                                                                                                                            homeless people with call services
                                                                                                                                                                            and application services to local shelters"),
                                                                                                                                                                    tags$li("basic medical needs at local pharmacies at low charges
                                                                                                                                                                            (drugs for people suffering from depression,mental illness,
                                                                                                                                                                            etc.)"))))),
                           p("4. Employment", tags$ul(tags$li("recruit people from previous success shelters to to help build new shelters(construction workers, administrative staff, etc.))",
                                                      tags$li("Buddy Program: connect previous homeless people with new homeless people. People escaped from homeless can introduce job opportunities and way to combat to new homeless people. ")))
                           



                           )),
                         
  position = "fixed-top",
  header = "City of Los Angeles Homeless Project"
  
))


server <- function(input, output){
  
  
}

shinyApp(ui,server)