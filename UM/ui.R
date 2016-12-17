require(shiny)
require(leaflet)
require(magrittr)
require(sp)
require(shinyjs)
library(shinyjs)
library(V8)



shinyUI(
  
  navbarPage("Xplor", id="nav",
                             
  tabPanel("Interaktiv karta",
           
    div(class = "outer",
     tags$head(
          includeCSS("style.css")
     ),
        
      
      leafletOutput("map", width = "100%", height = "100%"),
 
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                  width = 300, height = "auto",
      
                  selectInput("var", 
                              label = h4("Välj ett ämne"),
                              choices = c("Parker & grönområden", "Mångfald i bostadsutbudet",
                                          "Levandegöra gemensamma platser", "Kommunikationer",
                                          "Kultur & fritid","Utbildning","Omsorg","Skolan",
                                          "Trygghet","Hållbar utveckling")
                  ),
                  
                    shinyjs::useShinyjs(),
                    actionButton("könButton",label = "Kön"),
                    hidden(
                    checkboxGroupInput("Check1",label=h4 (""), choices = levels(data3$Kön))),
                    br(),
                    actionButton("utbildningButton",label = "Utbildningsnivå"),
                    hidden(
                    checkboxGroupInput("Check2",label=h4 (""), choices = levels(data3$Utbildningsnivå))),
                    br(),
                    actionButton("sysselsättningButton",label = "Sysselsättning"),
                    hidden(  
                    checkboxGroupInput("Check3",label=h4 (""), choices = levels(data3$Sysselsättning))),
                    br(),
                    actionButton("tidIUppväsButton",label = "Tid I Uppväs"),
                    hidden(
                    checkboxGroupInput("Check4",label=h4 (""), choices = levels(data3$Tid.i.UppVäs))),
                    br(),
                    actionButton("ålderButton",label = "Ålder"),
                    hidden(
                    checkboxGroupInput("Check5",label=h4 (""), choices = levels(data3$Ålder))),
                  
                  selectInput("var2", 
                              label = h4("Välj ett ämne"),
                              choices = c("Välj ett alternativ", "Vatten eller bostäder",
                                          "Service eller grönområden", "Centralort eller mindre tätort")
                                        )
                  )


    )
    )            
    )
        
)
