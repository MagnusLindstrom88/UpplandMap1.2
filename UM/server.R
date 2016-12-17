require(shiny)
require(leaflet)
require(magrittr)
require(sp)
require(htmltools)
library(dplyr)
require(shinyjs)
library(shinyjs)
library(V8)
library(RColorBrewer)
source("helper.R")

fragor2<-read.csv("f2.csv")
fragor<-readRDS("fragor.Rda")
data3<-read.csv("data3.csv")
data6<-readRDS("data6.rda")

shinyServer(function(input, output, session) {
  

  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles()%>%
      addPolygons(data=uv84, weight = 2, fillOpacity = 0.5, smoothFactor = 0.5,
                  popup = htmlEscape(uv84$NAMN))%>%
       addProviderTiles("CartoDB.Positron")

     
  })
  
  
  observeEvent(input$könButton, {
    toggle("Check1")
  })
  observeEvent(input$utbildningButton, {
    toggle("Check2")
  })
  observeEvent(input$sysselsättningButton, {
    toggle("Check3")
  })
  observeEvent(input$tidIUppväsButton, {
    toggle("Check4")
  })
  observeEvent(input$ålderButton, {
    toggle("Check5")
  })

  område_popup<-reactive({
    

    urval_data<-urval()
    områden<-karta(input$var,urval_data)
    z<-popup(input$var)
    område_popup <-paste( "<Strong>Område: </strong>",
                          områden[,1],
                          "<br><Strong>",z[1,],":</strong>",
                          områden[,2],
                          "<br><strong>",z[2,],":</strong>",
                          områden[,3],
                          "<br><strong>",z[3,],":</strong>",
                          områden[,4],
                          "<br><strong>",z[4,],":</strong>",
                          områden[,5],
                          "<br><strong>",z[5,],":</strong>",
                          områden[,6])

    
  })
  
  numeric_popup<-reactive({
    
    urval_data<-urval()
    områden<-numeric_map(input$var2,urval_data)
    
    numeric_popup<-paste("<Strong>Område: </strong>",
                         områden[,1],
                         "<br><Strong>Data:</strong>",
                         områden[,2])
    
  })
  
  observe({
    
    pop_up<-område_popup()
    urval_data<-urval()
    områden<-karta(input$var,urval_data)
    factpal <- colorFactor(topo.colors(5), områden$favorit)
    leafletProxy("map")%>%
      clearShapes()%>%
      clearControls()%>%
      addPolygons(data=uv84, weight = 2, fillOpacity = 0.6, smoothFactor = 0.5,
                  popup = pop_up, color = ~factpal(områden$favorit))%>%
      addLegend("bottomleft", pal = factpal, values = områden$favorit, na.label ="Ingen data", title ="Frågor" )
  })
    
    observe({
      if(!input$var2 =="Välj ett alternativ"){
       
      pop_up<-numeric_popup()
      urval_data<-urval()
      områden<-numeric_map(input$var2,urval_data)  
      legendvar<-numeric_legend(input$var2)
        
        faktorTest <-cut(områden[,2],breaks=c(-Inf,5,9,+Inf),
                         include.lowest=T,labels = c("Fråga 1 är viktigare", "Båda frågorna är lika viktiga", "Fråga 2 är viktigare"))
      
        
        colorTest <- colorFactor(c("blue3","green2","gold"),
                                    levels = faktorTest,ordered=FALSE)
                                 
        
        
        
      factpal <- colorNumeric("RdBu", områden$data)
      leafletProxy("map")%>%
        clearShapes()%>%
        clearControls()%>%
        addPolygons(data=uv84, weight = 2, fillOpacity = 0.8, smoothFactor = 0.5,
                    popup = pop_up, color = ~colorTest(faktorTest))%>%
        addLegend("bottomleft", pal = colorTest, values = faktorTest, na.label ="Ingen data", title =legendvar[1,1] )
      } else{
        
      }
      
    })
    


  urval<-function(){
    
    y<-data3  
    
    if(!is.null(input$Check1)){
      c1<-input$Check1  
      y<-subset(y,Kön %in% c1)
    }
    
    if(!is.null(input$Check2)){
      c2<-input$Check2  
      y<-subset(y,Utbildningsnivå %in% c2)
    }
    if(!is.null(input$Check3)){
      c3<-input$Check3  
      y<-subset(y,Sysselsättning %in% c3)
    }
    if(!is.null(input$Check4)){
      c4<-input$Check4  
      y<-subset(y,Tid.i.UppVäs %in% c4)
      
    }
    if(!is.null(input$Check5)){
      c5<-input$Check5
      y<-subset(y,Ålder %in% c5)
    }
    
    y2<-subset(y,select=c(X1a:Area))
    y3<-y2 %>%group_by(Area) %>%summarise_each(funs(mean))
    class(y3) <- "data.frame"
    cleanY2<-round_df(y3,1)
    
    return(cleanY2)
    
    
  }
  
})
