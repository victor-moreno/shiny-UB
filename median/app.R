library(shiny)
library(shinyBS)

ui <- shinyUI(fluidPage(
  
  
  titlePanel("Mean & median"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("n",
                  "Initial observations:", 
                  value = 10,
                  min = 1, 
                  max = 100),
      hr(),
      helpText("Click the plot to add observations."),
      bsToggleButton("reinicia", label = "Clean", value=TRUE)
      , width=3),
    
    mainPanel(
      tabPanel("Plot", 
               plotOutput("plot", clickId= "punto", height="275px")
      ) 
    )
  )
))


server <- function(input, output, session) {
  
  session$reinicia<-FALSE
  
  
  init<-function(n, reinicia, session){
    if(reinicia){
      x<-rnorm(n, 50, 5 )
      session$x<-x
      session$y<-runif(length(x),-0.8,-0.6)
      updateButton(session,"reinicia",value=TRUE, label="Clean")
    }
  }
  
  observe( { init(input$n, input$reinicia, session) } )
  
  pl<-function( session, punto, reinicia ){
  
    par(mar=c(2,1,1,1))
    if(reinicia){
      if(length(punto)>0 & !session$reinicia) {
        session$y<-c(session$y,runif(1,-0.8,-0.6) )
        x<-c(session$x, punto$x)
        session$x<-x
      } else{
        x<-session$x
        session$reinicia<-FALSE
      }
      me<-mean(x)
      md<-median(x)
      plot(x, y=session$y, ylim=c(-1,1), xlim=c(0,100), yaxt="n", pch=16, col=4, ylab="", xlab="")
      abline(v=me, col=2)
      abline(v=md, col=3)
      if (me < md) {
        text(me - .5, .7, "mean",   col=2, lwd=2 ,adj= 1)
        text(md + .5, .7, "median", col=3, lwd=2, adj= 0)
        
      } else {
        text(me + .5, .7, "mean",   col=2, lwd=2 ,adj=0)
        text(md - .5, .7, "median", col=3, lwd=2, adj=1)
      }
      
    } else {
      plot(NA, NA, ylim=c(-1,1), xlim=c(0,100), yaxt="n", pch=16, col=4, ylab="", xlab="")
      updateButton(session,"reinicia",value=FALSE, label="Start")
      session$reinicia<-TRUE
    }
    
    
  }
  
  output$plot <- renderPlot({ pl( session, input$punto, input$reinicia ) })
  
}

shinyApp(ui = ui, server = server)
