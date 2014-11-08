library(shiny)

ui <- shinyUI(fluidPage(  
  
  titlePanel("t-Student and Normal"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("df",
                  "Degrees of freedom", 
                  value = 1,
                  min = 1, 
                  max = 50),
      width=2),
    
    mainPanel(
      tabPanel("Plot", 
               plotOutput("plot")
      ) 
    )
  )
))


server <- function(input, output) {
    
  pl<-function(df){
      x<-seq(-5, 5, 0.01)
      plot(x, dnorm(x), ylim=c(0,.4), xlim=c(-5,5), yaxt="n", col=1, ylab="", xlab="", type="l", main="", bty = 'n')
      lines(x,dt(x,df), col=4 )
      legend(2,.3, c("Normal", "t-Student"), fill=c(0,4))

    }
    
  output$plot <- renderPlot({ pl( input$df ) })
  
}

shinyApp(ui = ui, server = server)

