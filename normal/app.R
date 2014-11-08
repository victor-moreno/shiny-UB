library(shiny)

ui <- shinyUI(fluidPage(  
  
  titlePanel("Normal distribution"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("mean", "mean:  ",  value = 0, min = -20, max = 20),
      sliderInput("sd",   "stdev: ", value = 1, min = 0, max = 10),
      sliderInput("max",  "X axis:",  value = c(-4,4), min = -50, max = 50),
      checkboxInput("lmean", "Show mean", value = FALSE),
      checkboxInput("lsd", "Show sd", value = FALSE),
      width=4),
    
    mainPanel(
      tabPanel("Plot", plotOutput("plot")
      ) 
    )
  )
))


server <- function(input, output) {
    
  pl<-function(m, s){

    }
    
  output$plot <- renderPlot({
    m<-input$mean
    s<-input$sd
    x<-seq(input$max[1], input$max[2], 0.01)
    y<-dnorm(x,m,s)
    mxy<-max(y)
    plot(x, y, ylim=range(c(0,min(mxy,100))), xlim=input$max, yaxt="n", col=4, lwd=2, ylab="", type="l", main="", bty = 'n')
    if (input$lmean) {
      abline(v=m, col=2, lwd=2)
      text(m-.2, mxy*.6, expression(mu), adj=1, col=2, cex=2)
    }
    if (input$lsd) {
      ys<-dnorm(m,m+s,s)
      arrows(m,ys , m+s, ys, col=3, length=.1, lwd=2)
      text(m+s/2, ys*1.1, expression(sigma), adj=0.5, col=3, cex=2)
    }
  })
  
}

shinyApp(ui = ui, server = server)

