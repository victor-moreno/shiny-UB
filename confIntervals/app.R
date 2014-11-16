require(shiny)

# Define UI for dataset viewer application
ui<- shinyUI(
  pageWithSidebar(
    # Application title.
    headerPanel("Simulating Confidence Intervals:")

      ,sidebarPanel(class="span10"
        ,wellPanel(
          helpText("This app is a demonstration of confidence intervals. The length of the bars are the size of the intervals. If the interval does not overlap with the population value (the black vertical line) then it is colored red to indicate a type 1 error.  Try increasing the sample size and see what happens")
          )
        ,wellPanel(
          numericInput(inputId = "nsamp"
                       ,label=strong("Sample Size")
                       ,value=100
                       ,min=5
                       ,max=10000)
          ,numericInput(inputId = "mean"
                       ,label = strong("Mean")
                       ,value = 0
                       ,min=1
                       ,max=10000000)
          
          ,numericInput(inputId = "variance"
                       ,label = strong("Variance")
                       ,value = 1
                       ,min=.01
                       ,max=100)
          
          ,numericInput(inputId = "conf.level"
                       ,label=strong("Confidence Level")
                       ,value=95
                       ,min=1
                       ,max=99)
          ,align="center")
        ,helpText("App created by Tyler Hunt, PsychoAnalytix.com")
        )
 
      # Show a summary of the dataset and an HTML table with the requested
      # number of observations. Note the use of the h4 function to provide
      # an additional header above each output section.
      ,mainPanel(
        plotOutput('conf.plot', width="90%", height="600px")
      )
    )
  )

server<-function(input, output) {

  output$conf.plot<-renderPlot(function(){

    generator(input$nsamp, input$mean, input$variance, (input$conf.level/100))

  })

}

generator = function(n, pop.mean , pop.sd, conf.lvl) {
  
  plot(NULL
       ,xlim = c(pop.mean-pop.sd,pop.mean+pop.sd)
       ,ylim = c(0,100)
       ,yaxt = 'n'
       ,xlab = (conf.lvl)
       ,ylab = (n)
#        ,main = "Confidence Intervals of 100 Samples"
       )
  
  abline(v = pop.mean, col = 'black')
  mtext(expression(mu), cex = 2, at = pop.mean)
  
  for (i in 1:100){
    x <- rnorm(n, mean = pop.mean, sd = pop.sd)
    test <- t.test(x,conf.level=conf.lvl)
    interval <- test$conf.int
    
    if(pop.mean>interval[1] & pop.mean<interval[2]){
      lines(c(interval[1],interval[2]),c(i,i), lwd=2,col='black')
    }
    else{
      lines(c(interval[1],interval[2]),c(i,i), lwd=2,col='red' )
    } 
  } 
}


shinyApp(ui = ui, server = server)




