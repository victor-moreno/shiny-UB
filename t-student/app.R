library(shiny)

normTail <-
  function(m=0, s=1, L=NULL, U=NULL, M=NULL, df=1000, curveColor=1, border=1, col='#569BBD', xlim=NULL, ylim=NULL, xlab='', ylab='', digits=2, axes=1, detail=999, xLab=c('number', 'symbol'), cex.axis=.8, xAxisIncr=1, ...){

    # modified from https://github.com/ShinyEd/ShinyEd/blob/master/dist_calc/helper/normTail.R
    # http://spark.rstudio.com/minebocek/dist_calc/
    
    if(is.null(xlim)[1]){
      xlim <- m + c(-1,1)*3.5*s
    }
    temp <- diff(range(xlim))
    x    <- seq(xlim[1] - temp/4, xlim[2] + temp/4, length.out=detail)
    y    <- dt((x-m)/s, df)/s
    if(is.null(ylim)[1]){
      ylim <- range(c(0,y))
    }
    plot(x, y, type='l', xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, axes=FALSE, col=curveColor, ...)
    if(!is.null(L[1])){
      these <- (x <= L)
      X <- c(x[these][1], x[these], rev(x[these])[1])
      Y <- c(0, y[these], 0)
      polygon(X, Y, border=border, col=col)
    }
    if(!is.null(U[1])){
      these <- (x >= U)
      X <- c(x[these][1], x[these], rev(x[these])[1])
      Y <- c(0, y[these], 0)
      polygon(X, Y, border=border, col=col)
    }
    if(all(!is.null(M[1:2]))){
      these <- (x >= M[1] & x <= M[2])
      X <- c(x[these][1], x[these], rev(x[these])[1])
      Y <- c(0, y[these], 0)
      polygon(X, Y, border=border, col=col)
    }
    
    if(axes == 1 || axes > 2){
      if(xLab[1]=='symbol'){
        xAt  <- m + (-3:3)*s
        xLab <- expression(mu-3*sigma, mu-2*sigma,
                           mu-sigma, mu,	mu+sigma,
                           mu+2*sigma, mu+3*sigma)
      } else if(xLab[1] != 'number'){
        stop('Argument "xLab" not recognized.\n')
      } else {
        temp <- seq(xAxisIncr, max(abs(xlim-m))/s, xAxisIncr)*s
        xAt <- m + c(-temp, 0, temp)
        xLab <- round(xAt, digits=digits)
      }
    }
    if(axes > 2){
      axis(1, at=xAt, labels=xLab, cex.axis=cex.axis)
      axis(2, cex.axis=cex.axis)
    } else if(axes > 1){
      axis(2, cex.axis=cex.axis)
    } else if(axes > 0){
      axis(1, at=xAt, labels=xLab, cex.axis=cex.axis)
    }
    
    abline(h=0)
  }



ui <- shinyUI(fluidPage(  
  
  titlePanel("t-Student calculator"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("df", "Degrees of freedom", value = 30, min = 1, max = 50),
      numericInput("q", "t-value", value=NULL, min=-5, max=5),
      numericInput("p", "lower tail probability", value=NULL, min=0, max=1),
      width=4),
    
    mainPanel(
      tabPanel("Plot", 
               plotOutput("plot")
#               , verbatimTextOutput("values")
      ) 
    )
  )
))


server <- function(input, output, session) {
  
  observe({ if( !is.na(input$q) & !is.na(input$p)){ 
    updateNumericInput(session, "p", value=NA)
    updateNumericInput(session, "q", value=NA)
  }
    })
    
  output$plot <- renderPlot({
    validate( need(!is.na(input$q) | !is.na(input$p), "Insert t-value of probability"), 
              need( input$p>0 & input$p<1, "Prob must be >0  & <1") )
    if( !is.na(input$q) ){
      qq<-input$q
      pp<-pt(qq, input$df)
    } else if( !is.na(input$p) ){
      pp<-input$p
      qq<-qt(pp, input$df)
    }
    normTail(m=0, s=1, df=input$df, L=qq, U=NULL, M=NULL, axes=3, cex.axis=1.5, xlim=c(-abs(qq)-3, abs(qq)+3)) 
    if(!is.na(input$q)) text(-abs(qq)-1,.2, paste0("P= ",round(pp,4)), cex=2)
    if(!is.na(input$p)) text(-abs(qq)-1,.15, paste0("t= ",round(qq,4)), cex=2)
  })
  
#   output$values<-renderPrint( c(input$p, input$q, input$df))
}

shinyApp(ui = ui, server = server)

