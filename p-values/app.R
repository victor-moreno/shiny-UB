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

chiTail <-
  function(U=NULL, df = 10, curveColor=1, border=1, col="#569BBD", xlim=NULL, ylim=NULL, xlab='', ylab='', detail=999){
    #     if(U <= 30){xlim <- c(0,30)}
    #     if(U > 30){xlim <- c(0,U+0.01*U)}
    temp <- diff(range(xlim))
    x    <- seq(xlim[1] - temp/4, xlim[2] + temp/4, length.out=detail)
    y    <- dchisq(x, df)
    ylim <- range(c(0,y))
    plot(x, y, type='l', xlim=xlim, ylim=ylim, axes=FALSE, col=curveColor, xlab = "", ylab = "")
    these <- (x >= U)
    X <- c(x[these][1], x[these], rev(x[these])[1])
    Y <- c(0, y[these], 0)
    polygon(X, Y, border=border, col=col)
    abline(h=0)
    axis(1)
#     axis(1, at = c(0,U,floor(xlim[2])), label = c(NA,round(U,4),floor(xlim[2])))
    ylim[2]
  }

ui <- shinyUI(fluidPage(  
  
  titlePanel("Probability calculator"),
  sidebarPanel(
    selectInput(inputId = "dist",
                label = "Distribution:", 
                choices=c("Normal"="z", "t-Student"="ts", "chi-Square"="cs"), 
                selected="z"),
    br(),
    uiOutput("ui_df"),
    uiOutput("ui_q"),
    uiOutput("ui_p"),
    uiOutput("ui_s")
  ),
  
  mainPanel(
    plotOutput("plot")
  )
))

server <- function(input, output, session) {
  
  observe({
    
    if (input$dist=="z") {
      output$ui_s  = renderUI( selectInput("s", "", choices = c("One side"=1,"Two sides"=2), selected=2) )
      output$ui_q  = renderUI( numericInput("q", "z-value", value=NULL, min=-5, max=5) )
      output$ui_p  = renderUI( numericInput("p", "Tail probability", value=NULL, min=0, max=1) )
      output$ui_df = renderUI(NULL)
    }
    if (input$dist=="ts") {
      output$ui_s  = renderUI( selectInput("s", "", choices = c("One side"=1,"Two sides"=2), selected=2) )
      output$ui_df = renderUI( sliderInput("df", "Degrees of freedom", value = 30, min = 1, max = 50) )
      output$ui_q  = renderUI( numericInput("q", "t-value", value=NULL, min=-5, max=5) )
      output$ui_p  = renderUI( numericInput("p", "Lower tail probability", value=NULL, min=0, max=1) )
    }
    if (input$dist=="cs") {
      output$ui_df = renderUI( sliderInput("df", "Degrees of freedom", value = 2, min = 1, max = 30) )
      output$ui_q  = renderUI( numericInput("q", "Chi-value", value=NULL, min=0, max=500) )
      output$ui_p  = renderUI( numericInput("p", "Upper tail probability", value=NULL, min=0, max=1) )
      output$ui_s = renderUI(NULL)
    }
    
  
    })
    
#   observe({
#     if( !is.na(input$q) & !is.na(input$p)){ 
#       updateNumericInput(session, "p", value=NA)
#       updateNumericInput(session, "q", value=NA)
#     }
#   })
      
  output$plot <- renderPlot({
    validate( need(!is.null(input$q) | !is.null(input$p), "Insert Value or Probability"))
    validate( need(!is.na(input$q) | !is.na(input$p), "Insert Value or Probability"))
    validate( need( !is.na(input$q) | (input$p>0 & input$p<1), "Prob must be >0  & <1") )
 
    if( input$dist=="z" ){
      if(input$s==1){
        if( !is.na(input$q) ){
          qq<-input$q
          pp<-pnorm(qq)
        } else if( !is.na(input$p) ){
          pp<-input$p
          qq<-qnorm(pp)
        }
        normTail(m=0, s=1, L=qq, U=NULL, M=NULL, axes=3, cex.axis=1.5, xlim=c(-4, 4)) 
        text(-4,.15, paste0("P = ",round(pp,4)), cex=1.5, adj=0)
        text(-4,.10, paste0("z = ",round(qq,4)), cex=1.5, adj=0)
      } else{ # 2 sides
        if( !is.na(input$q) ){
          qq<-input$q
          pp<-pnorm(abs(qq), lower.tail=FALSE)*2
        } else if( !is.na(input$p) ){
          pp<-input$p
          qq<-qnorm(pp/2)
        }
        normTail(m=0, s=1, L=-abs(qq), U=abs(qq), M=NULL, axes=3, cex.axis=1.5, xlim=c(-4, 4)) 
        text(0,.15, paste0("P = ",round(pp,4)), cex=1.5, adj=0.5)
        text(0,.10, paste0("z = ",round(qq,4)), cex=1.5, adj=0.5)
      }
      
    } 

    if( input$dist=="ts" ){
      if(input$s==1){
        if( !is.na(input$q) ){
        qq<-input$q
        pp<-pt(qq, input$df)
      } else if( !is.na(input$p) ){
        pp<-input$p
        qq<-qt(pp, input$df)
      }
      normTail(m=0, s=1, df=input$df, L=qq, U=NULL, M=NULL, axes=3, cex.axis=1.5, xlim=c(-abs(qq)-3, abs(qq)+3)) 
        text(-abs(qq)-2.5,.15, paste0("P = ",round(pp,4)), cex=1.5, adj=0)
        text(-abs(qq)-2.5,.10, paste0("t = ",round(qq,4)), cex=1.5, adj=0)
    } else {
      if( !is.na(input$q) ){
        qq<-input$q
        pp<-pt(abs(qq), input$df, lower.tail=FALSE)*2
      } else if( !is.na(input$p) ){
        pp<-input$p
        qq<-qt(pp/2, input$df)
      }
      normTail(m=0, s=1, df=input$df, L=-abs(qq), U=abs(qq), M=NULL, axes=3, cex.axis=1.5, xlim=c(-abs(qq)-3, abs(qq)+3)) 
      text(0,.15, paste0("P = ",round(pp,4)), cex=1.5, adj=0.5)
      text(0,.10, paste0("t = ",round(qq,4)), cex=1.5, adj=0.5)      
    }
    }
    
    if( input$dist=="cs" ){
      if( !is.na(input$q) ){
        qq<-input$q
        pp<-pchisq(qq, input$df, lower.tail = FALSE)
      } else if( !is.na(input$p) ){
        pp<-input$p
        qq<-qchisq(pp, input$df, lower.tail = FALSE)
      }
      qmax<-qchisq(.999, input$df)
      y<-chiTail(U=qq, df=input$df, xlim=c(0,qmax)) 
      text(qq,y*.5, paste0("P = ",round(pp,4)), cex=1.5, adj=0)
      text(qq,y*.4, bquote(chi^2 ==.(round(qq,4))), cex=1.5, adj=0)
    }
  })
  
}

shinyApp(ui = ui, server = server)

