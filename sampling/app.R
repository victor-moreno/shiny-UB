library(shiny)
library(ggplot2)

## modified from  "Sampling Regimes" by Jared Knwoles http://www.showmeshiny.com/sampling-regimes/

ui <- shinyUI(fluidPage(
  
  titlePanel("Sampling strategies"),
  sidebarLayout(
    
    sidebarPanel(
      sliderInput("obs","Size of Population:", min=50,max=1000,value=100,step=50),
      sliderInput("pr","Sampling proportion:", min=0,max=1,value=.2,step=0.1),
      selectInput("sampling", "Choose a sampling strategy:", 
                  choices = c("1 each Np", "Random", "Cluster randomized", "Stratified")),
      actionButton("Restart", label = "Restart"),
      width=3),
    
    mainPanel(
      plotOutput("distPlot"),
      verbatimTextOutput("summary")
    )
  )
))

server <- function(input,output){
  
  generateData<-function(i, obs){
    probs<-c(.1,.2,.4,.4)  
    restart<-i
    data.frame(x=rnorm(obs),y=runif(obs),
               z=sample(c("A","B","C","D"),obs,replace=TRUE,prob=probs))
  }
  
  stratified_sampling<-function(df, size) {
    o<-lapply( unique(df$z), function(x) {
      s<-df[df$z==x,]
      s[sample(1:NROW(s), size),]})
    do.call("rbind",o)
  }
  
  
  POPinput <- reactive({ generateData( input$Restart, input$obs ) })  
  
  SAMPinput <- reactive({
    n<-input$obs
    p<-input$pr
    N<-ceiling(n*p)
    switch(input$sampling,
           "1 each Np" = POPinput()[(1:n) %/% N ==0,],
           "Random" = POPinput()[sample(1:n, size=N),],
           "Cluster randomized" = POPinput()[POPinput()$z %in% sample(unique(POPinput()$z),2),],
           "Stratified"=stratified_sampling(POPinput(),size=ceiling(min(table(POPinput()$z)*p)))
    )
  })
  
  output$distPlot<-renderPlot({
    p<-ggplot()+geom_point(aes(x=x,y=y),data=POPinput())+
      geom_point(aes(x=x,y=y),data=SAMPinput(),color=I('blue'),shape=0,size=I(4))+ 
      facet_wrap(~z) 
    print(p)
  })
  output$summary<-renderPrint({ 
    
    population<-factor(POPinput()[,"z"], levels=c("A","B","C","D"))
    group<-factor(SAMPinput()[,"z"], levels=c("A","B","C","D"))
    cat("Samples by group (n / %):\n")
    sa<-table(group)
    options(warn=-1)
    print(formatC(sa, digits=0, width=5, format="d"), quo=FALSE)
    pr<-round(table(group)/table(population)*100,1)
    names(pr)<-NULL
    cat(formatC(pr, digits=1, width=5, format="f"))
  })
  
}

shinyApp(ui = ui, server = server)
