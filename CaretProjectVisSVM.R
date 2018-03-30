#SVM Visualisaion written by Seongjin Bien, TquanT 2018 

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Graphs"),
  sidebarLayout(
    sidebarPanel( 
      radioButtons("vsize", "Select the training sample size:",
                   c("80/20" = "eightyTwenty",
                     "64/33" = "oneThird",
                     "50/50" = "halfHalf")),
      checkboxGroupInput("vc", "Select Cost value(s):",
                         choiceNames = list("2", "4", "6", "8", "10"),
                         choiceValues = list("2", "4", "6", "8", "10"),
                         selected = "2"
      ),
      print("Test data details:"),
      verbatimTextOutput("testData"),
      print("Test data quality prediction:"),
      verbatimTextOutput("testQuality"),
      print("Test data real quality:"),
      verbatimTextOutput("realQuality"),
      actionButton("randomize", "Randomize data!")
    ),
    mainPanel(
      plotOutput("svmplot1")
    )
  )
  
)

server <- function(input, output){
  #Graph variables
  vsizeget <- reactive(input$vsize)
  vcget <- reactive(input$vc)
  
  svmETdf = svmET.train$results
  svmOTdf = svmOT.train$results
  svmHHdf = svmHH.train$results
  testData <- eightTwoTest.white[1,]
  testQuality <- predict(svmET.train, testData)
  
  observeEvent(input$randomize, {
    testData <- eightTwoTest.white[sample(1:nrow(eightTwoTest.white), 1),]
    testQuality <- predict(svmET.train, testData)
    cat("Button pressed!")
    print(testData)
    print(testQuality)
    output$testData <- renderPrint(
      print(testData)
    )
    output$testQuality <- renderPrint(
      print(testQuality)
    )
    output$realQuality <- renderPrint(
      print(testData$quality)
    )
  } )
  
  output$svmplot1 <- renderPlot(
    
    if(vsizeget() == "eightyTwenty"){
      ggplot(data = svmETdf, aes(x = sigma, y = Accuracy, color = C)) + geom_line(data=subset(svmETdf, C == vcget()[1])) + geom_point(data=subset(svmETdf, C == vcget()[1])) + 
        geom_line(data=subset(svmETdf, C == vcget()[2])) + geom_point(data=subset(svmETdf, C == vcget()[2])) + 
        geom_line(data=subset(svmETdf, C == vcget()[3])) + geom_point(data=subset(svmETdf, C == vcget()[3])) + 
        geom_line(data=subset(svmETdf, C == vcget()[4])) + geom_point(data=subset(svmETdf, C == vcget()[4])) + 
        geom_line(data=subset(svmETdf, C == vcget()[5])) + geom_point(data=subset(svmETdf, C == vcget()[5])) + 
        ylim(0.6080, 0.6510)
    }
    else if(vsizeget() == "oneThird"){
      ggplot(data = svmOTdf, aes(x = sigma, y = Accuracy, color = C)) + geom_line(data=subset(svmOTdf, C == vcget()[1])) + geom_point(data=subset(svmOTdf, C == vcget()[1])) + 
        geom_line(data=subset(svmOTdf, C == vcget()[2])) + geom_point(data=subset(svmOTdf, C == vcget()[2])) + 
        geom_line(data=subset(svmOTdf, C == vcget()[3])) + geom_point(data=subset(svmOTdf, C == vcget()[3])) + 
        geom_line(data=subset(svmOTdf, C == vcget()[4])) + geom_point(data=subset(svmOTdf, C == vcget()[4])) + 
        geom_line(data=subset(svmOTdf, C == vcget()[5])) + geom_point(data=subset(svmOTdf, C == vcget()[5])) + 
        ylim(0.5080, 0.6510)
    }
    else if(vsizeget() == "halfHalf"){
      ggplot(data = svmHHdf, aes(x = sigma, y = Accuracy, color = C)) + geom_line(data=subset(svmHHdf, C == vcget()[1])) + geom_point(data=subset(svmHHdf, C == vcget()[1])) + 
        geom_line(data=subset(svmHHdf, C == vcget()[2])) + geom_point(data=subset(svmHHdf, C == vcget()[2])) + 
        geom_line(data=subset(svmHHdf, C == vcget()[3])) + geom_point(data=subset(svmHHdf, C == vcget()[3])) + 
        geom_line(data=subset(svmHHdf, C == vcget()[4])) + geom_point(data=subset(svmHHdf, C == vcget()[4])) + 
        geom_line(data=subset(svmHHdf, C == vcget()[5])) + geom_point(data=subset(svmHHdf, C == vcget()[5])) + 
        ylim(0.5080, 0.6510)
    }
  )
}

shinyApp(ui=ui, server=server)