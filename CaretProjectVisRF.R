library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Random Forest Graphs"),
  sidebarLayout(
    sidebarPanel( 
      radioButtons("vsize", "Select the training sample size:",
                   c("80/20" = "eightyTwenty",
                     "64/33" = "oneThird",
                     "50/50" = "halfHalf")),
    
      print("Test data details:"),
      verbatimTextOutput("testData"),
      print("Test data quality prediction:"),
      verbatimTextOutput("testQuality"),
      print("Test data real quality:"),
      verbatimTextOutput("realQuality"),
      actionButton("randomize", "Randomize data!")
    ),
    mainPanel(
      plotOutput("rfplot1")
    )
  )
  
)

server <- function(input, output){
  #Graph variables
  vsizeget <- reactive(input$vsize)
  vkernelget <- reactive(input$vkernel)
  
  rfETdf = rfET.train$results
  rfOTdf = rfOT.train$results
  rfHHdf = rfHH.train$results
  testData <- eightTwoTest.white[1,]
  testQuality <- predict(rfET.train, testData)
  
  observeEvent(input$randomize, {
    testData <- eightTwoTest.white[sample(1:nrow(eightTwoTest.white), 1),]
    testQuality <- predict(rfET.train, testData)
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
  
  output$rfplot1 <- renderPlot(
    
    if(vsizeget() == "eightyTwenty"){
      testData <- eightTwoTest.white[1,]
      testQuality <- predict(rfET.train, testData)
      ggplot(data = rfETdf, aes(x = mtry, y = Accuracy)) + geom_line(data=rfETdf) + ylim(0.60, 0.67)
    }
    else if(vsizeget() == "oneThird"){
      testData <- oneThirdTest.white[1,]
      testQuality <- predict(rfOT.train, testData)
      ggplot(data = rfOTdf, aes(x = mtry, y = Accuracy)) + geom_line(data=rfOTdf) +  ylim(0.6, 0.67)
    }
    else if(vsizeget() == "halfHalf"){
      testData <- halfHalfTest.white[1,]
      testQuality <- predict(rfHH.train, testData)
      ggplot(data = rfHHdf, aes(x = mtry, y = Accuracy)) + geom_line(data=rfHHdf) +  ylim(0.6, 0.67)
    }
  )
  
}

shinyApp(ui=ui, server=server)