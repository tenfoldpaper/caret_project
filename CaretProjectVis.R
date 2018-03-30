library(shiny)
library(ggplot2)
## KKNN visualisation logic completeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
ui <- fluidPage(
  titlePanel("Graphs"),
  sidebarLayout(
    sidebarPanel( 
           radioButtons("vsize", "Select the training sample size:",
                        c("80/20" = "eightyTwenty",
                          "64/33" = "oneThird",
                          "50/50" = "halfHalf")),
           checkboxGroupInput("vkernel", "Select kernel(s):",
                              choiceNames = list("Rectangular", "Gaussian", "Cosine"),
                              choiceValues = list("rectangular", "gaussian", "cos"),
                              selected = "rectangular"
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
           plotOutput("kknnplot1")
    )
  )
  
)

server <- function(input, output){
    #Graph variables
  vsizeget <- reactive(input$vsize)
  vkernelget <- reactive(input$vkernel)
  
  knETdf = kknnET.train$results[kknnET.train$results$distance == 2,]
  knOTdf = kknnOT.train$results[kknnOT.train$results$distance == 2,]
  knHHdf = kknnHH.train$results[kknnHH.train$results$distance == 2,]
  testData <- eightTwoTest.white[1,]
  testQuality <- predict(kknnET.train, testData)
  
  observeEvent(input$randomize, {
    testData <- eightTwoTest.white[sample(1:nrow(eightTwoTest.white), 1),]
    testQuality <- predict(kknnET.train, testData)
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
  
  output$kknnplot1 <- renderPlot(
    
    if(vsizeget() == "eightyTwenty"){
      testData <- eightTwoTest.white[1,]
      testQuality <- predict(kknnET.train, testData)
      ggplot(data = knETdf, aes(x = kmax, y = Accuracy)) + geom_line(data=subset(knETdf, kernel == vkernelget()[1] |
                                                                                   kernel == vkernelget()[2] |
                                                                                   kernel == vkernelget()[3]), aes(colour = kernel)) + ylim(0.5080, 0.6510)
    }
    else if(vsizeget() == "oneThird"){
      testData <- oneThirdTest.white[1,]
      testQuality <- predict(kknnOT.train, testData)
      ggplot(data = knOTdf, aes(x = kmax, y = Accuracy)) + geom_line(data=subset(knOTdf, kernel == vkernelget()[1] |
                                                                                   kernel == vkernelget()[2] |
                                                                                   kernel == vkernelget()[3]), aes(colour = kernel)) + ylim(0.5080, 0.6510)
    }
    else if(vsizeget() == "halfHalf"){
      testData <- halfHalfTest.white[1,]
      testQuality <- predict(kknnHH.train, testData)
      ggplot(data = knHHdf, aes(x = kmax, y = Accuracy)) + geom_line(data=subset(knHHdf, kernel == vkernelget()[1] |
                                                                                   kernel == vkernelget()[2] |
                                                                                   kernel == vkernelget()[3]), aes(colour = kernel)) + ylim(0.5080, 0.6510)
    }
  )
  
}

shinyApp(ui=ui, server=server)