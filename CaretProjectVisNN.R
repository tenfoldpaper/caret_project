#SVM Visualisaion written by Seongjin Bien, TquanT 2018 

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("MLP Graph; 1st layer perceptron # = 5"),
  sidebarLayout(
    sidebarPanel( 
      radioButtons("vsize", "Select the training sample size:",
                   c("80/20" = "eightyTwenty",
                     "64/33" = "oneThird",
                     "50/50" = "halfHalf")),
      checkboxGroupInput("vl3", "Select Layer 3 perceptron number(s):",
                         choiceNames = list("5", "6", "7", "8", "9"),
                         choiceValues = list("5", "6", "7", "8", "9"),
                         selected = "5"
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
      plotOutput("mlpplot1")
    )
  )
  
)

server <- function(input, output){
  #Graph variables
  vsizeget <- reactive(input$vsize)
  vl3get <- reactive(input$vl3)
  
  mlpETdf = mlpET.train$results[mlpET.train$results$layer1 == 5,]
  mlpOTdf = mlpOT.train$results[mlpET.train$results$layer1 == 5,]
  mlpHHdf = mlpHH.train$results[mlpET.train$results$layer1 == 5,]
  testData <- eightTwoTest.white[1,]
  testQuality <- predict(mlpET.train, testData)
  
  observeEvent(input$randomize, {
    testData <- eightTwoTest.white[sample(1:nrow(eightTwoTest.white), 1),]
    testQuality <- predict(mlpET.train, testData)
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
  
  output$mlpplot1 <- renderPlot(
    
    if(vsizeget() == "eightyTwenty"){
      ggplot(data = mlpETdf, aes(x = layer2, y = Accuracy, color = layer3)) + geom_line(data=subset(mlpETdf, layer3 == vl3get()[1])) + geom_point(data=subset(mlpETdf, layer3 == vl3get()[1])) + 
        geom_line(data=subset(mlpETdf, layer3 == vl3get()[2])) + geom_point(data=subset(mlpETdf, layer3 == vl3get()[2])) + 
        geom_line(data=subset(mlpETdf, layer3 == vl3get()[3])) + geom_point(data=subset(mlpETdf, layer3 == vl3get()[3])) + 
        geom_line(data=subset(mlpETdf, layer3 == vl3get()[4])) + geom_point(data=subset(mlpETdf, layer3 == vl3get()[4])) + 
        geom_line(data=subset(mlpETdf, layer3 == vl3get()[5])) + geom_point(data=subset(mlpETdf, layer3 == vl3get()[5])) + 
        ylim(0.525, 0.56)
    }
    else if(vsizeget() == "oneThird"){
      ggplot(data = mlpOTdf, aes(x = layer2, y = Accuracy, color = layer3)) + geom_line(data=subset(mlpOTdf, layer3 == vl3get()[1])) + geom_point(data=subset(mlpOTdf, layer3 == vl3get()[1])) + 
        geom_line(data=subset(mlpOTdf, layer3 == vl3get()[2])) + geom_point(data=subset(mlpOTdf, layer3 == vl3get()[2])) + 
        geom_line(data=subset(mlpOTdf, layer3 == vl3get()[3])) + geom_point(data=subset(mlpOTdf, layer3 == vl3get()[3])) + 
        geom_line(data=subset(mlpOTdf, layer3 == vl3get()[4])) + geom_point(data=subset(mlpOTdf, layer3 == vl3get()[4])) + 
        geom_line(data=subset(mlpOTdf, layer3 == vl3get()[5])) + geom_point(data=subset(mlpOTdf, layer3 == vl3get()[5])) + 
        ylim(0.525, 0.56)
    }
    else if(vsizeget() == "halfHalf"){
      ggplot(data = mlpHHdf, aes(x = layer2, y = Accuracy, color = layer3)) + geom_line(data=subset(mlpHHdf, layer3 == vl3get()[1])) + geom_point(data=subset(mlpHHdf, layer3 == vl3get()[1])) + 
        geom_line(data=subset(mlpHHdf, layer3 == vl3get()[2])) + geom_point(data=subset(mlpHHdf, layer3 == vl3get()[2])) + 
        geom_line(data=subset(mlpHHdf, layer3 == vl3get()[3])) + geom_point(data=subset(mlpHHdf, layer3 == vl3get()[3])) + 
        geom_line(data=subset(mlpHHdf, layer3 == vl3get()[4])) + geom_point(data=subset(mlpHHdf, layer3 == vl3get()[4])) + 
        geom_line(data=subset(mlpHHdf, layer3 == vl3get()[5])) + geom_point(data=subset(mlpHHdf, layer3 == vl3get()[5])) + 
        ylim(0.525, 0.56)
    }
  )
}

shinyApp(ui=ui, server=server)