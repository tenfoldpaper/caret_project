library(shiny)
library(ggplot2)
## KKNN visualisation logic completeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
ui <- fluidPage(
  titlePanel("Graphs"),
  fluidRow(
    column(4, 
           radioButtons("vsize", "Select the training sample size:",
                        c("80/20" = "eightyTwenty",
                          "64/33" = "oneThird",
                          "50/50" = "halfHalf"))),
           checkboxGroupInput("vkernel", "Select kernel(s):",
                              choiceNames = list("Rectangular", "Gaussian", "Cosine"),
                              choiceValues = list("rectangular", "gaussian", "cos"),
                              selected = "rectangular"
    ),
    column(8,
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
  output$kknnplot1 <- renderPlot(
    
    if(vsizeget() == "eightyTwenty"){
      ggplot(data = knETdf, aes(x = kmax, y = Accuracy)) + geom_line(data=subset(knETdf, kernel == vkernelget()[1] |
                                                                                   kernel == vkernelget()[2] |
                                                                                   kernel == vkernelget()[3]), aes(colour = kernel)) + ylim(0.6080, 0.6110)
    }
    else if(vsizeget() == "oneThird"){
      ggplot(data = knOTdf, aes(x = kmax, y = Accuracy)) + geom_line(data=subset(knOTdf, kernel == vkernelget()[1] |
                                                                                   kernel == vkernelget()[2] |
                                                                                   kernel == vkernelget()[3]), aes(colour = kernel)) + ylim(0.5080, 0.8110)
    }
    else if(vsizeget() == "halfHalf"){
      ggplot(data = knHHdf, aes(x = kmax, y = Accuracy)) + geom_line(data=subset(knHHdf, kernel == vkernelget()[1] |
                                                                                   kernel == vkernelget()[2] |
                                                                                   kernel == vkernelget()[3]), aes(colour = kernel)) + ylim(0.5080, 0.8110)
    }
  )
}

shinyApp(ui=ui, server=server)