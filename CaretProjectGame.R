#Caret Project Game
library(shiny)
library(ggplot2)

ui <- fluidPage(
    titlePanel("The Winemaker"),
    tags$head(
      tags$style(type="text/css", "
                  label.control-label, 
                 .selectize-control.single{ display: table-cell; text-align: left; vertical-align: middle; } 
                 .form-inline { display: table-row;}
                 ")
    ),
    sidebarLayout(
        sidebarPanel(
          p("Make the best wine that fits the taste of our machine-learning model!\n
             Enter the variables below, and click on <Test> button. \n
             Your wine will be judged on a scale of 1 (worst) to 7 (best)."),
          
          column(6, style="padding-top:40px",
            sliderInput("var01", label="Fixed acidity", 3.5, 12, 15.5/2, step=0.05),
            sliderInput("var02", label="Volatile acidity", 0, 1, 0.5, step = 0.05),
            sliderInput("var03", label="Citric acid", 0, 1.8, 0.9, step = 0.05),
            sliderInput("var04", label="Residual sugar", 0.6, 24, 12, step=0.05),
            sliderInput("var05", label="Chlorides", 0.01, 0.25, 0.1, step=0.005),
            sliderInput("var06", label="Free SO2", 3, 131, 134/2, step=0.5)
            ),
          column(6, style="padding-top:40px",
            sliderInput("var07", label="Total SO2", 18, 315, 333/2,step=0.2),
            sliderInput("var08", label="Density", 0.98, 1.05, 0.99, step=0.005),
            sliderInput("var09", label="pH", 2.7, 3.9, 3.3, step=0.01),
            sliderInput("var10", label="Sulphates", 0.26, 1.05, 0.8,  step=0.05),
            sliderInput("var11", label="Alcohol", 8, 14.1, 11, step=0.05),
            actionButton("submit2", "Test", style="padding: 20px; padding-left: 90px; padding-right: 90px; font-size: 200%; horizontal-align: middle")
          )
        
        ),
        mainPanel(
          verbatimTextOutput("msg"),
          h3("Your wine's properties:"),
          verbatimTextOutput("properties"),
          h3("Your wine's quality:"),
          verbatimTextOutput("quality"),
          imageOutput("wineglass")
        )
    )
)

server <- function(input, output){
  varnames = c("var01", "var02", "var03", "var04", "var05", "var06", "var07", "var08", "var09", "var10", "var11")
  userData <- eightTwoTest.white[1,]
  userData$quality = 0
  qchar <- "0"
  vargetter <- reactive(c(
    input$var01,
    input$var02,
    input$var03,
    input$var04,
    input$var05,
    input$var06,
    input$var07,
    input$var08,
    input$var09,
    input$var10,
    input$var11)
  )
  
  observeEvent(input$submit2, {
    varList <- vargetter()
    userData[1] <- varList[1]
    for(i in 1:11){
      userData[i] <- varList[i]
    }
    output$quality <- renderPrint(
      
      print(as.integer(as.character(predict(rfET.train, userData)))-2)
    )
    output$properties <- renderPrint(
      print(userData)
    )
    qchar <- as.character(predict(rfET.train, userData))
    print(qchar)
    output$wineglass <- renderImage({
      print(qchar)
      if(qchar == "0"){
        return(list(
          src = "./images/wine_1.png",
          contentType = "image/png",
          alt = NULL
        ))
      }
      else if(qchar == "3"){
        return(list(
          src = "./images/wine_1.png",
          contentType = "image/png",
          alt = NULL
        ))
        }
        else if(qchar == "4"){
          return(list(
            src = "./images/wine_1.png",
            contentType = "image/png",
            alt = NULL
          ))
        }
        else if(qchar == "5"){
          return(list(
            src = "./images/wine_4.png",
            contentType = "image/png",
            alt = NULL
          ))
        }
        else if(qchar == "6"){
          return(list(
            src = "./images/wine_4.png",
            contentType = "image/png",
            alt = NULL
          ))
        }
        else if(qchar == "7"){
          return(list(
            src = "./images/wine_7.png",
            contentType = "image/png",
            alt = NULL
          ))
        }
        else if(qchar == "8"){
          return(list(
            src = "./images/wine_7.png",
            contentType = "image/png",
            alt = NULL
          ))
        }
        else if(qchar == "9"){
          return(list(
            src = "./images/wine_7.png",
            contentType = "image/png",
            alt = NULL
          ))
        }
      }, deleteFile = FALSE)
    }
  )
  
  
}
shinyApp(ui=ui, server=server)