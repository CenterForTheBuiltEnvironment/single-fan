library(shiny)

fluidPage(
  titlePanel('Air speed pattern for single-fan case'),
  inputPanel(numericInput(inputId = "ss", label = "Choose a fan speed setting:",
                          value = 7, min = 0, max = 7),
             numericInput(inputId = 'row', label = 'Choose a row #:',
                          value = 8, min = 1, max = 15),
             numericInput(inputId = 'col', label = 'Choose a column #:',
                          value = 7, min = 1, max = 12),
             radioButtons("sca","Scale type:", c("Self-selected" = "fix", "Min-max" = "unfix")),
             sliderInput("rang", "Range (self-selected scale):",
                         min = 0, max = 3, value = c(0,3), step = 0.1)
  ),
  wellPanel(fluidRow(column(3, plotOutput("plot1")),
                     column(3, plotOutput("plot2")),
                     column(3, plotOutput("plot3")),
                     column(3, plotOutput("plot4"))
  )),
  wellPanel(fluidRow(column(4, plotOutput("plot7")), #div(style = "height:50px"), 
                     column(4, plotOutput("plot8")),
                     column(4, plotOutput("plot9"))
  )),
  wellPanel(fluidRow(column(6, plotOutput("plot5")), 
                     column(6, plotOutput("plot6"))
  ))
)