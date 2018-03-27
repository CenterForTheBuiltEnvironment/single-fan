AS <- read.csv("Single_Fan.csv", header = TRUE, sep = ';')
x <- seq(38.5, 423.5, length.out = 12)
y <- seq(23.5, 513.5, length.out = 15)
AS <- as.matrix(AS)
colsca1 <- 25
colsca2 <- 20

library(shiny)

function(input, output) {
  
  output$plot1 <- renderPlot({
    sca <- switch(input$sca, fix = range(input$rang), unfix = range(c(max(AS[,(1+48*(7-input$ss)):(48+48*(7-input$ss))]), min(AS[,(1+48*(7-input$ss)):(48+48*(7-input$ss))]))), range(input$rang))
    sca2 <- switch(input$sca, fix = colsca1, unfix = colsca2, colsca1)
    filled.contour(x = x,
                   y = y,
                   z = as1 <- t(apply(AS[,(1+48*(7-input$ss)):(12+48*(7-input$ss))], 2, rev)),
                   xlim = range(x, finite = TRUE),
                   ylim = range(y, finite = TRUE),
                   plot.title = title(main = "Height = 1.7 m",
                                      xlab = "x-axis (cm)", ylab = "y-axis (cm)"),
                   zlim = sca,
                   nlevels = sca2,
                   col = topo.colors(30,1),
                   key.title = title(main = "m/s")
    )
  })
  output$plot2 <- renderPlot({
    sca <- switch(input$sca, fix = range(input$rang), unfix = range(c(max(AS[,(1+48*(7-input$ss)):(48+48*(7-input$ss))]), min(AS[,(1+48*(7-input$ss)):(48+48*(7-input$ss))]))), range(input$rang))
    sca2 <- switch(input$sca, fix = colsca1, unfix = colsca2, colsca1)
    filled.contour(x = x,
                   y = y,
                   z = as2 <- t(apply(AS[,(13+48*(7-input$ss)):(24+48*(7-input$ss))], 2, rev)),
                   xlim = range(x, finite = TRUE),
                   ylim = range(y, finite = TRUE),
                   plot.title = title(main = "Height = 1.1 m",
                                      xlab = "x-axis (cm)", ylab = "y-axis (cm)"),
                   zlim = sca,
                   nlevels = sca2, 
                   col = topo.colors(30,1),
                   key.title = title(main = "m/s")
    )})
  output$plot3 <- renderPlot({sca <- switch(input$sca, fix = range(input$rang), unfix = range(c(max(AS[,(1+48*(7-input$ss)):(48+48*(7-input$ss))]), min(AS[,(1+48*(7-input$ss)):(48+48*(7-input$ss))]))), range(input$rang))
  sca2 <- switch(input$sca, fix = colsca1, unfix = colsca2, colsca1)
  filled.contour(x = x,
                 y = y,
                 z = as3 <- t(apply(AS[,(25+48*(7-input$ss)):(36+48*(7-input$ss))], 2, rev)),
                 xlim = range(x, finite = TRUE),
                 ylim = range(y, finite = TRUE),
                 plot.title = title(main = "Height = 0.6 m",
                                    xlab = "x-axis (cm)", ylab = "y-axis (cm)"),
                 zlim = sca,
                 nlevels = sca2, 
                 col = topo.colors(30,1),
                 key.title = title(main = "m/s")
  )})
  output$plot4 <- renderPlot({sca <- switch(input$sca, fix = range(input$rang), unfix = range(c(max(AS[,(1+48*(7-input$ss)):(48+48*(7-input$ss))]), min(AS[,(1+48*(7-input$ss)):(48+48*(7-input$ss))]))), range(input$rang))
  sca2 <- switch(input$sca, fix = colsca1, unfix = colsca2, colsca1)
  filled.contour(x = x,
                 y = y,
                 z = as4 <- t(apply(AS[,(37+48*(7-input$ss)):(48+48*(7-input$ss))], 2, rev)),
                 xlim = range(x, finite = TRUE),
                 ylim = range(y, finite = TRUE),
                 plot.title = title(main = "Height = 0.1 m",
                                    xlab = "x-axis (cm)", ylab = "y-axis (cm)"),
                 zlim = sca,
                 nlevels = sca2, 
                 col = topo.colors(30,1),
                 key.title = title(main = "m/s")
  )})
  output$plot5 <- renderPlot({sca <- switch(input$sca, fix = range(input$rang), unfix = range(c(max(AS[,(1+48*(7-input$ss)):(48+48*(7-input$ss))]), min(AS[,(1+48*(7-input$ss)):(48+48*(7-input$ss))]))), range(input$rang))
  sca2 <- switch(input$sca, fix = colsca1, unfix = colsca2, colsca1)
  filled.contour(x = x,
                 y = c(10, 60, 110, 170),
                 z = t(apply(matrix(c(AS[input$row,(1+48*(7-input$ss)):(12+48*(7-input$ss))],AS[input$row,(13+48*(7-input$ss)):(24+48*(7-input$ss))],AS[input$row,(25+48*(7-input$ss)):(36+48*(7-input$ss))],AS[input$row,(37+48*(7-input$ss)):(48+48*(7-input$ss))]), nrow = 12, ncol = 4), 1, rev)),
                 xlim = range(x, finite = TRUE),
                 ylim = range(c(10, 170), finite = TRUE),
                 plot.title = title(main = "Air speed section in row",
                                    xlab = "x-axis (cm)", ylab = "Height (cm)"),
                 zlim = sca,
                 nlevels = sca2,
                 col = topo.colors(30,1),
                 key.title = title(main = "m/s")
  )})
  output$plot6 <- renderPlot({sca <- switch(input$sca, fix = range(input$rang), unfix = range(c(max(AS[,(1+48*(7-input$ss)):(48+48*(7-input$ss))]), min(AS[,(1+48*(7-input$ss)):(48+48*(7-input$ss))]))), range(input$rang))
  sca2 <- switch(input$sca, fix = colsca1, unfix = colsca2, colsca1)
  filled.contour(x = y,
                 y = c(10, 60, 110, 170),
                 z = apply(apply(matrix(c(AS[,input$col+48*(7-input$ss)], AS[,12+input$col+48*(7-input$ss)], AS[,24+input$col+48*(7-input$ss)],AS[,36+input$col+48*(7-input$ss)]), nrow = 15, ncol = 4), 1, rev), 1, rev),
                 xlim = range(y, finite = TRUE),
                 ylim = range(c(10, 170), finite = TRUE),
                 plot.title = title(main = "Air speed section in column",
                                    xlab = "y-axis (cm)", ylab = "Height (cm)"),
                 zlim = sca,
                 nlevels = sca2, 
                 col = topo.colors(30,1),
                 key.title = title(main = "m/s")
  )})
  output$plot7 <- renderPlot({
    sca <- switch(input$sca, fix = range(input$rang), unfix = range(c(max(AS[,(1+48*(7-input$ss)):(48+48*(7-input$ss))]), min(AS[,(1+48*(7-input$ss)):(48+48*(7-input$ss))]))), range(input$rang))
    sca2 <- switch(input$sca, fix = colsca1, unfix = colsca2, colsca1)
    filled.contour(x = x,
                   y = y,
                   z = as1 <- t(apply((AS[,(13+48*(7-input$ss)):(24+48*(7-input$ss))]+AS[,(25+48*(7-input$ss)):(36+48*(7-input$ss))]+AS[,(37+48*(7-input$ss)):(48+48*(7-input$ss))])/3, 2, rev)),
                   xlim = range(x, finite = TRUE),
                   ylim = range(y, finite = TRUE),
                   plot.title = title(main = "For seated person (1.1 m, 0.6 m, 0.1 m)",
                                      xlab = "x-axis (cm)", ylab = "y-axis (cm)"),
                   zlim = sca,
                   nlevels = sca2,
                   col = topo.colors(30,1),
                   key.title = title(main = "m/s")
    )
  })
  output$plot8 <- renderPlot({
    sca <- switch(input$sca, fix = range(input$rang), unfix = range(c(max(AS[,(1+48*(7-input$ss)):(48+48*(7-input$ss))]), min(AS[,(1+48*(7-input$ss)):(48+48*(7-input$ss))]))), range(input$rang))
    sca2 <- switch(input$sca, fix = colsca1, unfix = colsca2, colsca1)
    filled.contour(x = x,
                   y = y,
                   z = as1 <- t(apply((AS[,(1+48*(7-input$ss)):(12+48*(7-input$ss))]+AS[,(13+48*(7-input$ss)):(24+48*(7-input$ss))]+AS[,(37+48*(7-input$ss)):(48+48*(7-input$ss))])/3, 2, rev)),
                   xlim = range(x, finite = TRUE),
                   ylim = range(y, finite = TRUE),
                   plot.title = title(main = "For standing person (1.7 m, 1.1 m, 0.1 m)",
                                      xlab = "x-axis (cm)", ylab = "y-axis (cm)"),
                   zlim = sca,
                   nlevels = sca2,
                   col = topo.colors(30,1),
                   key.title = title(main = "m/s")
    )
  })
  output$plot9 <- renderPlot({
    sca <- switch(input$sca, fix = range(input$rang), unfix = range(c(max(AS[,(1+48*(7-input$ss)):(48+48*(7-input$ss))]), min(AS[,(1+48*(7-input$ss)):(48+48*(7-input$ss))]))), range(input$rang))
    sca2 <- switch(input$sca, fix = colsca1, unfix = colsca2, colsca1)
    filled.contour(x = x,
                   y = y,
                   z = as1 <- t(apply((AS[,(1+48*(7-input$ss)):(12+48*(7-input$ss))]+AS[,(13+48*(7-input$ss)):(24+48*(7-input$ss))]+AS[,(25+48*(7-input$ss)):(36+48*(7-input$ss))]+AS[,(37+48*(7-input$ss)):(48+48*(7-input$ss))])/4, 2, rev)),
                   xlim = range(x, finite = TRUE),
                   ylim = range(y, finite = TRUE),
                   plot.title = title(main = "Height average (1.7 m, 1.1 m, 0.6 m, 0.1 m)",
                                      xlab = "x-axis (cm)", ylab = "y-axis (cm)"),
                   zlim = sca,
                   nlevels = sca2,
                   col = topo.colors(30,1),
                   key.title = title(main = "m/s")
    )
  })
  
}