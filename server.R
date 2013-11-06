library(shiny)
library(ggplot2)

### Helper functions to run the simulation from outside the shinyServer function
getLine = function(df)
{
  df$slope = sin(df$theta)/cos(df$theta)
  df$int = df$y-df$slope*df$x
  return(df)
}
getEndPoints = function(df)
{
  df$xstart = df$x + .5*cos(df$theta)
  df$ystart = df$y + .5*sin(df$theta)
  df$xend = df$x - .5*cos(df$theta)
  df$yend = df$y - .5*sin(df$theta)
  return(df)
}
rneedle = function(n)
{
  df = data.frame(runif(n,0,5),runif(n,0,1), runif(n,-pi,pi))
  colnames(df) = c("x","y","theta")
  cross = NA
  df=getLine(df)
  df=getEndPoints(df)
  cross = (df$ystart > 1 | df$ystart < 0 | df$yend > 1 | df$yend < 0)
  df = cbind(df,cross)
  return(df)
}
plotneedle = function(df)
{
  p = ggplot(df, aes(x=xstart, y=ystart, xend=`xend`, yend=`yend`)) +
         geom_segment(color=factor(df$cross), size=I(1)) +
         geom_hline(yintercept=c(0,1))
  return(p)
}
buffon = function(df)
{
  return(2*dim(df)[1]/sum(df$cross))
}

df = rneedle(1000)

### Start of Shiny code
shinyServer(function(input, output) {
  
  titleText <- reactive({      #Give the application a dynamic title
    cat("titletext")
    paste("Buffon Estimate with", input$count, "needles")
  })
  
  estimateText = reactive({
    paste("The Buffon estimate of pi for",input$count,"needles is: ", buffon(df))
  })
  
  output$caption = renderText({titleText()})
  
  output$buffPlot = renderPlot({     # Plot the simulation results with ggplot
    df <<- rneedle(input$count)
    print(plotneedle(df))
  })
  output$estimateText = renderText({
    estimateText()
    })
})