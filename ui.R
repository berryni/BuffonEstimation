shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Buffon Estimation of Pi"),
  
  sidebarPanel(
    sliderInput("count", "Number of Needles:", 
                min=0, max=10000, value=1000)
    ),
  
  mainPanel(
    plotOutput("buffPlot"),
    verbatimTextOutput("estimateText")
  )
))