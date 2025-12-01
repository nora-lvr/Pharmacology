library(shiny)
library(tidyverse)
library(pwr)


#UI
ui <- fluidPage(
  
  titlePanel("Power Explorer"),
  
  sidebarPanel(
    
    sliderInput('p1', 'Treatment Group A\nProportion (Highest)', min=0, max=1,
                value=0.75, step=0.01, round=0),
    
    sliderInput('p2', 'Treatment Group B\nProportion (Lowest)', min=0, max=1,
                value=0.5, step=0.01, round=0),
    
    sliderInput('sig.level', 'Significance Level', min=0, max=1,
                value=0.05, step=0.01, round=0),
    
    sliderInput('power', 'Power', min=0, 1,
                value=0.8, step=0.01, round=0),
    
    selectInput('alternative', 'Type', c("greater", 'two.sided', 'less')) 
    
  ),
  
  mainPanel(
    plotOutput('plot')
  )
)

#SERVER

server <- function(input, output) {
  
  
  
  output$plot <- renderPlot({
    
    p.out <- pwr.p.test(h = round((ES.h(p1 = input$p1, p2 = input$p2)), digits = 3),
                        sig.level = input$sig.level,
                        power = input$power,
                        alternative = input$alternative) #'two.sided', 'less'
    plot(p.out)
    
  }, height=500)
  
}

shinyApp(ui, server)

