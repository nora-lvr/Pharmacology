#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# sample_size = 100 #(input$n)

df<-tibble(seq(0, 8000, by = 1))
colnames(df)<-c('count')

# alpha = 0.05 #(input$alpha)



# ggplot(df2)+
#   geom_point(aes(x = count, y = ci))+
#   geom_hline(yintercept = c(0.01, 0.05, 0.1 ), color = 'blue')+
#   ylab('% Confidence')+
#   scale_y_continuous(labels = scales::percent_format())
#   
# 




# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Confidence for A Proportion"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = 'set_n',
        label = "Sample Size",
        value = 100,
        min = 0,
        max = 10000#,
        #step = NA,
        #width = NULL
      ),
      numericInput(
        inputId = 'x',
        label = "Count",
        value = 30,
        min = 0,
        max = 10000#,
        #step = NA,
        #width = NULL
      ),
      # sliderInput("set_n",
      #             "Sample Size",
      #             min = 0,
      #             max = 8000,
      #             value = 100), 
      sliderInput("set_alpha",
                  "Set alpha",
                  min = 0,
                  max = 1,
                  value = 0.05)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  df2<-reactive({
    df%>%
      mutate(N = input$set_n)%>%
      filter(count<=N)%>%
      mutate(
        p_hat = (count/N), 
        alpha = input$set_alpha, 
        z = qnorm(1-alpha/2))%>%
      mutate(ci = c(1)*z*sqrt(p_hat*(1-p_hat)/N),
             low = p_hat + c(-1)*z*sqrt(p_hat*(1-p_hat)/N), 
             high = p_hat + c(1)*z*sqrt(p_hat*(1-p_hat)/N))
  })
  
  prop<-reactive({
    input$x/input$set_n
  })
  
  
  uncert<-reactive({
    df2()%>%
      filter(count == input$x)%>%
      mutate(ci = round(ci*100, digits =1 ))%>%
      select(ci)
  })
  
  
  output$distPlot <- renderPlot({
    ggplot(df2())+
      
      geom_hline(yintercept = 0.01, color = 'blue'#,
                 #linetype = 'dashed'
      )+
      geom_hline(yintercept = 0.05, color = 'blue'#, 
                 #linetype = 'dashed'
      )+
      #geom_hline(yintercept = 0.10, color = 'blue')+
      geom_point(aes(x = count, y = ci), color = 'black')+
      geom_vline(aes(xintercept = input$x, color = 'red'))+
      geom_text(aes(x = input$x, y = uncert()[[1,1]]/100, label = paste0('uncertainty = ',  uncert()[[1,1]],'%' )), 
                hjust = 0, color = 'red')+
      geom_text(aes(x = input$x, y = 0.001, label = paste0('probability = ', round(prop()*100, digits = 2), 
                                                           '% +/- ', uncert()[[1,1]],'%' )), 
                hjust = 0, color = 'red')+
      
      
      ylab('% Uncertanty')+
      scale_y_continuous(labels = scales::percent_format())+
      theme_bw()+
      theme(legend.position = 'none')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
