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

#sample_size = 200 #(input$n)

df<-tibble(seq(0, 9999, by = 1))
colnames(df)<-c('count')

#alpha = 0.05 #(input$alpha)

# df2<-df%>%
#   mutate(N = sample_size)%>%
#   filter(count<=N)%>%
#   mutate(
#     p_hat = (count/N), 
#     alpha = alpha, 
#     z = qnorm(1-alpha/2))%>%
#   mutate(ci = c(1)*z*sqrt(p_hat*(1-p_hat)/N),
#          low = p_hat + c(-1)*z*sqrt(p_hat*(1-p_hat)/N), 
#          high = p_hat + c(1)*z*sqrt(p_hat*(1-p_hat)/N))%>%
#   mutate(p_hat5 = p_hat+0.05)
# 
# 

# ggplot(df2)+
#   geom_line(aes(x = count, y = p_hat), color = 'black')+
#   geom_line(aes(x = count, y = p_hat+0.05), color = 'grey70')+
#   geom_line(aes(x = count, y = p_hat-0.05), color = 'grey70')+
#   geom_line(aes(x = count, y = p_hat+0.10), color = 'grey70')+
#   geom_line(aes(x = count, y = p_hat-0.10), color = 'grey70')+
#   geom_line(aes(x = count, y = p_hat+0.20), color = 'grey70')+
#   geom_line(aes(x = count, y = p_hat-0.20), color = 'grey70')+
#   geom_line(aes(x = count, y = p_hat+0.30), color = 'grey70')+
#   geom_line(aes(x = count, y = p_hat-0.30), color = 'grey70')+
#   geom_text(aes(x = -Inf, y = min(p_hat)+0.05, label = '5%'), color = 'grey70', hjust = 0)+
#   geom_text(aes(x = -Inf, y = min(p_hat)+0.10, label = '10%'), color = 'grey70', hjust = 0)+
#   geom_text(aes(x = -Inf, y = min(p_hat)+0.20, label = '20%'), color = 'grey70', hjust = 0)+
#   geom_text(aes(x = -Inf, y = min(p_hat)+0.30, label = '30%'), color = 'grey70', hjust = 0)+
#   geom_text(aes(x = Inf, y = max(p_hat)-0.05, label = '-5%'), color = 'grey70', hjust = 1)+
#   geom_text(aes(x = Inf, y = max(p_hat)-0.10, label = '-10%'), color = 'grey70', hjust = 1)+
#   geom_text(aes(x = Inf, y = max(p_hat)-0.20, label = '-20%'), color = 'grey70', hjust = 1)+
#   geom_text(aes(x = Inf, y = max(p_hat)-0.30, label = '-30%'), color = 'grey70', hjust = 1)+
#   geom_linerange(aes(x = count, ymin = low, ymax = high, color = ci)#, 
#                  #color = 'blue'
#                  )+
#   geom_point(aes(x = count, y = p_hat), color = 'black')+
#   ylab('%')+
#   scale_y_continuous(labels = scales::percent_format())+
#   theme_bw()+
#   theme(#legend.position = 'none', 
#         aspect.ratio = 1)+
#   coord_cartesian(ylim = c(0, 1))+
#   scale_color_gradient(low = 'black', high = 'blue')




# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Confidence for A Proportion"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = 'set_n',
        label = "Sample Size (between 0 and 10000)",
        value = 100,
        min = 0,
        max = 10000#,
        #step = NA,
        #width = NULL
      ),
      numericInput(
        inputId = 'x',
        label = "Count (between 0 and Sample Size)",
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
      mutate(ci_txt = round(ci*100, digits =1 ))%>%
      select(ci_txt)
  })
  
  
  output$distPlot <- renderPlot({
    ggplot(df2())+
      geom_vline(aes(xintercept = input$x, linewidth = .8), color = 'red', alpha = .5)+
      
      geom_line(aes(x = count, y = p_hat), color = 'black')+
      geom_line(aes(x = count, y = p_hat+0.05), color = 'grey70')+
      geom_line(aes(x = count, y = p_hat-0.05), color = 'grey70')+
      geom_line(aes(x = count, y = p_hat+0.10), color = 'grey70')+
      geom_line(aes(x = count, y = p_hat-0.10), color = 'grey70')+
      geom_line(aes(x = count, y = p_hat+0.20), color = 'grey70')+
      geom_line(aes(x = count, y = p_hat-0.20), color = 'grey70')+
      geom_line(aes(x = count, y = p_hat+0.30), color = 'grey70')+
      geom_line(aes(x = count, y = p_hat-0.30), color = 'grey70')+
      geom_text(aes(x = -Inf, y = min(p_hat)+0.05, label = '5%'), color = 'grey70', hjust = 0)+
      geom_text(aes(x = -Inf, y = min(p_hat)+0.10, label = '10%'), color = 'grey70', hjust = 0)+
      geom_text(aes(x = -Inf, y = min(p_hat)+0.20, label = '20%'), color = 'grey70', hjust = 0)+
      geom_text(aes(x = -Inf, y = min(p_hat)+0.30, label = '30%'), color = 'grey70', hjust = 0)+
      geom_text(aes(x = Inf, y = max(p_hat)-0.05, label = '-5%'), color = 'grey70', hjust = 1)+
      geom_text(aes(x = Inf, y = max(p_hat)-0.10, label = '-10%'), color = 'grey70', hjust = 1)+
      geom_text(aes(x = Inf, y = max(p_hat)-0.20, label = '-20%'), color = 'grey70', hjust = 1)+
      geom_text(aes(x = Inf, y = max(p_hat)-0.30, label = '-30%'), color = 'grey70', hjust = 1)+
      
      geom_linerange(aes(x = count, ymin = low, ymax = high, color = ci)#,
                     #color = 'blue'
      )+
      geom_point(aes(x = count, y = p_hat), color = 'black')+
      
      # geom_text(aes(x = input$x, y = uncert()[[1,1]]/100, label = paste0('uncertainty = ',  uncert()[[1,1]],'%' )),
      #           hjust = 0, color = 'red')+
      geom_text(aes(x = input$x, y = 0.001, label = paste0('probability = ', round(prop()*100, digits = 2),
                                                           '% +/- ', uncert()[[1,1]],'%' )),
                hjust = 0, color = 'red')+
      
      ylab('%')+
      scale_y_continuous(labels = scales::percent_format())+
      theme_bw()+
      theme(legend.position = 'none',
            aspect.ratio = 1)+
      coord_cartesian(ylim = c(0, 1))+
      scale_color_gradient(low = 'black', high = 'blue')
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
