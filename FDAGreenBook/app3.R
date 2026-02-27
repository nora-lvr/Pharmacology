library(shiny)
library(tidyverse)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(devtools)
library(bslib)
library(ggh4x)

source_url('https://raw.githubusercontent.com/nora-lvr/FunctionsLVR/refs/heads/main/DT_base.R')
 

#gb <- read_rds('FDAGreenBook/green2_pre.rds')%>% #for testing
 gb <- read_rds('green2_pre.rds') %>% #for deploy
  mutate(`Ingredient List` = str_sub(`Ingredient List`, 1, 45)) %>%
  mutate(AprovalType = case_when(
    str_detect(`Application Number or Monograph ID`, 'ANADA') ~ 'ANADA', 
    str_detect(`Application Number or Monograph ID`, 'NADA') ~ 'NADA', 
    TRUE ~ 'Other'
  )) %>%
  mutate(Aproval_Number = str_replace_all(`Application Number or Monograph ID`, "ANADA", "")) %>%
  mutate(Aproval_Number = str_replace_all(Aproval_Number, "NADA", ""))%>%
   mutate()%>%
  mutate(across(.cols = c("Product Type", 
                          "Marketing Category",
                         "Ingredient List",
                         "Non Proprietary Name",
                         "Proprietary Name",
                          "Labeler Name" ), 
                .fns = ~factor(.x)
                ))

#-----------------------------------------------
ui <- navbarPage(
  title = "FDA Green Book (Data from FDA Electronic Greenbook on Dec 1, 2025)",
  theme = shinytheme("yeti"),
  # theme = bs_theme(version = 5, bootswatch = "minty"),
  
  # CSS fix for picker dropdown height
  header = tags$head(
    tags$style(HTML("
      .dropdown-menu.inner {
        max-height: none !important;
      }
      .bootstrap-select .dropdown-menu {
        max-height: none !important;
      }
    "))
  ),
  
  # ---- Graph Tab ----
  tabPanel(
    "Graph",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        pickerInput(
          'as',
          'Select Ingredients',
          choices = sort(unique(gb$`Ingredient List`)),
          selected = sort(unique(gb$`Ingredient List`))[[1]],
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `deselect-all-text`= 'Unselect All',
            `select-all-text` = 'Select All'
          )
        ),
        pickerInput(
          'pt',
          'Select Product Type',
          choices = sort(unique(gb$prod_type_short)),
          selected = sort(unique(gb$prod_type_short)),
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `deselect-all-text`= 'Unselect All',
            `select-all-text` = 'Select All'
          )
        )
      ),
      mainPanel(
        width = 9,
        card(
          title = "FDA Green Book Plot",
          #full_screen = FALSE,
          # fill the card width
          plotOutput("distPlot", width = "98%")
        )
      )
    )
  ),
  
  # ---- Table Tab ----
  tabPanel(
    "Table",
    fluidRow(
      column(
        width = 12,
        DTOutput("tbl")
      )
    )
  )
)

#-----------------------------------------------
server <- function(input, output) {
  
  df_plot <- reactive({
    gb %>%
      filter(`Ingredient List` %in% input$as) %>%
      filter(prod_type_short %in% input$pt) %>%
      group_by(`Application Number or Monograph ID`,`Ingredient List`, 
               `Proprietary Name`, AprovalType, Aproval_Number ) %>%
      summarize(ct = n_distinct(`Proprietary Name`)) %>%
      ungroup()
  })
  
  df_tbl <- reactive({
    tibble(gb %>%
             filter(`Ingredient List` %in% input$as) %>%
             filter(prod_type_short %in% input$pt)) %>%
      fxn_DT_base2()
  })
  
  output$distPlot <- renderPlot({
    ggplot(df_plot()) +
      geom_bar(aes(x = `Proprietary Name`, y = `Ingredient List`, fill = `Proprietary Name`), stat = 'identity') +
      geom_text(aes(x = `Proprietary Name`, y = `Ingredient List`,
                    label = Aproval_Number),
                stat = 'identity', hjust = 1) +
      facet_grid(AprovalType ~ `Ingredient List`, scales = 'free') +
      theme_bw(base_size = 25) +  
      theme(legend.position = 'none',
            axis.text.x = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
            ) +
      xlab('') +
      ylab('') +
      coord_flip()
    

    
    
  })
  
  output$tbl <- renderDT(df_tbl())
}

shinyApp(ui = ui, server = server)

