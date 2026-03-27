library(shiny)
library(shinyWidgets)
library(tidyverse)
library(bslib) # Added for modern UI components

# --- Data Processing (Kept exactly as you had it) ---
# Note: Assuming your CSV is in the same relative path
df <- read_csv('data/TraceMineralLVR2025some2026.csv') %>%
  filter(!(str_detect(Analyte, 'Anal.'))) %>%
  filter(!(is.na(Analyte))) %>%
  rename(notes1 = `...7`, notes2 = `...8`) %>%
  mutate(value = parse_number(Value_ppm), 
         low = parse_number(Ref_Low_ppm), 
         high = parse_number(Ref_High_ppm))

anon_herd <- df %>%
  group_by(Herd) %>%
  summarize(ct_samples = sum(n())) %>%
  ungroup() %>%
  arrange(desc(ct_samples)) %>%
  rowid_to_column() %>%
  rename(anon_herd = rowid)

df2 <- df %>% left_join(anon_herd)

ref_df <- df %>%
  select(Analyte, low, high) %>%
  distinct() %>%
  arrange(Analyte, low, high) %>%
  group_by(Analyte) %>%
  mutate(ref_range_type = 1:n()) %>%
  ungroup() %>%
  filter(!((Analyte %in% 'Se') & (low > 1000))) %>%
  mutate(ref_range_name = case_when(
    (str_detect(Analyte, 'Vitam.') & (ref_range_type %in% 1)) ~ 'Neonate', 
    ((str_detect(Analyte, 'Vitam.')) & (ref_range_type %in% 2)) ~ 'Other', 
    (ref_range_type %in% 1) ~ 'Other', 
    (ref_range_type %in% 2) ~ 'Neonate', 
    TRUE ~ 'Undefined'
  ))

# --- UI Definition ---
ui <- page_sidebar(
  title = "Trace Mineral Results",
  theme = bs_theme(version = 5, bootswatch = "flatly"), # Modern Bootstrap 5 theme
  
  sidebar = sidebar(
    title = "Filter Controls",
    width = 325,
    
    pickerInput(
      inputId = "selected_analytes",
      label = "Select Analyte(s):",
      choices = sort(unique(df2$Analyte)),
      selected = c("Cu", "Se"),
      multiple = TRUE,
      options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)
    ),
    
    pickerInput(
      inputId = "herds",
      label = "Select Herd(s):",
      choices = sort(unique(df2$anon_herd)),
      selected = sort(unique(df2$anon_herd)),
      multiple = TRUE,
      options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)
    ),
    
    selectInput(
      inputId = 'ref', 
      label = "Choose Reference Range Type", 
      choices = sort(unique(ref_df$ref_range_name)),
      selected = 'Other'
    )
  ),
  
  # Main Content: Using a card to make the plot fill the remaining space
  card(
    full_screen = TRUE, # Adds the "expand" icon to the top right
    card_header("Analyte Distribution by Herd"),
    plotOutput("distPlot", height = "100%") # Crucial: set height to 100%
  )
)

# --- Server Logic ---
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    df_plot <- df2 %>%
      filter(anon_herd %in% input$herds) %>%
      filter(Analyte %in% input$selected_analytes)
    
    ref_df_plot <- ref_df %>% 
      filter(Analyte %in% input$selected_analytes) %>%
      filter(ref_range_name %in% input$ref)
    
    ggplot(df_plot) +
      geom_rect(data = ref_df_plot, 
                aes(xmin = -Inf, xmax = Inf, ymin = low, ymax = high), 
                fill = 'grey90') +
      geom_boxplot(aes(x = factor(anon_herd), y = value, 
                       color = factor(anon_herd), group = factor(anon_herd)),
                   color = 'black', 
                   fill = 'grey60', outlier.color = NA) +
      geom_point(aes(x = factor(anon_herd), y = value, color = factor(anon_herd)), 
                 size = 2, position = position_jitter(.2), shape = 8) +
      facet_wrap(Analyte ~ ., scale = 'free') +
      theme_minimal(base_size = 16) + # Slightly smaller base size for default view
      theme(
        legend.position = 'none',
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      ) +
      xlab('Herd') +
      labs(caption = 'Grey background indicates normal range')
  })
}

shinyApp(ui = ui, server = server)