library(shiny)
library(shinyWidgets)
library(tidyverse)
library(bslib)
library(bsicons)

# --- 1. Data Loading & Initial Cleaning ---
# Standardizing the dataframe outside the server for global access
#df <- read_csv('data/TraceMineralLVR2025some2026.csv') %>% #for testing
  df <- read_csv('TraceMineralLVR2025some2026.csv') %>% #for deploy
  
  filter(!(str_detect(Analyte, 'Anal.'))) %>%
  filter(!(is.na(Analyte))) %>%
  mutate(
    value = parse_number(Value_ppm),
    # Ensure these exist globally for the ref_df build
    low_orig = parse_number(Ref_Low_ppm),
    high_orig = parse_number(Ref_High_ppm)
  )

# Create Anonymous Herd Mapping
anon_herd_map <- df %>%
  group_by(Herd) %>%
  summarize(ct_samples = n(), .groups = "drop") %>%
  arrange(desc(ct_samples)) %>%
  rowid_to_column(var = "anon_herd")

df2 <- df %>% 
  left_join(anon_herd_map, by = "Herd")

# Build Reference Range Lookup Table
ref_df <- df %>%
  select(Analyte, low = low_orig, high = high_orig) %>%
  distinct() %>%
  filter(!((Analyte %in% 'Se') & (low > 1000))) %>%
  arrange(Analyte, low) %>%
  group_by(Analyte) %>%
  mutate(ref_range_type = row_number()) %>%
  ungroup() %>%
  mutate(ref_range_name = case_when(
    (str_detect(Analyte, 'Vitam.') & ref_range_type == 1) ~ 'Neonate', 
    (str_detect(Analyte, 'Vitam.') & ref_range_type == 2) ~ 'Other', 
    (ref_range_type == 1) ~ 'Other', 
    (ref_range_type == 2) ~ 'Neonate', 
    TRUE ~ 'Undefined'
  ))

# --- 2. UI Definition ---
ui <- page_sidebar(
  title = "Trace Mineral Dashboard",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    title = "Analysis Controls",
    width = 325,
    pickerInput("selected_analytes", "Select Analyte(s):", 
                choices = sort(unique(df2$Analyte)), 
                selected = c("Cu", "Se"), 
                multiple = TRUE, 
                options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)),
    
    pickerInput("herds", "Select Herd(s):", 
                choices = sort(unique(df2$anon_herd)), 
                selected = sort(unique(df2$anon_herd)), 
                multiple = TRUE, 
                options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)),
    
    selectInput('ref_type', "Reference Range Type", 
                choices = sort(unique(ref_df$ref_range_name)), 
                selected = 'Other')
  ),
  
  # Top Row: Dynamic Value Boxes
  uiOutput("box_row"),
  
  # Main Plot Card
  card(
    full_screen = TRUE,
    card_header("Graphics"),
    plotOutput("distPlot", height = "100%")
  )
)

# --- 3. Server Logic ---
server <- function(input, output) {
  
  # Central Reactive: This builds the dataset for BOTH the boxes and the plot
  processed_data <- reactive({
    req(input$selected_analytes, input$herds, input$ref_type)
    
    # 1. Get the specific reference values for the selected type
    current_refs <- ref_df %>% 
      filter(ref_range_name == input$ref_type) %>%
      select(Analyte, r_low = low, r_high = high)
    
    # 2. Filter data and join with the references
    df2 %>%
      filter(anon_herd %in% input$herds, 
             Analyte %in% input$selected_analytes) %>%
      left_join(current_refs, by = "Analyte") %>%
      mutate(
        # Robust out-of-range calculation using explicit names
        is_out = case_when(
          !is.na(r_low) & value < r_low  ~ TRUE,
          !is.na(r_high) & value > r_high ~ TRUE,
          TRUE ~ FALSE
        )
      )
  })
  
  # Value Boxes: Analyte name is the main "Value" (Large Font)
  output$box_row <- renderUI({
    data <- processed_data()
    analytes <- input$selected_analytes
    
    boxes <- lapply(analytes, function(a) {
      sub_data <- data %>% filter(Analyte == a)
      total <- nrow(sub_data)
      out_n <- sum(sub_data$is_out, na.rm = TRUE)
      pct   <- if(total > 0) round((out_n / total) * 100, 1) else 0
      
      value_box(
        title = paste0(pct, "% Out of Range"),
        value = a,  # This makes the Analyte name large
        p(paste(out_n, "of", total, "samples flagged")),
        theme = if(pct > 25) "danger" else "secondary",
        showcase = NULL
      )
    })
    
    layout_columns(!!!boxes)
  })
  
  # Main Plot
  output$distPlot <- renderPlot({
    data <- processed_data()
    req(nrow(data) > 0)
    
    # We need the reference ranges for the geom_rect background
    # We pull these from the joined 'data' to ensure they match exactly
    rect_data <- data %>%
      select(Analyte, r_low, r_high) %>%
      distinct()
    
    ggplot(data) +
      # Reference Range Shading
      geom_rect(data = rect_data, 
                aes(xmin = -Inf, xmax = Inf, ymin = r_low, ymax = r_high), 
                fill = 'grey90', alpha = 0.5) +
      # Boxplots
      geom_boxplot(aes(x = factor(anon_herd), y = value), 
                   outlier.shape = NA, fill = "white", alpha = 0.4) +
      # Individual points
      geom_point(aes(x = factor(anon_herd), y = value, color = is_out), 
                 position = position_jitter(0.2), size = 3, shape = 18) +
      facet_wrap(~Analyte, scales = "free") +
      # Cividis-inspired colors (Dark Blue for normal, Red for out)
      scale_color_manual(values = c("FALSE" = "#00204D", "TRUE" = "#FF0000")) +
      theme_minimal(base_size = 18) +
      theme(
        legend.position = "none",
        strip.text = element_text(face = "bold", size = 20),
        panel.spacing = unit(2, "lines")
      ) +
      labs(x = "Anonymous Herd ID", y = "Concentration (ppm)")
  })
}

shinyApp(ui, server)