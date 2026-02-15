library(shiny)
library(tidyverse)
library(sf)
library(shinythemes)
library(viridis)

# --- DATEN LADEN & VORBEREITUNG ---
CA_meta    <- readRDS("data/plaice_app_data.rds") 
map_data   <- readRDS("data/map_data_all.rds")
ICES_light <- readRDS("data/ices_shape_light.rds") %>% st_transform(4326)

# Ordnung der Regionen festlegen (West -> Ost)
region_levels <- c("Skagerrak", "Kattegat", "Öresund", "Kieler Bucht", 
                   "Arkona-Becken", "Bornholm-Becken", "Östlich von Bornholm")
CA_meta$Region <- factor(CA_meta$Region, levels = region_levels)


lon_min <- 9.5; lon_max <- 21.0
lat_min <- 53.5; lat_max <- 58.5

# 2. UI (USER INTERFACE) --------------------------------------------------
ui <- navbarPage(
  title = "ConditionPlaice",
  theme = shinytheme("flatly"),
  
  header = tags$head(
    tags$style(HTML("
      .well { background-color: #ffffff !important; border-radius: 8px !important; box-shadow: 0 4px 6px rgba(0,0,0,0.05) !important; border: 1px solid #e3e6f0 !important; }
      .shiny-output-error { visibility: hidden; }
      .main-header { font-weight: bold; color: #2c3e50; border-bottom: 2px solid #3498db; margin-bottom: 15px; }
      .kpi-box { text-align: center; padding: 10px; border-right: 1px solid #eee; }
      .kpi-box:last-child { border-right: none; }
      .kpi-title { font-size: 0.8em; color: #7f8c8d; text-transform: uppercase; font-weight: bold; }
      .kpi-value { font-size: 1.5em; font-weight: bold; margin-top: 5px; }
    "))
  ),
  
  # --- TAB 1: DASHBOARD ---
  tabPanel("Dashboard",
           fluidPage(
             # Obere KPI-Leiste
             fluidRow(
               column(6, div(class="well", style="padding: 10px; text-align: center;", 
                             h5("Overall Condition (K)", style="margin:0; color: #7f8c8d;"), 
                             h3(textOutput("k_value"), style="margin:5px 0; color: #2c3e50; font-weight: bold;"))),
               column(6, div(class="well", style="padding: 10px; text-align: center;", 
                             h5("Individuen (n)", style="margin:0; color: #7f8c8d;"), 
                             h3(textOutput("sample_n"), style="margin:5px 0; color: #18bc9c; font-weight: bold;")))
             ),
             
             # Das 3-Spalten-Layout
             fluidRow(
               # 1. SPALTE: STEUERUNG
               column(2,
                      div(class="well",
                          h4("Filter", class="main-header"),
                          sliderInput("year_dash", "Zeitraum:", 1994, 2024, 2024, sep = ""),
                          checkboxGroupInput("qs_dash", "Quartale:", 
                                             choices = c("Q1 (Frühjahr)" = 1, "Q4 (Herbst)" = 4), selected = c(1, 4)),
                          checkboxGroupInput("groups_dash", "Biogruppen:", 
                                             choices = c("Mature Females" = "Female_Mature", 
                                                         "Males & Juveniles" = "Males_Juveniles_Other"), 
                                             selected = c("Female_Mature", "Males_Juveniles_Other")),
                          hr(),
                          downloadButton("download_map_pdf", "Karte als PDF", class = "btn-primary", style="width: 100%;"),
                          hr(),
                          p("Data Source:", style="font-weight: bold; margin-bottom: 2px; font-size: 0.85em;"),
                          p(
                            "ICES Database of Trawl Surveys (",
                            tags$a(href = "https://datras.ices.dk/WebServices/Webservices.aspx", 
                                   "DATRAS", target = "_blank", style = "color: #3498db; text-decoration: underline;"),
                            "), BITS Survey.", 
                            style="font-size: 0.75em; color: #7f8c8d;"
                          )
                      )
               ),
               
               # 2. SPALTE: KARTE
               column(7,
                      div(class="well",
                          h4("Räumliche Verteilung", class="main-header"),
                          plotOutput("ggplot_map", height = "650px")
                      )
               ),
               
               # 3. SPALTE: DETAILS
               column(3,
                      div(class="well",
                          h4("Details", class="main-header"),
                          tableOutput("stats_table_detailed"),
                          hr(),
                          helpText("Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor.")
                      )
               )
             ) # Ende fluidRow Dashboard
           ) # Ende fluidPage Dashboard
  ), # Ende tabPanel Dashboard
  
  # --- TAB 2: EXPLORATIVE STATISTIK ---
  # --- TAB 2: EXPLORATIVE STATISTIK (UI) ---
  tabPanel("Explorative Statistik",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               # Selektor für den Grafiktyp (Gilt für beide Unter-Tabs)
               h4("Analyse-Modus", class = "main-header"),
               selectInput("plot_type_exp", "Grafiktyp wählen:",
                           choices = c("Zeitlicher Trend (Linie)" = "line", 
                                       "Verteilung (Boxplot)" = "box",
                                       "Platzhalter" = "lw")),
               hr(),
               
               conditionalPanel(
                 condition = "input.explorativ_tabs == 'Overall Trend'",
                 h4("Filter", class = "main-header"),
                 sliderInput("year_range_overall", "Zeitraum:", 1994, 2024, c(1994, 2024), sep = ""),
                 checkboxGroupInput("qs_overall", "Quartale:", 
                                    choices = c("Q1 (Frühjahr)" = 1, "Q4 (Herbst)" = 4), selected = c(1, 4)),
                 checkboxGroupInput("groups_overall", "Biogruppen:", 
                                    choices = c("Mature Females" = "Female_Mature", "Males & Juveniles" = "Males_Juveniles_Other"),
                                    selected = c("Female_Mature", "Males_Juveniles_Other"))
               ),
               
               conditionalPanel(
                 condition = "input.explorativ_tabs == 'Regionale Analyse'",
                 h4("Filter", class = "main-header"),
                 checkboxGroupInput("regions_exp", "Regionen:", 
                                    choices = region_levels, 
                                    selected = c("Kieler Bucht", "Arkona-Becken", "Bornholm-Becken")),
                 checkboxGroupInput("qs_exp", "Quartale:", 
                                    choices = c("Q1 (Frühjahr)" = 1, "Q4 (Herbst)" = 4), selected = c(1, 4)),
                 checkboxGroupInput("groups_exp", "Biogruppen:", 
                                    choices = c("Mature Females" = "Female_Mature", "Males & Juveniles" = "Males_Juveniles_Other"),
                                    selected = c("Female_Mature", "Males_Juveniles_Other"))
               )
             ),
             
             mainPanel(
               width = 9,
               tabsetPanel(id = "explorativ_tabs",
                           tabPanel("Overall Trend", 
                                    uiOutput("stats_overall_bar"),
                                    div(class="well", plotOutput("plot_overall", height = "600px"))),
                           tabPanel("Regionale Analyse", 
                                    uiOutput("stats_regional_bar"),
                                    div(class="well", plotOutput("plot_regional", height = "750px")))
               )
             )
           )
  ),
  # --- TAB 3: TROPHISCHE INTERAKTIONEN & UMWELT ---
  tabPanel("Trophische Interaktionen",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               h4("Filter", class="main-header"),
               radioButtons("trophic_mode", "Modus wählen:",
                            choices = c("Dichte-Effekte (Karten)" = "density", 
                                        "Umwelt-Korrelationen" = "env")),
               hr(),
               sliderInput("year_trophic", "Zeitraum:", 1994, 2024, 2024, sep = ""),
               checkboxGroupInput("qs_trophic", "Quartale:", 
                                  choices = c("Q1" = 1, "Q4" = 4), selected = 1),
               helpText("Hinweis: In diesem Tab werden die Zusammenhänge zwischen Kondition, Bestandsdichte und Hydrographie untersucht.")
             ),
             mainPanel(
               width = 9,
               div(class="well",
                   conditionalPanel(
                     condition = "input.trophic_mode == 'density'",
                     fluidRow(
                       column(6, plotOutput("plot_plaice_density", height = "450px")),
                       column(6, plotOutput("plot_cod_density", height = "450px"))
                     )
                   ),
                   conditionalPanel(
                     condition = "input.trophic_mode == 'env'",
                     plotOutput("plot_env_corr", height = "500px"),
                     hr(),
                     plotOutput("plot_env_map", height = "400px")
                   )
               )
             )
           )
  ),
  
  # --- TAB 4: DOKUMENTATION ---
  tabPanel("Dokumentation",
           fluidPage(
             column(8, offset = 2, div(class="well",
                                       h3("Methodik & Datengrundlage", class="main-header"),
                                       h4("Fulton's Konditionsfaktor (K)"),
                                       p("Der Konditionsfaktor wird berechnet als:"),
                                       withMathJax("$$K = 100 \\cdot \\frac{W}{L^3}$$"),
                                       p("wobei W das Gewicht in Gramm und L die Länge in Zentimetern ist."),
                                       hr(),
                                       h4("Datenquelle"),
                                       p("Die Daten stammen aus dem ICES DATRAS Portal (BITS Survey). Der Zeitraum umfasst 1994 bis heute.")
             ))
           )
  ),
  
  # --- TAB 5: LITERATUR ---
  tabPanel("Literatur",
           fluidPage(
             column(8, offset = 2, div(class="well",
                                       h3("Referenzen", class="main-header"),
                                       tags$ul(
                                         tags$li("ICES. (2023). Baltic International Trawl Survey (BITS) Manual."),
                                         tags$li("Fulton, T. W. (1904). The Rate of Growth of Fishes."),
                                         tags$li("R Core Team (2026). R: A language and environment for statistical computing.")
                                       )
       ))
    )
  )
)

# 3. SERVER (LOGIK) -------------------------------------------------------
server <- function(input, output) {
  
  # --- 1. DASHBOARD LOGIK ---
  filtered_fish <- reactive({
    CA_meta %>% 
      filter(Year == input$year_dash,
             Quarter %in% input$qs_dash,
             BioGroup %in% input$groups_dash)
  })
  
  # KPI Ausgaben
  output$k_value <- renderText({
    res <- filtered_fish() %>% summarise(m = median(K_Fulton, na.rm = TRUE))
    sprintf("%.3f", res$m)
  })
  
  output$sample_n <- renderText({
    format(nrow(filtered_fish()), big.mark = ".")
  })
  
  # Exakte Tabelle (Einzelfisch-Basis) mit Styling
  output$stats_table_detailed <- renderTable({
    df <- filtered_fish()
    validate(need(nrow(df) > 0, "Keine Daten."))
    
    # Helfer-Funktion für schicke Überschriften
    style_header <- function(text) {
      paste0("<b style='color: #e74c3c; text-decoration: underline;'>", text, "</b>")
    }
    
    # 1. Mediane berechnen
    reg <- df %>% group_by(Region) %>% 
      summarise(K = median(K_Fulton, na.rm = TRUE), n = n()) %>% 
      rename(Kategorie = Region) %>%
      mutate(Kategorie = as.character(Kategorie))
    
    qua <- df %>% group_by(Quarter) %>% 
      summarise(K = median(K_Fulton, na.rm = TRUE), n = n()) %>% 
      mutate(Kategorie = paste("Quartal", Quarter)) %>% 
      select(-Quarter)
    
    bio <- df %>% group_by(BioGroup) %>% 
      summarise(K = median(K_Fulton, na.rm = TRUE), n = n()) %>% 
      rename(Kategorie = BioGroup)
    
    # 2. Zusammenfügen mit gestylten Headern
    bind_rows(
      tibble(Kategorie = style_header("REGION"), K = NA, n = NA), 
      reg,
      tibble(Kategorie = style_header("ZEITRAUM"), K = NA, n = NA), 
      qua,
      tibble(Kategorie = style_header("BIOGRUPPE"), K = NA, n = NA), 
      bio
    )
  }, 
  digits = 3, 
  na = "", 
  sanitize.text.function = function(x) x, # WICHTIG: Erlaubt HTML in der Tabelle
  include.rownames = FALSE,
  width = "100%")
  
  # Karte im Clean Style
  output$ggplot_map <- renderPlot({
    plot_data <- map_data %>% 
      filter(Year == input$year_dash,
             Quarter %in% input$qs_dash,
             BioGroup %in% input$groups_dash)
    
    ggplot() +
      # ICES Geometrie
      geom_sf(data = ICES_light, fill = "grey90", color = "grey40", linewidth = 0.3) +
      # Datenpunkte
      geom_point(data = plot_data,
                 aes(x = HaulLong, y = HaulLat, color = median_K),
                 size = 2, alpha = 0.8) +
      # SD-Label (dezent im Hintergrund)
      geom_sf_label(data = ICES_light, aes(label = SubDivisio), 
                    size = 3, alpha = 0.4, nudge_x = -0.1) +
      # Farbskala & Legende
      scale_color_viridis_c(option = "magma", direction = -1, name = "Median Condition (K)") +
      # Faceting mit sauberen Namen
           facet_grid(BioGroup ~ Quarter, labeller = labeller(
        Quarter = c(`1` = "Q1 (Frühjahr)", `4` = "Q4 (Herbst)"),
        BioGroup = c("Female_Mature" = "Mature Females", "Males_Juveniles_Other" = "Males & Juveniles")
      )) +
      coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +
      labs(title = paste("Spatial Distribution of Condition Factor (K) of European Plaice in the Baltic Sea,", input$year_dash),
           caption = "Source: ICES DATRAS Database (BITS Survey)",
           x = "Longitude", y = "Latitude") +
      
      # Styling
      theme_bw(base_size = 14) +
      theme(
        legend.position = "bottom",
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_rect(fill = "grey98"),
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "grey30"),
        legend.key.width = unit(1.5, "cm"),
        plot.caption = element_text(size = 9, color = "grey30", face = "italic")
      ) 
      
  })
 
  # --- 2. LOGIK FÜR EXPLORATIVE STATISTIK ---
  # --- EXPLORATIVE STATISTIK: KENNZAHLEN ---
  
  # Zentraler Filter für den Overall-Tab
  df_overall <- reactive({
    CA_meta %>% filter(
      Year >= input$year_range_overall[1], 
      Year <= input$year_range_overall[2],
      Quarter %in% input$qs_overall, 
      BioGroup %in% input$groups_overall
    )
  })
  
  output$stats_overall_bar <- renderUI({
    df <- CA_meta %>% filter(
      Year >= input$year_range_overall[1], 
      Year <= input$year_range_overall[2],
      Quarter %in% input$qs_overall, 
      BioGroup %in% input$groups_overall
    )
    
    if(nrow(df) < 5) return(div(class="well", "Nicht genügend Daten für Statistik."))
    
    # 1. Modell & Kennzahlen
    fit <- lm(K_Fulton ~ Year, data = df)
    summary_fit <- summary(fit)
    p_val <- summary_fit$coefficients[2,4]
    r2 <- summary_fit$r.squared
    
    # 2. Rückgang
    y_start <- min(df$Year); y_end <- max(df$Year)
    k_start <- median(df$K_Fulton[df$Year == y_start], na.rm = TRUE)
    k_end   <- median(df$K_Fulton[df$Year == y_end], na.rm = TRUE)
    percent_decline <- round(((k_end - k_start) / k_start) * 100, 1)
    
    # 3. p-Wert Formatierung
    p_text <- if(p_val < 0.001) {
      "Hoch signifikant (p < 0,001)"
    } else if(p_val < 0.01) {
      "Sehr signifikant (p < 0,01)"
    } else if(p_val < 0.05) {
      paste0("Signifikant (p = ", round(p_val, 3), ")")
    } else {
      paste0("Nicht signifikant (p = ", round(p_val, 2), ")")
    }
    
    div(class="well", style="padding: 15px; margin-bottom: 10px; border-left: 5px solid #3498db;",
        fluidRow(
          # Rückgang
          column(3, div(class="kpi-box", 
                        div(class="kpi-title", "Gesamt-Rückgang"), 
                        div(class="kpi-value", style="color:#c0392b; font-weight: bold;", 
                            paste0(percent_decline, " %")))),
          
          # p-Wert
          column(3, div(class="kpi-box", 
                        div(class="kpi-title", "Signifikanz"), 
                        div(class="kpi-value", style="font-size: 1.05em; padding-top:8px;", p_text))),
          
          # R^2 als Zahl
          column(3, div(class="kpi-box", 
                        div(class="kpi-title", "Modellgüte (R²)"), 
                        div(class="kpi-value", style="font-size: 1.1em; padding-top:8px;", 
                            round(r2, 3)))),
          
          # n
          column(3, div(class="kpi-box", 
                        div(class="kpi-title", "Datenbasis (n)"), 
                        div(class="kpi-value", style="color:#7f8c8d;", format(nrow(df), big.mark="."))))
        )
    )
  })
  
  output$stats_regional_bar <- renderUI({
    df <- CA_meta %>% filter(Region %in% input$regions_exp, Quarter %in% input$qs_exp, BioGroup %in% input$groups_exp)
    if(nrow(df) < 10) return(NULL)
    
    worst <- df %>% group_by(Region) %>% summarise(slope = coef(lm(K_Fulton ~ Year))[2]) %>% arrange(slope) %>% slice(1)
    
    div(class="well", style="padding: 10px; background-color: #fcf8e3 !important;",
        icon("info-circle"), paste(" Kritischster Trend aktuell in Region:", worst$Region))
  })
  
  # --- EXPLORATIVE STATISTIK: PLOTS ---
  
  output$plot_overall <- renderPlot({
    df <- df_overall() # Nutzt den reaktiven Filter von oben
    validate(need(nrow(df) > 0, "Keine Daten."))
    
    # --- LOGIK-WEICHE BASIEREND AUF DROP-DOWN ---
    if (input$plot_type_exp == "line") {
    
    # Statistik pro Feld berechnen
    y_start <- min(df$Year); y_end <- max(df$Year)
    k_start <- median(df$K_Fulton[df$Year == y_start], na.rm = TRUE)
    k_end   <- median(df$K_Fulton[df$Year == y_end], na.rm = TRUE)
    percent_decline <- round(((k_end - k_start) / k_start) * 100, 1)
    
    stat_labels <- df %>%
      group_by(BioGroup, Quarter) %>%
      do(mod = lm(K_Fulton ~ Year, data = .)) %>%
      mutate(
        r2 = round(summary(mod)$r.squared, 3),
        p_val = summary(mod)$coefficients[2,4],
        # Schicke Formatierung des p-Wertes
        p_label = if_else(p_val < 0.001, "p < 0.001", paste0("p = ", round(p_val, 3))),
        label = paste0("R² = ", r2, "\n", p_label)
      )
    
    #Plot: Overall Trend
    ggplot(df, aes(x = Year, y = K_Fulton, color = BioGroup)) +
      stat_summary(fun = median, geom = "line", linewidth = 1.2) +
      geom_smooth(method = "lm", color = "black", linetype = "dashed", linewidth = 0.6, se = FALSE) +
      
      # Hier kommen die Labels in die obere linke Ecke (x = -Inf, y = Inf)
      geom_text(data = stat_labels, 
                aes(x = -Inf, y = Inf, label = label),
                hjust = -0.1, vjust = 1.2, 
                inherit.aes = FALSE, 
                size = 4.5, 
                fontface = "bold",
                color = "grey20") +
      
      facet_grid(BioGroup ~ Quarter, labeller = labeller(
        Quarter = c(`1` = "Q1 (Frühjahr)", `4` = "Q4 (Herbst)"),
        BioGroup = c("Female_Mature" = "Females", "Males_Juveniles_Other" = "Males/Juv.")
      )) +
      scale_color_brewer(palette = "Set1") +
      # Styling
      theme_bw(base_size = 14) +
      theme(
        legend.position = "bottom",
        strip.background = element_rect(fill = "grey98"),
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "grey30"),
        plot.caption = element_text(size = 9, color = "grey30", face = "italic")
      ) +
        labs(
        title = "Detaillierte Trend-Statistik nach BioGruppen",
        subtitle = "Lineare Regression pro Quartal und Biogruppe",
        caption = "Source: ICES DATRAS Database (BITS Survey)",
        y = expression(bold("Median Condition (K)")),
        x = "Jahr"
      )
    
    } else if (input$plot_type_exp == "box") {
      
      # --- NEUER BOXPLOT MODUS ---
      ggplot(df, aes(x = factor(Year), y = K_Fulton, fill = BioGroup)) +
        geom_boxplot(outlier.alpha = 0.2, notch = TRUE) +
        facet_grid(BioGroup ~ Quarter, labeller = labeller(
          Quarter = c(`1` = "Q1 (Frühjahr)", `4` = "Q4 (Herbst)"),
          BioGroup = c("Female_Mature" = "Females", "Males_Juveniles_Other" = "Males/Juv.")
        )) +
        scale_fill_brewer(palette = "Set1") +
        theme_bw(base_size = 14) +
        theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Varianz der Kondition (Boxplots)", x = "Jahr", y = "Condition (K)")
      
    } else if (input$plot_type_exp == "lw") {
      
      # --- Platzhalter MODUS ---
   
    }
  })
    
    

  
  output$plot_regional <- renderPlot({
    # 1. Filterung
    df <- CA_meta %>% 
      filter(Region %in% input$regions_exp, 
             Quarter %in% input$qs_exp, 
             BioGroup %in% input$groups_exp, 
             Year >= 1994)
    
    validate(need(nrow(df) > 10, "Nicht genügend Daten für die regionale Statistik."))
    
    # 2. Statistik-Labels berechnen (analog zum Overall Design)
    stat_labels_reg <- df %>%
      group_by(Region, BioGroup) %>%
      do(mod = lm(K_Fulton ~ Year, data = .)) %>%
      mutate(
        r2 = round(summary(mod)$r.squared, 3),
        p_val = summary(mod)$coefficients[2,4],
        # Gleiche p-Wert Logik wie im Overall Tab
        p_label = if_else(p_val < 0.001, "p < 0.001", paste0("p = ", round(p_val, 3))),
        group_label = if_else(BioGroup == "Female_Mature", "Weibchen", "Män./Juv."),
        label_text = paste0(group_label, ": R² = ", r2, " | ", p_label)
      ) %>%
      group_by(Region) %>%
      summarise(final_label = paste(label_text, collapse = "\n"))
    
    # 3. Plotting: Fokus auf Regression (Linie fett, Daten dezent)
    ggplot(df, aes(x = Year, y = K_Fulton, color = BioGroup, fill = BioGroup)) +
      
      # 1. Datenpunkte (Mediane) dezent in den Hintergrund
      stat_summary(fun = median, geom = "point", size = 2, alpha = 0.4) +
      
      # 2. Regression Fett im Vordergrund (ohne SE für mehr Klarheit)
      # 'color = BioGroup' sorgt dafür, dass die Linien Rot/Blau sind (Set1)
      geom_smooth(method = "lm", linewidth = 1.5, se = FALSE) + 
      
      # 3. Statistik-Labels (Bleiben im Design identisch zum Overall Trend)
      geom_text(data = stat_labels_reg, 
                aes(x = -Inf, y = Inf, label = final_label),
                hjust = -0.05, vjust = 1.2, 
                inherit.aes = FALSE, 
                size = 4, 
                fontface = "bold", 
                color = "grey20", 
                lineheight = 1.1) +
      
      # Layout & Faceting
      facet_wrap(~Region, ncol = 3) + 
      coord_cartesian(ylim = c(0.75, 1.25)) +
      
      # Farben: Set1 liefert in der Regel Blau für die erste und Rot für die zweite Gruppe
      scale_color_brewer(palette = "Set1", labels = c("Female_Mature" = "Females", "Males_Juveniles_Other" = "Males/Juv.")) +
      scale_fill_brewer(palette = "Set1", labels = c("Female_Mature" = "Females", "Males_Juveniles_Other" = "Males/Juv.")) +
      
      # Styling
      theme_bw(base_size = 14) +
      theme(
        legend.position = "bottom",
        strip.background = element_rect(fill = "grey98"),
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "grey30"),
        plot.caption = element_text(size = 9, color = "grey30", face = "italic")
      ) +
      labs(
        title = "Regionale Trend-Statistik nach BioGruppen",
        subtitle = "Lineare Regression pro Subdivision (1994 - 2024)",
        caption = "Source: ICES DATRAS Database (BITS Survey)",
        x = "Jahr",
        y = expression(bold("Median Condition (K)")),
        color = "BioGroup:", fill = "BioGroup:"
      )
  })
  
  # --- 3. LOGIK FÜR Trophik-Date ---
  # Filter für Trophik-Daten
  trophic_data <- reactive({
    map_data %>% filter(Year == input$year_trophic, Quarter %in% input$qs_trophic)
  })
  
  # Karte 1: Schollen-Dichte
  output$plot_plaice_density <- renderPlot({
    ggplot(trophic_data()) +
      geom_sf(data = ICES_light, fill = "grey95", inherit.aes = FALSE) +
      geom_point(aes(x = HaulLong, y = HaulLat, color = median_K, size = cpue_plaice), alpha = 0.7) +
      scale_color_viridis_c(option = "magma", direction = -1, name = "Fulton-K") +
      scale_size_continuous(name = "Schollen-Dichte") +
      coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +
      theme_minimal() + labs(title = "Kondition vs. Schollen-Dichte")
  })
  
  # Karte 2: Dorsch-Dichte
  output$plot_cod_density <- renderPlot({
    ggplot(trophic_data()) +
      geom_sf(data = ICES_light, fill = "grey95", inherit.aes = FALSE) +
      geom_point(aes(x = HaulLong, y = HaulLat, color = median_K, size = cpue_cod_large), alpha = 0.7) +
      scale_color_viridis_c(option = "magma", direction = -1, name = "Fulton-K") +
      scale_size_continuous(name = "Dorsch-Dichte (Large)") +
      coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +
      theme_minimal() + labs(title = "Kondition vs. Dorsch-Dichte")
  })
  
  # Korrelations-Plot (Salzgehalt)
  output$plot_env_corr <- renderPlot({
    ggplot(trophic_data(), aes(x = sal_bottom, y = median_K)) +
      geom_point(aes(color = temp_bottom, size = n_plaice_measured), alpha = 0.6) +
      geom_smooth(method = "lm", color = "black", linetype = "dashed") +
      scale_color_viridis_c(option = "plasma", name = "Temp (°C)") +
      facet_wrap(~BioGroup) +
      theme_bw() + labs(title = "Einfluss des Salzgehalts auf die Kondition", x = "Salzgehalt (PSU)", y = "Median Fulton-K")
  })
  
  # Umwelt-Karte (Temperatur/Tiefe)
  output$plot_env_map <- renderPlot({
    ggplot(trophic_data()) +
      geom_sf(data = ICES_light, fill = "grey95", color = "grey70", inherit.aes = FALSE) +
      geom_point(aes(x = HaulLong, y = HaulLat, color = temp_bottom, size = depth), alpha = 0.7) +
      scale_color_viridis_c(option = "viridis", name = "Boden-Temp (°C)") +
      coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +
      theme_minimal() + labs(title = "Hydrographische Bedingungen")
  })
  
  
}

shinyApp(ui, server)