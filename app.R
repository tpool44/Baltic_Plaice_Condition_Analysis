# LIBRARIES
library(shiny)
library(tidyverse)
library(sf)
library(shinythemes)
library(viridis)
library(DT)   
library(broom)
library(mgcv)      
library(gratia)    
library(patchwork) 

# GLOBALE OPTIONEN 
options(OutDec = ",")                # Komma als Dezimaltrenner
options(timeout = 600)               # Erhöht Timeout für Uploads (10 Min statt 1 Min)
options(rsconnect.http.timeout = 60) # Speziell für shinyapps.io Uploads

# --- DATEN LADEN & VORBEREITUNG ---
data_ind <- readRDS("output/plaice_individual_data.rds")
data_haul <- readRDS("output/plaice_haul_aggregated.rds")
ices_shape <- readRDS("output/ices_shape_light.rds")

# Ordnung der Regionen festlegen (West -> Ost)
region_levels <- c(  "Skagerrak", "Kattegat", "Beltsee & Kieler Bucht", "Öresund",
                     "Arkona-Becken", "Bornholm-Becken", "Südöstliche Ostsee")

data_ind$Region <- factor(data_ind$Region, levels = region_levels)

lon_min <- 9.5; lon_max <- 21.0
lat_min <- 53.5; lat_max <- 58.5

style_gam_plot <- function(p, title, xlab) {
  p + 
    labs(title = title, x = xlab, y = "Partieller Effekt auf K") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(size = 12)
    )
}

# 2. UI (USER INTERFACE) --------------------------------------------------
ui <- navbarPage(
  title = tags$a(
    href = "#",
    onclick = "if(typeof(nav) !== 'undefined') { $('.navbar-nav a[data-value=\"Dashboard\"]').click(); }",
    style = "text-decoration: none; color: inherit; cursor: pointer; display: flex; align-items: center; height: auto; padding: 0;",
    
    tags$img(
      src = "logo.png", 
      style = "height: 38px; width: auto; margin-right: 12px; border-radius: 50%; vertical-align: middle;"
    ), 
    
    div(
      style = "display: flex; flex-direction: column; justify-content: center; line-height: 1.2;",
      div( # Container für Name + Beta in einer Zeile
        style = "display: flex; align-items: center;",
        span("ConditionPlaice", 
             style = "font-size: 17px; font-weight: bold; white-space: nowrap;"),
        span("BETA", 
             style = "font-size: 9px; background-color: #f39c12; color: white; padding: 1px 5px; 
                border-radius: 3px; margin-left: 8px; font-weight: 800; letter-spacing: 0.5px;")
      ),
      tags$small(
        style = "font-size: 10px; color: #bdc3c7; font-weight: normal; margin-top: 2px;", 
        "powered by Flatfish Lab"
      )
    )
  ),
  id = "nav",
  theme = shinytheme("flatly"),
  
  header = tags$head(
    tags$style(HTML("
      /* --- HEADER & NAVBAR FIXES --- */
      .navbar { min-height: 65px !important; }
      
      .navbar-brand { 
        height: 65px !important; 
        display: flex !important; 
        align-items: center !important; 
        padding-top: 0 !important; 
        padding-bottom: 0 !important; 
      }
      
      /* Tabs ebenfalls vertikal mittig ausrichten */
      .navbar-nav > li > a { 
        padding-top: 22px !important; 
        padding-bottom: 22px !important; 
        line-height: 21px !important;
      }

      /* --- GITHUB CORNER FIX (Wieder sichtbar machen) --- */
      .github-corner { 
        position: absolute; 
        top: 0;                /* Basis oben */
        right: 0; 
        height: 65px;          /* Gleiche Höhe wie Navbar */
        display: flex !important; 
        align-items: center !important; 
        z-index: 9999 !important; /* Erzwingt Sichtbarkeit über allen Layern */
        padding-right: 25px; 
      }
      
      .github-corner a { 
        color: #ecf0f1 !important; 
        text-decoration: none !important; 
        font-weight: 500;
        font-size: 14px;
        display: flex;
        align-items: center;
      }
      
      .github-corner a:hover { color: #3498db !important; }

      /* --- DEIN BESTEHENDES STYLING --- */
      .well { background-color: #ffffff !important; border-radius: 8px !important; 
              box-shadow: 0 4px 6px rgba(0,0,0,0.05) !important; border: 1px solid #e3e6f0 !important; }
      
      .main-header { font-weight: bold; color: #2c3e50; border-bottom: 2px solid #3498db; margin-bottom: 15px; }
      
      .shiny-output-error { visibility: hidden; }
      
      .kpi-box { text-align: center; padding: 10px; border-right: 1px solid #eee; }
      .kpi-box:last-child { border-right: none; }
      .kpi-title { font-size: 0.85em; color: #7f8c8d; text-transform: uppercase; font-weight: bold; }
      .kpi-value { font-size: 1.5em; font-weight: bold; color: #2c3e50; margin-top: 5px; }
      
      .table-header-styled { color: #e74c3c; text-decoration: underline; font-weight: bold; }
    ")),
    
    # Der Container für den GitHub-Link
    # GitHub Link Container mit Leerzeichen-Fix
    tags$div(class = "github-corner",
             tags$a(href = "https://github.com/tpool44/Baltic_Plaice_Condition_Analysis", 
                    target = "_blank", 
                    icon("github"), 
                    HTML("&nbsp;"), "GitHub")) # HTML("&nbsp;") fügt das Leerzeichen ein
  ),
  # --- TAB 1: DASHBOARD ---
  tabPanel("Dashboard", icon = icon("map"),
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
             
             # 3-Spalten-Layout
             fluidRow(
               # 1. SPALTE: EINSTELLUNGEN
               column(2,
                      div(class="well",
                          h4("Einstellungen", class="main-header"),
                          selectInput("year_dash", "Jahr auswählen:", 
                                      choices = sort(unique(data_ind$Year), decreasing = TRUE), 
                                      selected = 2024),
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
                            tags$a(href = "https://www.ices.dk/data/data-portals/Pages/DATRAS.aspx", 
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
                          p("Der Fulton-Konditionsfaktor (K) dient in der Fischereibiologie als Proxy für den Ernährungszustand oder für das allgemeine Wohlbefinden eines Fisches.", 
                            style="font-size: 0.9em;"),
                          tags$ul(style="font-size: 0.85em; color: #7f8c8d;",
                                  tags$li("Werte > 1.2: Sehr gute Kondition"),
                                  tags$li("Werte ≈ 1.0: Normale (gute) Kondition")),
                      )
               )
             )
           )
  ),
  
  # --- TAB 2: ANALYSE & STATISTIK ---
  navbarMenu("Analyse & Statistik", icon = icon("chart-line"),
             
             # --- UNTERSEITE 1: ZEITLICHE TRENDS ---
             tabPanel("Zeitliche Trends",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     h4("Einstellungen", class="main-header"),
                                     selectInput("plot_type_overall", "Grafiktyp auswählen:", 
                                                 choices = c("Trendlinie" = "line", "Boxplots" = "box")),
                                     hr(),
                                     sliderInput("year_range_overall", "Zeitraum auswählen:", 1994, 2024, c(1994, 2024), sep = ""),
                                     checkboxGroupInput("qs_overall", "Quartale:", 
                                                        choices = c("Q1 (Frühjahr)" = 1, "Q4 (Herbst)" = 4), selected = c(1, 4)),
                                     checkboxGroupInput("groups_overall", "Biogruppen:", 
                                                        choices = c("Mature Females" = "Female_Mature", 
                                                                    "Males & Juveniles" = "Males_Juveniles_Other"), 
                                                        selected = c("Female_Mature", "Males_Juveniles_Other")),
                                     hr(),
                                     p("Data Source:", style="font-weight: bold; margin-bottom: 2px; font-size: 0.85em;"),
                                     p(
                                       "ICES Database of Trawl Surveys (",
                                       tags$a(href = "https://www.ices.dk/data/data-portals/Pages/DATRAS.aspx", 
                                              "DATRAS", target = "_blank", style = "color: #3498db; text-decoration: underline;"),
                                       "), BITS Survey.", 
                                       style="font-size: 0.75em; color: #7f8c8d;")
                        ),
                        mainPanel(width = 9, 
                                  uiOutput("stats_overall_bar"),
                                  div(class="well", plotOutput("plot_overall", height = "650px")))
                      )
             ),
             
             # --- UNTERSEITE 2: REGIONALE ANALYSE ---
             tabPanel("Regionale Analyse",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     h4("Einstellungen", class="main-header"),
                                     selectInput("plot_type_regional", "Darstellungsform:", 
                                                 choices = c("Trendlinie" = "line", "Boxplots" = "box")),
                                     hr(),
                                     checkboxGroupInput("regions_exp", "Regionen wählen:", 
                                                        choices = region_levels, selected = region_levels[3:5]), 
                                     hr(),
                                     checkboxGroupInput("qs_exp", "Quartale:", 
                                                        choices = c("Q1 (Frühjahr)" = 1, "Q4 (Herbst)" = 4), selected = c(1, 4)),
                                     checkboxGroupInput("groups_exp", "Biogruppen:", 
                                                        choices = c("Mature Females" = "Female_Mature", 
                                                                    "Males & Juveniles" = "Males_Juveniles_Other"), 
                                                        selected = c("Female_Mature", "Males_Juveniles_Other")),
                                     hr(),
                                     p("Data Source:", style="font-weight: bold; margin-bottom: 2px; font-size: 0.85em;"),
                                     p(
                                       "ICES Database of Trawl Surveys (",
                                       tags$a(href = "https://www.ices.dk/data/data-portals/Pages/DATRAS.aspx", 
                                              "DATRAS", target = "_blank", style = "color: #3498db; text-decoration: underline;"),
                                       "), BITS Survey.", 
                                       style="font-size: 0.75em; color: #7f8c8d;")
                        ),
                        mainPanel(width = 9, 
                                  uiOutput("stats_regional_bar"),
                                  tabsetPanel(type = "tabs",
                                              tabPanel(title = "Grafik", icon = icon("chart-line"),
                                                       br(), div(class="well", plotOutput("plot_regional", height = "750px"))),
                                              tabPanel(title = "Statistik-Tabelle", icon = icon("table"),
                                                       br(), div(class="well", h4("Detaillierte Regressionsparameter"), DT::DTOutput("table_regional_stats")))
                                  )
                        )
                      )
             ),
             
             # --- UNTERSEITE 3: UMWELT- & BIOPARAMETER ---
             tabPanel("Umwelt- & Bioparameter",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     h4("Einstellungen", class="main-header"),
                                     radioButtons("env_view", "Modell auswählen:", 
                                                  choices = c("Korrelations-Matrix" = "corr", 
                                                              "GAM-Modell" = "gam")),
                                     hr(),
                                     checkboxGroupInput("gam_variables", "Parameter auswählen:", 
                                                        choices = c("Bodentemperatur" = "temp_bottom", 
                                                                    "Salinität" = "sal_bottom", 
                                                                    "Wassertiefe" = "depth",
                                                                    "Schollendichte (log)" = "log_cpue_plaice",
                                                                    "Dorschdichte (log)" = "log_cpue_cod"), 
                                                        selected = c("temp_bottom", "sal_bottom","log_cpue_plaice","log_cpue_cod")),
                                     hr(),
                                     p("Data Source:", style="font-weight: bold; margin-bottom: 2px; font-size: 0.85em;"),
                                     p(
                                       "ICES Database of Trawl Surveys (",
                                       tags$a(href = "https://www.ices.dk/data/data-portals/Pages/DATRAS.aspx", 
                                              "DATRAS", target = "_blank", style = "color: #3498db; text-decoration: underline;"),
                                       "), BITS Survey.", 
                                       style="font-size: 0.75em; color: #7f8c8d;"),
                        ), 
                        mainPanel(width = 9,
                                  uiOutput("stats_env_info"), 
                                  
                                  # Tab-System für Ergebnisse
                                  tabsetPanel(
                                    id = "env_results_tabs",
                                    type = "pills",
                                    
                                    # TAB 1: Grafik
                                    tabPanel("Visualisierung", icon = icon("chart-area"),
                                             br(),
                                             conditionalPanel(
                                               condition = "input.env_view == 'corr'",
                                               div(class="well", plotOutput("plot_corr_matrix", height = "600px"))
                                             ),
                                             conditionalPanel(
                                               condition = "input.env_view == 'gam'",
                                               div(class="well", plotOutput("plot_gam_effects", height = "650px"))
                                             )
                                    ),
                                    
                                    # TAB 2: Statistik-Details
                                    tabPanel("Modell-Details", icon = icon("microscope"),
                                             br(),
                                             conditionalPanel(
                                               condition = "input.env_view == 'gam'",
                                               div(class="well",
                                                   h4("Statistische Modellparameter (GAM)"),
                                                   DT::DTOutput("table_gam_results"),
                                                   hr(),
                                                   tags$small(
                                                     p(strong("Erklärung der Parameter:")),
                                                     tags$ul(
                                                       tags$li(strong("edf (effective degrees of freedom):"), "1 = linear, >1 = nicht-linear/kurvig."),
                                                       tags$li(strong("p-Wert:"), "Signifikanz des jeweiligen Umweltfaktors.")
                                                     )
                                                   )
                                               )
                                             ),
                                             conditionalPanel(
                                               condition = "input.env_view == 'corr'",
                                               div(class="well", p("Koeffizienten siehe 'Visualisierung'."))
                                             )
                                    ) 
                                  ) 
                        ) 
                      ) 
             )
  ),
  
  # --- TAB 3: DOKUMENTATION ---
  tabPanel("Dokumentation", icon = icon("book"),
           fluidPage(
             column(12, tags$iframe(src = "Dokumentation.html", style = "width: 100%; height: 85vh; border: none;"))
           )
  ),
  
  # --- FOOTER ---
  footer = tags$footer(
    style = "position: fixed; bottom: 0; width: 100%; height: 20px; background: #f8f9fa; font-size: 10px; border-top: 1px solid #e7e7e7; display: flex; align-items: center; justify-content: center; z-index: 1000;",
    div(HTML("&copy; 2026 Flatfish Lab &bull; Marine Ecology Data Science &bull; <strong>Version 0.9.2-beta</strong>"))
  )
)

# 3. SERVER (LOGIK) -------------------------------------------------------
server <- function(input, output) {
  
  # --- 1. DASHBOARD LOGIK ---
  filtered_fish <- reactive({
    data_ind %>% 
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
  
  # Tabelle (Einzelfisch-Basis) mit Styling
  output$stats_table_detailed <- renderTable({
    df <- filtered_fish()
    validate(need(nrow(df) > 0, "Keine Daten."))
    
    # Helfer-Funktion für Überschriften
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
  sanitize.text.function = function(x) x, # Erlaubt HTML in der Tabelle
  include.rownames = FALSE,
  width = "100%")
  
  # Karte
  output$ggplot_map <- renderPlot({
    plot_data <- data_haul %>% 
      filter(Year == input$year_dash,
             Quarter %in% input$qs_dash,
             BioGroup %in% input$groups_dash)
    
    ggplot() +
      # ICES Geometrie
      geom_sf(data = ices_shape, fill = "grey90", color = "grey40", linewidth = 0.3) +
      # Datenpunkte
      geom_point(data = plot_data,
                 aes(x = HaulLong, y = HaulLat, color = median_K),
                 size = 2, alpha = 0.8) +
      # SD-Label (dezent im Hintergrund)
      geom_sf_label(data = ices_shape, aes(label = SubDivisio), 
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
 
  
  # --- 2. ANALYSE LOGIK: ZEITLICHE TRENDS ---
  # 1. Reaktiver Filter
  df_overall <- reactive({
    validate(need(input$year_range_overall, "Lade Zeitraum..."))
    data_ind %>% filter(
      Year >= input$year_range_overall[1], 
      Year <= input$year_range_overall[2],
      Quarter %in% input$qs_overall, 
      BioGroup %in% input$groups_overall
    )
  })
  
  # 2. KPI-Leiste
  output$stats_overall_bar <- renderUI({
    df <- df_overall()
    if(nrow(df) < 10) return(div(class="well", "Nicht genügend Daten."))
    
    sel_min <- input$year_range_overall[1] 
    sel_max <- input$year_range_overall[2]
    
    # PRÜFUNG: Sind alle Quartale und alle Gruppen ausgewählt?
    all_quarters <- length(input$qs_overall) == 2
    all_groups   <- length(input$groups_overall) == 2
    
    if (all_quarters && all_groups) {
      # --- SONDERFALL: GLOBALER BERICHTS-MODUS ---
      # Ein einziges Modell über alle Daten (wie im Bericht)
      global_mod <- lm(K_Fulton ~ Year, data = df)
      
      k_pred_start <- predict(global_mod, newdata = data.frame(Year = sel_min))
      k_pred_end   <- predict(global_mod, newdata = data.frame(Year = sel_max))
      
      final_decline <- round(((k_pred_end - k_pred_start) / k_pred_start) * 100, 1)
      
      # Statistiken für die weiteren Boxen
      summary_fit <- summary(global_mod)
      r2    <- summary_fit$r.squared
      p_val <- summary_fit$coefficients[2,4]
      kpi_label <- "Globaler Modell-Trend"
      
    } else {
      # --- STANDARD: GRUPPEN-DURCHSCHNITT ---
      summary_stats <- df %>%
        group_by(BioGroup, Quarter) %>%
        summarise(
          fit = list(lm(K_Fulton ~ Year)),
          k_pred_start = predict(fit[[1]], newdata = data.frame(Year = sel_min)),
          k_pred_end   = predict(fit[[1]], newdata = data.frame(Year = sel_max)),
          .groups = "drop"
        ) %>%
        mutate(decline_pct = ((k_pred_end - k_pred_start) / k_pred_start) * 100)
      
      final_decline <- round(mean(summary_stats$decline_pct, na.rm = TRUE), 1)
      
      # Für R2/p-Wert das globale Modell des Filters
      global_fit  <- summary(lm(K_Fulton ~ Year, data = df))
      r2    <- global_fit$r.squared
      p_val <- global_fit$coefficients[2,4]
      kpi_label <- "Ø Modell-Trend"
    }
    
    # UI Logik für Farbe und Text
    p_text <- if(p_val < 0.001) "Hoch signifikant" else if(p_val < 0.05) "Signifikant" else "Nicht signifikant"
    trend_color <- if(final_decline <= 0) "#e74c3c" else "#27ae60"
    
    div(class="well", style="padding: 15px; border-left: 5px solid #3498db;",
        fluidRow(
          column(3, div(class="kpi-box", 
                        div(class="kpi-title", kpi_label), 
                        div(class="kpi-value", style=paste0("color:", trend_color), paste0(final_decline, "%")))),
          column(3, div(class="kpi-box", div(class="kpi-title", "Signifikanz"), div(class="kpi-value", p_text))),
          column(3, div(class="kpi-box", div(class="kpi-title", "Modellgüte (R²)"), div(class="kpi-value", round(r2, 2)))),
          column(3, div(class="kpi-box", div(class="kpi-title", "Individuen n"), div(class="kpi-value", nrow(df))))
        ))
  })
  
  # 3. Plot
  output$plot_overall <- renderPlot({
    df <- df_overall()
    validate(need(nrow(df) > 0, "Keine Daten für diese Auswahl."))
    
    if (input$plot_type_overall == "line") {
      
      # Berechnung der Labels für die Facets
      stat_labels <- df %>%
        group_by(BioGroup, Quarter) %>%
        summarise(
          r2 = round(summary(lm(K_Fulton ~ Year))$r.squared, 2),
          p_val = summary(lm(K_Fulton ~ Year))$coefficients[2,4],
          .groups = 'drop'
        ) %>%
        mutate(
          p_label = if_else(p_val < 0.001, "p < 0.001", paste0("p = ", round(p_val, 3))),
          label = paste0("R² = ", r2, "\n", p_label)
        )
      
      ggplot(df, aes(x = Year, y = K_Fulton, color = BioGroup)) +
        stat_summary(fun = median, geom = "line", linewidth = 1.2) +
        geom_smooth(method = "lm", color = "black", linetype = "dashed", linewidth = 0.6, se = FALSE) +
        geom_text(data = stat_labels, 
                  aes(x = -Inf, y = Inf, label = label),
                  hjust = -0.1, vjust = 1.2, inherit.aes = FALSE, 
                  size = 4.5, fontface = "bold", color = "grey20") +
        facet_grid(BioGroup ~ Quarter, labeller = labeller(
          Quarter = c(`1` = "Q1 (Frühjahr)", `4` = "Q4 (Herbst)"),
          BioGroup = c("Female_Mature" = "Females", "Males_Juveniles_Other" = "Males/Juv.")
        )) +
        scale_color_brewer(palette = "Set1") +
        theme_bw(base_size = 14) +
        theme(legend.position = "bottom", strip.text = element_text(face = "bold")) +
        labs(title = "Detaillierte Trend-Statistik", y = "Median Kondition (K)", x = "Jahr")
      
    } else {
      # PLATZHALTER FÜR BOXPLOTS
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Boxplots: Coming Soon\n\nDiese Funktion wird derzeit überarbeitet.",
                 size = 6, fontface = "italic", color = "#7f8c8d") +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "#fdfdfd", color = "#e3e6f0"),
          panel.border = element_blank()
        )
    }
  })
  
  # --- 3. ANALYSE LOGIK: REGIONALE TRENDS ---
  
  # Reaktiver Filter für Regionen-Tab
  df_regional <- reactive({
    validate(need(input$regions_exp, "Bitte wählen Sie mindestens eine Region aus."))
    data_ind %>% filter(
      Region %in% input$regions_exp, 
      Quarter %in% input$qs_exp, 
      BioGroup %in% input$groups_exp,
      Year >= 1994
    )
  })
  
  # Regionaler Plot
  output$plot_regional <- renderPlot({
    df <- df_regional()
    validate(need(nrow(df) > 10, "Nicht genügend Daten für die regionale Statistik."))
    
    # Statistik-Labels für Regionen berechnen
    stat_labels_reg <- df %>%
      group_by(Region, BioGroup) %>%
      summarise(
        r2 = round(summary(lm(K_Fulton ~ Year))$r.squared, 3),
        p_val = summary(lm(K_Fulton ~ Year))$coefficients[2,4],
        .groups = 'drop'
      ) %>%
      mutate(
        group_label = if_else(BioGroup == "Female_Mature", "Mature Females", "Males/Juv."),
        p_label = if_else(p_val < 0.001, "p < 0.001", paste0("p = ", round(p_val, 3))),
        label_text = paste0(group_label, ": R²=", r2, " | ", p_label)
      ) %>%
      group_by(Region) %>%
      summarise(final_label = paste(label_text, collapse = "\n"))
    
    if (input$plot_type_regional == "line") {
      ggplot(df, aes(x = Year, y = K_Fulton, color = BioGroup)) +
        stat_summary(fun = median, geom = "point", size = 2, alpha = 0.4) +
        geom_smooth(method = "lm", linewidth = 1.2, se = FALSE) + 
        geom_text(data = stat_labels_reg, 
                  aes(x = -Inf, y = -Inf, label = final_label),
                  hjust = -0.05, vjust = -0.5, 
                  inherit.aes = FALSE, 
                  size = 3.5, fontface = "bold", color = "grey20") +
        facet_wrap(~Region, ncol = 3) + 
        scale_color_brewer(palette = "Set1", 
                           labels = c("Female_Mature" = "Mature Females", 
                                      "Males_Juveniles_Other" = "Males/Juv.")) +
        theme_bw(base_size = 14) +
        theme(legend.position = "bottom", strip.text = element_text(face = "bold")) +
        labs(title = "Regionale Trends im Vergleich", 
             x = "Jahr", 
             y = "Median Kondition (K)",
             color = "Biogruppe")
    } else {
      # PLATZHALTER FÜR BOXPLOTS
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Boxplots: Coming Soon\n\nDiese Funktion wird derzeit überarbeitet.",
                 size = 6, fontface = "italic", color = "#7f8c8d") +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "#fdfdfd", color = "#e3e6f0"),
          panel.border = element_blank()
        )
    }
  })
  # 4. Statistik-Tabelle
  output$table_regional_stats <- renderDT({
    df <- df_regional()
    validate(need(nrow(df) > 0, "Nicht genügend Daten."))
    
    tab_data <- df %>%
      group_by(Region, BioGroup) %>%
      summarise(
        n = n(),
        mod = list(lm(K_Fulton ~ Year)),
        slope = round(coef(mod[[1]])[2], 5),
        r2 = round(summary(mod[[1]])$r.squared, 3),
        p_val_raw = summary(mod[[1]])$coefficients[2,4],
        k_pred_start = predict(mod[[1]], newdata = data.frame(Year = min(df$Year))),
        k_pred_end   = predict(mod[[1]], newdata = data.frame(Year = max(df$Year))),
        decline_pct = round(((k_pred_end - k_pred_start) / k_pred_start) * 100, 1),
        k_max = round(max(aggregate(K_Fulton ~ Year, data = pick(everything()), median)$K_Fulton), 2),
        k_min = round(min(aggregate(K_Fulton ~ Year, data = pick(everything()), median)$K_Fulton), 2),
        .groups = "drop"
      ) %>%
      mutate(
        BioGroup = if_else(BioGroup == "Female_Mature", "Mature Females", "Males/Juv."),
        p_val = if_else(p_val_raw < 0.001, "< 0.001", as.character(round(p_val_raw, 3)))
      ) %>%
      select(Region, BioGroup, n, slope, r2, p_val, decline_pct, k_max, k_min)

    datatable(tab_data, 
              colnames = c("Region", "Gruppe", "n", "Slope", "R²", "p-Wert", "% Rückg.", "Max K", "Min K"),
              options = list(
                pageLength = -1,  # -1 zeigt ALLE Zeilen an
                dom = 't',        # Zeigt nur die Tabelle ('t'), keine Suche/Pagination-Buttons
                order = list(list(6, 'asc')), 
                autoWidth = TRUE
              ),
              selection = 'none',
              rownames = FALSE) %>%
      formatStyle(
        'decline_pct',
        backgroundColor = styleInterval(0, c('#d9534f', '#5cb85c')),
        color = 'white',
        fontWeight = 'bold'
      )
  })
  
  # --- 4. UMWELT-LOGIK --------------------------------------------
  
  # 4.1 Reaktiver Datensatz für Umwelt
  df_env_fixed <- reactive({
    data_haul %>%
      filter(!is.na(temp_bottom), !is.na(sal_bottom), !is.na(median_K)) %>% 
      mutate(
        log_cpue_plaice = log10(cpue_plaice + 1),
        log_cpue_cod = log10(cpue_cod_large + 1)
      )
  })
  
  # 4.2 Korrelationsmatrix Plot
  output$plot_corr_matrix <- renderPlot({
    # Nutzt den fixierten Datensatz ohne Zeitfilter
    dat_raw <- df_env_fixed()
    
    # Check, ob Daten da sind
    validate(
      need(nrow(dat_raw) > 10, "Nicht genügend Datenpunkte vorhanden.")
    )
    
    # 1. Definition, welche Spalte korreliert werden soll
    target_vars <- c("median_K", input$gam_variables)
    
    # 2. Daten auswählen und umbenennen für die Grafik
    nice_names <- c(
      "median_K"        = "Kondition (K)",
      "temp_bottom"     = "Temp (°C)",
      "sal_bottom"      = "Salz (PSU)",
      "depth"           = "Tiefe (m)",
      "log_cpue_plaice" = "Dichte Scholle (log)",
      "log_cpue_cod"    = "Dichte Dorsch (log)"
    )
    
    # Nur die Spalten wählen, die auch existieren und vom User gewählt wurden
    dat_sel <- dat_raw %>%
      select(all_of(target_vars)) %>%
      drop_na() # Korrelation braucht vollständige Paare
    
    # Spaltennamen für den Plot übersetzen
    colnames(dat_sel) <- nice_names[colnames(dat_sel)]
    
    # 3. Korrelation berechnen (Spearman)
    cor_matrix <- cor(dat_sel, method = "spearman")
    
    # 4. Plotten
    col_palette <- colorRampPalette(c("#B2182B", "#F7F7F7", "#2166AC"))(200)
    
    corrplot::corrplot(
      cor_matrix, 
      method = "color", 
      type = "upper", 
      addCoef.col = "black", 
      number.cex = 0.9, 
      tl.col = "black",
      tl.srt = 45,    
      col = col_palette, 
      diag = FALSE, 
      mar = c(1,1,1,1)
    )
  })
  
  # 4.3 GAM Modellierung & Plot
  output$plot_gam_effects <- renderPlot({
    dat <- df_env_fixed()
    vars <- input$gam_variables
    
    validate(
      need(length(vars) > 0, "Bitte wählen Sie mindestens einen Parameter aus."),
      need(nrow(dat) > 50, "Nicht genügend Daten vorhanden.")
    )
    
    # 1. Schöne Namen definieren (wie in der Tabelle)
    var_names <- c(
      "temp_bottom"     = "Bodentemperatur (°C)",
      "log_cpue_plaice" = "Schollendichte [log10(CPUE+1)]",
      "sal_bottom"      = "Salinität (PSU)",
      "log_cpue_cod"    = "Dorschdichte [log10(CPUE+1)]",
      "depth"           = "Wassertiefe (m)"
    )
    
    # 2. Modell berechnen 
    formula_str <- paste("median_K ~ BioGroup +", paste0("s(", vars, ", k=10)", collapse = " + "))
    model <- mgcv::gam(as.formula(formula_str), data = dat)
    
    # 3. Einzel-Plots extrahieren
    # gratia::draw() gibt bei mehreren Termen eine Liste von ggplots zurück
    raw_plots <- gratia::draw(model, parametric = FALSE)
    
    # 4. x-Achsen der Einzelplots dynamisch umbenennen
    for(i in seq_along(vars)) {
      current_var <- vars[i]
      raw_plots[[i]] <- raw_plots[[i]] + 
        labs(x = var_names[current_var], 
             title = paste("Effekt:", var_names[current_var]))
    }
    
    # 5. Zusammenführung mit Patchwork (& Styling)
    raw_plots + 
      plot_annotation(
        title = "Partielle Effekte der Umweltparameter auf die Kondition (K)",
        subtitle = "Modell basiert auf dem gesamten Zeitraum (Stationsbasis)",
        caption = paste0("Basis: n = ", nrow(dat), " Stationen")
      ) & 
      theme_minimal(base_size = 12) &
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5)
  })
  
  # 4.4 GAM Ergebnistabelle
  output$table_gam_results <- renderDT({
    dat <- df_env_fixed()
    vars <- input$gam_variables
    validate(need(length(vars) > 0, ""))
    
    # Modell berechnen
    formula_str <- paste("median_K ~ BioGroup +", paste0("s(", vars, ", k=10)", collapse = " + "))
    model <- mgcv::gam(as.formula(formula_str), data = dat)
    
    # Namen für die Tabelle
    var_names <- c(
      "temp_bottom" = "Bodentemperatur",
      "log_cpue_plaice" = "Schollendichte (log)",
      "sal_bottom" = "Salzgehalt",
      "log_cpue_cod" = "Dorschdichte (log)",
      "depth" = "Wassertiefe"
    )
    
    # Statistiken aufbereiten
    gam_res <- broom::tidy(model) %>%
      mutate(
        term = str_remove_all(term, "s\\(|\\)"), # "s(temp_bottom)" -> "temp_bottom"
        Einflussfaktor = var_names[term],
        p.value = if_else(p.value < 0.001, "< 0.001 ***", as.character(round(p.value, 3))),
        edf = round(edf, 2),
        statistic = round(statistic, 2)
      ) %>%
      select(Einflussfaktor, `Effekt-Stärke (edf)` = edf, `F-Wert` = statistic, `p-Wert` = p.value)
    
    datatable(gam_res, options = list(dom = 't', pageLength = -1), rownames = FALSE)
  })
  
}

shinyApp(ui, server)