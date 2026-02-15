# --- preprocessing.R --
library(tidyverse)
library(sf)

# 1. DATEN EINLESEN -------------------------------------------------------
HH <- read.csv("data/HH.csv")
HL <- read.csv("data/HL.csv")
CA <- read.csv("data/CA.csv")

shapefile_path <- "data/ICES_Areas/ICES_Areas_20160601_cut_dense_3857.shp"
ICES_areas_wgs <- st_read(shapefile_path) %>% st_transform(4326) %>% st_make_valid()

# 2. VORVERARBEITUNG & REINIGUNG ------------------------------------------
# Umwandlung: -9 in NA
CA[CA == -9] <- NA
HH[HH == -9] <- NA
HL[HL == -9] <- NA

# Funktion zur Längenstandardisierung
standardize_length <- function(df) {
  df %>%
    mutate(
      # Klassenbreite in cm bestimmen
      ClassWidth_cm = case_when(
        LngtCode == "." ~ 0.1,  # 1 mm class → 0.1 cm
        LngtCode == "0" ~ 0.5,  # 0.5 cm class → 0.5 cm
        LngtCode == "1" ~ 1,    # 1 cm class → 1 cm
        LngtCode == "2" ~ 2,    # 2 cm class → 2 cm
        LngtCode == "5" ~ 5,    # 5 cm class → 5 cm
        TRUE ~ NA_real_
      ),
      
      # Länge standardisiert in cm (untere Grenze)
      Length_cm = case_when(
        LngtCode %in% c(".", "0") ~ LngtClass / 10, # mm → cm
        TRUE ~ LngtClass                             # bereits cm
      ),
      
      # Mittelwert der Klasse
      Length_mid_cm = Length_cm + ClassWidth_cm / 2
    )
}

# Anwendung auf Daten
CA <- standardize_length(CA)
HL <- standardize_length(HL)

# CPUE Vorbereitung: Dorsch und Scholle
# Zählung der Fische pro Hol, um sie später an HH zu binden
cod_counts_large <- HL %>% 
  filter(ValidAphiaID == "126436", Length_mid_cm >= 35) %>%  #Nur große Dorsche
  group_by(Survey, Year, Quarter, Country, Ship, Gear, HaulNo) %>%
  summarise(Total_Cod_Count_Large = sum(HLNoAtLngt, na.rm = TRUE), .groups = "drop")

plaice_counts <- HL %>% 
  filter(ValidAphiaID == "127143") %>%
  group_by(Survey, Year, Quarter, Country, Ship, Gear, HaulNo) %>%
  summarise(Total_Plaice_Count = sum(HLNoAtLngt, na.rm = TRUE), .groups = "drop")


# HH Datensatz bereinigen
HH <- HH %>%
  filter(
    HaulVal == "V",                 # Nur valide Hols
    !is.na(HaulDur), HaulDur >= 10, # Mindestens 10 Min (Festlegung)
    !is.na(HaulLat), !is.na(HaulLong) # Keine NAs in Koordinaten
  )


# 3. RÄUMLICHE ZUORDNUNG --------------------------------------------------
# Hauls aus HH extrahieren (Koordinaten, HaulDuration, Umweltdaten sichern)
hauls_sf <- HH %>%
  # Relevante Spalten auswählen
  select(
    Survey, Year, Quarter, Country, Ship, Gear, HaulNo,
    # HaulDur für CPUE-Berechnung
    HaulDur, 
    # Koordinaten
    HaulLat, HaulLong,
    # Umweltdaten
    Depth, SurTemp, BotTemp, SurSal, BotSal
  ) %>%
  distinct() %>% # Dubletten entfernen
  # In sf-Objekt umwandeln (Simple Features)
  st_as_sf(coords = c("HaulLong", "HaulLat"), crs = 4326, remove = FALSE)

# Räumliche Zuordnung (ICES Areas), CPUE-Integration & Finalisierung
hauls_final <- st_join(hauls_sf, ICES_areas_wgs, join = st_intersects) %>%
  st_drop_geometry() %>% 
  # Spalten aus hauls_sf wählen
  select(Survey, Year, Quarter, Country, Ship, Gear, HaulNo, SubDivisio, 
         HaulLat, HaulLong, HaulDur,
         Depth, SurTemp, BotTemp, SurSal, BotSal
  ) %>%
  # Dorsch- und Schollen-Zählungen dazuspielen
  left_join(cod_counts_large, by = c("Survey", "Year", "Quarter", "Country", "Ship", "Gear", "HaulNo")) %>%
  left_join(plaice_counts, by = c("Survey", "Year", "Quarter", "Country", "Ship", "Gear", "HaulNo")) %>%
  mutate(
    # NAs auf 0 setzen für Hols ohne Fang
    Total_Cod_Count_Large = replace_na(Total_Cod_Count_Large, 0),
    Total_Plaice_Count = replace_na(Total_Plaice_Count, 0),
    # CPUE-Berechnung: Anzahl pro 60 Minuten Schleppzeit
    CPUE_Cod_Large = (Total_Cod_Count_Large / HaulDur) * 60,
    CPUE_Plaice = (Total_Plaice_Count / HaulDur) * 60
  ) %>%
  # Sicherheitshalber Eindeutigkeit prüfen
  distinct(Survey, Year, Quarter, Country, Ship, Gear, HaulNo, .keep_all = TRUE)
  
# 4. BIOLOGISCHE ANALYSE (Biogroups, Outlier & Fulton K) ----------------------
# Definition aller Codes für "REIFE" Stadien
mature_codes <- c(
  "2", "3", "4", "5", "52", "53", "54", "55", "62", "63", "64", "65", 
  "B", "Ba", "Bb", "C", "Ca", "Cb", "D", "Da", "Db", "E", "M", 
  "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", 
  "R1_2", "R1_3", "R1_4", "R2_4", "R2_6", "R2_8", 
  "RF2", "RF3", "RF4", "RF5", "RF6", "S2", "S3", "S4", "S5", "S6"
)

CA_meta <- CA %>% 
  # Nur Schollen mit Gewicht und Länge
  filter(ValidAphiaID == "127143", !is.na(IndWgt), !is.na(Length_mid_cm)) %>%
  # Trennung der reifen Weibchen von der Restpopulation (Males/Juveniles), 
  # um Verzerrungen durch Gonadengewicht in der Regression zu vermeiden
  mutate(BioGroup = case_when(
    (Sex %in% c("F", "B")) & Maturity %in% mature_codes ~ "Female_Mature",
    TRUE ~ "Males_Juveniles_Other"
  )) %>%
  # Ausreißerbereinigung pro Jahr, Quartal und Gruppe:
  # Ausreißer werden entfernt, wenn sie >3 Standardabweichungen vom vorhergesagten Gewicht abweichen
  # 3rd Standard Deviation Regel auf Basis der Log-Log Regression
  group_by(Year, Quarter, BioGroup) %>%
  filter(n() > 5) %>%  # Mindestanzahl für statistische Belastbarkeit
  mutate(
    # Berechnung der Residuen der Längen-Gewichts-Regression
    log_resid = resid(lm(log10(IndWgt) ~ log10(Length_mid_cm))),
    sd_resid = sd(log_resid),
    is_outlier = abs(log_resid) > (3 * sd_resid)
  ) %>%
  ungroup() %>%
  filter(is_outlier == FALSE) %>%
  # Berechnung Fulton's Condition Factor (K)
    # Maßzahl für die Kondition: K=100*W/L^3 
    # W = Gewicht des Fisches (g)
    # L = Länge des Fisches (cm)
  mutate(K_Fulton = 100 * (IndWgt / (Length_mid_cm^3))) %>%
  # Join mit den Haul-Informationen (Umwelt, CPUE, SubDivisio)
  left_join(hauls_final, by = c("Survey", "Year", "Quarter", "Country", "Ship", "Gear", "HaulNo")) %>%
  # Regionen benennen
  mutate(Region = case_when(
    SubDivisio == "20" ~ "Skagerrak", 
    SubDivisio == "21" ~ "Kattegat",
    SubDivisio == "22" ~ "Kieler Bucht", 
    SubDivisio == "23" ~ "Öresund",
    SubDivisio == "24" ~ "Arkona-Becken",
    SubDivisio == "25" ~ "Bornholm-Becken", 
    SubDivisio == "26" ~ "Östlich von Bornholm", 
    TRUE ~ NA_character_
  )) %>%
  # Geografische Sortierung (Faktor)
  mutate(Region = factor(Region, levels = c(
    "Skagerrak", "Kattegat", "Öresund", "Kieler Bucht",
    "Arkona-Becken", "Bornholm-Becken", "Östlich von Bornholm"
  ))) %>%
  filter(!is.na(Region)) # Nur Fische aus Zielgebieten



# 5. AGGREGATION FÜR KARTE & CPUE-ANALYSEN ----------------------------
# Zusammenfassung der Daten auf Haul-Ebene
map_data_all <- CA_meta %>%
  group_by(Year, Quarter, Region, SubDivisio, BioGroup, HaulLat, HaulLong, HaulNo) %>%
  summarise(
    median_K = median(K_Fulton, na.rm = TRUE),
    n_plaice_measured = n(),                       # Anzahl gemessener Schollen im Hol
    cpue_plaice = mean(CPUE_Plaice),       # Dichte Scholle
    cpue_cod_large = mean(CPUE_Cod_Large), # Dichte Dorsch (>35cm)
    temp_bottom = mean(BotTemp),           # Umweltparameter
    sal_bottom = mean(BotSal),
    depth = mean(Depth),
    .groups = "drop"
  ) %>%
  # Bereinigung von statistischen Artefakten (NaN zu NA)
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))


# 6. EXPORT DER RDS-DATEIEN -----------------------------------------------
# Einzelfisch-Daten: Für Boxplots, Zeitreihen und Regressionen
saveRDS(CA_meta, "data/plaice_app_data.rds")

# Aggregierte Daten: Für die interaktive Karte und CPUE-Korrelationen
saveRDS(map_data_all, "data/map_data_all.rds")

# Optimiertes Shapefile: Für den Hintergrund der Karte
# Zuschneidung der Shapefile auf Untersuchungsgebiet
lon_min <- min(CA_meta$HaulLong, na.rm = TRUE) - 0.5
lon_max <- max(CA_meta$HaulLong, na.rm = TRUE) + 0.5
lat_min <- min(CA_meta$HaulLat, na.rm = TRUE) - 0.5
lat_max <- max(CA_meta$HaulLat, na.rm = TRUE) + 0.5

ICES_light <- ICES_areas_wgs %>%
  st_crop(xmin = lon_min, xmax = lon_max, ymin = lat_min, ymax = lat_max) %>%
  st_simplify(dTolerance = 0.001) 

saveRDS(ICES_light, "data/ices_shape_light.rds")

cat("--- Preprocessing am:", as.character(Sys.time()), "\n")
cat("Dateien in 'data/' gespeichert:\n")
cat("- plaice_app_data.rds (Einzelfische)\n")
cat("- map_data_all.rds (Aggregiert pro Hol)\n")
cat("- ices_shape_light.rds (Karten-Hintergrund)\n")
