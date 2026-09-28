# ============================================================
# Grafschaft in Zahlen
# Landwirtschaft – Tierhaltung je Einwohner 2020
# Datenaufbereitung
# ============================================================

library(xml2)
library(dplyr)
library(stringr)
library(purrr)
library(readr)


# ------------------------------------------------------------
# 1. Funktion zum Auslesen einer LSN-XML-Datei
# ------------------------------------------------------------

lese_tierbestand <- function(datei, gesamt_bezeichnung, neuer_name) {
  
  doc <- read_xml(datei)
  
  ns <- c(
    ss = "urn:schemas-microsoft-com:office:spreadsheet"
  )
  
  rows <- xml_find_all(
    doc,
    ".//ss:Worksheet/ss:Table/ss:Row",
    ns = ns
  )
  
  aktueller_code <- NA_character_
  aktueller_name <- NA_character_
  
  ergebnis <- list()
  
  for (i in seq_along(rows)) {
    
    cells <- xml_find_all(
      rows[[i]],
      "./ss:Cell",
      ns = ns
    )
    
    if (length(cells) == 0) {
      next
    }
    
    werte <- map_chr(
      cells,
      function(cell) {
        
        dat <- xml_find_first(
          cell,
          "./ss:Data",
          ns = ns
        )
        
        if (inherits(dat, "xml_missing")) {
          ""
        } else {
          str_trim(xml_text(dat))
        }
      }
    )
    
    
    # --------------------------------------------------------
    # Gebiet erkennen
    # Beispiele:
    # 456     Grafschaft Bentheim
    # 456001  Bad Bentheim,Stadt
    # 456010  Isterberg
    # --------------------------------------------------------
    
    gebiet <- str_match(
      werte[1],
      "^\\s*(456(?:\\d{3})?)\\s+(.+?)\\s*$"
    )
    
    if (!is.na(gebiet[1, 1])) {
      
      aktueller_code <- gebiet[1, 2]
      
      aktueller_name <- gebiet[1, 3]
    }
    
    
    # --------------------------------------------------------
    # Nur jeweilige Zeile "... insgesamt" verwenden
    # Letzte Spalte = Tierzahl 2020
    # --------------------------------------------------------
    
    if (
      !is.na(aktueller_code) &&
      length(werte) >= 2 &&
      werte[1] == gesamt_bezeichnung
    ) {
      
      wert_2020 <- werte[length(werte)]
      
      # Leere / unterdrückte Werte NICHT als Null behandeln
      if (wert_2020 %in% c("", "-")) {
        
        wert_2020 <- NA_real_
        
      } else {
        
        wert_2020 <- as.numeric(wert_2020)
        
      }
      
      ergebnis[[length(ergebnis) + 1]] <- tibble(
        
        code = as.integer(aktueller_code),
        
        lsn_name = aktueller_name,
        
        wert = wert_2020
      )
    }
  }
  
  
  bind_rows(ergebnis) |>
    
    # Samtgemeinde-Summen entfernen:
    # 456401 Emlichheim SG
    # 456402 Neuenhaus SG
    # 456403 Schüttorf SG
    # 456404 Uelsen SG
    #
    # Übrig bleiben:
    # Landkreis + 25 einzelne Gemeinden
    
    filter(
      code == 456 |
        !code %in% c(
          456401,
          456402,
          456403,
          456404
        )
    ) |>
    
    select(
      code,
      !!neuer_name := wert
    )
}


# ------------------------------------------------------------
# 2. Tierbestände 2020 auslesen
# ------------------------------------------------------------

rinder <- lese_tierbestand(
  
  "data/landwirtschaft/Rinder.xml",
  
  "Rinder insgesamt",
  
  "rinder_2020"
)


schweine <- lese_tierbestand(
  
  "data/landwirtschaft/Schweine.xml",
  
  "Schweine insgesamt",
  
  "schweine_2020"
)


huehner <- lese_tierbestand(
  
  "data/landwirtschaft/Huehner.xml",
  
  "Hühner insgesamt",
  
  "huehner_2020"
)


# ------------------------------------------------------------
# 3. Einwohner der 25 Gemeinden im Jahr 2020
# ------------------------------------------------------------

einwohner_gemeinden <-
  
  read_csv2(
    "data/bevoelkerung_gemeinden_1821_2026_mit_alter.csv",
    show_col_types = FALSE
  ) |>
  
  filter(
    year == 2020
  ) |>
  
  transmute(
    
    code = as.integer(lsn_code),
    
    gemeinde = display_name,
    
    einwohner_2020 = population
  )


# ------------------------------------------------------------
# 4. Einwohner Grafschaft Bentheim insgesamt
# ------------------------------------------------------------

einwohner_grafschaft <-
  
  read_csv2(
    "data/bevoelkerung_grafschaft_1821_2026_mit_alter.csv",
    show_col_types = FALSE
  ) |>
  
  filter(
    jahr == 2020
  ) |>
  
  transmute(
    
    code = 456L,
    
    gemeinde = "Grafschaft insgesamt",
    
    einwohner_2020 = bevoelkerung
  )


einwohner <-
  
  bind_rows(
    
    einwohner_grafschaft,
    
    einwohner_gemeinden
  )


# ------------------------------------------------------------
# 5. Alles verbinden
# ------------------------------------------------------------

landwirtschaft_2020 <-
  
  einwohner |>
  
  left_join(
    rinder,
    by = "code"
  ) |>
  
  left_join(
    schweine,
    by = "code"
  ) |>
  
  left_join(
    huehner,
    by = "code"
  ) |>
  
  mutate(
    
    rinder_je_einwohner =
      rinder_2020 / einwohner_2020,
    
    schweine_je_einwohner =
      schweine_2020 / einwohner_2020,
    
    huehner_je_einwohner =
      huehner_2020 / einwohner_2020
  ) |>
  
  arrange(
    gemeinde
  )


# ------------------------------------------------------------
# 6. Kontrolle in RStudio
# ------------------------------------------------------------

print(
  landwirtschaft_2020,
  n = Inf
)


# ------------------------------------------------------------
# 7. Masterdatei speichern
# ------------------------------------------------------------

write_csv2(
  
  landwirtschaft_2020,
  
  "data/landwirtschaft/landwirtschaft_tierhaltung_2020.csv",
  
  na = ""
)


# ------------------------------------------------------------
# 8. Kurze Kontrolle
# ------------------------------------------------------------

cat(
  "\nAnzahl Zeilen:",
  nrow(landwirtschaft_2020),
  "\n"
)

cat(
  "Erwartet: 26 (25 Gemeinden + Grafschaft insgesamt)\n"
  
  
  
  # ============================================================
  # 9. Datenkontrolle
  # ============================================================
  
  cat("\n--- Fehlende Tierbestände ---\n")
  
  landwirtschaft_2020 |>
    summarise(
      rinder_fehlend   = sum(is.na(rinder_2020)),
      schweine_fehlend = sum(is.na(schweine_2020)),
      huehner_fehlend  = sum(is.na(huehner_2020))
    ) |>
    print()
  
  
  cat("\n--- Höchste Werte: Rinder je Einwohner ---\n")
  
  landwirtschaft_2020 |>
    arrange(desc(rinder_je_einwohner)) |>
    select(
      gemeinde,
      einwohner_2020,
      rinder_2020,
      rinder_je_einwohner
    ) |>
    slice_head(n = 10) |>
    print(n = 10)
  
  
  cat("\n--- Höchste Werte: Schweine je Einwohner ---\n")
  
  landwirtschaft_2020 |>
    arrange(desc(schweine_je_einwohner)) |>
    select(
      gemeinde,
      einwohner_2020,
      schweine_2020,
      schweine_je_einwohner
    ) |>
    slice_head(n = 10) |>
    print(n = 10)
  
  
  cat("\n--- Höchste Werte: Hühner je Einwohner ---\n")
  
  landwirtschaft_2020 |>
    arrange(desc(huehner_je_einwohner)) |>
    select(
      gemeinde,
      einwohner_2020,
      huehner_2020,
      huehner_je_einwohner
    ) |>
    slice_head(n = 10) |>
    print(n = 10)
)

cat("\n--- Fehlende Schweinewerte ---\n")

landwirtschaft_2020 |>
  filter(is.na(schweine_2020)) |>
  select(gemeinde) |>
  print(n = Inf)


cat("\n--- Fehlende Hühnerwerte ---\n")

landwirtschaft_2020 |>
  filter(is.na(huehner_2020)) |>
  select(gemeinde) |>
  print(n = Inf)

##
cat("\n--- Fehlende Schweinewerte ---\n")

landwirtschaft_2020 |>
  filter(is.na(schweine_2020)) |>
  select(gemeinde) |>
  print(n = Inf)


cat("\n--- Fehlende Hühnerwerte ---\n")

landwirtschaft_2020 |>
  filter(is.na(huehner_2020)) |>
  select(gemeinde) |>
  print(n = Inf)