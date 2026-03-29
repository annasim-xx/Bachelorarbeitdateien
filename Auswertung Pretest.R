# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Schritt 0: Einlesen der Rohdaten + Qualitätszählung
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

                                                                                # Aktivieren aller nötigen Pakete
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(writexl)

                                                                                # Einlesen der Tabelle

file_path2 <- "C:/Users/annai/Downloads/Dokumentation Pretest.xlsx"

df_sozio  <- read_excel(file_path2, sheet = "TN Infos")
df_raw <- read_excel(file_path2, sheet = "Antworten")

                                                                                # Trimmen der Spaltennamen
names(df_raw) <- names(df_raw) |> str_trim()
names(df_sozio) <- names(df_sozio) |> str_trim()
 

df_sozio <- df_sozio %>%
  mutate(
    id = as.character(id),
    alter = as.numeric(alter),
    geschlecht = ifelse(is.na(geschlecht), NA_character_, as.character(geschlecht))
  )

# 1) N (gesamt, unique IDs, Missing)
n_overall <- df_sozio %>%
  summarise(
    n_zeilen = n(),
    n_id_unique = n_distinct(id),
    n_id_missing = sum(is.na(id)),
    n_alter_missing = sum(is.na(alter)),
    n_geschlecht_missing = sum(is.na(geschlecht))
  )

# 2) Alter: Deskriptiv (M, SD, Median, Min/Max)
alter_desc <- df_sozio %>%
  summarise(
    n = sum(!is.na(alter)),
    mean = round(mean(alter, na.rm = TRUE), 2),
    sd = round(sd(alter, na.rm = TRUE), 2),
    median = round(median(alter, na.rm = TRUE), 2),
    min = round(min(alter, na.rm = TRUE), 2),
    max = round(max(alter, na.rm = TRUE), 2)
  )

# 3) Geschlecht: Häufigkeiten (n) und Prozent (%)
geschlecht_freq <- df_sozio %>%
  mutate(geschlecht = replace_na(geschlecht, "Missing")) %>%
  count(geschlecht, name = "n") %>%
  mutate(
    pct = round(100 * n / sum(n), 1)
  ) %>%
  arrange(desc(n))

# 4) Alter nach Geschlecht: n + Deskriptiv
alter_by_geschlecht <- df_sozio %>%
  filter(!is.na(alter)) %>%
  mutate(geschlecht = replace_na(geschlecht, "Missing")) %>%
  group_by(geschlecht) %>%
  summarise(
    n = n(),
    mean = round(mean(alter), 2),
    sd = round(sd(alter), 2),
    median = round(median(alter), 2),
    min = round(min(alter), 2),
    max = round(max(alter), 2),
    .groups = "drop"
  ) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))

n_overall
alter_desc
geschlecht_freq
alter_by_geschlecht

                                                                               # Relevante Variablen
vars <- c("glaubwürdigkeit", "vertrautheit", "verständlichkeit",
          "wahrheitswert", "valenz", "bereitsgesehen")

                                                                                # Vereinheitlichung der Rohwerte  ohne inhaltliches Recoding
as_raw_chr <- function(x) {
  x |> as.character() |>
    str_replace_all("\u00A0", " ") |>
    str_replace_all("[\r\n\t]", " ") |>
    str_squish()
}

                                                                                # Erstellen eines Gesamtqualitätsprofils
qc_overall <- bind_rows(lapply(vars, function(v) {
  x0 <- df_raw[[v]]
  x  <- as_raw_chr(x0)
  tibble(
    variable = v,
    n_total = length(x0),
    n_trueNA = sum(is.na(x0)),
    n_blank = sum(!is.na(x0) & str_length(x) == 0),
    n_nonmissing = sum(!is.na(x0) & str_length(x) > 0)
  )
}))

qc_overall

                                                                                # Häufigkeiten je Antwortkategorie (roh)
freq_overall <- bind_rows(lapply(vars, function(v) {
  x0 <- df_raw[[v]]
  x  <- as_raw_chr(x0)
  tibble(variable = v, value = x) %>%
    mutate(
      value = case_when(
        is.na(x0) ~ "<TRUE_NA>",
        str_length(value) == 0 ~ "<BLANK>",
        str_to_lower(value) == "missing" ~ "<MISSING_STRING>",
        value %in% c("TRUE","FALSE") ~ paste0("<LOGICAL_", value, ">"),
        TRUE ~ value
      )
    ) %>%
    count(variable, value, sort = TRUE)
}))

freq_overall

                                                                                # Qualitätsprofil pro Fallnummer (roh)
qc_by_item <- bind_rows(lapply(vars, function(v) {
  x0 <- df_raw[[v]]
  x  <- as_raw_chr(x0)
  tibble(
    fallnummer = df_raw$fallnummer,
    variable   = v,
    raw0       = x0,
    raw_chr    = x
  ) %>%
    group_by(fallnummer, variable) %>%
    summarise(
      n_total = n(),
      n_trueNA = sum(is.na(raw0)),
      n_blank  = sum(!is.na(raw0) & str_length(raw_chr) == 0),
      n_missing_string = sum(!is.na(raw0) & str_to_lower(raw_chr) == "missing"),
      n_logical = sum(raw_chr %in% c("TRUE","FALSE")),
      .groups = "drop"
    )
}))

qc_by_item


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Schritt 1: Cleaning + numerische Kodierung + 0–1 Transformation der Skalen
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                                                                                # Aufräumen der Texteingaben
norm_txt <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("\u00A0", " ") %>%
    str_replace_all("[\r\n\t]", " ") %>%
    str_squish() %>%
    str_to_lower()
}
                                                                                # Umgang mit ggf. fehlenden oder missing EInträgen
to_na_if_missing <- function(x) {
  x <- norm_txt(x)
  x[x %in% c("", "keine angabe", "missing")] <- NA_character_
  x
}
                                                                                # Funktion zur Transformation der Eingaben in Glaubwürdigkeit, Verständlichkeit und Vertrautheit in numeric
trans4_num <- function(x) {
  x <- to_na_if_missing(x)
  case_when(
    x == "trifft nicht zu" ~ 1,
    x == "trifft eher nicht zu" ~ 2,
    x == "trifft eher zu" ~ 3,
    x == "trifft zu" ~ 4,
    TRUE ~ NA_real_
  )
}
                                                                                #  Funktion zur Transformation der Eingaben in Wahrheitswahrscheinlichkeit in numeric
truth4_num <- function(x) {
  x <- to_na_if_missing(x)
  x[x %in% c("true", "false")] <- NA_character_
  case_when(
    x == "unwahr" ~ 1,
    x == "eher unwahr" ~ 2,
    x == "eher wahr" ~ 3,
    x == "wahr" ~ 4,
    TRUE ~ NA_real_
  )
}
                                                                                #  Funktion zur Transformation der Eingaben in Valenz in numeric
valence5_num <- function(x) {
  x <- to_na_if_missing(x)
  case_when(
    x == "...negativ" ~ 1,
    x == "...eher negativ" ~ 2,
    x == "...neutral" ~ 3,
    x == "...eher positiv" ~ 4,
    x == "...positiv" ~ 5,
    TRUE ~ NA_real_
  )
}
                                                                                #  Funktion zur Transformation der Eingaben in FakeNewsKontext in numeric, Einstufen als NA bei Unsicherheit
seen_yes_num <- function(x) {
  x <- to_na_if_missing(x)
  case_when(
    x == "ja" ~ 1,
    x == "nein" ~ 0,
    str_detect(x, "weiß ich nicht oder kann ich nicht einschätzen") ~ NA_real_,
    TRUE ~ NA_real_
  )
}
                                                                                # Ausführen der Transformation durch oben festgelegte Funktionen, von raw zu clean              
df_num <- df_raw %>%
  mutate(
    glaub_clean = to_na_if_missing(glaubwürdigkeit),
    vertraut_clean = to_na_if_missing(vertrautheit),
    verst_clean = to_na_if_missing(verständlichkeit),
    wahr_clean = to_na_if_missing(wahrheitswert),
    valenz_clean = to_na_if_missing(valenz),
    seen_clean = to_na_if_missing(bereitsgesehen),
    
    glaub_num = trans4_num(glaub_clean),
    vertraut_num = trans4_num(vertraut_clean),
    verst_num = trans4_num(verst_clean),
    wahr_num = truth4_num(wahr_clean),
    valenz_num = valence5_num(valenz_clean),
                                                                                # Skalieren auf eine 0-1 Skala zur einheitlichen Vergleichbarkeit    
    seen_yes = seen_yes_num(seen_clean)
  ) %>%
  mutate(
    glaub01 = (glaub_num - 1) / 3,
    vertraut01 = (vertraut_num - 1) / 3,
    verst01 = (verst_num - 1) / 3,
    wahr01 = (wahr_num - 1) / 3,
    valenz01 = (valenz_num - 1) / 4,
    
    conflict_pw = abs(glaub01 - wahr01)
  )
                                                                                # Überprüfen auf nicht beachtete Elemente zur Qualitätssicherung
allowed_likert4 <- c("trifft nicht zu","trifft eher nicht zu","trifft eher zu","trifft zu")
allowed_truth4  <- c("unwahr","eher unwahr","eher wahr","wahr")
allowed_val5    <- c("...negativ","...eher negativ","...neutral","...eher positiv","...positiv")

unknown_levels <- list(
  glaub = df_num %>% filter(!is.na(glaub_clean) & !(glaub_clean %in% allowed_likert4)) %>% count(glaub_clean, sort=TRUE),
  vertraut = df_num %>% filter(!is.na(vertraut_clean) & !(vertraut_clean %in% allowed_likert4)) %>% count(vertraut_clean, sort=TRUE),
  verst = df_num %>% filter(!is.na(verst_clean) & !(verst_clean %in% allowed_likert4)) %>% count(verst_clean, sort=TRUE),
  wahr = df_num %>% filter(!is.na(wahr_clean) & !(wahr_clean %in% allowed_truth4) & !(wahr_clean %in% c("true","false"))) %>% count(wahr_clean, sort=TRUE),
  valenz = df_num %>% filter(!is.na(valenz_clean) & !(valenz_clean %in% allowed_val5)) %>% count(valenz_clean, sort=TRUE),
  seen = df_num %>% filter(!is.na(seen_clean) & !(seen_clean %in% c("ja","nein")) &
                             !str_detect(seen_clean, "weiß ich nicht oder kann ich nicht einschätzen")) %>%
    count(seen_clean, sort=TRUE)
)

unknown_levels

qc_after <- tibble(
  variable = c("glaub_num","vertraut_num","verst_num","wahr_num","valenz_num","seen_yes","conflict_pw"),
  n_valid  = c(sum(!is.na(df_num$glaub_num)),
               sum(!is.na(df_num$vertraut_num)),
               sum(!is.na(df_num$verst_num)),
               sum(!is.na(df_num$wahr_num)),
               sum(!is.na(df_num$valenz_num)),
               sum(!is.na(df_num$seen_yes)),
               sum(!is.na(df_num$conflict_pw))),
  n_NA     = c(sum(is.na(df_num$glaub_num)),
               sum(is.na(df_num$vertraut_num)),
               sum(is.na(df_num$verst_num)),
               sum(is.na(df_num$wahr_num)),
               sum(is.na(df_num$valenz_num)),
               sum(is.na(df_num$seen_yes)),
               sum(is.na(df_num$conflict_pw)))
)

qc_after

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Schritt 2: Kennwerte pro Aussage
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                                                                                # Qualitätssicherung, Fallnummern prüfen
item_stats <- df_num %>%
  group_by(fallnummer) %>%
  summarise(
    n_total = n(),
    
    n_glaub = sum(!is.na(glaub_num)),
    n_vertraut = sum(!is.na(vertraut_num)),
    n_verst = sum(!is.na(verst_num)),
    n_wahr = sum(!is.na(wahr_num)),
    n_valenz = sum(!is.na(valenz_num)),
    
    n_seen_known = sum(!is.na(seen_yes)),
                                                                                # Mediane berechnen
    med_glaub = median(glaub_num, na.rm = TRUE),
    med_vertraut = median(vertraut_num, na.rm = TRUE),
    med_verst = median(verst_num, na.rm = TRUE),
    med_wahr = median(wahr_num, na.rm = TRUE),
    med_valenz = median(valenz_num, na.rm = TRUE),
                                                                                # Streuungen berechnen
    iqr_glaub = IQR(glaub_num, na.rm = TRUE),
    iqr_vertraut = IQR(vertraut_num, na.rm = TRUE),
    iqr_verst = IQR(verst_num, na.rm = TRUE),
    iqr_wahr = IQR(wahr_num, na.rm = TRUE),
    iqr_valenz = IQR(valenz_num, na.rm = TRUE),
                                                                                
    prop_seen_yes = ifelse(n_seen_known == 0, NA_real_, mean(seen_yes == 1, na.rm = TRUE)), #Wahrscheinlichkeit von Vertrautheit und im Kontext von FakeNews bekannt
    
    amb_disp = iqr_glaub + iqr_wahr,                                            # Streuung Glaubwürdig und für wahr befunden 
    
                                                                                # Konflikt berechnen
    n_conflict_valid = sum(!is.na(conflict_pw)),
    mean_conflict_pw = mean(conflict_pw, na.rm = TRUE),
    
                                                                                # Anteil Personen mit ≥ 1 Likert-Stufe Abstand für Glaubwürdigkeit und Wahrheitswerteinschätzung (auf 0–1 Skala: >= 1/3)
    prop_conflict_high = mean(conflict_pw >= (1/3), na.rm = TRUE),
    conflict = prop_conflict_high,
    
    rho_glaub_wahr = ifelse(                                                    #Spearmanrangkorrelation pro Fallnummer zwischen Glaubwürdigkeit und Wahrheitswertwahrscheinlichkeit
      sum(!is.na(glaub_num) & !is.na(wahr_num)) < 3, NA_real_,
      cor(glaub_num, wahr_num, method = "spearman", use = "complete.obs")
    ),
    rho_vertraut_seen = ifelse(
      sum(!is.na(vertraut_num) & !is.na(seen_yes)) < 3, NA_real_,               #Spearmanrangkorrelation pro Fallnummer zwischen Vertrauutheit und im FakeNews Kontext bekannt
      cor(vertraut_num, seen_yes, method = "spearman", use = "complete.obs")
    ),
    
    .groups = "drop"
  ) %>%
  arrange(fallnummer)

item_stats

q_verst_hi <- quantile(item_stats$med_verst, 0.70, na.rm = TRUE)                # 70% Quantil = hoch verständlich  für Verständlichkeit Mediane
q_iqrv_lo  <- quantile(item_stats$iqr_verst, 0.30, na.rm = TRUE)                # 30% Quantil = niedrig verständlich  für Verständlichkeit IQRs

item_stats <- item_stats %>%
  mutate(good_comprehension = (med_verst >= q_verst_hi) & (iqr_verst <= q_iqrv_lo)) %>%
  mutate(val_neutral_dist = abs(med_valenz - 3) / 2)                            # Zusammenführen der Quantile als gute Verständlichkeit

vs_effect <- df_num %>%                                                         # Filtern auf Fälle mit Vertrautheit und gesehen im FakeNews Kontext
  filter(!is.na(vertraut_num), !is.na(seen_yes)) %>%
  mutate(vertraut_high = vertraut_num >= 3) %>%
  group_by(fallnummer) %>%
  summarise(
    p_seen_high = mean(seen_yes[vertraut_high] == 1, na.rm = TRUE),
    p_seen_low  = mean(seen_yes[!vertraut_high] == 1, na.rm = TRUE),
    lift_seen   = p_seen_high - p_seen_low,
    n = n(),
    .groups="drop"
  )

item_stats <- item_stats %>%
  left_join(vs_effect %>% select(fallnummer, lift_seen), by="fallnummer")

item_stats


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Schritt 3: Optimierung unter Nebenbedingungen
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                                                                                # Kandidatenpool sortiert nach Fallnummer
cand0 <- item_stats %>%
  filter(fallnummer %in% 1:14) %>%
  arrange(fallnummer)
                                                                                # Bestimmen des Maximums der Verständlichkeit-IQR, Division durch 0 vorbeugen
max_iqrv <- max(cand0$iqr_verst, na.rm = TRUE); if (!is.finite(max_iqrv) || max_iqrv == 0) max_iqrv <- 1
max_amb  <- max(cand0$amb_disp,  na.rm = TRUE); if (!is.finite(max_amb)  || max_amb  == 0) max_amb  <- 1
                                                                                # Hilfsvariablen
cand0 <- cand0 %>%
  mutate(
    verst01    = (med_verst - 1) / 3,
    stab_bad   = iqr_verst / max_iqrv,
    seen01     = prop_seen_yes,
    amb_disp01 = amb_disp / max_amb,
    conflict01 = conflict  # bleibt 0..1
  )
                                                                                # Quantilschwellen festlegen, angelehnt an die aus Schritt 3
q_verst_hi <- quantile(cand0$med_verst,    0.70, na.rm = TRUE)
q_iqrv_lo  <- quantile(cand0$iqr_verst,    0.30, na.rm = TRUE)
q_seen_lo  <- quantile(cand0$seen01,       0.30, na.rm = TRUE)
q_conf_hi  <- quantile(cand0$conflict01,   0.70, na.rm = TRUE)
q_amb_hi   <- quantile(cand0$amb_disp01,   0.70, na.rm = TRUE)
                                                                                # Erstellen einer Kandidatenliste, mit STrengeleveln
make_eligible <- function(level = 1) {
  base <- cand0 %>%
    mutate(
      ok_n = (n_verst > 0) & (n_glaub > 0) & (n_wahr > 0),                      # Mindestanforderung: gültige Antworten existieren
      ok_verst = med_verst >= q_verst_hi,                                       # Item ist verständlich, Median
      ok_stab  = iqr_verst <= q_iqrv_lo,                                        # Stabile Verständlichkeit, IQR
      ok_seen  = is.na(seen01) | seen01 <= q_seen_lo,                           # Item ist nicht vorbekannt
      ok_ambig = (conflict01 >= q_conf_hi) | (amb_disp01 >= q_amb_hi)           #Ambiguität! Entweder Konflikt hoch oder Streuungsabiguität hoch
    )
                                                                                # Ausgabe nach
  if (level == 1) return(base %>% filter(ok_n, ok_verst, ok_stab, ok_seen, ok_ambig))
  if (level == 2) return(base %>% filter(ok_n, ok_verst, ok_stab, ok_seen))
  if (level == 3) return(base %>% filter(ok_n, ok_verst, ok_stab))
  if (level == 4) return(base %>% filter(ok_n, ok_verst))
  base %>% filter(ok_n)
}

eligible <- make_eligible(1)
if (nrow(eligible) < 8) eligible <- make_eligible(2)
if (nrow(eligible) < 8) eligible <- make_eligible(3)
if (nrow(eligible) < 8) eligible <- make_eligible(4)
if (nrow(eligible) < 8) eligible <- make_eligible(5)

eligible <- eligible %>%                                                        # Vergabe von Gesamtscores
  mutate(
    score_i =
      1.2 * verst01 + #Belohnt
      1.0 * conflict01 + #Belohnt
      0.6 * amb_disp01 - #Belohnt
      1.2 * coalesce(seen01, 1) - #Bestraft
      0.8 * stab_bad # Betraft
  )

ids <- eligible$fallnummer                                                      # Fallnummern möglicher Kandidaten
if (length(ids) < 8) stop("Zu wenige Kandidaten (eligible) für eine 8er-Auswahl.")

comb_mat <- combn(ids, 8)                                                       # Mögliche Kombinationen                      
                                                                                # Bewertungsfunktion
eval_set <- function(set_ids, dat) {
  sub <- dat %>% filter(fallnummer %in% set_ids)
  
  mean_verst_ok <- mean(sub$verst01, na.rm = TRUE) >= quantile(dat$verst01, 0.70, na.rm = TRUE)
  mean_seen_ok  <- mean(coalesce(sub$seen01, 1), na.rm = TRUE) <= quantile(coalesce(dat$seen01, 1), 0.30, na.rm = TRUE)
  mean_stab_ok  <- mean(sub$stab_bad, na.rm = TRUE) <= quantile(dat$stab_bad, 0.30, na.rm = TRUE)
  
  n_ambig <- sum((sub$conflict01 >= q_conf_hi) | (sub$amb_disp01 >= q_amb_hi), na.rm = TRUE)
  ambig_ok <- n_ambig >= 4
  
  if (!(mean_verst_ok && mean_seen_ok && mean_stab_ok && ambig_ok)) return(NA_real_)
  sum(sub$score_i, na.rm = TRUE)
}

scores <- apply(comb_mat, 2, eval_set, dat = eligible)                          # Bewertung aller 8erKombinationen, Setscores

if (all(is.na(scores))) {                                                       # Verwerfen und nutzen des Maximums, wenn Bedingungen zu hart
  scores <- apply(comb_mat, 2, function(set_ids, dat) {
    sub <- dat %>% filter(fallnummer %in% set_ids)
    sum(sub$score_i, na.rm = TRUE)
  }, dat = eligible)
}

show_item_scores <- function(eligible_df, top_n = NULL) {                       # Alle Scores anzeigen
  out <- eligible_df %>%
    arrange(desc(score_i)) %>%
    select(
      fallnummer, score_i,
      verst01, conflict01, amb_disp01, seen01, stab_bad,
      med_verst, iqr_verst, prop_seen_yes, amb_disp, conflict
    )
  
  if (!is.null(top_n)) out <- out %>% slice_head(n = top_n)
  out
}
all_scores <- show_item_scores(eligible)
all_scores


best_set <- comb_mat[, which.max(scores)]                                       # Auswahl des besten Sets, Top 8 Fälle/Aussagen

selected_8 <- eligible %>%
  filter(fallnummer %in% best_set) %>%
  arrange(desc(score_i)) %>%
  select(
    fallnummer, score_i,
    med_verst, iqr_verst,
    prop_seen_yes,
    med_glaub, med_wahr,
    amb_disp, conflict,
    rho_glaub_wahr, rho_vertraut_seen
  )

selected_8


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  schritt 4: Erstellen der Set-Diagnostik
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

set_diag <- item_stats %>%
  filter(fallnummer %in% selected_8$fallnummer) %>%
  summarise(
    n_items = n(),
    
    mean_med_verst = mean(med_verst, na.rm = TRUE),
    mean_iqr_verst = mean(iqr_verst, na.rm = TRUE),
    min_med_verst  = min(med_verst, na.rm = TRUE),
    
    mean_seen_yes = mean(prop_seen_yes, na.rm = TRUE),
    max_seen_yes  = max(prop_seen_yes, na.rm = TRUE),
    
    mean_amb_disp = mean(amb_disp, na.rm = TRUE),
    mean_conflict = mean(conflict, na.rm = TRUE),
    n_high_conflict = sum(conflict >= quantile(item_stats$conflict, 0.70, na.rm = TRUE), na.rm = TRUE),
    n_high_ambdisp   = sum(amb_disp >= quantile(item_stats$amb_disp, 0.70, na.rm = TRUE), na.rm = TRUE),
    
    mean_med_valenz = mean(med_valenz, na.rm = TRUE),
    mean_val_neutral_dist = mean(abs(med_valenz - 3) / 2, na.rm = TRUE),
    max_val_neutral_dist  = max(abs(med_valenz - 3) / 2, na.rm = TRUE),
    
    mean_rho_glaub_wahr = mean(rho_glaub_wahr, na.rm = TRUE),
    mean_rho_vertraut_seen = mean(rho_vertraut_seen, na.rm = TRUE),
    
    .groups = "drop"
  )

set_diag

set_items_detail <- item_stats %>%
  filter(fallnummer %in% selected_8$fallnummer) %>%
  arrange(fallnummer) %>%
  mutate(val_neutral_dist = abs(med_valenz - 3) / 2) %>%
  select(
    fallnummer,
    med_verst, iqr_verst,
    prop_seen_yes, n_seen_known,
    med_glaub, med_wahr,
    amb_disp, conflict,
    med_valenz, val_neutral_dist,
    rho_glaub_wahr, rho_vertraut_seen,
    n_conflict_valid, mean_conflict_pw, prop_conflict_high
  )

set_items_detail