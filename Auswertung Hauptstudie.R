########################################################################
################### Aufbereitung der Daten
########################################################################

# ----------------------------
# 0) Pakete
# ----------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(forcats)
library(tibble)
library(sandwich)
library(lmtest)

# ----------------------------
# 1) Einlesen
# ----------------------------
file_path <- "C:/Users/annai/Downloads/Wahrnehmung_und_Bewertung_von_Nachrichteninhalten.xlsx"

Alle  <- read_excel(file_path, sheet = "Alles")
pfad1 <- read_excel(file_path, sheet = "Pfad1")
pfad2 <- read_excel(file_path, sheet = "Pfad2")
pfad3 <- read_excel(file_path, sheet = "Pfad3")
pfad4 <- read_excel(file_path, sheet = "Pfad4")

# ----------------------------
# 2) Spaltenzuordnung (exakt wie im Export)
# ----------------------------
col_id     <- "Teilnehmer"
col_age    <- "Wie alt sind Sie?"
col_gender <- "Was ist Ihr Geschlecht?"
col_media  <- "Beschreiben Sie Ihre Mediennutzung (Soziale Medien und Nachrichtendienste, digital und Print) in Stunden pro Woche. Es geht nicht um die gesamte Bildschirmzeit."
col_edu    <- "Was ist Ihr höchster Bildungsabschluss?"

need_cols <- c(col_id, col_age, col_gender, col_media, col_edu)
stopifnot(all(need_cols %in% names(Alle)))
stopifnot(all(need_cols %in% names(pfad1)))
stopifnot(all(need_cols %in% names(pfad2)))
stopifnot(all(need_cols %in% names(pfad3)))
stopifnot(all(need_cols %in% names(pfad4)))



########################################################################
################### Deskriptive Analysen
########################################################################

# ============================================================
# TEIL A – DEMOGRAPHIE (Alle + Pfad1-4)
# ============================================================

# ----------------------------
# A1) Recode-Funktionen (Mediennutzung / Bildung)
# ----------------------------
recode_media <- function(x) {
  x0 <- x %>% as.character() %>% str_squish()
  lev <- c(
    "Bis zu 1 Stunde",
    "Bis zu 2 Stunden",
    "Bis zu 3 Stunden",
    "Bis zu 7 Stunden",
    "Bis zu 10 Stunden",
    "Bis zu 17 Stunden",
    "Bis zu 24 Stunden",
    "Bis zu 31 Stunden",
    "Über 31 Stunden",
    "Unsicher"
  )
  out <- case_when(
    str_detect(x0, regex("unsicher", ignore_case = TRUE)) ~ "Unsicher",
    str_detect(x0, regex("über\\s*31|ueber\\s*31", ignore_case = TRUE)) ~ "Über 31 Stunden",
    str_detect(x0, regex("bis\\s*zu\\s*31", ignore_case = TRUE)) ~ "Bis zu 31 Stunden",
    str_detect(x0, regex("bis\\s*zu\\s*24", ignore_case = TRUE)) ~ "Bis zu 24 Stunden",
    str_detect(x0, regex("bis\\s*zu\\s*17", ignore_case = TRUE)) ~ "Bis zu 17 Stunden",
    str_detect(x0, regex("bis\\s*zu\\s*10", ignore_case = TRUE)) ~ "Bis zu 10 Stunden",
    str_detect(x0, regex("bis\\s*zu\\s*7",  ignore_case = TRUE)) ~ "Bis zu 7 Stunden",
    str_detect(x0, regex("bis\\s*zu\\s*3",  ignore_case = TRUE)) ~ "Bis zu 3 Stunden",
    str_detect(x0, regex("bis\\s*zu\\s*2",  ignore_case = TRUE)) ~ "Bis zu 2 Stunden",
    str_detect(x0, regex("bis\\s*zu\\s*1",  ignore_case = TRUE)) ~ "Bis zu 1 Stunde",
    TRUE ~ NA_character_
  )
  factor(out, levels = lev, ordered = TRUE)
}

recode_edu <- function(x) {
  x0 <- x %>% as.character() %>% str_squish()
  lev <- c(
    "kein Abschluss",
    "Hauptschule",
    "Realschule",
    "Fachhochschulreife",
    "Allgemeine Hochschulreife (Abitur)",
    "Berufsausbildung",
    "Fachwirt IHK",
    "Betriebswirt IHK",
    "Bachelor",
    "Diplom",
    "Master",
    "Promotion",
    "Anderer"
  )
  out <- case_when(
    str_detect(x0, regex("^kein|kein\\s+abschluss", ignore_case = TRUE)) ~ "kein Abschluss",
    str_detect(x0, regex("haupt", ignore_case = TRUE)) ~ "Hauptschule",
    str_detect(x0, regex("real", ignore_case = TRUE)) ~ "Realschule",
    str_detect(x0, regex("fachhochschulreife|fhr", ignore_case = TRUE)) ~ "Fachhochschulreife",
    str_detect(x0, regex("allgemeine\\s+hochschulreife|abitur", ignore_case = TRUE)) ~ "Allgemeine Hochschulreife (Abitur)",
    str_detect(x0, regex("berufsausbildung|ausbildung", ignore_case = TRUE)) ~ "Berufsausbildung",
    str_detect(x0, regex("fachwirt", ignore_case = TRUE)) ~ "Fachwirt IHK",
    str_detect(x0, regex("betriebswirt", ignore_case = TRUE)) ~ "Betriebswirt IHK",
    str_detect(x0, regex("bachelor", ignore_case = TRUE)) ~ "Bachelor",
    str_detect(x0, regex("diplom", ignore_case = TRUE)) ~ "Diplom",
    str_detect(x0, regex("master", ignore_case = TRUE)) ~ "Master",
    str_detect(x0, regex("promotion|doktor", ignore_case = TRUE)) ~ "Promotion",
    str_detect(x0, regex("ander", ignore_case = TRUE)) ~ "Anderer",
    TRUE ~ NA_character_
  )
  factor(out, levels = lev)
}

median_ord_level <- function(f) {
  x <- f[!is.na(f) & as.character(f) != "Unsicher"]
  if (length(x) == 0) return(NA_character_)
  idx <- sort(as.numeric(x))
  levels(f)[idx[ceiling(length(idx) / 2)]]
}

top_open_midpoint <- 35
media_midpoint <- function(media_cat_chr) {
  case_when(
    media_cat_chr == "Bis zu 1 Stunde"   ~ 0.5,
    media_cat_chr == "Bis zu 2 Stunden"  ~ 1.5,
    media_cat_chr == "Bis zu 3 Stunden"  ~ 2.5,
    media_cat_chr == "Bis zu 7 Stunden"  ~ 5.0,
    media_cat_chr == "Bis zu 10 Stunden" ~ 8.5,
    media_cat_chr == "Bis zu 17 Stunden" ~ 13.5,
    media_cat_chr == "Bis zu 24 Stunden" ~ 20.5,
    media_cat_chr == "Bis zu 31 Stunden" ~ 27.5,
    media_cat_chr == "Über 31 Stunden"   ~ top_open_midpoint,
    TRUE ~ NA_real_
  )
}

# ----------------------------
# A2) Helper: Demographie-Tibble pro Datensatz bauen
# ----------------------------
build_demo <- function(dat, group_label, path_label = NA_integer_) {
  dat %>%
    transmute(
      group      = group_label,
      path_sheet = path_label,
      id         = .data[[col_id]],
      age        = suppressWarnings(as.numeric(.data[[col_age]])),
      gender_raw = as.character(.data[[col_gender]]) %>% str_squish(),
      media_raw  = as.character(.data[[col_media]])  %>% str_squish(),
      edu_raw    = as.character(.data[[col_edu]])    %>% str_squish()
    ) %>%
    mutate(
      media_cat = recode_media(media_raw),
      edu_cat   = recode_edu(edu_raw),
      
      edu_group = case_when(
        edu_cat %in% c("kein Abschluss","Hauptschule","Realschule","Fachhochschulreife","Allgemeine Hochschulreife (Abitur)") ~ "Schule",
        edu_cat %in% c("Berufsausbildung","Fachwirt IHK","Betriebswirt IHK") ~ "Beruflich",
        edu_cat %in% c("Bachelor","Diplom","Master","Promotion") ~ "Akademisch",
        edu_cat %in% c("Anderer") ~ "Anderer",
        TRUE ~ NA_character_
      ),
      
      is_academic   = edu_cat %in% c("Bachelor","Diplom","Master","Promotion"),
      is_abi_fhr    = edu_cat %in% c("Fachhochschulreife","Allgemeine Hochschulreife (Abitur)"),
      is_vocational = edu_cat %in% c("Berufsausbildung","Fachwirt IHK","Betriebswirt IHK"),
      
      media_unsure  = as.character(media_cat) == "Unsicher",
      media_hours_approx = media_midpoint(as.character(media_cat))
    )
}

demo_all <- bind_rows(
  build_demo(Alle,  "Alle",  NA_integer_),
  build_demo(pfad1, "Pfad1", 1),
  build_demo(pfad2, "Pfad2", 2),
  build_demo(pfad3, "Pfad3", 3),
  build_demo(pfad4, "Pfad4", 4)
)

demo_main <- demo_all %>%
  filter(group == "Alle") %>%
  distinct(id, .keep_all = TRUE) %>%
  mutate(
    gender = na_if(gender_raw, "") %>% str_to_lower(),
    gender = case_when(
      str_detect(gender, "weib") ~ "weiblich",
      str_detect(gender, "männ|maenn") ~ "männlich",
      str_detect(gender, "div|nichtbin|non") ~ "divers",
      TRUE ~ gender
    ),
    gender_f = factor(gender),
    edu_group_f = factor(edu_group, levels = c("Schule","Beruflich","Akademisch","Anderer")),
    media_hours = ifelse(media_unsure, NA_real_, media_hours_approx),
    age_z = as.numeric(scale(age)),
    media_hours_z = as.numeric(scale(media_hours))
  ) %>%
  select(id, age, age_z, gender_f, edu_group_f, media_cat, media_hours, media_hours_z)

# ----------------------------
# A3) Demographie-Tabellen
# ----------------------------
n_by_group <- demo_all %>% count(group, name = "n")

age_by_group <- demo_all %>%
  group_by(group) %>%
  summarise(
    n = n(),
    n_age = sum(!is.na(age)),
    mean = mean(age, na.rm = TRUE),
    sd = sd(age, na.rm = TRUE),
    median = median(age, na.rm = TRUE),
    iqr = IQR(age, na.rm = TRUE),
    min = min(age, na.rm = TRUE),
    max = max(age, na.rm = TRUE),
    .groups = "drop"
  )

gender_by_group <- demo_all %>%
  mutate(gender_raw = na_if(gender_raw, "")) %>%
  count(group, gender_raw, name = "n") %>%
  group_by(group) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  arrange(group, desc(n))

media_freq_by_group <- demo_all %>%
  count(group, media_cat, name = "n") %>%
  group_by(group) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  arrange(group, media_cat)

media_summary_by_group <- demo_all %>%
  group_by(group) %>%
  summarise(
    n = n(),
    n_valid = sum(!is.na(media_cat) & as.character(media_cat) != "Unsicher"),
    median_cat = median_ord_level(media_cat),
    share_unsure = mean(media_unsure, na.rm = TRUE),
    .groups = "drop"
  )

media_hours_by_group <- demo_all %>%
  filter(!is.na(media_hours_approx), !media_unsure) %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean = mean(media_hours_approx),
    sd = sd(media_hours_approx),
    median = median(media_hours_approx),
    iqr = IQR(media_hours_approx),
    min = min(media_hours_approx),
    max = max(media_hours_approx),
    .groups = "drop"
  )

edu_freq_by_group <- demo_all %>%
  count(group, edu_cat, name = "n") %>%
  group_by(group) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  arrange(group, edu_cat)

edu_group_freq_by_group <- demo_all %>%
  count(group, edu_group, name = "n") %>%
  group_by(group) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  arrange(group, edu_group)

edu_indicators_by_group <- demo_all %>%
  group_by(group) %>%
  summarise(
    share_academic = mean(is_academic, na.rm = TRUE),
    share_abi_fhr = mean(is_abi_fhr, na.rm = TRUE),
    share_vocational = mean(is_vocational, na.rm = TRUE),
    .groups = "drop"
  )

missing_by_group <- demo_all %>%
  group_by(group) %>%
  summarise(
    n = n(),
    miss_age = mean(is.na(age)),
    miss_gender = mean(is.na(gender_raw) | gender_raw == ""),
    miss_media = mean(is.na(media_cat)),
    miss_edu = mean(is.na(edu_cat)),
    .groups = "drop"
  )

n_by_group
age_by_group
gender_by_group
media_freq_by_group
media_summary_by_group
media_hours_by_group
edu_freq_by_group
edu_group_freq_by_group
edu_indicators_by_group
missing_by_group

# ============================================================
# TEIL B – ITEM-DATEN (Sheet "Alles"): WIDE -> LONG -> ITEMS
# ============================================================

item_regex <- regex(
  "^p\\s*([1-4])\\s*[\\._\\- ]*\\s*([1-8])\\s*\\.\\s*(1|2a|2b|3|4a|4b)\\b",
  ignore_case = TRUE
)

item_cols <- names(Alle)[str_detect(names(Alle), item_regex)]
stopifnot(length(item_cols) > 0)

to_chr <- function(x) ifelse(is.na(x), NA_character_, as.character(x))
to_num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", as.character(x))))

long_raw <- Alle %>%
  transmute(
    id = .data[[col_id]],
    across(all_of(item_cols), identity)
  ) %>%
  pivot_longer(
    cols = all_of(item_cols),
    names_to = "var",
    values_to = "value"
  ) %>%
  mutate(
    value_chr = to_chr(value) %>% str_squish(),
    m = str_match(var, item_regex),
    path = as.integer(m[, 2]),
    news = as.integer(m[, 3]),
    measure = m[, 4],
    time = case_when(
      measure %in% c("1", "2a", "2b") ~ "pre",
      measure %in% c("3", "4a", "4b") ~ "post",
      TRUE ~ NA_character_
    ),
    is_skipped = str_detect(value_chr, regex("^\\s*übersprungen\\s*$", ignore_case = TRUE)),
    is_missing = is.na(value_chr) | value_chr == ""
  ) %>%
  select(-m)

seen_tbl <- long_raw %>%
  group_by(id, path, news) %>%
  summarise(
    seen_item = any(!is_skipped & !is_missing),
    all_skipped = all(is_skipped | is_missing),
    .groups = "drop"
  )

long <- long_raw %>%
  left_join(seen_tbl, by = c("id", "path", "news")) %>%
  mutate(
    skipped_structural = !seen_item & all_skipped,
    missing_true = seen_item & is_missing & !is_skipped
  )

resp_path_tbl <- seen_tbl %>%
  group_by(id, path) %>%
  summarise(n_seen = sum(seen_item), .groups = "drop") %>%
  group_by(id) %>%
  slice_max(order_by = n_seen, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(id, resp_path = path, n_seen)

long <- long %>%
  left_join(resp_path_tbl, by = "id")

mapping <- tibble::tribble(
  ~path, ~news, ~cell_raw,
  1, 1, "m.real",   2, 1, "m.fake",   3, 1, "KI.real",  4, 1, "KI.fake",
  1, 2, "m.fake",   2, 2, "KI.real",  3, 2, "KI.fake",  4, 2, "m.real",
  1, 3, "KI.real",  2, 3, "KI.fake",  3, 3, "m.real",   4, 3, "m.fake",
  1, 4, "KI.fake",  2, 4, "m.real",   3, 4, "m.fake",   4, 4, "KI.real",
  1, 5, "m.real",   2, 5, "m.fake",   3, 5, "KI.real",  4, 5, "KI.fake",
  1, 6, "m.fake",   2, 6, "KI.real",  3, 6, "KI.fake",  4, 6, "m.real",
  1, 7, "KI.real",  2, 7, "KI.fake",  3, 7, "m.real",   4, 7, "m.fake",
  1, 8, "KI.fake",  2, 8, "m.real",   3, 8, "m.fake",   4, 8, "KI.real"
) %>%
  mutate(
    cell = str_to_lower(cell_raw),
    source = case_when(
      str_detect(cell_raw, regex("^ki\\.", ignore_case = TRUE)) ~ "KI",
      str_detect(cell_raw, regex("^m\\.", ignore_case = TRUE))  ~ "Mensch",
      TRUE ~ NA_character_
    ),
    veracity = case_when(
      str_detect(cell_raw, regex("\\.real$", ignore_case = TRUE)) ~ "Real",
      str_detect(cell_raw, regex("\\.fake$", ignore_case = TRUE)) ~ "Fake",
      TRUE ~ NA_character_
    ),
    text_id = news
  ) %>%
  select(path, news, text_id, source, veracity, cell)

cell_levels <- c("m.real", "m.fake", "ki.real", "ki.fake")
mapping <- mapping %>%
  mutate(
    cell_factor = factor(cell, levels = cell_levels),
    cell_index = as.integer(cell_factor),
    text_id_32 = (text_id - 1L) * 4L + cell_index
  ) %>%
  select(-cell_factor, -cell_index)

long <- long %>%
  left_join(mapping, by = c("path", "news"))

recode_choice <- function(x) {
  x0 <- to_chr(x) %>% str_squish() %>% str_to_lower()
  case_when(
    str_detect(x0, "weiß|weiss|unsicher|kann\\s*ich\\s*nicht|keine\\s*ahnung") ~ "wn",
    str_detect(x0, "fake|falsch") ~ "fake",
    str_detect(x0, "real|echt|wahr") ~ "real",
    TRUE ~ NA_character_
  )
}

items <- long %>%
  filter(seen_item) %>%
  select(id, resp_path, path, news, text_id, text_id_32, source, veracity, cell, measure, value_chr) %>%
  pivot_wider(
    names_from = measure,
    values_from = value_chr,
    names_prefix = "m_"
  ) %>%
  mutate(
    pre_choice_raw  = m_1,
    post_choice_raw = m_3,
    pre_choice  = recode_choice(pre_choice_raw),
    post_choice = recode_choice(post_choice_raw),
    
    pre_scale_real  = to_num(m_2a),
    pre_scale_fake  = to_num(m_2b),
    post_scale_real = to_num(m_4a),
    post_scale_fake = to_num(m_4b),
    
    post_defined = pre_choice %in% c("real","fake"),
    post_by_design_skipped = pre_choice == "wn",
    
    post_choice_routed = case_when(
      post_by_design_skipped ~ "skipped_pre_wn",
      post_defined ~ post_choice,
      TRUE ~ NA_character_
    ),
    post_choice_routed = factor(post_choice_routed, levels = c("real","fake","wn","skipped_pre_wn"))
  )

n_persons_pre <- items %>% summarise(n_persons_pre = n_distinct(id[!is.na(pre_choice)]))

n_persons_prepost <- items %>%
  filter(post_defined, pre_choice %in% c("real","fake"), post_choice %in% c("real","fake")) %>%
  summarise(n_persons_prepost = n_distinct(id))

items_per_person <- seen_tbl %>%
  group_by(id) %>%
  summarise(n_seen = sum(seen_item), .groups = "drop")

items_per_person_desc <- items_per_person %>%
  summarise(n_persons = n(), median = median(n_seen), min = min(n_seen), max = max(n_seen))

# ============================================================
# TEIL C – DESKRIPTIVE OUTPUTS (TABELLEN) mit Routing-Korrektur
# ============================================================

desc_stats <- function(x) {
  x <- x[!is.na(x)]
  tibble(
    n = length(x),
    mean = if (length(x) == 0) NA_real_ else mean(x),
    sd = if (length(x) <= 1) NA_real_ else sd(x),
    median = if (length(x) == 0) NA_real_ else median(x),
    iqr = if (length(x) == 0) NA_real_ else IQR(x),
    min = if (length(x) == 0) NA_real_ else min(x),
    max = if (length(x) == 0) NA_real_ else max(x)
  )
}

prop_within <- function(dat, group_vars, target_var) {
  dat %>%
    count(across(all_of(group_vars)), .data[[target_var]], name = "n") %>%
    group_by(across(all_of(group_vars))) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup()
}

# PRE
pre_choice_total <- items %>%
  count(pre_choice, name = "n") %>%
  mutate(pct = n / sum(n))

pre_choice_by_cell <- prop_within(items, c("cell"), "pre_choice")

pre_choice_missing_by_cond <- items %>%
  mutate(pre_missing = is.na(pre_choice)) %>%
  group_by(source, veracity) %>%
  summarise(n = n(), n_missing = sum(pre_missing), share_missing = mean(pre_missing), .groups = "drop")

n_items_by_cond_pre <- items %>%
  group_by(source, veracity) %>%
  summarise(n = n(), .groups = "drop")

pre_routing_check <- items %>%
  summarise(
    n = n(),
    rate_missing_2a_given_real = mean(pre_choice == "real" & is.na(pre_scale_real), na.rm = TRUE),
    rate_missing_2b_given_fake = mean(pre_choice == "fake" & is.na(pre_scale_fake), na.rm = TRUE)
  )

# POST
post_choice_total <- items %>%
  filter(post_defined) %>%
  count(post_choice, name = "n") %>%
  mutate(pct = n / sum(n))

post_choice_by_cell <- items %>%
  filter(post_defined) %>%
  count(cell, post_choice, name = "n") %>%
  group_by(cell) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

n_items_by_cond_post <- items %>%
  filter(post_defined) %>%
  group_by(source, veracity) %>%
  summarise(n = n(), .groups = "drop")

post_routing_check <- items %>%
  filter(post_defined) %>%
  summarise(
    n = n(),
    rate_missing_4a_given_real = mean(post_choice == "real" & is.na(post_scale_real), na.rm = TRUE),
    rate_missing_4b_given_fake = mean(post_choice == "fake" & is.na(post_scale_fake), na.rm = TRUE)
  )

# Transitionen + Wechsel
transition_total_routed <- items %>%
  count(pre_choice, post_choice_routed, name = "n") %>%
  group_by(pre_choice) %>%
  mutate(pct_within_pre = n / sum(n)) %>%
  ungroup()

change_rate_total <- items %>%
  filter(post_defined) %>%
  mutate(changed = !is.na(pre_choice) & !is.na(post_choice) & pre_choice != post_choice) %>%
  summarise(rate_changed = mean(changed, na.rm = TRUE), n = n())

change_direction_total <- items %>%
  filter(post_defined) %>%
  mutate(direction = paste0(pre_choice, "->", post_choice)) %>%
  count(direction, name = "n") %>%
  mutate(pct = n / sum(n))

wn_rates <- items %>%
  summarise(
    pre_wn_rate_all = mean(pre_choice == "wn", na.rm = TRUE),
    post_wn_rate_given_post_defined = mean((post_choice == "wn") & post_defined, na.rm = TRUE),
    share_post_skipped_by_design = mean(post_by_design_skipped, na.rm = TRUE)
  )

pre_wn_by_cond <- items %>%
  group_by(source, veracity) %>%
  summarise(n = n(), share_wn = mean(pre_choice == "wn", na.rm = TRUE), .groups = "drop")

pre_choice_total
pre_choice_by_cell
pre_choice_missing_by_cond
n_items_by_cond_pre
pre_routing_check

post_choice_total
post_choice_by_cell
n_items_by_cond_post
post_routing_check

transition_total_routed
change_rate_total
change_direction_total
wn_rates
pre_wn_by_cond

n_persons_pre
n_persons_prepost
items_per_person_desc

# ============================================================
# >>> NACH TEIL C: KLEINE-KATEGORIEN BEREINIGEN (NUR FÜR MODELLE)
# ============================================================

demo_main_model <- demo_main %>%
  mutate(
    edu_group_model = as.character(edu_group_f),
    edu_group_model = ifelse(edu_group_model == "Anderer", NA_character_, edu_group_model),
    edu_group_model_f = factor(edu_group_model, levels = c("Schule","Beruflich","Akademisch")),
    
    gender_model = as.character(gender_f),
    gender_model = case_when(
      gender_model == "weiblich" ~ "weiblich",
      is.na(gender_model) ~ NA_character_,
      TRUE ~ "nicht_weiblich"
    ),
    gender_model_f = factor(gender_model, levels = c("weiblich","nicht_weiblich"))
  ) %>%
  select(-edu_group_model, -gender_model)

# ============================================================
# TEIL D – item_profile: Pre/Post + Confidence + Routing + Join Demographie
# ============================================================

item_profile <- items %>%
  left_join(demo_main_model, by = "id") %>%
  transmute(
    id, resp_path, path, news, text_id, text_id_32,
    source, veracity, cell,
    pre_choice, post_choice,
    post_defined, post_by_design_skipped, post_choice_routed,
    
    age, age_z,
    gender_f, gender_model_f,
    edu_group_f, edu_group_model_f,
    media_cat, media_hours, media_hours_z,
    
    pre_conf = case_when(
      pre_choice == "real" ~ pre_scale_real,
      pre_choice == "fake" ~ pre_scale_fake,
      TRUE ~ NA_real_
    ),
    
    post_conf = case_when(
      post_defined & post_choice == "real" ~ post_scale_real,
      post_defined & post_choice == "fake" ~ post_scale_fake,
      TRUE ~ NA_real_
    ),
    
    pre_conf_missing_given_choice  = (pre_choice %in% c("real","fake")) & is.na(case_when(
      pre_choice == "real" ~ pre_scale_real,
      pre_choice == "fake" ~ pre_scale_fake
    )),
    
    post_conf_missing_given_choice = post_defined & (post_choice %in% c("real","fake")) & is.na(case_when(
      post_choice == "real" ~ post_scale_real,
      post_choice == "fake" ~ post_scale_fake
    )),
    
    changed = post_defined & !is.na(pre_choice) & !is.na(post_choice) & (pre_choice != post_choice),
    direction = ifelse(post_defined & !is.na(pre_choice) & !is.na(post_choice),
                       paste0(pre_choice, "->", post_choice),
                       NA_character_),
    
    conf_change = ifelse(!is.na(pre_conf) & !is.na(post_conf), post_conf - pre_conf, NA_real_),
    
    conf_change_same_judgment = ifelse(
      post_defined &
        pre_choice %in% c("real","fake") &
        post_choice %in% c("real","fake") &
        pre_choice == post_choice &
        !is.na(pre_conf) & !is.na(post_conf),
      post_conf - pre_conf,
      NA_real_
    )
  )

dup_check <- item_profile %>% count(id, text_id_32) %>% filter(n > 1)
dup_check


########################################################################
################### Hypothesenbezogene Analysen
########################################################################

# ------------------------------------------------------------
# 1) Pfad-Gewichte auf Personenebene
# ------------------------------------------------------------

id_w <- item_profile %>%
  distinct(id, resp_path) %>%
  group_by(resp_path) %>%
  mutate(n_path = n()) %>%
  ungroup() %>%
  mutate(w = 1 / n_path) %>%
  select(id, resp_path, n_path, w)

# ------------------------------------------------------------
# 2) Analysedatensätze für H1 / H2 / H3
# ------------------------------------------------------------

# H1: Post-Fake-Urteil
dat_judgment <- item_profile %>%
  filter(
    post_defined,
    pre_choice %in% c("real", "fake"),
    post_choice %in% c("real", "fake"),
    !is.na(source), !is.na(veracity), !is.na(text_id)
  ) %>%
  left_join(id_w, by = "id") %>%
  mutate(
    source_f   = factor(source, levels = c("Mensch", "KI")),
    veracity_f = factor(veracity, levels = c("Real", "Fake")),
    post_fake  = as.integer(post_choice == "fake")
  )

# H2/H3: Confidence-Change bei stabilen Urteilen
dat_conf_same <- item_profile %>%
  filter(
    post_defined,
    pre_choice %in% c("real", "fake"),
    post_choice %in% c("real", "fake"),
    pre_choice == post_choice,
    !is.na(pre_conf), !is.na(post_conf),
    !is.na(source), !is.na(veracity), !is.na(text_id)
  ) %>%
  left_join(id_w, by = "id") %>%
  mutate(
    source_f   = factor(source, levels = c("Mensch", "KI")),
    veracity_f = factor(veracity, levels = c("Real", "Fake")),
    conf_change_same_judgment = post_conf - pre_conf
  )

# Optional: Missingness-Report
conf_missing_report <- item_profile %>%
  summarise(
    n_items = n(),
    share_post_skipped_by_design = mean(post_by_design_skipped, na.rm = TRUE),
    pre_wn_rate_all = mean(pre_choice == "wn", na.rm = TRUE),
    post_defined_rate = mean(post_defined, na.rm = TRUE),
    post_conf_missing_rate_given_post = mean(post_defined & post_conf_missing_given_choice, na.rm = TRUE),
    pre_conf_missing_rate_given_choice = mean(pre_conf_missing_given_choice, na.rm = TRUE)
  )

conf_missing_report

# ------------------------------------------------------------
# 3) Robuste SE + Hilfsfunktionen
# ------------------------------------------------------------

robust_vcov <- function(model, cluster_vec) {
  sandwich::vcovCL(model, cluster = cluster_vec, type = "HC1")
}

robust_test <- function(model, cluster_vec) {
  V  <- robust_vcov(model, cluster_vec)
  ct <- lmtest::coeftest(model, vcov = V)
  ct <- as.matrix(ct)
  tibble(
    term     = rownames(ct),
    estimate = as.numeric(ct[, 1]),
    se       = as.numeric(ct[, 2]),
    stat     = as.numeric(ct[, 3]),
    p        = as.numeric(ct[, 4])
  )
}

lincombo_robust <- function(model, cluster_vec, coefs_named) {
  b <- coef(model)
  V <- robust_vcov(model, cluster_vec)
  common <- intersect(names(coefs_named), names(b))
  a <- rep(0, length(b)); names(a) <- names(b)
  a[common] <- coefs_named[common]
  est <- sum(a * b)
  se  <- as.numeric(sqrt(t(a) %*% V %*% a))
  stat <- est / se
  p <- 2 * (1 - pnorm(abs(stat)))
  tibble(term = "lincombo", estimate = est, se = se, stat = stat, p = p)
}

simple_effect_ki_at_fake <- function(model, cluster_vec) {
  lincombo_robust(
    model, cluster_vec,
    c("source_fKI" = 1, "source_fKI:veracity_fFake" = 1)
  ) %>%
    mutate(term = "KI effect at veracity=Fake (β_KI_Fake = β_KI + β_int)")
}

add_ci_linear <- function(tbl, level = 0.95) {
  z <- qnorm(1 - (1 - level) / 2)
  tbl %>%
    mutate(
      ci_lower = estimate - z * se,
      ci_upper = estimate + z * se
    )
}

add_or_ci_logit <- function(tbl, level = 0.95) {
  z <- qnorm(1 - (1 - level) / 2)
  tbl %>%
    mutate(
      ci_lower = estimate - z * se,
      ci_upper = estimate + z * se,
      OR = exp(estimate),
      OR_ci_lower = exp(ci_lower),
      OR_ci_upper = exp(ci_upper)
    )
}

# ------------------------------------------------------------
# 4) H1 – KI-Labels erhöhen Wahrscheinlichkeit eines Fake-Urteils
# H1-Haupttest: Source + Veracity (ohne Interaktion)
# Design-Check: Source × Veracity
# ------------------------------------------------------------

# Haupttest H1
m_h1_main_w <- glm(
  post_fake ~ source_f + veracity_f + factor(id) + factor(text_id),
  data = dat_judgment,
  family = binomial(),
  weights = w
)

m_h1_main_nw <- glm(
  post_fake ~ source_f + veracity_f + factor(id) + factor(text_id),
  data = dat_judgment,
  family = binomial()
)

# Design-Check H1
m_h1_mod_w <- glm(
  post_fake ~ source_f * veracity_f + factor(id) + factor(text_id),
  data = dat_judgment,
  family = binomial(),
  weights = w
)

m_h1_mod_nw <- glm(
  post_fake ~ source_f * veracity_f + factor(id) + factor(text_id),
  data = dat_judgment,
  family = binomial()
)

h1_main_w_robust <- robust_test(m_h1_main_w, dat_judgment$id)
h1_main_nw_robust <- robust_test(m_h1_main_nw, dat_judgment$id)
h1_mod_w_robust <- robust_test(m_h1_mod_w, dat_judgment$id)
h1_mod_nw_robust <- robust_test(m_h1_mod_nw, dat_judgment$id)

h1_main_w_robust_ci <- add_or_ci_logit(h1_main_w_robust)
h1_main_nw_robust_ci <- add_or_ci_logit(h1_main_nw_robust)
h1_mod_w_robust_ci <- add_or_ci_logit(h1_mod_w_robust)
h1_mod_nw_robust_ci <- add_or_ci_logit(h1_mod_nw_robust)

# Optional: Simple Effect bei Fake-Stimuli aus Interaktionsmodell
h1_mod_w_simple_fake <- simple_effect_ki_at_fake(m_h1_mod_w, dat_judgment$id)
h1_mod_nw_simple_fake <- simple_effect_ki_at_fake(m_h1_mod_nw, dat_judgment$id)

h1_mod_w_simple_fake_ci <- add_or_ci_logit(h1_mod_w_simple_fake)
h1_mod_nw_simple_fake_ci <- add_or_ci_logit(h1_mod_nw_simple_fake)

# ------------------------------------------------------------
# 5) H2 – KI-Labels senken Selbstsicherheit bei stabilen Urteilen
# H2-Haupttest: Source + Veracity (ohne Interaktion)
# Design-Check: Source × Veracity
# ------------------------------------------------------------

# Haupttest H2
m_h2_main_w <- lm(
  conf_change_same_judgment ~ source_f + veracity_f + factor(id) + factor(text_id),
  data = dat_conf_same,
  weights = w
)

m_h2_main_nw <- lm(
  conf_change_same_judgment ~ source_f + veracity_f + factor(id) + factor(text_id),
  data = dat_conf_same
)

# Design-Check H2
m_h2_mod_w <- lm(
  conf_change_same_judgment ~ source_f * veracity_f + factor(id) + factor(text_id),
  data = dat_conf_same,
  weights = w
)

m_h2_mod_nw <- lm(
  conf_change_same_judgment ~ source_f * veracity_f + factor(id) + factor(text_id),
  data = dat_conf_same
)

h2_main_w_robust <- robust_test(m_h2_main_w, dat_conf_same$id)
h2_main_nw_robust <- robust_test(m_h2_main_nw, dat_conf_same$id)
h2_mod_w_robust <- robust_test(m_h2_mod_w, dat_conf_same$id)
h2_mod_nw_robust <- robust_test(m_h2_mod_nw, dat_conf_same$id)

h2_main_w_robust_ci <- add_ci_linear(h2_main_w_robust)
h2_main_nw_robust_ci <- add_ci_linear(h2_main_nw_robust)
h2_mod_w_robust_ci <- add_ci_linear(h2_mod_w_robust)
h2_mod_nw_robust_ci <- add_ci_linear(h2_mod_nw_robust)

# Optional: Simple Effect bei Fake-Stimuli aus Interaktionsmodell
h2_mod_w_simple_fake <- simple_effect_ki_at_fake(m_h2_mod_w, dat_conf_same$id)
h2_mod_nw_simple_fake <- simple_effect_ki_at_fake(m_h2_mod_nw, dat_conf_same$id)

h2_mod_w_simple_fake_ci <- add_ci_linear(h2_mod_w_simple_fake)
h2_mod_nw_simple_fake_ci <- add_ci_linear(h2_mod_nw_simple_fake)

# ------------------------------------------------------------
# 6) H3 – OKI/Mensch-Labels erhöhen Selbstsicherheit bei stabilen Urteilen
# KORRIGIERT:
# - nur Mensch/OKI-Subset
# - Stimulus-FE mit Sum-Kontrasten
# - Intercept = über Stimuli kontrolliertes Gesamtmittel
# - einseitiger Test auf Δ > 0
# ------------------------------------------------------------

dat_h3_human_mean <- dat_conf_same %>%
  filter(source_f == "Mensch") %>%
  mutate(text_id_f = factor(text_id))

contrasts(dat_h3_human_mean$text_id_f) <- contr.sum(nlevels(dat_h3_human_mean$text_id_f))

m_h3_human_mean_w <- lm(
  conf_change_same_judgment ~ text_id_f,
  data = dat_h3_human_mean,
  weights = w
)

m_h3_human_mean_nw <- lm(
  conf_change_same_judgment ~ text_id_f,
  data = dat_h3_human_mean
)

h3_human_mean_w_robust <- robust_test(m_h3_human_mean_w, dat_h3_human_mean$id)
h3_human_mean_nw_robust <- robust_test(m_h3_human_mean_nw, dat_h3_human_mean$id)

h3_human_mean_w_robust_ci <- add_ci_linear(h3_human_mean_w_robust)
h3_human_mean_nw_robust_ci <- add_ci_linear(h3_human_mean_nw_robust)

# ------------------------------------------------------------
# 7) H3 – einseitige Auswertung (Δ > 0)
# ------------------------------------------------------------

one_sided_p_gt0 <- function(estimate, se) {
  z <- estimate / se
  1 - pnorm(z)
}

one_sided_lb_95 <- function(estimate, se) {
  estimate - 1.645 * se
}

h3_one_sided_summary <- function(rob_tbl, label) {
  x <- rob_tbl %>% filter(term == "(Intercept)")
  stopifnot(nrow(x) == 1)
  est <- x$estimate
  se  <- x$se
  tibble(
    model = label,
    term = "(Intercept)",
    estimate = est,
    se = se,
    stat = est / se,
    p_two_sided = x$p,
    p_one_sided_gt0 = one_sided_p_gt0(est, se),
    ci_lower = est - 1.96 * se,
    ci_upper = est + 1.96 * se,
    lb_one_sided_95 = one_sided_lb_95(est, se)
  )
}

h3_w_one  <- h3_one_sided_summary(h3_human_mean_w_robust,  "H3_Mensch_W")
h3_nw_one <- h3_one_sided_summary(h3_human_mean_nw_robust, "H3_Mensch_NW")

# ------------------------------------------------------------
# 8) Decision Table
# H1/H2:
# - Haupttest = Modelle ohne Interaktion
# - Interaktion = separater Design-Check
# ------------------------------------------------------------

get_term_row <- function(tbl, term_name) {
  x <- tbl %>% filter(term == term_name)
  if (nrow(x) == 0) {
    return(tibble(
      term = term_name,
      estimate = NA_real_,
      se = NA_real_,
      stat = NA_real_,
      p = NA_real_
    ))
  }
  x %>% slice(1)
}

make_decision <- function(estimate, p, direction = c("positive", "negative"), alpha = 0.05) {
  direction <- match.arg(direction)
  
  if (is.na(estimate) || is.na(p)) return(NA_character_)
  if (p >= alpha) return("nicht unterstützt")
  
  if (direction == "positive" && estimate > 0) return("unterstützt")
  if (direction == "negative" && estimate < 0) return("unterstützt")
  
  return("nicht unterstützt")
}

# H1-Haupttest
h1_w_main <- get_term_row(h1_main_w_robust_ci, "source_fKI")
h1_nw_main <- get_term_row(h1_main_nw_robust_ci, "source_fKI")

# H1-Interaktion
h1_w_int  <- get_term_row(h1_mod_w_robust_ci, "source_fKI:veracity_fFake")
h1_nw_int <- get_term_row(h1_mod_nw_robust_ci, "source_fKI:veracity_fFake")

# H2-Haupttest
h2_w_main <- get_term_row(h2_main_w_robust_ci, "source_fKI")
h2_nw_main <- get_term_row(h2_main_nw_robust_ci, "source_fKI")

# H2-Interaktion
h2_w_int  <- get_term_row(h2_mod_w_robust_ci, "source_fKI:veracity_fFake")
h2_nw_int <- get_term_row(h2_mod_nw_robust_ci, "source_fKI:veracity_fFake")

decision_table_main <- bind_rows(
  tibble(
    hypothesis = "H1",
    analysis = "gewichtet",
    effect = "Allgemeiner KI-Effekt auf Fake-Urteil",
    estimate = h1_w_main$estimate,
    se = h1_w_main$se,
    p = h1_w_main$p,
    ci_lower = h1_w_main$ci_lower,
    ci_upper = h1_w_main$ci_upper,
    OR = h1_w_main$OR,
    OR_ci_lower = h1_w_main$OR_ci_lower,
    OR_ci_upper = h1_w_main$OR_ci_upper,
    decision = make_decision(h1_w_main$estimate, h1_w_main$p, direction = "positive")
  ),
  tibble(
    hypothesis = "H1",
    analysis = "ungewichtet",
    effect = "Allgemeiner KI-Effekt auf Fake-Urteil",
    estimate = h1_nw_main$estimate,
    se = h1_nw_main$se,
    p = h1_nw_main$p,
    ci_lower = h1_nw_main$ci_lower,
    ci_upper = h1_nw_main$ci_upper,
    OR = h1_nw_main$OR,
    OR_ci_lower = h1_nw_main$OR_ci_lower,
    OR_ci_upper = h1_nw_main$OR_ci_upper,
    decision = make_decision(h1_nw_main$estimate, h1_nw_main$p, direction = "positive")
  ),
  tibble(
    hypothesis = "H2",
    analysis = "gewichtet",
    effect = "Allgemeiner KI-Effekt auf Confidence-Change (stabil)",
    estimate = h2_w_main$estimate,
    se = h2_w_main$se,
    p = h2_w_main$p,
    ci_lower = h2_w_main$ci_lower,
    ci_upper = h2_w_main$ci_upper,
    OR = NA_real_,
    OR_ci_lower = NA_real_,
    OR_ci_upper = NA_real_,
    decision = make_decision(h2_w_main$estimate, h2_w_main$p, direction = "negative")
  ),
  tibble(
    hypothesis = "H2",
    analysis = "ungewichtet",
    effect = "Allgemeiner KI-Effekt auf Confidence-Change (stabil)",
    estimate = h2_nw_main$estimate,
    se = h2_nw_main$se,
    p = h2_nw_main$p,
    ci_lower = h2_nw_main$ci_lower,
    ci_upper = h2_nw_main$ci_upper,
    OR = NA_real_,
    OR_ci_lower = NA_real_,
    OR_ci_upper = NA_real_,
    decision = make_decision(h2_nw_main$estimate, h2_nw_main$p, direction = "negative")
  ),
  tibble(
    hypothesis = "H3",
    analysis = "gewichtet",
    effect = "Grand Mean > 0 im Mensch/OKI-Subset (einseitig)",
    estimate = h3_w_one$estimate,
    se = h3_w_one$se,
    p = h3_w_one$p_one_sided_gt0,
    ci_lower = h3_w_one$ci_lower,
    ci_upper = h3_w_one$ci_upper,
    OR = NA_real_,
    OR_ci_lower = NA_real_,
    OR_ci_upper = NA_real_,
    decision = make_decision(h3_w_one$estimate, h3_w_one$p_one_sided_gt0, direction = "positive")
  ),
  tibble(
    hypothesis = "H3",
    analysis = "ungewichtet",
    effect = "Grand Mean > 0 im Mensch/OKI-Subset (einseitig)",
    estimate = h3_nw_one$estimate,
    se = h3_nw_one$se,
    p = h3_nw_one$p_one_sided_gt0,
    ci_lower = h3_nw_one$ci_lower,
    ci_upper = h3_nw_one$ci_upper,
    OR = NA_real_,
    OR_ci_lower = NA_real_,
    OR_ci_upper = NA_real_,
    decision = make_decision(h3_nw_one$estimate, h3_nw_one$p_one_sided_gt0, direction = "positive")
  )
)

decision_table_moderation <- bind_rows(
  tibble(
    hypothesis = "H1",
    analysis = "gewichtet",
    effect = "Design-Check: Interaktion KI × Veracity",
    estimate = h1_w_int$estimate,
    se = h1_w_int$se,
    p = h1_w_int$p,
    ci_lower = h1_w_int$ci_lower,
    ci_upper = h1_w_int$ci_upper,
    OR = h1_w_int$OR,
    OR_ci_lower = h1_w_int$OR_ci_lower,
    OR_ci_upper = h1_w_int$OR_ci_upper,
    decision = ifelse(!is.na(h1_w_int$p) & h1_w_int$p < 0.05, "Interaktion signifikant", "keine Evidenz für Interaktion")
  ),
  tibble(
    hypothesis = "H1",
    analysis = "ungewichtet",
    effect = "Design-Check: Interaktion KI × Veracity",
    estimate = h1_nw_int$estimate,
    se = h1_nw_int$se,
    p = h1_nw_int$p,
    ci_lower = h1_nw_int$ci_lower,
    ci_upper = h1_nw_int$ci_upper,
    OR = h1_nw_int$OR,
    OR_ci_lower = h1_nw_int$OR_ci_lower,
    OR_ci_upper = h1_nw_int$OR_ci_upper,
    decision = ifelse(!is.na(h1_nw_int$p) & h1_nw_int$p < 0.05, "Interaktion signifikant", "keine Evidenz für Interaktion")
  ),
  tibble(
    hypothesis = "H2",
    analysis = "gewichtet",
    effect = "Design-Check: Interaktion KI × Veracity",
    estimate = h2_w_int$estimate,
    se = h2_w_int$se,
    p = h2_w_int$p,
    ci_lower = h2_w_int$ci_lower,
    ci_upper = h2_w_int$ci_upper,
    OR = NA_real_,
    OR_ci_lower = NA_real_,
    OR_ci_upper = NA_real_,
    decision = ifelse(!is.na(h2_w_int$p) & h2_w_int$p < 0.05, "Interaktion signifikant", "keine Evidenz für Interaktion")
  ),
  tibble(
    hypothesis = "H2",
    analysis = "ungewichtet",
    effect = "Design-Check: Interaktion KI × Veracity",
    estimate = h2_nw_int$estimate,
    se = h2_nw_int$se,
    p = h2_nw_int$p,
    ci_lower = h2_nw_int$ci_lower,
    ci_upper = h2_nw_int$ci_upper,
    OR = NA_real_,
    OR_ci_lower = NA_real_,
    OR_ci_upper = NA_real_,
    decision = ifelse(!is.na(h2_nw_int$p) & h2_nw_int$p < 0.05, "Interaktion signifikant", "keine Evidenz für Interaktion")
  )
)

decision_summary_primary <- decision_table_main %>%
  filter(analysis == "gewichtet") %>%
  transmute(
    hypothesis,
    primary_effect = effect,
    estimate,
    se,
    p,
    ci_lower,
    ci_upper,
    OR,
    OR_ci_lower,
    OR_ci_upper,
    decision
  )

# ------------------------------------------------------------
# 9) Ausgabe
# ------------------------------------------------------------

cat("\n==============================\n")
cat("HYPOTHESENTESTS H1–H3\n")
cat("==============================\n\n")

cat("---- Missingness / Datenbasis ----\n")
print(conf_missing_report); cat("\n")

cat("---- H1 Haupttest: allgemeiner KI-Effekt auf Fake-Urteil (mit OR + 95%-KI) ----\n")
print(h1_main_w_robust_ci); cat("\n")
print(h1_main_nw_robust_ci); cat("\n")

cat("---- H1 Design-Check: Interaktion Source × Veracity (mit OR + 95%-KI) ----\n")
print(h1_mod_w_robust_ci); cat("\n")
print(h1_mod_nw_robust_ci); cat("\n")

cat("---- H1 Optional: KI-Effekt bei Fake-Stimuli aus Interaktionsmodell ----\n")
print(h1_mod_w_simple_fake_ci); cat("\n")
print(h1_mod_nw_simple_fake_ci); cat("\n")

cat("---- H2 Haupttest: allgemeiner KI-Effekt auf Confidence-Change (mit 95%-KI) ----\n")
print(h2_main_w_robust_ci); cat("\n")
print(h2_main_nw_robust_ci); cat("\n")

cat("---- H2 Design-Check: Interaktion Source × Veracity (mit 95%-KI) ----\n")
print(h2_mod_w_robust_ci); cat("\n")
print(h2_mod_nw_robust_ci); cat("\n")

cat("---- H2 Optional: KI-Effekt bei Fake-Stimuli aus Interaktionsmodell ----\n")
print(h2_mod_w_simple_fake_ci); cat("\n")
print(h2_mod_nw_simple_fake_ci); cat("\n")

cat("---- H3: Mensch/OKI-Subset -> Grand Mean > 0 ----\n")
print(h3_human_mean_w_robust_ci); cat("\n")
print(h3_human_mean_nw_robust_ci); cat("\n")
print(h3_w_one); cat("\n")
print(h3_nw_one); cat("\n")

cat("---- Decision Table ----\n")
print(decision_table_main); cat("\n")
print(decision_table_moderation); cat("\n")
print(decision_summary_primary); cat("\n")

########################################################################
################### Explorative Analysen, Part 1
########################################################################

# ============================================================
# EXPLORATIV 1 – FAKTOR: SWITCHER VS. STABIL
# ============================================================

cat("\n==============================\n")
cat("EXPLORATIV: TRANSITIONEN & SWITCHER\n")
cat("==============================\n\n")

cat("---- Wechselrate & Richtungsverteilung (post_defined; ungewichtet) ----\n\n")
cat("Wechselrate gesamt:\n"); print(change_rate_total); cat("\n")
cat("Richtungsverteilung:\n"); print(change_direction_total); cat("\n")

cat("---- Transition-Raten nach Source×Veracity ----\n\n")
cat("Gewichtet (Pfadgewichte):\n"); print(desc_transitions_w); cat("\n")
cat("Ungewichtet (Häufigkeitsbasis):\n"); print(desc_transitions_nw); cat("\n")

cat("---- Switcher-Sample ----\n\n")
cat("n_items:\n"); print(nrow(dat_conf_switch)); cat("\n")
cat("n_persons:\n"); print(dplyr::n_distinct(dat_conf_switch$id)); cat("\n\n")

cat("---- Switcher: Confidence-Change (Item-Ebene) ----\n\n")
cat("Gewichtet (mean + w_disp + w_sd_df):\n"); print(switch_conf_total_w); cat("\n")
cat("Ungewichtet (mean/sd + median/iqr):\n"); print(switch_conf_total_nw); cat("\n")

cat("Nach Source – gewichtet:\n"); print(switch_conf_by_source_w); cat("\n")
cat("Nach Source – ungewichtet:\n"); print(switch_conf_by_source_nw); cat("\n")

cat("Nach Source×Stimulus-Veracity – gewichtet:\n"); print(switch_conf_by_cond_w); cat("\n")
cat("Nach Source×Stimulus-Veracity – ungewichtet:\n"); print(switch_conf_by_cond_nw); cat("\n")

cat("Wechselrichtung (Pre->Post Urteil) nach Source×Stimulus-Veracity (ungewichtet; mit Zellflag):\n")
print(switch_dir_by_cond_nw); cat("\n")

cat("---- Switcher: Confidence-Change (Person-Ebene; pro Person aggregiert) ----\n\n")
cat("Gesamt – ungewichtet:\n"); print(switch_person_total_nw); cat("\n")
cat("Gesamt – gewichtet:\n"); print(switch_person_total_w); cat("\n")
cat("Nach Source – ungewichtet:\n"); print(switch_person_by_source_nw); cat("\n")
cat("Nach Source – gewichtet:\n"); print(switch_person_by_source_w); cat("\n")

# ------------------------------------------------------------
# Gemeinsamer Datensatz: Switcher vs. Stabil
# ------------------------------------------------------------

conf_switch_stable <- item_profile %>%
  filter(
    post_defined,
    pre_choice %in% c("real", "fake"),
    post_choice %in% c("real", "fake"),
    !is.na(pre_conf), !is.na(post_conf),
    !is.na(source), !is.na(veracity), !is.na(text_id)
  ) %>%
  left_join(id_w, by = "id") %>%
  mutate(
    status = ifelse(pre_choice != post_choice, "Switcher", "Stabil"),
    status = factor(status, levels = c("Stabil", "Switcher")),
    source_f = factor(source, levels = c("Mensch", "KI")),
    veracity_f = factor(veracity, levels = c("Real", "Fake")),
    conf_change = post_conf - pre_conf
  )

# ------------------------------------------------------------
# Switcher vs. Stabil – Gesamtvergleich
# ------------------------------------------------------------

conf_switch_stable_total_w <- conf_switch_stable %>%
  group_by(status) %>%
  summarise(
    n = n(),
    w_sum = sum(w, na.rm = TRUE),
    mean_pre_conf = weighted.mean(pre_conf, w, na.rm = TRUE),
    mean_post_conf = weighted.mean(post_conf, w, na.rm = TRUE),
    mean_conf_change = weighted.mean(conf_change, w, na.rm = TRUE),
    w_disp_pre = w_dispersion(pre_conf, w),
    w_disp_post = w_dispersion(post_conf, w),
    w_disp_change = w_dispersion(conf_change, w),
    w_sd_df_change = w_sd_df(conf_change, w),
    .groups = "drop"
  )

conf_switch_stable_total_nw <- conf_switch_stable %>%
  group_by(status) %>%
  summarise(
    n = n(),
    mean_pre_conf = mean(pre_conf, na.rm = TRUE),
    mean_post_conf = mean(post_conf, na.rm = TRUE),
    mean_conf_change = mean(conf_change, na.rm = TRUE),
    sd_pre_conf = sd(pre_conf, na.rm = TRUE),
    sd_post_conf = sd(post_conf, na.rm = TRUE),
    sd_conf_change = sd(conf_change, na.rm = TRUE),
    median_conf_change = median(conf_change, na.rm = TRUE),
    iqr_conf_change = IQR(conf_change, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------
# Switcher vs. Stabil – nach Source
# ------------------------------------------------------------

conf_switch_stable_by_source_w <- conf_switch_stable %>%
  group_by(source_f, status) %>%
  summarise(
    n = n(),
    n_flag_lt20 = flag_small_n_tbl(n, 20),
    w_sum = sum(w, na.rm = TRUE),
    mean_pre_conf = weighted.mean(pre_conf, w, na.rm = TRUE),
    mean_post_conf = weighted.mean(post_conf, w, na.rm = TRUE),
    mean_conf_change = weighted.mean(conf_change, w, na.rm = TRUE),
    w_disp_change = w_dispersion(conf_change, w),
    w_sd_df_change = w_sd_df(conf_change, w),
    .groups = "drop"
  )

conf_switch_stable_by_source_nw <- conf_switch_stable %>%
  group_by(source_f, status) %>%
  summarise(
    n = n(),
    n_flag_lt20 = flag_small_n_tbl(n, 20),
    mean_pre_conf = mean(pre_conf, na.rm = TRUE),
    mean_post_conf = mean(post_conf, na.rm = TRUE),
    mean_conf_change = mean(conf_change, na.rm = TRUE),
    sd_conf_change = sd(conf_change, na.rm = TRUE),
    median_conf_change = median(conf_change, na.rm = TRUE),
    iqr_conf_change = IQR(conf_change, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------
# Alte Vergleichstabellen beibehalten
# ------------------------------------------------------------

conf_change_by_switcher <- conf_switch_stable %>%
  filter(status == "Switcher") %>%
  summarise(
    n = n(),
    mean_pre_conf = mean(pre_conf, na.rm = TRUE),
    mean_post_conf = mean(post_conf, na.rm = TRUE),
    mean_conf_change = mean(conf_change, na.rm = TRUE),
    sd_pre_conf = sd(pre_conf, na.rm = TRUE),
    sd_post_conf = sd(post_conf, na.rm = TRUE),
    .groups = "drop"
  )

conf_change_stable <- conf_switch_stable %>%
  filter(status == "Stabil") %>%
  summarise(
    n = n(),
    mean_pre_conf = mean(pre_conf, na.rm = TRUE),
    mean_post_conf = mean(post_conf, na.rm = TRUE),
    mean_conf_change = mean(conf_change, na.rm = TRUE),
    sd_pre_conf = sd(pre_conf, na.rm = TRUE),
    sd_post_conf = sd(post_conf, na.rm = TRUE),
    .groups = "drop"
  )

combined_conf_change <- bind_rows(
  tibble(
    group = "Switcher",
    n = conf_change_by_switcher$n,
    mean_pre_conf = conf_change_by_switcher$mean_pre_conf,
    mean_post_conf = conf_change_by_switcher$mean_post_conf,
    mean_conf_change = conf_change_by_switcher$mean_conf_change,
    sd_pre_conf = conf_change_by_switcher$sd_pre_conf,
    sd_post_conf = conf_change_by_switcher$sd_post_conf
  ),
  tibble(
    group = "Stabile Urteile",
    n = conf_change_stable$n,
    mean_pre_conf = conf_change_stable$mean_pre_conf,
    mean_post_conf = conf_change_stable$mean_post_conf,
    mean_conf_change = conf_change_stable$mean_conf_change,
    sd_pre_conf = conf_change_stable$sd_pre_conf,
    sd_post_conf = conf_change_stable$sd_post_conf
  )
)

cat("---- Confidence im Pre- und Post-Urteil: Switcher vs. stabile Urteile ----\n\n")
print(conf_change_by_switcher); cat("\n")
print(conf_change_stable); cat("\n")
print(combined_conf_change); cat("\n")

cat("---- Switcher vs. Stabil: gewichteter Gesamtvergleich ----\n\n")
print(conf_switch_stable_total_w); cat("\n")

cat("---- Switcher vs. Stabil: ungewichteter Gesamtvergleich ----\n\n")
print(conf_switch_stable_total_nw); cat("\n")

cat("---- Switcher vs. Stabil nach Source – gewichtet ----\n\n")
print(conf_switch_stable_by_source_w); cat("\n")

cat("---- Switcher vs. Stabil nach Source – ungewichtet ----\n\n")
print(conf_switch_stable_by_source_nw); cat("\n")

cat("---- Switcher vs. Stabil nach Source×Veracity – gewichtet ----\n\n")
print(conf_switch_stable_by_cond_w); cat("\n")

cat("---- Switcher vs. Stabil nach Source×Veracity – ungewichtet ----\n\n")
print(conf_switch_stable_by_cond_nw); cat("\n")

m_conf_switch_w <- lm(
  conf_change_switch ~ source_f * veracity_f + switch_dir + factor(id) + factor(text_id),
  data = dat_conf_switch, weights = w
)

conf_switch_w_robust <- robust_test(m_conf_switch_w, dat_conf_switch$id)

cat("\n*************** Ergebnisse für Switcher-Confidence ***************\n")
print(conf_switch_w_robust)


########################################################################
################### Explorative Analysen, Part 2
########################################################################


cat("\n==============================\n")
cat("EXPLORATIV: PRE-CHOICE (CONFIDENCE-CHANGE NACH REAL/FAKE UND SWITCH/STABIL)\n")
cat("==============================\n\n")

# ------------------------------------------------------------
# Datensatz für Pre-Choice-Analyse
# ------------------------------------------------------------

conf_prechoice_status <- item_profile %>%
  filter(
    post_defined,
    pre_choice %in% c("real", "fake"),
    post_choice %in% c("real", "fake"),
    !is.na(pre_conf), !is.na(post_conf),
    !is.na(source), !is.na(veracity), !is.na(text_id)
  ) %>%
  left_join(id_w, by = "id") %>%
  mutate(
    status = ifelse(pre_choice != post_choice, "Switcher", "Stabil"),
    status = factor(status, levels = c("Stabil", "Switcher")),
    source_f = factor(source, levels = c("Mensch", "KI")),
    veracity_f = factor(veracity, levels = c("Real", "Fake")),
    conf_change = post_conf - pre_conf
  )

# ------------------------------------------------------------
# Gewichtete Auswertung: Pre-Choice × Status
# ------------------------------------------------------------

conf_change_by_prechoice_status_w <- conf_prechoice_status %>%
  group_by(pre_choice, status) %>%
  summarise(
    n = n(),
    n_flag_lt20 = flag_small_n_tbl(n, 20),
    w_sum = sum(w, na.rm = TRUE),
    mean_conf_change = weighted.mean(conf_change, w, na.rm = TRUE),
    w_disp_change = w_dispersion(conf_change, w),
    w_sd_df_change = w_sd_df(conf_change, w),
    .groups = "drop"
  ) %>%
  arrange(pre_choice, status)





# ------------------------------------------------------------
# Ungewichtete Sensitivitätsauswertung: Pre-Choice × Status
# ------------------------------------------------------------

conf_change_by_prechoice_status_nw <- conf_prechoice_status %>%
  group_by(pre_choice, status) %>%
  summarise(
    n = n(),
    n_flag_lt20 = flag_small_n_tbl(n, 20),
    mean_conf_change = mean(conf_change, na.rm = TRUE),
    sd_conf_change = sd(conf_change, na.rm = TRUE),
    median_conf_change = median(conf_change, na.rm = TRUE),
    iqr_conf_change = IQR(conf_change, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(pre_choice, status)

cat("---- Confidence-Change nach Pre-Choice × Status ----\n\n")
cat("Gewichtet:\n")
print(conf_change_by_prechoice_status_w); cat("\n")

cat("Ungewichtet:\n")
print(conf_change_by_prechoice_status_nw); cat("\n")






# ------------------------------------------------------------
# Optional: zusätzlich nach Source
# ------------------------------------------------------------

conf_change_by_prechoice_status_source_w <- conf_prechoice_status %>%
  group_by(source_f, pre_choice, status) %>%
  summarise(
    n = n(),
    n_flag_lt20 = flag_small_n_tbl(n, 20),
    w_sum = sum(w, na.rm = TRUE),
    mean_conf_change = weighted.mean(conf_change, w, na.rm = TRUE),
    w_disp_change = w_dispersion(conf_change, w),
    w_sd_df_change = w_sd_df(conf_change, w),
    .groups = "drop"
  ) %>%
  arrange(source_f, pre_choice, status)

cat("---- Confidence-Change nach Source × Pre-Choice × Status (gewichtet) ----\n\n")
print(conf_change_by_prechoice_status_source_w); cat("\n")

########################################################################
################### Explorative Analysen, Part 3
########################################################################

cat("\n==============================\n")
cat("EXPLORATIV: SOZIODEMOGRAFISCHE MODERATION DER CONFIDENCE-VERÄNDERUNG\n")
cat("==============================\n\n")

# ------------------------------------------------------------
# Datensatz für explorative Moderationsanalysen
# ------------------------------------------------------------

conf_demo <- item_profile %>%
  filter(
    post_defined,
    pre_choice %in% c("real", "fake"),
    post_choice %in% c("real", "fake"),
    !is.na(pre_conf), !is.na(post_conf),
    !is.na(source), !is.na(veracity), !is.na(text_id)
  ) %>%
  left_join(
    id_w %>% select(id, w),
    by = "id"
  ) %>%
  mutate(
    source_f = factor(source, levels = c("Mensch", "KI")),
    veracity_f = factor(veracity, levels = c("Real", "Fake")),
    status = ifelse(pre_choice != post_choice, "Switcher", "Stabil"),
    status = factor(status, levels = c("Stabil", "Switcher")),
    conf_change = post_conf - pre_conf
  )

# ------------------------------------------------------------
# Gewichtete Deskriptiven: Geschlecht × Pre-Choice
# ------------------------------------------------------------

gender_results_w <- conf_demo %>%
  group_by(gender_model_f, pre_choice) %>%
  summarise(
    n = n(),
    n_flag_lt20 = flag_small_n_tbl(n, 20),
    w_sum = sum(w, na.rm = TRUE),
    mean_conf_change = weighted.mean(conf_change, w, na.rm = TRUE),
    w_disp_change = w_dispersion(conf_change, w),
    w_sd_df_change = w_sd_df(conf_change, w),
    .groups = "drop"
  )

# ------------------------------------------------------------
# Gewichtete Deskriptiven: Bildung × Pre-Choice
# WICHTIG: Modellvariable edu_group_model_f
# ------------------------------------------------------------

edu_results_w <- conf_demo %>%
  filter(!is.na(edu_group_model_f)) %>%
  group_by(edu_group_model_f, pre_choice) %>%
  summarise(
    n = n(),
    n_flag_lt20 = flag_small_n_tbl(n, 20),
    w_sum = sum(w, na.rm = TRUE),
    mean_conf_change = weighted.mean(conf_change, w, na.rm = TRUE),
    w_disp_change = w_dispersion(conf_change, w),
    w_sd_df_change = w_sd_df(conf_change, w),
    .groups = "drop"
  )

# ------------------------------------------------------------
# Optional: ungewichtete Sensitivitäts-Deskriptiven
# ------------------------------------------------------------

gender_results_nw <- conf_demo %>%
  group_by(gender_model_f, pre_choice) %>%
  summarise(
    n = n(),
    n_flag_lt20 = flag_small_n_tbl(n, 20),
    mean_conf_change = mean(conf_change, na.rm = TRUE),
    sd_conf_change = sd(conf_change, na.rm = TRUE),
    median_conf_change = median(conf_change, na.rm = TRUE),
    iqr_conf_change = IQR(conf_change, na.rm = TRUE),
    .groups = "drop"
  )

edu_results_nw <- conf_demo %>%
  filter(!is.na(edu_group_model_f)) %>%
  group_by(edu_group_model_f, pre_choice) %>%
  summarise(
    n = n(),
    n_flag_lt20 = flag_small_n_tbl(n, 20),
    mean_conf_change = mean(conf_change, na.rm = TRUE),
    sd_conf_change = sd(conf_change, na.rm = TRUE),
    median_conf_change = median(conf_change, na.rm = TRUE),
    iqr_conf_change = IQR(conf_change, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------
# Modelle: Alter
# ------------------------------------------------------------

cat("---- Einfluss des Alters auf Confidence-Change ----\n\n")

m_age <- lm(
  conf_change ~ age_z + pre_choice + age_z * pre_choice +
    source_f + veracity_f + status + factor(text_id),
  data = conf_demo,
  weights = w
)

age_robust <- robust_test(m_age, conf_demo$id)
print(age_robust); cat("\n")

# ------------------------------------------------------------
# Modelle: Geschlecht
# ------------------------------------------------------------

cat("---- Einfluss des Geschlechts auf Confidence-Change ----\n\n")

m_gender <- lm(
  conf_change ~ gender_model_f + pre_choice + gender_model_f * pre_choice +
    source_f + veracity_f + status + factor(text_id),
  data = conf_demo,
  weights = w
)

gender_robust <- robust_test(m_gender, conf_demo$id)
print(gender_robust); cat("\n")

# ------------------------------------------------------------
# Modelle: Medienkonsum
# ------------------------------------------------------------

cat("---- Einfluss des Medienkonsums auf Confidence-Change ----\n\n")

m_media <- lm(
  conf_change ~ media_hours_z + pre_choice + media_hours_z * pre_choice +
    source_f + veracity_f + status + factor(text_id),
  data = conf_demo,
  weights = w
)

media_robust <- robust_test(m_media, conf_demo$id)
print(media_robust); cat("\n")

# ------------------------------------------------------------
# Modelle: Bildung
# WICHTIG: Modellvariable edu_group_model_f statt edu_group_f
# ------------------------------------------------------------

cat("---- Einfluss des Bildungsniveaus auf Confidence-Change ----\n\n")

m_edu <- lm(
  conf_change ~ edu_group_model_f + pre_choice + edu_group_model_f * pre_choice +
    source_f + veracity_f + status + factor(text_id),
  data = conf_demo,
  weights = w
)

edu_robust <- robust_test(m_edu, conf_demo$id)
print(edu_robust); cat("\n")

# ------------------------------------------------------------
# Gewichtete deskriptive Zusammenfassungen
# ------------------------------------------------------------

cat("\n==============================\n")
cat("GEWICHTETE DESKRIPTIVE ZUSAMMENFASSUNGEN\n")
cat("==============================\n\n")

cat("Ergebnisse zur Confidence-Veränderung nach Geschlecht und Pre-Choice:\n")
print(gender_results_w); cat("\n")

cat("Ergebnisse zur Confidence-Veränderung nach Bildungsniveau und Pre-Choice:\n")
print(edu_results_w); cat("\n")

# ------------------------------------------------------------
# Optional: ungewichtete Sensitivitäts-Deskriptiven
# ------------------------------------------------------------

cat("\n==============================\n")
cat("UNGEWICHTETE SENSITIVITÄTS-DESKRIPTIVEN\n")
cat("==============================\n\n")

cat("Geschlecht × Pre-Choice:\n")
print(gender_results_nw); cat("\n")

cat("Bildung × Pre-Choice:\n")
print(edu_results_nw); cat("\n")

cat("\n==============================\n")
cat("END OF EXPLORATIV: SOZIODEMOGRAFISCHE MODERATION\n")
cat("==============================\n\n")