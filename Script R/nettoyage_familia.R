# Chargement des librairies pour le nettoyage et la préparation
library(tidyverse)
library(tidytext)
library(text)
library(naniar)
library(xml2)

# Chargement des données exportées
df <- read_csv("database/df_projets_familia.csv.csv")

# 3. Analyse des données manquantes
missing_summary <- df %>%
  summarise(across(everything(), ~mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "proportion_na") %>%
  arrange(desc(proportion_na))

# 1. Nettoyage des mots-clés
df <- df %>%
  mutate(mots_cles = mots_cles %>%
           str_to_lower() %>%
           str_replace_all("\\s+", " ") %>%
           str_trim())

# 2. Nettoyage des auteurs
df <- df %>%
  mutate(auteurs = auteurs %>%
           str_replace_all("\\s+", " ") %>%
           str_to_title() %>%
           str_trim())

# 4. Normalisation des dates
df <- df %>%
  mutate(annee_extrait = str_extract(annee, "\\d{4}"),
         annee_extrait = as.integer(annee_extrait))

# 5. Standardisation des types de documents
df <- df %>%
  mutate(type_doc_std = case_when(
    str_detect(type_doc, regex("th[èe]se|mémoire", ignore_case = TRUE)) ~ "Thèse/Mémoire",
    str_detect(type_doc, regex("rapport", ignore_case = TRUE)) ~ "Rapport",
    str_detect(type_doc, regex("article", ignore_case = TRUE)) ~ "Article",
    TRUE ~ "Autre"
  ))

# 6. Nettoyage des titres
df <- df %>%
  mutate(titre_clean = titre %>%
           str_to_lower() %>%
           str_replace_all("[\"'’]", "") %>%
           str_squish())

# 7. Nettoyage HTML dans les champs de sommaire
df <- df %>%
  mutate(sommaire_clean = map_chr(sommaire, ~{
    if (is.na(.x) || .x == "") return(NA_character_)
    as.character(xml2::xml_text(xml2::read_html(paste0("<body>", .x, "</body>"))))
  }))

# 8. Extraction des composantes méthodologiques et du résumé

df <- df %>%
  mutate(
    methode_instruments = str_extract(sommaire_clean, regex("Instruments\\s*:\\s*(.*?)\\s*Type\\s*de\\s*traitement", ignore_case = TRUE)) %>%
      str_remove_all(regex("Instruments\\s*:\\s*|Type\\s*de\\s*traitement", ignore_case = TRUE)) %>%
      str_trim(),

    methode_analyse = str_extract(sommaire_clean, regex("Type de traitement des donn[ée]es\\s*:\\s*([^\\n\\.]*)", ignore_case = TRUE, multiline = TRUE)) %>%
      str_replace(regex("Type de traitement des donn[ée]es\\s*:\\s*", ignore_case = TRUE), "") %>%
      str_remove("\\s*3\\s*$")
  )

# 9. Sélection des colonnes pertinentes pour la suite
df <- df %>%
  select(titre_clean, auteurs, annee_extrait, type_doc_std, mots_cles, methode_instruments, methode_analyse)

write.csv(df, "database/df_projets_familia_finale.csv", row.names = FALSE)
