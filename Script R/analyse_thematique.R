# Charger les librairies nécessaires
library(tidyverse)
library(broom)
library(ggplot2)

# Importer les données
df <- read_csv("df_projets_familia_finale.csv")

# Filtrer les données entre 2000 et 2023
df_filtered <- df %>%
  filter(annee_extrait >= 2000 & annee_extrait <= 2023)

# Compter le nombre d’articles par année
articles_per_year <- df_filtered %>%
  count(annee_extrait, name = "n_articles")

# Régression linéaire
lm_model <- lm(n_articles ~ annee_extrait, data = articles_per_year)
summary(lm_model)

# Régression de Poisson
poisson_model <- glm(n_articles ~ annee_extrait, family = poisson(), data = articles_per_year)
summary(poisson_model)

# Prédictions des deux modèles
articles_per_year <- articles_per_year %>%
  mutate(
    pred_lm = predict(lm_model),
    pred_poisson = predict(poisson_model, type = "response")
  )

# Graphique comparatif
ggplot(articles_per_year, aes(x = annee_extrait, y = n_articles)) +
  geom_point(color = "blue") +
  geom_line(aes(y = pred_lm), color = "red", linetype = "dashed") +
  geom_line(aes(y = pred_poisson), color = "green") +
  labs(
    title = "Évolution du nombre d'articles (2000–2023)",
    subtitle = "Régression linéaire (rouge pointillé) vs Poisson (vert)",
    x = "Année",
    y = "Nombre d'articles"
  ) +
  theme_minimal()

# ---- Suite : Analyse par thématique ----

# Filtrer les données avec thématiques valides
df_thematiques <- df_filtered %>%
  filter(!is.na(thematiques_clean))

# 1. Tableau du nombre d’articles par thématique et par année
table_thematiques <- df_thematiques %>%
  count(annee_extrait, thematiques_clean, name = "n_articles") %>%
  pivot_wider(names_from = thematiques_clean, values_from = n_articles, values_fill = 0)

# 2. Graphique du nombre d’articles par thématique
df_thematiques %>%
  count(annee_extrait, thematiques_clean) %>%
  ggplot(aes(x = annee_extrait, y = n, color = thematiques_clean)) +
  geom_line() +
  geom_point() +
  labs(title = "Nombre d'articles par thématique (2000–2023)",
       x = "Année", y = "Nombre d'articles", color = "Thématique") +
  theme_minimal()

# ---- Analyse de la proportion relative ----

# 3. Calcul des proportions par thématique
df_prop <- df_thematiques %>%
  count(annee_extrait, thematiques_clean, name = "n") %>%
  group_by(annee_extrait) %>%
  mutate(total = sum(n),
         proportion = n / total) %>%
  ungroup()

# 4. Graphique des proportions relatives
df_prop %>%
  ggplot(aes(x = annee_extrait, y = proportion, color = thematiques_clean)) +
  geom_line() +
  geom_point() +
  labs(title = "Proportion relative des thématiques (2000–2023)",
       x = "Année", y = "Proportion", color = "Thématique") +
  theme_minimal()

# ---- Régression binomiale pondérée pour tendances ----

# Centrage de l'année
df_prop <- df_prop %>%
  mutate(year_centered = annee_extrait - mean(annee_extrait))

# Régression binomiale pondérée par thématique
results <- df_prop %>%
  group_by(thematiques_clean) %>%
  do(tidy(glm(cbind(n, total - n) ~ year_centered,
              data = ., family = "binomial"))) %>%
  filter(term == "year_centered") %>%
  mutate(tendance = if_else(estimate > 0, "hausse", "baisse")) %>%
  arrange(p.value)

# 5. Graphique des tendances significatives
results %>%
  filter(p.value < 0.05) %>%
  ggplot(aes(x = estimate, y = reorder(thematiques_clean, estimate), fill = tendance)) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Tendance des proportions thématiques (2000–2023)",
       x = "Coefficient de tendance", y = "Thématique") +
  scale_fill_manual(values = c("hausse" = "forestgreen", "baisse" = "firebrick")) +
  theme_minimal()

# ---- Analyse de stabilité des thématiques ----

# Calcul du coefficient de variation (CV) par thématique
volatilite_thematiques <- df_prop %>%
  group_by(thematiques_clean) %>%
  summarise(
    moyenne_prop = mean(proportion, na.rm = TRUE),
    ecart_type_prop = sd(proportion, na.rm = TRUE),
    cv_prop = ecart_type_prop / moyenne_prop
  ) %>%
  arrange(desc(cv_prop))

# Visualisation de la volatilité
volatilite_thematiques %>%
  ggplot(aes(x = cv_prop, y = reorder(thematiques_clean, cv_prop))) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Volatilité des thématiques (2000–2023)",
    subtitle = "Calculée à partir du coefficient de variation (CV)",
    x = "Coefficient de variation (CV)",
    y = "Thématique"
  ) +
  theme_minimal()

