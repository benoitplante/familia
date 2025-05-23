library(chromote)
library(rvest)
library(dplyr)
library(stringr)
library(tibble)
library(jsonlite)

# 🔍 Fonction d'extraction à partir des balises <input name="numeroNotice">
extract_projects <- function(page_html) {
  page <- read_html(page_html)
  inputs <- page %>% html_elements("input[name='numeroNotice']")

  json_list <- inputs %>%
    html_attr("value") %>%
    lapply(fromJSON)

  df <- bind_rows(json_list) %>%
    transmute(
      titre = Titre,
      auteurs = Auteurs,
      annee = date,
      type_doc = TypeDocument,
      mots_cles = MotsCles,
      thematiques = Thematiques,
      disciplines = Disciplines,
      types_document = TypesDocs,
      sommaire = Sommaire,
      reference = Notice,
      revue = T2,
      volume = VL,
      numero = IS,
      pages = SP,
      url = URL
    )

  return(df)
}

# 🔁 Fonction de scraping multi-pages
scrape_all_pages <- function(base_url) {
  b <- ChromoteSession$new()
  b$Page$navigate(base_url)
  b$Page$loadEventFired()
  Sys.sleep(4)

  all_data <- list()
  page_num <- 1

  repeat {
    message("📄 Chargement de la page ", page_num, "...")

    tryCatch({
      b$Runtime$evaluate(
        expression = "
        new Promise(resolve => {
          const waitForResults = () => {
            const items = document.querySelectorAll('input[name=\\'numeroNotice\\']');
            if (items.length > 0) {
              resolve('ok');
            } else {
              setTimeout(waitForResults, 500);
            }
          };
          waitForResults();
        });
        "
      )
    }, error = function(e) {
      message("❌ Timeout lors de l’attente des données.")
    })

    Sys.sleep(1)

    html <- b$DOM$getDocument()
    node_id <- html$root$nodeId
    html_content <- b$DOM$getOuterHTML(nodeId = node_id)$outerHTML

    projects <- extract_projects(html_content)
    all_data[[page_num]] <- projects
    message("✅ Page ", page_num, " récupérée.")

    next_url <- tryCatch({
      b$Runtime$evaluate(
        expression = "
        (function() {
          const nextBtn = document.querySelector('a[data-ci-pagination-page][rel=\"next\"]');
          return nextBtn ? nextBtn.href : null;
        })();
        "
      )$result$value
    }, error = function(e) {
      message("⚠️ Impossible de détecter l’URL suivante."); NULL
    })

    if (is.null(next_url)) {
      message("📌 Fin : plus de page suivante.")
      break
    }

    tryCatch({
      message("➡️ Navigation vers : ", next_url)
      b$Page$navigate(next_url)
      b$Page$loadEventFired()
      Sys.sleep(5)
    }, error = function(e) {
      message("❌ Échec navigation vers : ", next_url)
      break
    })

    page_num <- page_num + 1
  }

  b$close()
  final_data <- bind_rows(all_data)
  return(final_data)
}

# ▶️ Lancer le scraping
base_url <- "https://familia.ucs.inrs.ca/resultat-de-recherche/?discipline[]=438"
raw_data <- scrape_all_pages(base_url)

# 👁️ Aperçu brut
print(head(raw_data, 5), width = Inf)

# 💾 (optionnel) Exporter les résultats
write.csv(raw_data, "projets_familia_complet.csv", row.names = FALSE)
