# <!-- coding: utf-8 -->
# le catalogue Géoservices de l'IGN
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
#
## https://thinkr.fr/rvest/
varDir <- "d:/tmp"
#
# récupération d'une page sur le site avec mise en cache
geoservices_get <- function(lien = "catalogue", force = FALSE) {
  url <- sprintf("https://geoservices.ign.fr/%s", lien)
  file <- sprintf("%s/%s.html", varDir, lien)
  if( ! file.exists(file) | force == TRUE) {
    carp("lecture url: %s", url)
    try(download.file(url, file, mode = "wb"), silent = TRUE)
  }
  url <- file
  html <- read_html(url)
  return(invisible(html))
}
#
# le catalogue
geoservices_catalogue <- function(catalogue = "catalogue", force = FALSE) {
  library(tidyverse)
  library(rvest)
  html <- geoservices_get(catalogue, force = force)
  links <- html %>%
    html_nodes("div.elemTtr") %>%
    html_nodes("a")
  textes <- links %>%
    html_text()
  liens <- links %>%
    html_attr("href")
  df <- data.frame(lien = liens, texte = textes) %>%
    glimpse()
}
#
# les services
# source("geo/scripts/geoservices.R"); geoservices_services()
geoservices_services <- function(force = FALSE) {
  library(tidyverse)
  library(rvest)
  library(rio)
  liens.df <- data.frame()
  df <- geoservices_catalogue(catalogue = "catalogue") %>%
    mutate(lien = gsub("^/", "", lien))
  for (i in 1:nrow(df)) {
    Lien <- df[i, "lien"]
    df1 <- geoservices_service(Lien, force = force) %>%
      mutate(service = Lien)
    liens.df <- rbind(liens.df, df1)
  }
  glimpse(liens.df)
  file <- sprintf("%s/liens.csv", varDir)
  rio::export(liens.df, file)
}
geoservices_service <- function(lien, force = FALSE) {
  library(tidyverse)
  library(rvest)
  html <<- geoservices_get(lien)
  links <- html %>%
    html_nodes("div.field--item") %>%
    html_nodes("a")
  liens <- links %>%
    html_attr("href")
  df <- data.frame(lien = liens)
}
geoservices_web <- function(lien, force = FALSE) {
  library(tidyverse)
  library(rvest)
  print(sprintf("lien: %s", lien))
  html <- geoservices_get(lien)
  sections <<- html %>%
    html_nodes("div.field--items > div.field--item")
  tables.df <- data.frame()
  for (section in sections) {
    pre <<- section %>%
      html_nodes("code") %>%
      html_text()
    tables.list <- section %>%
      html_nodes("table") %>%
      html_table()
    t.df <- bind_rows(tables.list)
    for (p in pre) {
      t.df <- t.df %>%
        mutate(url = p)
      tables.df <- bind_rows(tables.df, t.df)
    }
  }
  return(invisible(tables.df))
}
#
# les services web experts
# source("geo/scripts/geoservices.R"); geoservices_web_experts()
geoservices_web_experts <- function(catalogue = "services-web-experts", force = FALSE) {
  library(tidyverse)
  library(rvest)
  library(rio)
  liens.df <- data.frame()
  html <- geoservices_get(catalogue, force = force)
  links <- html %>%
    html_nodes("div.field--item") %>%
    html_nodes("a")
  textes <- links %>%
    html_text()
  liens <- links %>%
    html_attr("href")
  df <- data.frame(lien = liens, texte = textes) %>%
    mutate(lien = gsub("^/", "", lien)) %>%
    filter(! grepl("calcul", lien)) %>%
    glimpse()
  for (i in 1:nrow(df)) {
    Lien <- df[i, "lien"]
    df1 <- geoservices_web(Lien, force = force) %>%
      mutate(service = Lien)
    liens.df <- rbind(liens.df, df1)
  }
  glimpse(liens.df)
  file <- sprintf("%s/%s.csv", varDir, catalogue)
  rio::export(liens.df, file)
  print(sprintf("file: %s", file))
}