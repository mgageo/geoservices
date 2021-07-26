# <!-- coding: utf-8 -->
# le catalogue Géoservices de l'IGN
#
# auteur : Marc Gauthier
# licence: Creative Commons Paternité - Pas d'Utilisation Commerciale - Partage des Conditions Initiales à l'Identique 2.0 France
#
## https://thinkr.fr/rvest/
varDir <- "geo/GEOSERVICES"
#
# enchainement des traitements
# source("geo/scripts/geoservices.R"); geoservices_jour()
geoservices_jour <- function(force = FALSE) {
  carp("début")
  geoservices_donnees(force = force)
  geoservices_web_experts(force = force)
  geoservices_web_wmts_capabilities(force = force)
  geoservices_web_wmts_capabilities_html(force = force)
  carp("fin")
}
#
# les données
# source("geo/scripts/geoservices.R"); geoservices_donnees()
geoservices_donnees <- function(catalogue = "catalogue", force = FALSE) {
  library(tidyverse)
  library(rvest)
  library(rio)
  carp("début")
  liens.df <- data.frame()
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
  df <- geoservices_catalogue(catalogue = "catalogue") %>%
    mutate(lien = gsub("^/", "", lien))
  for (i in 1:nrow(df)) {
    Lien <- df[i, "lien"]
    df1 <- geoservices_donnee(Lien, force = force) %>%
      mutate(service = Lien)
    liens.df <- rbind(liens.df, df1)
  }
  liens.df <- liens.df %>%
    filter(grepl("^ftp", lien))
  file <- sprintf("%s/donnees.csv", varDir)
  rio::export(liens.df, file)
  carp("fin")
}

geoservices_donnee <- function(lien, force = FALSE) {
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
#
# les services web experts
# source("geo/scripts/geoservices.R"); geoservices_web_experts()
geoservices_web_experts <- function(catalogue = "services-web-experts", force = FALSE) {
  library(tidyverse)
  library(rvest)
  library(rio)
  carp("début")
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
    filter(! grepl("calcul", lien))
  for (i in 1:nrow(df)) {
    Lien <- df[i, "lien"]
    df1 <- geoservices_web(Lien, force = force) %>%
      mutate(service = Lien)
    liens.df <- rbind(liens.df, df1)
  }
  glimpse(liens.df)
  file <- sprintf("%s/%s.csv", varDir, catalogue)
  rio::export(liens.df, file)
  carp("file: %s", file)
  carp("fin")

}
#
# !! une donnée peut être accessible par plusieurs services ogc
geoservices_web <- function(lien, force = FALSE) {
  library(tidyverse)
  library(rvest)
  print(sprintf("lien: %s", lien))
  html <- geoservices_get(lien)
  sections <<- html %>%
    html_nodes("div.field--items > div.field--item")
  tables.df <- data.frame()
  for (section in sections) {
    pre <- section %>%
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
# récupération d'une page sur le site avec mise en cache
geoservices_get <- function(lien = "catalogue", force = FALSE) {
  carp("lien: %s", lien)
  file <- sprintf("%s/%s.html", varDir, lien)
  if( ! file.exists(file) | force == TRUE) {
    url <- sprintf("https://geoservices.ign.fr/%s", lien)
    carp("lecture url: %s", url)
    try(download.file(url, file, mode = "wb"), silent = TRUE)
  }
  html <- read_html(file)
  return(invisible(html))
}
#
# génération d'une page html avec les services wmts
# source("geo/scripts/geoservices.R"); geoservices_web_wmts_html()
geoservices_web_wmts_html <- function(catalogue = "services-web-experts", force = FALSE) {
  library(tidyverse)
  library(janitor)
  library(rio)
  Format <- "%sGetTile&VERSION=1.0.0&LAYER=%s&STYLE=normal&TILEMATRIXSET=PM&TILEMATRIX=13&TILEROW=2845&TILECOL=4059&FORMAT=image/jpeg"
  FormatImg <- '<h3>%s</h3><img src="%s" alt="%s">'
  dsn <- sprintf("%s/%s.csv", varDir, catalogue)
  df <- rio::import(dsn, encoding = "UTF-8") %>%
    filter(grepl("wmts", url)) %>%
    clean_names() %>%
    arrange(service, nom_technique) %>%
    mutate(url = gsub("[^=]*$", "", url, perl = TRUE)) %>%
    mutate(request = sprintf(Format, url, nom_technique)) %>%
    mutate(img = sprintf(FormatImg, donnee, request, donnee)) %>%
    glimpse()
  html <- paste(df$img, collapse="<br>\n")
  print(html)
  dsn <- sprintf("%s/%s.html", varDir, catalogue)
  write(html, dsn)
}
#
# interrogation des services wmts
# source("geo/scripts/geoservices.R"); geoservices_web_wmts_capabilities()
geoservices_web_wmts_capabilities <- function(catalogue = "services-web-experts", force = FALSE) {
  library(tidyverse)
  library(janitor)
  library(rio)
  carp("les services wmts")
  Format <- "%sGetTile&VERSION=1.0.0&LAYER=%s&STYLE=normal&TILEMATRIXSET=PM&TILEMATRIX=13&TILEROW=2845&TILECOL=4059&FORMAT=image/jpeg"
  FormatImg <- '<h3>%s</h3><img src="%s" alt="%s">'
  dsn <- sprintf("%s/%s.csv", varDir, catalogue)
  df0 <- rio::import(dsn, encoding = "UTF-8")
  df <- df0 %>%
    filter(grepl("wmts", url)) %>%
# pb sur Données "cartes" en Vecteur Tuilé
    filter(grepl("request", url, ignore.case = TRUE)) %>%
    filter(! grepl("altimetrie", url, ignore.case = TRUE)) %>%
#    filter(grepl("clc", url, ignore.case = TRUE)) %>%
    clean_names() %>%
    group_by(service, url) %>%
    summarize(nb = n()) %>%
    glimpse()
  carp("les services wmts sans request dans l'url")
  df0 %>%
    filter(!grepl("request", url, ignore.case = TRUE)) %>%
    glimpse()
  capabilities.df <- data.frame()
  for (i in 1:nrow(df)) {
    x <- geoservices_capabilities_get(df[i, ], force = force)
    df1 <- geoservices_capabilities_parse_xml(x, force = force) %>%
      mutate(url = df[[i, "url"]])
    capabilities.df <- bind_rows(capabilities.df, df1)
  }
  cols <- gsub("^.*:", "", colnames(capabilities.df))
  colnames(capabilities.df) <- cols
  glimpse(capabilities.df)
  dsn <- sprintf("%s/%s_capabilities.csv", varDir, catalogue)
  rio::export(capabilities.df, dsn)
}
#
# génération d'une page html avec les services wmts
# source("geo/scripts/geoservices.R"); geoservices_web_wmts_capabilities_html()
geoservices_web_wmts_capabilities_html <- function(catalogue = "services-web-experts", force = FALSE) {
  library(tidyverse)
  library(janitor)
  library(rio)
  FormatRequest <- "%sGetTile&VERSION=1.0.0&LAYER=%s&STYLE=%s&TILEMATRIXSET=%s&TILEMATRIX=13&TILEROW=2845&TILECOL=4059&FORMAT=%s"
  FormatImg <- '<h3>%s</h3><img src="%s" alt="%s">'
  dsn <- sprintf("%s/%s_capabilities.csv", varDir, catalogue)
  df <- rio::import(dsn, encoding = "UTF-8") %>%
    glimpse() %>%
    filter(TileMatrix == "13") %>%
    mutate(url = gsub("[^=]*$", "", url, perl = TRUE)) %>%
    mutate(request = sprintf(FormatRequest, url, Identifier, style, TileMatrixSet, Format)) %>%
    mutate(img = sprintf(FormatImg, Title, request, Title))
  html <- paste(df$img, collapse="<br>\n")
  dsn <- sprintf("%s/%s_capabilities.html", varDir, catalogue)
  write(html, dsn)
  carp("dsn: %s", dsn)
}
#
# récupération d'une page sur le site avec mise en cache
geoservices_capabilities_get <- function(df, force = FALSE) {
  library(xml2)
  url <- df[[1, "url"]]
  file <- sprintf("%s/%s.xml", varDir, df[1, "service"])
  carp("file: %s", file)
  if( ! file.exists(file) | force == TRUE) {
    carp("lecture url: %s", url)
    try(download.file(url, file, mode = "wb"), silent = FALSE)
  }
  x <- read_xml(file, useInternal = TRUE, getDTD = FALSE)
  carp("namespace: %s", xml_ns(x))
#  x <- read_html(file)
  return(invisible(x))
}

#
# https://urbandatapalette.com/post/2021-03-xml-dataframe-r/
geoservices_capabilities_parse_html <- function(x, force = FALSE) {
  library(xml2)
  library(rvest)
  carp()
  layerNodes <<- xml_find_all(x, ".//layer")
  df <- data.frame()
  variables <- c("title", "identifier", "format", "tilematrixset", "wgs84boundingbox")
  for (layerNode in layerNodes) {
    df1 <- data.frame()
    for (v in variables) {
      xp <- sprintf(".//%s", v)
      t <- xml_find_first(layerNode, xp) %>%
        xml_text()
      df1[1, v] <- t
    }
    carp("identifier: %s", df1[1, "identifier"])
    style <<- xml_find_first(layerNode, ".//style")
    identifier <<- xml_text(xml_find_first(style, ".//identifier"))
    stop("****")
    tmls <- xml_find_all(layerNode, ".//tilematrixlimits")
    tmls.df <- data.frame()
    for(tml in tmls) {
      tml.df <- geoservices_xml2df(tml)
      tmls.df <- rbind(tmls.df, tml.df)
    }
    tmls.df <- tmls.df %>%
      mutate(identifier = df1[[1, "identifier"]])
    df1 <- df1 %>%
      left_join(tmls.df, by = c("identifier"))
    df <- rbind(df, df1)
  }
  return(invisible(df))
}
#
# transformation de l'xml en dataframe
# deux solutions :
# - read_xml : assez complexe
# - read_html : normalement plus simple
# !! mais pb avec style
#
# https://urbandatapalette.com/post/2021-03-xml-dataframe-r/
# read_xml
# les namespaces par défaut sont d1, d2
geoservices_capabilities_parse_xml <- function(x, force = FALSE) {
  library(xml2)
  carp()
  layerNodes <- xml_find_all(x, ".//d1:Layer")
  variables <- c("ows:Title", "ows:Identifier", "d1:Format", "d1:TileMatrixSet", "ows:WGS84BoundingBox")
  df <- data.frame()
  i <- 0
  for (layerNode in layerNodes) {
    df1 <- data.frame()
    for (v in variables) {
      xp <- sprintf(".//%s", v)
      t <- xml_find_first(layerNode, xp) %>%
        xml_text()
      df1[1, v] <- t
    }
    style <- xml_find_first(layerNode, ".//d1:Style")
    if (purrr::is_empty(style)) {
     st <- "normal"
    } else {
      st <- xml_text(xml_find_first(style, ".//ows:Identifier"))
    }
    df1[1, "style"] <- st
#    stop("****")
    tmls <- xml_find_all(layerNode, ".//d1:TileMatrixLimits")
    tmls.df <- data.frame()
    for(tml in tmls) {
      tml.df <- geoservices_xml2df(tml)
      tmls.df <- rbind(tmls.df, tml.df)
    }
    tmls.df <- tmls.df %>%
      mutate(identifier = df1[[1, "ows:Identifier"]])
    df1 <- df1 %>%
      left_join(tmls.df, by = c("ows:Identifier" = "identifier"))
    df <- rbind(df, df1)
  }
  return(invisible(df))
}
#
# transformation en dataframe des enfants d'un noeud
geoservices_xml2df <- function(x, force = FALSE) {
  fieldnames <- xml_name(xml_children(x))
  fields <- xml_text(xml_children(x))
  df  <-  data.frame(matrix(nrow = 0, ncol = length(fieldnames)))
  df[1, ] <- fields
  colnames(df) <- fieldnames
  return(invisible(df))
}
#
# ======================================================================================
#
# comme en perl
carp <- function(...) {
  curcall <- as.character(deparse(sys.call(-1)))
  arguments <- as.list(match.call(expand.dots=FALSE))[-1]
  arguments <- arguments$...
  msg <- ""
  if (length(arguments) >= 1 ) {
    msg <- sprintf(...)
  }
  print(sprintf("%s %s", curcall, msg))
  flush.console()
}