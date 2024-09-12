# Script revised from https://github.com/jhelvy/jhelvy_quarto/raw/main/_common.R
library(htmltools)
library(stringr)
library(dplyr)
library(readr)
library(fontawesome)

knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.retina = 3,
  comment = "#>"
)

get_pubs <- function() {
  pubs <- gsheet::gsheet2tbl(
    url = 'https://docs.google.com/spreadsheets/d/1eCDMQyzpL4fll3f4Co2UF4biDvxvEbaFJ8_8x03WYZA')
  pubs <- make_citations(pubs)
  pubs$summary <- ifelse(is.na(pubs$summary), FALSE, pubs$summary)
  pubs$stub <- make_stubs(pubs)
  pubs$url_summary <- file.path('research', pubs$stub, "index.html")
  return(pubs)
}

make_citations <- function(pubs) {
  pubs$citation <- unlist(lapply(split(pubs, 1:nrow(pubs)), make_citation))
  return(pubs)
}

make_citation <- function(pub) {
  if (!is.na(pub$journal)) {
    pub$journal <- glue::glue('_{pub$journal}_.')
  }
  if (!is.na(pub$number)) {
    pub$number <- glue::glue('{pub$number}.')
  }
  if (!is.na(pub$doi)) {
    pub$doi <- make_doi(pub$doi)
  }
  pub$year <- glue::glue("({pub$year})")
  pub$title <- glue::glue('"{pub$title}"')
  pub[,which(is.na(pub))] <- ''
  return(paste(
    pub$author, pub$year, pub$title, pub$journal, 
    pub$number, pub$doi
  ))
}

make_doi <- function(doi) {
  return(glue::glue('DOI: [{doi}](https://doi.org/{doi})'))
}

make_stubs <- function(pubs) {
  journal <- str_to_lower(pubs$journal)
  journal <- str_replace_all(journal, ':', '')
  journal <- str_replace_all(journal, '`', '')
  journal <- str_replace_all(journal, "'", '')
  journal <- str_replace_all(journal, "\\.", '')
  journal <- str_replace_all(journal, "&", '')
  journal <- str_replace_all(journal, ',', '')
  journal <- str_replace_all(journal, '  ', '-')
  journal <- str_replace_all(journal, ' ', '-')
  return(paste0(pubs$year, '-', journal))
}

make_pub_list <- function(pubs, category) {
  x <- pubs[which(pubs$category == category),]
  pub_list <- list()
  for (i in 1:nrow(x)) {
    pub_list[[i]] <- make_pub(x[i,], index = i)
  }
  return(htmltools::HTML(paste(unlist(pub_list), collapse = "")))
}

make_pub <- function(pub, index = NULL) {
  header <- FALSE
  if (is.null(index)) {
    cite <- pub$citation
    icons <- make_icons(pub)
  } else {
    cite <- glue::glue('{index}) {pub$citation}')
    icons <- glue::glue('<ul style="list-style: none;"><li>{make_icons(pub)}</li></ul>')
    if (index == 1) { header <- TRUE }
  }
  # return(markdown_to_html(cite))
  return(htmltools::HTML(glue::glue(
    '<div class="pub">
    <div class="grid">
    <div class="g-col-11"> {markdown_to_html(cite)} </div>
    </div>
    </div>
    {icons}
    </div>'
  )))
}

markdown_to_html <- function(text) {
  if (is.null(text)) { return(text) }
  
  # Replace the author names with underlined last names
  text <- gsub(
    pattern = "\\\\\\*([^,]+), ([^,]+)", 
    replacement = "<u>\\\\*\\1</u>, \\2", 
    text
  )
  text <- gsub(
    pattern = "\\\\\\*\\\\\\*([^,]+), ([^,]+)", 
    replacement = "<u>\\\\*\\\\*\\1</u>, \\2", 
    text
  )
  
  # Render the text as HTML
  return(HTML(markdown::renderMarkdown(text = text)))
}

make_icons <- function(pub) {
  html <- c()
  if (!is.na(pub$url_pub)) {
    html <- c(html, as.character(icon_link(
      icon = "fas fa-external-link-alt",
      text = "View",
      url  = pub$url_pub
    )))
  }
  if (!is.na(pub$url_pdf)) {
    html <- c(html, as.character(icon_link(
      icon = "fa fa-file-pdf",
      text = "PDF",
      url  = pub$url_pdf
    )))
  }
  if (!is.na(pub$url_repo)) {
    html <- c(html, as.character(icon_link(
      icon = "fab fa-github",
      text = "Code & Data",
      url  = pub$url_repo
    )))
  }
  if (!is.na(pub$url_other)) {
    html <- c(html, as.character(icon_link(
      icon = "fas fa-external-link-alt",
      text = pub$other_label,
      url  = pub$url_other
    )))
  }
  return(paste(html, collapse = ""))
}

# The icon_link() function is in {distilltools}, but I've modified this
# one to include  a custom class to be able to have more control over the
# CSS and an optional target argument

icon_link <- function(
    icon = NULL,
    text = NULL,
    url = NULL,
    class = "icon-link",
    target = "_blank"
) {
  if (!is.null(icon)) {
    text <- make_icon_text(icon, text)
  }
  return(htmltools::a(
    href = url, text, class = class, target = target, rel = "noopener"
  ))
}

make_icon_text <- function(icon, text) {
  return(HTML(paste0(make_icon(icon), " ", text)))
}

make_icon <- function(icon) {
  return(tag("i", list(class = icon)))
}

last_updated <- function() {
  return(span(
    paste0(
      'Last updated on ',
      format(Sys.Date(), format="%B %d, %Y")
    ),
    style = "font-size:0.8rem;")
  )
}
