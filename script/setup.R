

#############################################
## The following loads the needed packages ##
#############################################

# load the required packages
packages <- c(
  "here", # for the project's organization
  "tidyverse", "tidytext", "wordcloud", "lexicon", "quanteda", "quanteda.textstats",
  "tm", "ggrepel", "udpipe", "widyr", "textdata", "reshape2",  # for wrangling
  "rvest", "magick", "pdftools", # web scrapping
  "patchwork", "broom", "ggwordcloud", "igraph", "patchwork", # for plotting
  "knitr", "kableExtra", "bookdown", "rmarkdown", "bslib", "summarytools",
  "flextable", # for the report
  "readxl", "readr", # read files
  "irlba", "sentimentr", "quanteda.textmodels", "quanteda.textplots",
  "ranger", "caret", "text2vec", "stringi", "jsonlite", "reticulate", "spacyr", # for analysis
  "sentimentr", # for sentiments
  "rio" # data import/export
)

purrr::walk(packages, library, character.only = TRUE) 

######################################################
## The following sets a few option for nice reports ##
######################################################

# general options
options(
  digits = 3,
  str = strOptions(strict.width = "cut"),
  width = 69,
  tibble.width = 69,
  cli.unicode = FALSE
)

# ggplot options
theme_set(theme_light())

# knitr options
opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  # cache = TRUE,
  # fig.retina = 0.8, # figures are either vectors or 300 dpi diagrams
  # dpi = 300,
  # out.width = "70%",
  fig.align = "center",
  # fig.width = 6,
  # fig.asp = 0.618,
  fig.show = "hold",
  message = FALSE,
  echo = FALSE,
  warning = FALSE
)
