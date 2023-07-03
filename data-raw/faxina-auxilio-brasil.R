library(tidyverse)

names_files <- list.files("data-raw/auxilio-brasil/")

dados <- readr::read_csv(paste0("data-raw/auxilio-brasil/",names_files[1]))
glimpse(dados)
