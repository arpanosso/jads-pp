library(tidyverse)

names_files <- list.files("data-raw/auxilio-brasil/")

readr::read_csv(paste0("data-raw/auxilio-brasil/",names_files[1]))
