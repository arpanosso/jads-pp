library(tidyverse)
source("R/my-functions.R")

memory.limit(10001)
files <- list.files("data-raw/estabelecimento/")
files_way <- paste0("data-raw/estabelecimento/",files)
df_nome <- read_rds("data/df_nome.rds") %>%
 select(nome, id_municipio_6)


dff <- map_df(files_way,read_my_file, .id = "files_way")

sisvan_estab <-left_join(dff, df_nome, "id_municipio_6") %>%
  mutate(municipio = nome) %>%
  mutate_if(is.integer64, as.integer) %>%
  select(-nome) %>%
  mutate(ano = ifelse(files_way == "10", 2015, ano)
  )
glimpse(sisvan_estab)

write_rds(sisvan_estab,"data/sisvan_estab.rds")

