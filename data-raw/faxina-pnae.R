library(tidyverse)

pnae <- read_rds("data-raw/pnae.rds") %>%
  janitor::clean_names() %>%
  mutate(
    municipio = str_replace_all(municipio,
                                "MOGI-GUACU",
                                "MOGI GUACU"),
    municipio = str_replace_all(municipio,
                                "BIRITIBA MIRIM",
                                "BIRITIBA-MIRIM"),
    municipio = str_replace_all(municipio,
                                "MOJI-MIRIM",
                                "MOGI MIRIM"),
    municipio = str_replace_all(municipio,
                                "BRODOWSKY",
                                "BRODOWSKI"),
    municipio = ifelse(municipio == "EMBU",
                       "EMBU DAS ARTES",
                       municipio)
  )

glimpse(pnae)

df_nome <- read_rds("data/df_nome.rds")
glimpse(df_nome)

n_pnae <- pnae$municipio %>% unique() %>% sort()

n_nome <- df_nome$nome %>% unique() %>% sort() %>%
  str_to_lower() %>%
  str_replace_all(pattern = "á","a") %>%
  str_replace_all(pattern = "ã","a") %>%
  str_replace_all(pattern = "â","a") %>%
  str_replace_all(pattern = "ê|é","e") %>%
  str_replace_all(pattern = "í","i") %>%
  str_replace_all(pattern = "ó|ô|õ","o") %>%
  str_replace_all(pattern = "ú","u") %>%
  str_replace_all(pattern = "ç","c") %>%
  str_remove_all(pattern = "'") %>%
  str_to_upper()


data.frame(n_nome, n_pnae ) %>%
  mutate(log = n_nome == n_pnae) %>%
  filter(log == FALSE)
