library(tidyverse)


# Recursos repassados
pnae <- read_rds("data-raw/pnae/pnae.rds") %>%
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
                       municipio),
    nomin=municipio
  )

df_nome <- read_rds("data/df_nome.rds")
df_nome$nomin <- df_nome$nome %>%
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

pnae_recurso <- left_join(pnae, df_nome, by="nomin")
glimpse(pnae_recurso)
write_rds(pnae_recurso,"data/pnae_recurso.rds")

# Escolas Atendidas pelo PNAE
escolas <- read_csv("data-raw/pnae/EscolasAtendidas.csv") %>%
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
                       municipio),
    nomin=municipio
  )
glimpse(escolas)

pnae_escolas <- left_join(escolas, df_nome, by="nomin")
glimpse(pnae_escolas)
write_rds(pnae_escolas,"data/pnae_escolas.rds")

# Conselho de Alimentação Escolar
ConselhoAlimentacaoEscolar <- read_csv("data-raw/pnae/ConselhoAlimentacaoEscolar.csv") %>%
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
                       municipio),
    nomin=municipio
  )
glimpse(ConselhoAlimentacaoEscolar)

pnae_conselho_alim_esco <- left_join(escolas, df_nome, by="nomin")
glimpse(pnae_conselho_alim_esco)
write_rds(pnae_conselho_alim_esco,"data/pnae_conselho_alim_esco.rds")

# Alunos atendidos pelo PNAE - 2022
alunos <- read_csv("data-raw/pnae/Alunos_Atendidos.csv") %>%
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
                       municipio),
    nomin=municipio
  )
glimpse(alunos)

pnae_alunos_atendidos <- left_join(alunos, df_nome, by="nomin")
glimpse(pnae_alunos_atendidos)
write_rds(pnae_alunos_atendidos,"data/pnae_alunos_atendidos.rds")
