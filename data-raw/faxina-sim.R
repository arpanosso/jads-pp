library(tidyverse)
# da <- read_rds("data-raw/sim/municipio.rds")
# glimpse(da)
#
# df_nome <- read_rds("data/df_nome.rds")
# glimpse(df_nome)
# sim_municipio <- left_join(da %>% filter(sigla_uf == "SP"),
#                  df_nome %>%
#                    select(id_municipio, nome),"id_municipio"
# ) %>%
#   drop_na()
#
# da <- read_rds("data-raw/sim/municipio_causa.rds")
# glimpse(da)
# sim_municipio_causa <- left_join(da %>% filter(sigla_uf == "SP"),
#                            df_nome %>%
#                              select(id_municipio, nome),"id_municipio"
# ) %>%
#   drop_na()
#
#
#
# da <- read_rds("data-raw/sim/municipio_causa_idade.rds")
# glimpse(da)
# sim_municipio_causa_idade <- left_join(da %>% filter(sigla_uf == "SP"),
#                                  df_nome %>%
#                                    select(id_municipio, nome),"id_municipio"
# ) %>%
#   drop_na()

da <- read_rds("data-raw/sim/municipio_causa_idade_sexo_raca.rds")
glimpse(da)
sim_municipio_causa_idade_sexo_raca <- left_join(da %>% filter(sigla_uf == "SP"),
                                       df_nome %>%
                                         select(id_municipio,
                                                nome_regiao_imediata,
                                                nome_regiao_intermediaria,
                                                nome),"id_municipio"
)

# glimpse(sim_municipio)
# glimpse(sim_municipio_causa)
# glimpse(sim_municipio_causa_idade)
# glimpse(sim_municipio_causa_idade_sexo_raca)
#
# sim_municipio %>%
#   filter(id_municipio == "3500105")
#
# sim_municipio_causa %>%
#   mutate(ano = as.numeric(ano),
#          numero_obitos = as.numeric(numero_obitos)) %>%
#   filter(ano >= 1999) %>%
#   group_by(ano, id_municipio) %>%
#   summarise( n =sum(numero_obitos)) %>%
#   filter(id_municipio == "3500105")
#
# sim_municipio_causa_idade %>%
#   mutate(ano = as.numeric(ano),
#          numero_obitos = as.numeric(numero_obitos)) %>%
#   filter(ano >= 1999) %>%
#   group_by(ano, id_municipio) %>%
#   summarise( n =sum(numero_obitos)) %>%
#   filter(id_municipio == "3500105")

sim_municipio_causa_idade_sexo_raca %>%
  filter(!is.na(nome)) %>%
  mutate(ano = as.numeric(ano),
         numero_obitos = as.numeric(numero_obitos),
         idade = as.numeric(idade)) %>%
  filter(ano >= 1999) %>%
  group_by(ano, id_municipio) %>%
  summarise( n =sum(numero_obitos)) %>%
  filter(id_municipio == "3500105")

glimpse(sim_municipio_causa_idade_sexo_raca)
readr::write_rds(sim_municipio_causa_idade_sexo_raca,
        "data/sim_municipio.rds")
