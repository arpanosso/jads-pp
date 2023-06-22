library(tidyverse)
library(basedosdados)
library(bigrquery)
library(DBI)
library(dbplyr)
library(dplyr)

# Defina o seu projeto no Google Cloud
set_billing_id("diagnostico-pp")


# Para carregar o dado direto no R nomes dos municipios
query <- bdplyr("br_bd_diretorios_brasil.municipio")
df_nome <- bd_collect(query)
glimpse(df_nome)
write_rds(df_nome %>% filter(sigla_uf == "SP"),
          "data/df_nome.rds")
# # atenção basica
# query <- bdplyr("br_ms_atencao_basica.municipio")
# df_atencao_basica <- bd_collect(query)
# glimpse(df_atencao_basica)
# readr::write_rds(atencao_basica,"data/atencao_basica.rds")
# writexl::write_xlsx(atencao_basica,"data/atencao_basica.xlsx")


### parte agrícola
# query <- bdplyr("br_ibge_pam.municipio_lavouras_permanentes")
# df_permanente <- bd_collect(query)
#
# query <- bdplyr("br_ibge_pam.municipio_lavouras_temporarias")
# df_temporaria <- bd_collect(query)
#
# glimpse(df_permanente)
# permanente <- left_join(df_permanente %>% filter(sigla_uf == "SP"),
#                 df_nome %>% select(id_municipio, nome),"id_municipio"
# )
# glimpse(permanente)
# readr::write_rds(permanente,"data/permanente.rds")
#
# my_na_count <- function(x){
#   sum(is.na(x))
# }
# permanente$na_count <- apply(permanente[5:12],1,my_na_count)
# writexl::write_xlsx(permanente %>%
#                       filter(na_count != 8) ,"data/permanente.xlsx")


#
#
# glimpse(df_temporaria)
# temporaria <- left_join(df_temporaria %>% filter(sigla_uf == "SP"),
#                         df_nome %>% select(id_municipio, nome),"id_municipio"
# )
# glimpse(temporaria)
# readr::write_rds(temporaria,"data/temporaria.rds")
# writexl::write_xlsx(temporaria,"data/temporaria.xlsx")


### vulnerabilidade social
# Para carregar o dado direto no R
# query <- bdplyr("br_ipea_avs.municipio")
# df_vulnerabilidade <- bd_collect(query)
# glimpse(df_vulnerabilidade)
# vulnerabilidade <- left_join(df_vulnerabilidade %>% filter(sigla_uf == "SP"),
#                         df_nome %>% select(id_municipio, nome),"id_municipio"
# )
# glimpse(vulnerabilidade)
# readr::write_rds(vulnerabilidade,"data/vulnerabilidade.rds")
# writexl::write_xlsx(vulnerabilidade,"data/vulnerabilidade.xlsx")


## pib
# Para carregar o dado direto no R
# lista_muni_sp <- vulnerabilidade %>% pull(id_municipio) %>% unique()
#
# query <- bdplyr("br_ibge_pib.municipio")
# df_pib <- bd_collect(query)
# glimpse(df_pib)
# pib <- left_join(df_pib,
#                         df_nome %>% select(id_municipio, nome),"id_municipio"
# )
# pib <- pib %>% filter( id_municipio %in% lista_muni_sp)
# glimpse(pib)
# readr::write_rds(pib,"data/pib.rds")
# writexl::write_xlsx(pib,"data/pib.xlsx")


# ieps
# query <- bdplyr("br_ieps_saude.municipio")
# df_ieps <- bd_collect(query)
# glimpse(df_ieps)
# ieps <- left_join(df_ieps %>% filter(sigla_uf == "SP"),
#                         df_nome %>% select(id_municipio, nome),"id_municipio"
# )
# glimpse(ieps)
# readr::write_rds(ieps,"data/ieps.rds")
# writexl::write_xlsx(ieps,"data/ieps.xlsx")

## indicadores do ministerio bolsa familia
# Para carregar o dado direto no R
# query <- bdplyr("br_mc_indicadores.transferencias_municipio")
# df_im <- bd_collect(query)
# glimpse(df_im)
# im <- left_join(df_im,
#                          df_nome %>% select(id_municipio, nome),"id_municipio"
# )
# im <- im %>% filter( id_municipio %in% lista_muni_sp)
# glimpse(im)
# readr::write_rds(im,"data/im.rds")
# writexl::write_xlsx(im,"data/im.xlsx")


# Establecimento de saude
# Para carregar o dado direto no R
# query <- bdplyr("br_ms_cnes.profissional")
# df <- bd_collect(query)
#
# basedosdados::set_billing_id("diagnostico-pp")
#
# qquery<-"SELECT *
# FROM `basedosdados.br_ms_cnes.estabelecimento`
# WHERE sigla_uf = 'SP' AND ano ="
# ano <- 2015
# qquery<-paste0(qquery,ano)
#
# for(i in seq_along(qquery)){
#   set_billing_id("diagnostico-pp")
#   da <- read_sql(qquery)
#   readr::write_rds(da,paste0("data-raw/estabelecimento/da-",ano,".rds"))
# }

# ##### profissional
# qquery<-"SELECT *
# FROM `basedosdados.br_ms_cnes.profissional`
# WHERE sigla_uf = 'SP' AND ano ="
# ano <- 2010:2015
# qquery<-paste0(qquery,ano)
#
# for(i in seq_along(qquery)){
#   set_billing_id("diagnostico-pp")
#   da <- read_sql(qquery[i])
#   readr::write_rds(da,paste0("data-raw/profissional/da-",ano[i],".rds"))
# }


### Município,
# qquery<-"SELECT *
# FROM `basedosdados.br_ms_sim.municipio`
# WHERE sigla_uf = 'SP'"
#
# set_billing_id("diagnostico-pp")
# da <- read_sql(qquery)
# glimpse(da)
# readr::write_rds(da %>%
#                    mutate(across(c(ano,numero_obitos),as.character)),
# paste0("data-raw/sim/municipio.rds"))

### município causa,
# qquery<-"SELECT *
# FROM `basedosdados.br_ms_sim.municipio_causa`
# WHERE sigla_uf = 'SP'"
#
# set_billing_id("diagnostico-pp")
# da <- read_sql(qquery)
# glimpse(da)
# readr::write_rds(da %>%
#                    mutate(across(c(ano,
#                                    numero_obitos),as.character)),
#                  paste0("data-raw/sim/municipio_causa.rds"))

### município_causa_idade,
# qquery<-"SELECT *
# FROM `basedosdados.br_ms_sim.municipio_causa_idade`
# WHERE sigla_uf = 'SP'"
#
# set_billing_id("diagnostico-pp")
# da <- read_sql(qquery)
# glimpse(da)
# readr::write_rds(da %>%
#                    mutate(across(c(ano,
#                                    idade,
#                                    numero_obitos), as.character))
#                  ,paste0("data-raw/sim/municipio_causa_idade.rds"))

### município_causa_idade_sexo_Raça
qquery<-"SELECT *
FROM `basedosdados.br_ms_sim.municipio_causa_idade_sexo_raca`
WHERE sigla_uf = 'SP'"

set_billing_id("diagnostico-pp")
da <- read_sql(qquery)
glimpse(da)
readr::write_rds(da %>%
                   mutate(across(c(ano,
                                   idade,
                                   numero_obitos), as.character))
                 ,paste0("data-raw/sim/municipio_causa_idade_sexo_raca.rds"))



### auxilio emergencial
# Para carregar o dado direto no R
# memory.limit(16001)
#
# qquery<-"SELECT
# mes,
# sigla_uf,
# id_municipio,
# enquadramento,
# parcela,
# valor_beneficio,
# FROM `basedosdados.br_mc_auxilio_emergencial.microdados`
# WHERE sigla_uf = 'SP' AND mes ="
# mes <- paste0("'2021-",
#               c("01","02","03","04","05","06","07","08","09","10","11","12"),
#               "'"
# )
# qquery<-paste0(qquery,mes)
# for(i in seq_along(qquery)){
#   set_billing_id("diagnostico-pp")
#   dae <- read_sql(qquery[i])
#   readr::write_rds(dae,paste0("data-raw/auxilio-emergencial/dae-",
#                               stringr::str_remove_all(mes[i],"'"),
#                               ".rds"))
# }
#
#








