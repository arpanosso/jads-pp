library(tidyverse)

u_sisvan  <- "https://sisaps.saude.gov.br/sisvan/relatoriopublico/estadonutricional"
body_sisvan <- list(
  tpRelatorio="2",
  coVisualizacao="1",
  nuAno="2008",
  'nuMes[]' = "99", ####
  tpFiltro="M",
  coRegiao="",
  coUfIbge="35",
  coMunicipioIbge="99",
  noRegional= "",
  st_cobertura="99",
  nu_ciclo_vida="2",
  nu_idade_inicio="",
  nu_idade_fim="-SELECIONE-",
  nu_indice_cri="4",
  nu_indice_ado="2",
  nu_idade_ges="99",
  ds_sexo2="1",
  ds_raca_cor2="99",
  co_sistema_origem="0",
  CO_POVO_COMUNIDADE="TODOS",
  CO_ESCOLARIDADE="TODOS",
  verTela=""
)

r_sisvan <- httr::POST(
  u_sisvan,
  body = body_sisvan,
  httr::write_disk("web-scraping/sisvan.html",
                   overwrite = TRUE))

minha_pagina_teste <- readr::read_file("web-scraping/sisvan.html")

readr::guess_encoding("https://sisaps.saude.gov.br/sisvan/relatoriopublico/index")

httr::content(r_sisvan, encoding = "UTF-8")  %>%
  xml2::xml_find_first("//table")

httr::content(r_sisvan, encoding = "UTF-8")  %>%
  xml2::xml_find_first("//table/tbody") %>%
  rvest::html_table(header = FALSE) %>%
  tibble::as_tibble() %>%
  mutate(across(where(is.numeric), as.character))

# Criando a lista para os anos do estado nutiriconal
lista_ano_estado_nutricional <- 2008:2022
ciclo_vida <- 3:4
idade_inicio <- ""
idade_fim <- "-SELECIONE-"
indice_cri="1"
indice_ado="1"

parametros_1 <- expand.grid(lista_ano_estado_nutricional,
                          ciclo_vida,idade_inicio,
                          idade_fim,indice_cri,
                          indice_ado) %>%
  mutate(across(where(is.numeric), as.character))

ciclo_vida <- 2
idade_inicio <- ""
idade_fim <- "-SELECIONE-"
indice_cri="4"
indice_ado=1:2
parametros_2 <- expand.grid(lista_ano_estado_nutricional,
                            ciclo_vida,idade_inicio,
                            idade_fim,indice_cri,
                            indice_ado) %>%
  mutate(across(where(is.numeric), as.character))

ciclo_vida <- 1
idade_inicio <- c(0,5)
idade_fim <- c(5,10)
indice_cri=1:4
indice_ado="1"
parametros_3 <- expand.grid(lista_ano_estado_nutricional,
                            ciclo_vida,idade_inicio,
                            idade_fim,indice_cri,
                            indice_ado) %>%
  filter(!(Var3 == 0 & Var4==10)) %>%
  filter(!(Var3 == 5 & Var4==5)) %>%
  mutate(across(where(is.numeric), as.character))

parametros = rbind(parametros_1,parametros_2,parametros_3)
nrow(parametros)

download_sisvan <- function(indice){
  ano = as.character(parametros[indice,1])
  fase_da_vida = as.character(parametros[indice,2])
  idade_inicio = as.character(parametros[indice,3])
  idade_fim  = as.character(parametros[indice,4])
  indice_cri= as.character(parametros[indice,5])
  indice_ado== as.character(parametros[indice,6])

  body_sisvan <- list(
    tpRelatorio="2",
    coVisualizacao="1",
    nuAno=ano, ####
    'nuMes[]' = "99",
    tpFiltro="M",
    coRegiao="",
    coUfIbge="35",
    coMunicipioIbge="99",
    noRegional= "",
    st_cobertura="99",
    nu_ciclo_vida=fase_da_vida, ####
    nu_idade_inicio=idade_inicio,
    nu_idade_fim=idade_fim,
    nu_indice_cri=indice_cri,
    nu_indice_ado=indice_ado,
    nu_idade_ges="99",
    ds_sexo2="1",
    ds_raca_cor2="99",
    co_sistema_origem="0",
    CO_POVO_COMUNIDADE="TODOS",
    CO_ESCOLARIDADE="TODOS",
    verTela=""
  )
  if(idade_inicio == ""){
    idade_iniciof="99"
  }else{
    idade_iniciof = idade_inicio
  }
  arquivo <- paste0("web-scraping/output/estado_nutricional_",
                    fase_da_vida,"_",ano,"_",
                    idade_iniciof,"_",
                    indice_cri,"_",
                    indice_ado,
                    ".html")
  r_sisvan <- httr::POST(
    "https://sisaps.saude.gov.br/sisvan/relatoriopublico/estadonutricional",
    body = body_sisvan,
    httr::write_disk(arquivo,overwrite = TRUE))
  return(arquivo)
}

# vamos testar com 3 arquivos
i <- 1:3
tictoc::tic()
purrr::map(i, purrr::possibly(download_sisvan, ""))
tictoc::toc()


future::plan("multisession")
tictoc::tic()
furrr::future_map(i, purrr::possibly(download_sisvan, ""))
tictoc::toc()

# Definindo todas as iterações
i <- 1:nrow(parametros)

# Craindo a função maybe_
maybe_download_sisvan_prog <- function(indice, prog){
  prog()
  f <- purrr::possibly(download_sisvan, "")
  f(indice)
}


# Rodando com a barra de progresso
progressr::with_progress({
  prog <- progressr::progressor(along = i)
  furrr::future_map(i, maybe_download_sisvan_prog, prog=prog)
})

# PARSEANDO
arquivos_baixados <- fs::dir_ls("web-scraping/output/")


parse_job <- function(dir){
  html <- xml2::read_html(dir, encoding = "UTF-8")
  html %>%
    xml2::xml_find_first("//table/tbody") %>%
    rvest::html_table(header = FALSE) %>%
    tibble::as_tibble() %>%
    mutate(across(where(is.numeric), as.character))
}

estadonutricional <- furrr::future_map_dfr(arquivos_baixados,
                                     purrr::possibly(parse_job, data.frame())
                                     , .id = "arquivos_baixados")

estado_nutricional <- estadonutricional %>%
  filter(X1 == "SUDESTE") %>%
  mutate(arquivos_baixados =
           str_remove_all(arquivos_baixados,
                          "web-scraping/output/estado_nutricional_|.html")) %>%
  separate(arquivos_baixados,c("fase_da_vida",
                               "ano",
                               "idade_inicio",
                               "indice_cri",
                               "indice_ado"),"_") %>%
  mutate(
    fase_da_vida = case_when(
      fase_da_vida == "1" ~ "Criança",
      fase_da_vida == "2" ~ "Adolescente",
      fase_da_vida == "3" ~ "Adulto",
      fase_da_vida == "4" ~ "Idoso",
    ),
    idade_inicio=ifelse(idade_inicio == "0","0-4",
                        ifelse(idade_inicio == "5","5-10",idade_inicio)),
    indice_cri = case_when(
      indice_cri=="1" ~ "Peso X Idade",
      indice_cri=="2" ~ "Peso X Altura",
      indice_cri=="3" ~ "Altura X Idade",
      indice_cri=="4" ~ "IMC X Idade",
    ),
    indice_ado = ifelse(indice_ado=="1","Altura X Idade",
                        "IMC X Idade"),
    indice = ifelse(fase_da_vida == "Criança",indice_cri,
                    ifelse(fase_da_vida == "Adolescente",indice_ado,"99"))
  ) %>% rename(idade = idade_inicio) %>%
  relocate(ano, fase_da_vida, idade, indice, indice_cri,indice_ado)

readr::write_rds(estado_nutricional,"data-raw/estado_nutricional.rds")











