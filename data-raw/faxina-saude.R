## Carregando pacotes
library(tidyverse)
library(readxl)
library(janitor)
library(readr)

## Pegando os nomes dos arquivos
names_files <- list.files("data-raw/saude/")

## pegando os nomes das planilhas
sheets <- readxl::excel_sheets(paste0("data-raw/saude/",names_files[1]))

## retirando a mãe da leitura
ftr <- !sheets == "mae"
sheets <- sheets[-ftr]

## craindo o banco de dados auxiliar
for(i in 1:9){
  plan <- sheets[i]
  da <- read_xlsx(paste0("data-raw/saude/",names_files[1]),
                  sheet = plan) %>%
    clean_names()

  # arrumando o nome
  if(plan == "ultra") {
    names(da) <- names(da) %>% str_replace("pro1","ultra")
    names(da) <- names(da) %>% str_replace("adolecentes_ultra",
                                           "anosadolecentes_ultra")
  }
  if(plan == "proces1"){
    names(da) <- names(da) %>% str_replace("_gl","pro1")
    names(da) <- names(da) %>% str_replace("adolecentes_pro1",
                                           "anosadolecentes_pro1")
  }
  if(plan == "Frutas"){
    names(da) <- names(da) %>% str_replace("anosfeijao","anosfrutas")
    names(da) <- names(da) %>% str_replace("adolecentes_frutas",
                                           "anosadolecentes_frutas")
    names(da) <- names(da) %>% str_replace("adultosanos",
                                           "anosidosos")
  }
  if(plan == "GL"){
    names(da) <- names(da) %>% str_replace("adolecentes_gl",
                                           "anosadolecentes_gl")
  }
  if(plan == "BD"){
    names(da) <- names(da) %>% str_replace("adolcentes",
                                           "adolecentes")
  }
  if(plan == "feijao"){
    names(da) <- names(da) %>% str_replace("adolecentes_feijao",
                                           "anosadolecentes_feijao")
  }


  # separando as colunas ano e cidade
  if(names(da)[1] == "x1") {da <- da %>%
    separate(x1,c("cidade","ano")) %>%
    mutate_at("ano", as.numeric)
  }else {
    if(names(da)[1] == "variavel"){
      da <- da %>%
        separate(variavel,c("cidade","ano")) %>%
        mutate_at("ano", as.numeric)
    }
  }
  da_a <- da %>%
    #select(cidade, ano, contains("2_4")) %>%
    select(cidade, ano, contains("total")) %>%
    pivot_longer(cols = contains("total"),
                 names_to = "id",
                 values_to = "total") %>%
    filter(
      !str_detect(id, 'anos')
    ) %>%
    mutate(id = str_remove_all(id,"total_|total|ultra_|_ultra_|_vl_|vl_|_pro1_|pro1_|_frutas_|frutas_|_gl_|gl_|_3r_|3r_|_bd_|bd_|_eb_|eb_|eb|feijao_")) %>%
    rename(amostra = total)

  da_t <- da %>%
    #select(cidade, ano, contains("2_4")) %>%
    select(cidade, ano, contains("total")) %>%
    pivot_longer(cols = contains("total"),
                 names_to = "id",
                 values_to = "total") %>%
    filter(
      str_detect(id, 'anos')
    ) %>%
    mutate(id = str_remove_all(id,"(total_|cri_|crianca_|anos_|anos|_anos|_ultra|ultra|_vl|vl_|vl|pro1|_pro1|frutas|_frutas|_gl|gl|3r_|3r|de_|_bd|bd|_eb_|eb_|eb|e_b|_eb|feijao|_feijao)"))

  if(i == 1) {
    final <- left_join(da_t,da_a,by=c("cidade","ano","id")) %>%
      mutate(perc = amostra/total,
             tipo = plan) %>%
      relocate(cidade,ano,id,tipo)
  } else {
    auxiliar <- left_join(da_t,da_a,by=c("cidade","ano","id")) %>%
      mutate(perc = amostra/total,
             tipo = plan) %>%
      relocate(cidade,ano,id,tipo)
    final <- rbind(final,auxiliar)
  }
}; View(final)

## salvando o banco de dados
# write_rds(final,"data/saude.rds")


readr::read_rds("data-raw/estado_nutricional.rds")












