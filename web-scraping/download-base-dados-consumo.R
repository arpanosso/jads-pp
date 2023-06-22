library(tidyverse)

anos <- 2015:2021
parametros <- expand.grid(folder,anos)
folder <- list.files("data-raw/saude/",
                     recursive = TRUE)[-63]

read_consumption <- function(index){
  my_directory <- paste0("data-raw/saude/",parametros[index,1])
  sheet_sp <- parametros[index,2]
  df <- readxl::read_excel(my_directory,
                           skip = 1,
                           sheet =as.character(sheet_sp)) %>%
    janitor::clean_names()
  nc <- ncol(df)
  if(nc==6){

    names(df) <- c("uf", "codigo_ibge", "municipio", "total",
                   "percent", "monitorados")
  }else{
    names(df) <- c("regiao","codigo_uf","uf", "codigo_ibge", "municipio",
                   "total", "percent", "monitorados")
    df <- df %>%
      select(uf:monitorados)

  }
  df <- df %>%
    filter(uf == "SP") %>%
    mutate(percent = total/monitorados)
  df$folder <- my_directory
  df$ano <- sheet_sp
  df
}
read_consumption(1)
consumo <- map_df(1:nrow(parametros), .f = read_consumption) %>%
  relocate(folder,ano) %>%
  mutate(folder = str_remove(folder, "data-raw/saude/")) %>%
  separate(folder,c("faixa_etaria","fase_da_vida","tipo_relatorio"),sep="/") %>%
  mutate(tipo_relatorio = str_remove(tipo_relatorio, ".xlsx"))

readr::write_rds(consumo,"data/consumo.rds")
writexl::write_xlsx(consumo,"data/consumo.xlsx")
