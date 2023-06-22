library(tidyverse)
estado_nutricional <- readr::read_rds("data-raw/estado_nutricional.rds")
glimpse(estado_nutricional)

muda_numero <- function(x){
  as.numeric(
    str_remove_all(x,"%|-")
  )
}

# crianças ----------------------------------------------------------------
cri_PI <- estado_nutricional %>%
  filter(fase_da_vida == "Criança",
         indice == "Peso X Idade") %>%
  mutate(
    ano = ano %>% muda_numero(),
    peso_muito_baixo_q = X6 %>% muda_numero(),
    peso_muito_baixo_p = X7 %>% muda_numero(),
    peso_baixo_q = X8 %>% muda_numero(),
    peso_baixo_p = X9 %>% muda_numero(),
    peso_adequado_q = X10 %>% muda_numero(),
    peso_adequado_p = X11 %>% muda_numero(),
    peso_elevado_q = X12 %>% muda_numero(),
    peso_elevado_p = X13 %>% muda_numero(),
    total = X14 %>% muda_numero()
  ) %>%
  select(-(X6:X18), -indice_cri, -indice_ado) %>%
  rename(
    regiao = X1,
    codigo_uf = X2,
    uf = X3,
    codigo_ibge = X4,
    municipio = X5
  )
cri_PI %>%  glimpse()
readr::write_rds(cri_PI,"data/cri_peso_idade.rds")
writexl::write_xlsx(cri_PI,"data/cri_peso_idade.xlsx")

cri_PA <- estado_nutricional %>%
  filter(fase_da_vida == "Criança",
         indice == "Peso X Altura") %>%
  mutate(
    ano = ano %>% muda_numero(),
    magreza_ac_q = X6 %>% muda_numero(),
    magreza_ac_p = X7 %>% muda_numero(),
    magreza_q = X8 %>% muda_numero(),
    magreza_p = X9 %>% muda_numero(),
    adequado_q = X10 %>% muda_numero(),
    adequado_p = X11 %>% muda_numero(),
    risco_sobrepeso_q = X12 %>% muda_numero(),
    risco_sobrepeso_p = X13 %>% muda_numero(),
    sobrepeso_q = X14 %>% muda_numero(),
    sobrepeso_p = X15 %>% muda_numero(),
    obesidade_q = X16 %>% muda_numero(),
    obesidade_p = X17 %>% muda_numero(),
    total = X18 %>% muda_numero()
  ) %>%
  select(-(X6:X18), -indice_cri, -indice_ado) %>%
  rename(
    regiao = X1,
    codigo_uf = X2,
    uf = X3,
    codigo_ibge = X4,
    municipio = X5
  )
cri_PA %>%  glimpse()
readr::write_rds(cri_PA,"data/cri_peso_altura.rds")
writexl::write_xlsx(cri_PA,"data/cri_peso_altura.xlsx")

cri_AI <- estado_nutricional %>%
  filter(fase_da_vida == "Criança",
         indice == "Altura X Idade") %>%
  mutate(
    ano = ano %>% muda_numero(),
    altura_muito_baixa_q = X6 %>% muda_numero(),
    altura_muito_baixa_p = X7 %>% muda_numero(),
    altura_baixa_q = X8 %>% muda_numero(),
    altura_baixa_p = X9 %>% muda_numero(),
    altura_adequada_q = X10 %>% muda_numero(),
    altura_adequada_p = X11 %>% muda_numero(),
    total = X12 %>% muda_numero()
  ) %>%
  select(-(X6:X18), -indice_cri, -indice_ado) %>%
  rename(
    regiao = X1,
    codigo_uf = X2,
    uf = X3,
    codigo_ibge = X4,
    municipio = X5
  )
cri_AI %>%  glimpse()
readr::write_rds(cri_AI,"data/cri_altura_idade.rds")
writexl::write_xlsx(cri_AI,"data/cri_altura_idade.xlsx")

cri_IMC <- estado_nutricional %>%
  filter(fase_da_vida == "Criança",
         indice == "IMC X Idade") %>%
  mutate(
    ano = ano %>% muda_numero(),
    magreza_ac_q = X6 %>% muda_numero(),
    magreza_ac_p = X7 %>% muda_numero(),
    magreza_q = X8 %>% muda_numero(),
    magreza_p = X9 %>% muda_numero(),
    adequado_q = X10 %>% muda_numero(),
    adequado_p = X11 %>% muda_numero(),
    risco_sobrepeso_q = X12 %>% muda_numero(),
    risco_sobrepeso_p = X13 %>% muda_numero(),
    sobrepeso_q = X14 %>% muda_numero(),
    sobrepeso_p = X15 %>% muda_numero(),
    obesidade_q = X16 %>% muda_numero(),
    obesidade_p = X17 %>% muda_numero(),
    total = X18 %>% muda_numero()
  ) %>%
  select(-(X6:X18), -indice_cri, -indice_ado) %>%
  rename(
    regiao = X1,
    codigo_uf = X2,
    uf = X3,
    codigo_ibge = X4,
    municipio = X5
  )
cri_IMC %>%  glimpse()
readr::write_rds(cri_IMC,"data/cri_imc.rds")
writexl::write_xlsx(cri_IMC,"data/cri_imc.xlsx")

# adolescente -------------------------------------------------------------
adol_AI <- estado_nutricional %>%
  filter(fase_da_vida == "Adolescente",
         indice == "Altura X Idade") %>%
  mutate(
    ano = ano %>% muda_numero(),
    altura_muito_baixa_q = X6 %>% muda_numero(),
    altura_muito_baixa_p = X7 %>% muda_numero(),
    altura_baixa_q = X8 %>% muda_numero(),
    altura_baixa_p = X9 %>% muda_numero(),
    altura_adequada_q = X10 %>% muda_numero(),
    altura_adequada_p = X11 %>% muda_numero(),
    total = X12 %>% muda_numero()
  ) %>%
  select(-(X6:X18), -indice_cri, -indice_ado) %>%
  rename(
    regiao = X1,
    codigo_uf = X2,
    uf = X3,
    codigo_ibge = X4,
    municipio = X5
  )
adol_AI %>%  glimpse()
readr::write_rds(adol_AI,"data/adolescente_altura_idade.rds")
writexl::write_xlsx(adol_AI,"data/adolescente_altura_idade.xlsx")

adol_IMC <- estado_nutricional %>%
  filter(fase_da_vida == "Adolescente",
         indice == "IMC X Idade") %>%
  mutate(
    ano = ano %>% muda_numero(),
    magreza_ac_q = X6 %>% muda_numero(),
    magreza_ac_p = X7 %>% muda_numero(),
    magreza_q = X8 %>% muda_numero(),
    magreza_p = X9 %>% muda_numero(),
    adequado_q = X10 %>% muda_numero(),
    adequado_p = X11 %>% muda_numero(),
    risco_sobrepeso_q = X12 %>% muda_numero(),
    risco_sobrepeso_p = X13 %>% muda_numero(),
    sobrepeso_q = X14 %>% muda_numero(),
    sobrepeso_p = X15 %>% muda_numero(),
    obesidade_q = X16 %>% muda_numero(),
    obesidade_p = X17 %>% muda_numero(),
    total = X18 %>% muda_numero()
  ) %>%
  select(-(X6:X18), -indice_cri, -indice_ado) %>%
  rename(
    regiao = X1,
    codigo_uf = X2,
    uf = X3,
    codigo_ibge = X4,
    municipio = X5
  )
adol_IMC %>%  glimpse()
readr::write_rds(adol_IMC,"data/adolescente_imc.rds")
writexl::write_xlsx(adol_IMC,"data/adolescente_imc.xlsx")

# adultos -----------------------------------------------------------------
adulto_IMC <- estado_nutricional %>%
  filter(fase_da_vida == "Adulto") %>%
  mutate(
    ano = ano %>% muda_numero(),
    baixo_peso_q = X6 %>% muda_numero(),
    baixo_peso__p = X7 %>% muda_numero(),
    adequado_peso__q = X8 %>% muda_numero(),
    adequado_peso__p = X9 %>% muda_numero(),
    sobrepeso_q = X10 %>% muda_numero(),
    sobrepeso_p = X11 %>% muda_numero(),
    obesidade_I_q = X12 %>% muda_numero(),
    obesidade_I_p = X13 %>% muda_numero(),
    obesidade_II_q = X14 %>% muda_numero(),
    obesidade_II_p = X15 %>% muda_numero(),
    obesidade_III_q = X16 %>% muda_numero(),
    obesidade_III_p = X17 %>% muda_numero(),
    total = X18 %>% muda_numero()
  ) %>%
  select(-(X6:X18), -indice_cri, -indice_ado) %>%
  rename(
    regiao = X1,
    codigo_uf = X2,
    uf = X3,
    codigo_ibge = X4,
    municipio = X5
  )
adulto_IMC %>%  glimpse()
readr::write_rds(adulto_IMC,"data/adulto_imc.rds")
writexl::write_xlsx(adulto_IMC,"data/adulto_imc.xlsx")

# Idosos ------------------------------------------------------------------
idoso_IMC <- estado_nutricional %>%
  filter(fase_da_vida == "Idoso") %>%
  mutate(
    ano = ano %>% muda_numero(),
    baixo_peso_q = X6 %>% muda_numero(),
    baixo_peso__p = X7 %>% muda_numero(),
    adequado_peso__q = X8 %>% muda_numero(),
    adequado_peso__p = X9 %>% muda_numero(),
    sobrepeso_q = X10 %>% muda_numero(),
    sobrepeso_p = X11 %>% muda_numero(),
    total = X12 %>% muda_numero()
  ) %>%
  select(-(X6:X18), -indice_cri, -indice_ado) %>%
  rename(
    regiao = X1,
    codigo_uf = X2,
    uf = X3,
    codigo_ibge = X4,
    municipio = X5
  )
idoso_IMC %>%  glimpse()
readr::write_rds(idoso_IMC,"data/idoso_imc.rds")
writexl::write_xlsx(idoso_IMC,"data/idoso_imc.xlsx")


# Saude -e Estado - Nutricional -------------------------------------------
saude <- read_rds("data-raw/estado_nutricional.rds")

saude_estado_nutricional <- saude %>%
  mutate(ano = as.numeric(ano)) %>%
  rename(regiao = X1,
         codigo_uf = X2,
         uf = X3,
         id_municipio = X4,
         municipio = X5) %>%
  mutate(across(X6:X18, ~ stringr::str_remove_all(., "%|-")),
         idade = ifelse(idade == "99", "", idade),
         indice = ifelse(indice == "99", "IMC", indice)) %>%
  mutate_at(vars(X6:X18), as.numeric) %>%
  select(-X7,-X9,-X11,-X13,-X15,-X17)
glimpse(saude_estado_nutricional)

df_nome <- read_rds("data/df_nome.rds") %>%
  mutate(id_municipio = id_municipio_6)

glimpse(df_nome)

saude_estado_nutricional <- left_join(saude_estado_nutricional, df_nome %>%
            select(id_municipio,nome),
            "id_municipio") %>%
  mutate(municipio = nome) %>% select(-nome)


unique(saude_estado_nutricional$indice)

fase_da_vida <- saude_estado_nutricional %>%
  pull(fase_da_vida) %>% unique
for(i in seq_along(fase_da_vida)){
  fdv <- fase_da_vida[i]
  indice <- saude_estado_nutricional %>%
    filter(fase_da_vida == fdv) %>%
    pull(indice) %>% unique
  for(j in seq_along(indice)){
    ind <- indice[j]
    if(ind == "Peso X Idade" & fdv == "Criança"){
      df_aux <- saude_estado_nutricional %>%
        filter(fase_da_vida == fdv,
               indice == ind) %>%
        rename(muito_baixo = X6,
               baixo = X8,
               adequado = X10,
               elevado = X12) %>%
        select(-(X14:X18)) %>%
        pivot_longer(cols = muito_baixo:elevado,
                     names_to = "classe",
                     values_to = "valor")
      }

    if(ind == "Peso X Altura" & fdv == "Criança"){
      df_aux <- saude_estado_nutricional %>%
        filter(fase_da_vida == fdv,
               indice == ind) %>%
        rename(magreza_acentuada = X6,
               magreza = X8,
               adequado = X10,
               risco_sobrepeso = X12,
               sobrepeso = X14,
               obesidade = X16) %>%
        select(-(X18)) %>%
        pivot_longer(cols = magreza_acentuada:obesidade,
                     names_to = "classe",
                     values_to = "valor")
    }

    if(ind == "Altura X Idade" & fdv == "Criança"){
      df_aux <- saude_estado_nutricional %>%
        filter(fase_da_vida == fdv,
               indice == ind) %>%
        rename(muito_baixa = X6,
               baixa = X8,
               adequada = X10) %>%
        select(-(X12:X18)) %>%
        pivot_longer(cols = muito_baixa:adequada,
                     names_to = "classe",
                     values_to = "valor")
    }

    if(ind == "IMC X Idade" & fdv == "Criança"){
      df_aux <- saude_estado_nutricional %>%
        filter(fase_da_vida == fdv,
               indice == ind) %>%
        rename(magreza_acentuada = X6,
               magreza = X8,
               adequado = X10,
               risco_sobrepeso = X12,
               sobrepeso = X14,
               obesidade = X16) %>%
        select(-(X18)) %>%
        pivot_longer(cols = magreza_acentuada:obesidade,
                     names_to = "classe",
                     values_to = "valor")
    }

    if(ind == "Altura X Idade" & fdv == "Adolescente"){
      df_aux <- saude_estado_nutricional %>%
        filter(fase_da_vida == fdv,
               indice == ind) %>%
        rename(muito_baixa = X6,
               baixa = X8,
               adequada = X10) %>%
        select(-(X12:X18)) %>%
        pivot_longer(cols = muito_baixa:adequada,
                     names_to = "classe",
                     values_to = "valor")
    }

    if(ind == "IMC X Idade" & fdv == "Adolescente"){
      df_aux <- saude_estado_nutricional %>%
        filter(fase_da_vida == fdv,
               indice == ind) %>%
        rename(magreza_acentuada = X6,
               magreza = X8,
               adequado = X10,
               sobrepeso = X12,
               obesidade = X14,
               obesidade_grave = X16) %>%
        select(-(X18)) %>%
        pivot_longer(cols = magreza_acentuada:obesidade_grave,
                     names_to = "classe",
                     values_to = "valor")
    }

    if(ind == "IMC" & fdv == "Adulto"){
      df_aux <- saude_estado_nutricional %>%
        filter(fase_da_vida == fdv,
               indice == ind) %>%
        rename(baixo_peso = X6,
               adequado = X8,
               sobrepeso = X10,
               obesidade_grau_I = X12,
               obesidade_grau_II = X14,
               obesidade_grau_III = X16) %>%
        select(-(X18)) %>%
        pivot_longer(cols = baixo_peso:obesidade_grau_III,
                     names_to = "classe",
                     values_to = "valor")
    }

    if(ind == "IMC" & fdv == "Idoso"){
      df_aux <- saude_estado_nutricional %>%
        filter(fase_da_vida == fdv,
               indice == ind) %>%
        rename(baixo_peso = X6,
               adequado = X8,
               sobrepeso = X10) %>%
        select(-(X12:X18)) %>%
        pivot_longer(cols = baixo_peso:sobrepeso,
                     names_to = "classe",
                     values_to = "valor")
    }

    if(i ==1 & j == 1){
      df_final <- df_aux
    } else {
      df_final <- rbind(df_final, df_aux)
    }
  }
}

write_rds(df_final,"data/df_final.rds")





