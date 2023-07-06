
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Carregando Pacotes

``` r
library(tidyverse)
```

------------------------------------------------------------------------

**DESAFIO 1**. Promover o acesso universal à alimentação adequada e
saudável, com prioridade para as famílias e pessoas em situação de
insegurança alimentar e nutricional

Temas: Transferência de Renda; Alimentação escolar; Distribuição de
alimentos.

1.1 Programa Bolsa Família  
1.1.1 Pessoas Beneficiárias **(OK)**  
1.1.2 Famílias Beneficiárias **(OK)**  
1.1.3 Valor Pago **(OK)**

``` r
im <- read_rds("data/im.rds") %>% 
  select(nome,id_municipio:valor_pago_pbf)
data_set <- im %>% filter(ano >= 2010) %>% 
  group_by(nome, ano) %>% 
  summarise(familias_beneficiarias_pbf=sum(familias_beneficiarias_pbf,na.rm=TRUE)/12,
            pessoas_beneficiarias_pbf=sum(pessoas_beneficiarias_pbf,na.rm=TRUE)/12,
            valor_pago_pbf=sum(valor_pago_pbf,na.rm=TRUE))
glimpse(data_set)
#> Rows: 7,095
#> Columns: 5
#> Groups: nome [645]
#> $ nome                       <chr> "Adamantina", "Adamantina", "Adamantina", "~
#> $ ano                        <int> 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2~
#> $ familias_beneficiarias_pbf <dbl> 623.0000, 593.5833, 600.5833, 597.0000, 475~
#> $ pessoas_beneficiarias_pbf  <dbl> 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0~
#> $ valor_pago_pbf             <dbl> 564177, 683102, 820970, 871484, 742443, 689~
```

------------------------------------------------------------------------

**DESAFIO 2**. Combater a insegurança alimentar e nutricional e promover
a inclusão produtiva rural em grupos populacionais específicos, com
ênfase em povos e comunidades tradicionais e outros grupos sociais
vulneráveis no meio rural.

Temas: Insegurança alimentar e nutricional; Inclusão produtiva rural;
Acesso à terra e gestão territorial; Biodiversidade; Saúde indígena;
extrativistas e ribeirinhos; Acesso a políticas públicas.

*MACRO DESAFIO*: Promoção de Sistemas Alimentares Saudáveis e
Sustentáveis Desafios: o conjunto dos desafios 3, 4 e 5 contemplam este
macro desafio.

------------------------------------------------------------------------

**DESAFIO 3**. Promover a produção de alimentos saudáveis e
sustentáveis, a estruturação da agricultura familiar e o fortalecimento
de sistemas de produção de base agroecológica

Temas: Fortalecimento da agricultura familiar; Reforma agrária;
Transição agroecológica; Mulheres; Juventude; Sementes; Mudanças
climáticas.

3.1 Produção de alimentos  
3.2 Área Plantada **(OK)**  
3.2.1 Área Plantada Agricultura Familiar  
3.2.2 Área Plantada Agricultura não familiar  
3.3. Rendimento Médio **(OK)**  
3.4 Proporção Área Colhida **(OK)**  
3.5 Proporção Valor da Produção **(OK)**

``` r
lavoura_permanente <- read_rds("data/permanente.rds") 
lavoura_temporaria <- read_rds("data/temporaria.rds")
lavouras <- rbind(lavoura_permanente %>% 
        mutate(tipo = "permanente"),
      lavoura_temporaria %>% 
        mutate(tipo = "temporaria")
      )
my_na_count <- function(x) sum(is.na(x))
lavouras$na <- apply(lavouras[5:12],1,my_na_count)
lavouras %>% 
  filter(na !=8, ano >= 2010) %>% 
  arrange(ano) %>% 
  relocate(tipo) %>% 
  group_by(tipo,produto) %>% 
  summarise(pvp = sum(prop_valor_producao,na.rm = TRUE)) %>% 
  mutate(produto = fct_reorder(produto, pvp)) %>% 
  ggplot(aes(x=pvp,y=produto,fill=tipo)) +
  geom_col(color="black")+
  facet_wrap(~tipo,scales = "free") + 
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# Precisamos resumir lavouras
lavouras <- lavouras %>% 
            filter(na != 8, ano >= 2010,
                   produto != "Café (em grão) Arábica",
                   produto != "Café (em grão) Canephora") %>% 
  select(-na)
lavouras <- lavouras %>% 
  group_by(tipo, ano, nome) %>% 
  summarise(
    n_produtos = n(),
    area_plantada = sum(area_plantada,na.rm=TRUE),
    rendimento_medio = mean(rendimento_medio,na.rm=TRUE),
    prop_area_colhida = mean(prop_area_colhida,na.rm=TRUE) ,
    prop_valor_producao = mean(prop_valor_producao,na.rm=TRUE)
    
  ) %>% 
  pivot_wider(names_from = tipo,
              values_from = n_produtos:prop_valor_producao)

data_set <- left_join(
  data_set,
  lavouras,
  by = c("ano","nome")
)
```

------------------------------------------------------------------------

**DESAFIO 4**. Promover o abastecimento e o acesso regular e permanente
da população brasileira à alimentação

Temas: Compras públicas; Abastecimento; Legislação sanitária; Economia
solidaria; Perdas e desperdícios de alimentos; Equipamentos públicos de
SAN; Agricultura urbana.

**4.1 Programa da Alimentação Escolar**  
4.1.1 Número de Alunos Beneficiários **(OK)**  
4.1.2 Valor gasto com Compras da Agricultura Familiar 30%  
4.1.3 Valor Formalizado  
4.1.4 Valor Executado **(OK)**

``` r
pnae_alunos <- read_rds("data/pnae_alunos_atendidos.rds")
glimpse(pnae_alunos)
#> Rows: 79,548
#> Columns: 32
#> $ co_alunos_atendidos       <dbl> 6344929, 6344930, 6344931, 6344932, 6344933,~
#> $ ano                       <dbl> 1999, 1999, 1999, 1999, 1999, 1999, 1999, 19~
#> $ estado                    <chr> "SP", "SP", "SP", "SP", "SP", "SP", "SP", "S~
#> $ municipio                 <chr> "ADAMANTINA", "ADAMANTINA", "ADOLFO", "AGUAI~
#> $ regiao                    <chr> "SUDESTE", "SUDESTE", "SUDESTE", "SUDESTE", ~
#> $ esfera_governo            <chr> "ESTADUAL", "MUNICIPAL", "MUNICIPAL", "ESTAD~
#> $ etapa_ensino              <chr> "ENSINO FUNDAMENTAL", "ENSINO FUNDAMENTAL", ~
#> $ qt_alunos_pnae            <dbl> 3329, 2185, 749, 3340, 1897, 498, 802, 2323,~
#> $ nomin                     <chr> "ADAMANTINA", "ADAMANTINA", "ADOLFO", "AGUAI~
#> $ id_municipio              <chr> "3500105", "3500105", "3500204", "3500303", ~
#> $ id_municipio_6            <chr> "350010", "350010", "350020", "350030", "350~
#> $ id_municipio_tse          <chr> "61018", "61018", "61034", "61050", "61050",~
#> $ id_municipio_rf           <chr> "6101", "6101", "6103", "6105", "6105", "610~
#> $ id_municipio_bcb          <chr> "20248", "20248", "24677", "29517", "29517",~
#> $ nome                      <chr> "Adamantina", "Adamantina", "Adolfo", "Aguaí~
#> $ capital_uf                <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
#> $ id_comarca                <chr> "3500105", "3500105", "3525706", "3500303", ~
#> $ id_regiao_saude           <chr> "35091", "35091", "35156", "35142", "35142",~
#> $ nome_regiao_saude         <chr> "Adamantina", "Adamantina", "José Bonifácio"~
#> $ id_regiao_imediata        <chr> "350019", "350019", "350025", "350044", "350~
#> $ nome_regiao_imediata      <chr> "Adamantina - Lucélia", "Adamantina - Lucéli~
#> $ id_regiao_intermediaria   <chr> "3505", "3505", "3507", "3510", "3510", "351~
#> $ nome_regiao_intermediaria <chr> "Presidente Prudente", "Presidente Prudente"~
#> $ id_microrregiao           <chr> "35035", "35035", "35004", "35029", "35029",~
#> $ nome_microrregiao         <chr> "Adamantina", "Adamantina", "São José do Rio~
#> $ id_mesorregiao            <chr> "3508", "3508", "3501", "3507", "3507", "350~
#> $ nome_mesorregiao          <chr> "Presidente Prudente", "Presidente Prudente"~
#> $ ddd                       <chr> "18", "18", "17", "19", "19", "19", "19", "1~
#> $ id_uf                     <chr> "35", "35", "35", "35", "35", "35", "35", "3~
#> $ sigla_uf                  <chr> "SP", "SP", "SP", "SP", "SP", "SP", "SP", "S~
#> $ nome_uf                   <chr> "São Paulo", "São Paulo", "São Paulo", "São ~
#> $ nome_regiao               <chr> "Sudeste", "Sudeste", "Sudeste", "Sudeste", ~
lista_etapas <- pnae_alunos$etapa_ensino %>% unique()
```

``` r
pnae_recurso <- read_rds("data/pnae_recurso.rds") 
pnae_recurso <-  pnae_recurso %>% 
  mutate(modalidade_ensino = ifelse(modalidade_ensino == "EJA",
                                    "EDUCAÇÃO DE JOVENS E ADULTOS (EJA)",
                                    modalidade_ensino))
# pnae_recurso$modalidade_ensino %>% unique()
```

``` r
pnae_recurso <- pnae_recurso %>% 
  group_by(nome, ano, esfera_governo, modalidade_ensino) %>% 
  summarise(vl_total_escolas = mean(vl_total_escolas)) %>% 
  rename(etapa_ensino = modalidade_ensino) %>% 
  filter(etapa_ensino %in% lista_etapas)

 
pnae <- left_join(pnae_alunos %>% 
          select(ano:qt_alunos_pnae,nome),
        pnae_recurso,
        by =c("nome","ano","esfera_governo","etapa_ensino")
        )
# glimpse(pnae)
```

``` r
# pnae_escola <- read_rds("data/pnae_escolas.rds")
# pnae_conselho <- read_rds("data/pnae_conselho_alim_esco.rds")
```

``` r
data_set <- left_join(data_set, pnae %>% 
            drop_na() %>% 
            group_by(ano, nome) %>% 
            summarise(qt_alunos_pnae = sum(qt_alunos_pnae,na.rm=TRUE),
                      vl_total_escolas = sum(vl_total_escolas,na.rm=TRUE)
                      ), 
          by=c("ano","nome"))
```

**4.2 Banco de Leite **  
4.2.1 Próprio **(OK)**  
4.2.2 Terceirizado **(OK)**  
**4.3 Serviço Lactário**  
4.3.1 Próprio **(OK)**  
4.3.2 Terceirizado **(OK)**  
**4.4 Serviço Nutrição**  
4.4.1 Próprio **(OK)**  
4.4.2 Terceirizado **(OK)**

``` r
sisvan_estab <- read_rds("data/sisvan_estab.rds")
sisvan <- sisvan_estab %>% 
  group_by(municipio, ano) %>% 
  summarise(
    servico_nutricao_proprio= sum(indicador_servico_nutricao_proprio),
    servico_nutricao_terceirizado = sum(indicador_servico_nutricao_terceirizado),
    servico_lactario_proprio= sum(indicador_servico_lactario_proprio),                         
    servico_lactario_terceirizado= sum(indicador_servico_lactario_terceirizado),                    servico_banco_leite_proprio= sum(indicador_servico_banco_leite_proprio),                        servico_banco_leite_terceirizado= sum(indicador_servico_banco_leite_terceirizado),
    # total = trunc(n()/12)
  ) %>% mutate(nome = municipio)
```

``` r
data_set %>%  glimpse()
#> Rows: 7,095
#> Columns: 17
#> Groups: nome [645]
#> $ nome                           <chr> "Adamantina", "Adamantina", "Adamantina~
#> $ ano                            <dbl> 2010, 2011, 2012, 2013, 2014, 2015, 201~
#> $ familias_beneficiarias_pbf     <dbl> 623.0000, 593.5833, 600.5833, 597.0000,~
#> $ pessoas_beneficiarias_pbf      <dbl> 0.0000, 0.0000, 0.0000, 0.0000, 0.0000,~
#> $ valor_pago_pbf                 <dbl> 564177, 683102, 820970, 871484, 742443,~
#> $ n_produtos_permanente          <int> 13, 13, 8, 8, 9, 9, 8, 7, 8, 7, NA, 8, ~
#> $ n_produtos_temporaria          <int> 7, 8, 8, 8, 8, 7, 8, 8, 7, 7, NA, 9, 9,~
#> $ area_plantada_permanente       <int> 931, 925, 918, 878, 883, 873, 862, 862,~
#> $ area_plantada_temporaria       <int> 14946, 14480, 14382, 14284, 13419, 1388~
#> $ rendimento_medio_permanente    <dbl> 17445.31, 15640.31, 13261.38, 13636.38,~
#> $ rendimento_medio_temporaria    <dbl> 24549.14, 28561.12, 19216.71, 28291.38,~
#> $ prop_area_colhida_permanente   <dbl> 7.693846, 7.691538, 12.500000, 12.50125~
#> $ prop_area_colhida_temporaria   <dbl> 14.28429, 12.50000, 12.50125, 12.49875,~
#> $ prop_valor_producao_permanente <dbl> 7.692308, 7.692308, 12.501250, 12.50000~
#> $ prop_valor_producao_temporaria <dbl> 14.28571, 12.49875, 12.49875, 12.49750,~
#> $ qt_alunos_pnae                 <dbl> 2904, 2738, 2614, 2810, 2745, 2705, 259~
#> $ vl_total_escolas               <dbl> 299280.00, 296700.00, 318288.00, 363100~
sisvan %>%  glimpse()
#> Rows: 10,965
#> Columns: 9
#> Groups: municipio [645]
#> $ municipio                        <chr> "Adamantina", "Adamantina", "Adamanti~
#> $ ano                              <dbl> 2006, 2007, 2008, 2009, 2010, 2011, 2~
#> $ servico_nutricao_proprio         <int> 24, 24, 24, 24, 24, 24, 24, 24, 24, 4~
#> $ servico_nutricao_terceirizado    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
#> $ servico_lactario_proprio         <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
#> $ servico_lactario_terceirizado    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
#> $ servico_banco_leite_proprio      <int> 0, 0, 0, 0, 9, 12, 12, 12, 23, 24, 24~
#> $ servico_banco_leite_terceirizado <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
#> $ nome                             <chr> "Adamantina", "Adamantina", "Adamanti~
data_set <- left_join(data_set, sisvan, by = c("ano","nome"))
data_set
#> # A tibble: 7,095 x 24
#> # Groups:   nome [645]
#>    nome         ano familias_beneficiari~1 pessoas_beneficiaria~2 valor_pago_pbf
#>    <chr>      <dbl>                  <dbl>                  <dbl>          <dbl>
#>  1 Adamantina  2010                   623                      0          564177
#>  2 Adamantina  2011                   594.                     0          683102
#>  3 Adamantina  2012                   601.                     0          820970
#>  4 Adamantina  2013                   597                      0          871484
#>  5 Adamantina  2014                   475.                     0          742443
#>  6 Adamantina  2015                   421.                     0          689508
#>  7 Adamantina  2016                   368.                     0          579115
#>  8 Adamantina  2017                   302.                     0          440561
#>  9 Adamantina  2018                   285.                   944.         424297
#> 10 Adamantina  2019                   282.                   876.         421973
#> # i 7,085 more rows
#> # i abbreviated names: 1: familias_beneficiarias_pbf,
#> #   2: pessoas_beneficiarias_pbf
#> # i 19 more variables: n_produtos_permanente <int>,
#> #   n_produtos_temporaria <int>, area_plantada_permanente <int>,
#> #   area_plantada_temporaria <int>, rendimento_medio_permanente <dbl>,
#> #   rendimento_medio_temporaria <dbl>, prop_area_colhida_permanente <dbl>, ...
```

------------------------------------------------------------------------

**DESAFIO 5**. Promover e proteger a alimentação adequada e saudável da
população brasileira, com estratégias de educação alimentar e
nutricional e medidas regulatórias

Temas: Promoção da alimentação saudável; Promoção da alimentação
saudável no ambiente escolar; Ações regulatórias; Controle dos riscos
relacionados ao consumo de alimentos e a exposição ao uso de
agrotóxicos.

------------------------------------------------------------------------

**DESAFIO 6**. Controlar e prevenir os agravos decorrentes da má
alimentação Diretriz correspondente: 5

Temas: Desnutrição; excesso de peso e obesidade; doenças e agravos
relacionados à má nutrição.

6.1 P/I abaixo do percentil 0,1: criança com peso muito baixo para a
idade.  
6.2 P/I maior ou igual ao percentil 0,1 e menor que o percentil 3:
criança com peso baixo para a idade.  
6.3 P/I maior ou igual ao percentil 3 e menor que o percentil 10:
criança em risco nutricional.  
6.4 P/I maior ou igual ao percentil 10 e menor que o percentil 97:
criança com peso adequado para a idade (eutrófica).  
6.5 P/I maior ou igual ao percentil 97: criança com risco de
sobrepeso.  
6.6 P/A abaixo do percentil 3: criança com baixo peso para sua altura.  
6.7 P/A maior ou igual ao percentil 3 e menor que o percentil 10:
criança com risco de baixo peso para sua altura.  
6.8 P/A maior ou igual ao percentil 10 e menor que o percentil 97:
criança com peso adequado para sua altura.  
6.9 P/A maior ou igual ao percentil 97: criança com risco de sobrepeso
para sua estatura.  
6.10 A/I abaixo do percentil 3: criança com altura baixa para a idade.  
6.11 A/I maior ou igual ao percentil 3 e menor que o percentil 10:
criança com risco para altura baixa para a idade.  
6.12 A/I maior ou igual ao percentil 10 e menor que o percentil 97:
criança com altura adequada para sua idade.  
6.13 A/I maior ou igual ao percentil 97: criança com altura elevada para
sua idade  
6.14 Percentil de IMC por idade abaixo de 5: adolescente com baixo
peso.  
6.15 Percentil de IMC por idade maior ou igual a 5 e menor que 85:
adolescente com peso adequado (eutrófico).  
6.16 Percentil de IMC por idade maior ou igual a 85: adolescente com
sobrepeso.  
6.17 Valores de IMC abaixo de 18,5: adulto com baixo peso.  
6.18 Valores de IMC maior ou igual a 18,5 e menor que 25,0: adulto com
peso adequado (eutrófico).  
6.19 Valores de IMC maior ou igual a 25,0 e menor que 30,0: adulto com
sobrepeso.  
6.20 Valores de IMC maior ou igual a 30,0: adulto com obesidade.  
6.21 índice de Massa Corporal (IMC) para avaliação do estado nutricional
de idosos.  
6.22 Valores de IMC menor ou igual a 22,0: idoso com baixo peso.  
6.23 Valores de IMC maior que 22,0 e menor que 27,0: idoso com peso
adequado (eutrófico).  
6.24 Valores de IMC maior ou igual a 27,0: idoso com sobrepeso.  
6.25 Valores de IMC por idade gestacional abaixo da curva inferior:
gestante com baixo peso.  
6.26 Valores de IMC por idade gestacional entre a curva inferior e a
segunda curva: gestante com peso adequado (eutrófica).  
6.27 Valores de IMC por idade gestacional entre a segunda curva e a
curva superior: gestante com sobrepeso.  
6.28 Valores de IMC por idade gestacional acima da curva superior:
gestante com obesidade

``` r
estado_nutricional <- read_rds("data/df_final.rds") %>% 
  group_by(ano,fase_da_vida,idade,indice,municipio) %>% 
  mutate(valor_p = valor/sum(valor)*100) %>% 
  ungroup()
df_nome <- read_rds("data/df_nome.rds")
estado_nutricional <- left_join(estado_nutricional, 
                                df_nome %>% select(nome, id_municipio_6) %>% 
                       rename(id_municipio  = id_municipio_6),
                     by = c("id_municipio")) %>% 
  select(-regiao,-uf,-codigo_uf,-id_municipio,-municipio,-indice_cri,
         -indice_ado,-valor) %>% relocate(ano,nome) %>% 
  pivot_wider(names_from = c(fase_da_vida,idade,indice,classe),
              values_from = c(valor_p)) %>% 
  janitor::clean_names()
```

7.  Consumo  
    7.1 Consumo de ultra processados **(OK)** 7.1.2 Consumo embutidos
    **(OK)** 7.1.3 Consumo processados **(OK)**

``` r
consumo <- read_rds("data/consumo/consumo.rds")
consumo <- left_join(consumo, df_nome %>% select(nome, id_municipio_6) %>% 
                       rename(codigo_ibge = id_municipio_6) %>%
                       mutate(codigo_ibge = as.numeric(codigo_ibge)),
                     by = c("codigo_ibge"))

tipos_relat <- c("CONS_ULTRA","CONS_EMBUT")
consumo <- consumo %>% select(-faixa_etaria, -codigo_ibge, -municipio,
                   -uf, -total, -monitorados) %>% 
  filter(tipo_relatorio %in% tipos_relat,
         fase_da_vida != "entre-6-meses-23-meses")

consumo <- consumo %>% 
  pivot_wider(names_from = c(fase_da_vida,tipo_relatorio),
              values_from = percent) %>% 
  janitor::clean_names()
```

``` r
consumo_estado_nutricional <- left_join(consumo, estado_nutricional,
          by=c("ano","nome"))
data_set <- left_join(data_set,consumo_estado_nutricional,
          by=c("ano","nome")) %>% 
  select(-municipio)
glimpse(data_set)
#> Rows: 7,095
#> Columns: 89
#> Groups: nome [645]
#> $ nome                                         <chr> "Adamantina", "Adamantina~
#> $ ano                                          <dbl> 2010, 2011, 2012, 2013, 2~
#> $ familias_beneficiarias_pbf                   <dbl> 623.0000, 593.5833, 600.5~
#> $ pessoas_beneficiarias_pbf                    <dbl> 0.0000, 0.0000, 0.0000, 0~
#> $ valor_pago_pbf                               <dbl> 564177, 683102, 820970, 8~
#> $ n_produtos_permanente                        <int> 13, 13, 8, 8, 9, 9, 8, 7,~
#> $ n_produtos_temporaria                        <int> 7, 8, 8, 8, 8, 7, 8, 8, 7~
#> $ area_plantada_permanente                     <int> 931, 925, 918, 878, 883, ~
#> $ area_plantada_temporaria                     <int> 14946, 14480, 14382, 1428~
#> $ rendimento_medio_permanente                  <dbl> 17445.31, 15640.31, 13261~
#> $ rendimento_medio_temporaria                  <dbl> 24549.14, 28561.12, 19216~
#> $ prop_area_colhida_permanente                 <dbl> 7.693846, 7.691538, 12.50~
#> $ prop_area_colhida_temporaria                 <dbl> 14.28429, 12.50000, 12.50~
#> $ prop_valor_producao_permanente               <dbl> 7.692308, 7.692308, 12.50~
#> $ prop_valor_producao_temporaria               <dbl> 14.28571, 12.49875, 12.49~
#> $ qt_alunos_pnae                               <dbl> 2904, 2738, 2614, 2810, 2~
#> $ vl_total_escolas                             <dbl> 299280.00, 296700.00, 318~
#> $ servico_nutricao_proprio                     <int> 24, 24, 24, 24, 24, 44, 4~
#> $ servico_nutricao_terceirizado                <int> 0, 0, 0, 0, 0, 0, 0, 0, 0~
#> $ servico_lactario_proprio                     <int> 0, 0, 0, 0, 0, 0, 0, 0, 0~
#> $ servico_lactario_terceirizado                <int> 0, 0, 0, 0, 0, 0, 0, 0, 0~
#> $ servico_banco_leite_proprio                  <int> 9, 12, 12, 12, 23, 24, 24~
#> $ servico_banco_leite_terceirizado             <int> 0, 0, 0, 0, 0, 0, 0, 0, 0~
#> $ adolecentes_cons_embut                       <dbl> NA, NA, NA, NA, NA, 0.333~
#> $ adolecentes_cons_ultra                       <dbl> NA, NA, NA, NA, NA, 1.000~
#> $ adultos_cons_embut                           <dbl> NA, NA, NA, NA, NA, 0.388~
#> $ adultos_cons_ultra                           <dbl> NA, NA, NA, NA, NA, 0.805~
#> $ criancas_2_4_cons_embut                      <dbl> NA, NA, NA, NA, NA, 0.318~
#> $ criancas_2_4_cons_ultra                      <dbl> NA, NA, NA, NA, NA, 0.772~
#> $ criancas_5_9_cons_embut                      <dbl> NA, NA, NA, NA, NA, 0.166~
#> $ criancas_5_9_cons_ultra                      <dbl> NA, NA, NA, NA, NA, 0.75,~
#> $ idosos_cons_embut                            <dbl> NA, NA, NA, NA, NA, 0.666~
#> $ idosos_cons_ultra                            <dbl> NA, NA, NA, NA, NA, 0.666~
#> $ crianca_0_4_peso_x_idade_muito_baixo         <dbl> NA, NA, NA, NA, NA, 1.197~
#> $ crianca_0_4_peso_x_idade_baixo               <dbl> NA, NA, NA, NA, NA, 2.670~
#> $ crianca_0_4_peso_x_idade_adequado            <dbl> NA, NA, NA, NA, NA, 88.39~
#> $ crianca_0_4_peso_x_idade_elevado             <dbl> NA, NA, NA, NA, NA, 7.734~
#> $ crianca_5_10_peso_x_idade_muito_baixo        <dbl> NA, NA, NA, NA, NA, 1.060~
#> $ crianca_5_10_peso_x_idade_baixo              <dbl> NA, NA, NA, NA, NA, 2.226~
#> $ crianca_5_10_peso_x_idade_adequado           <dbl> NA, NA, NA, NA, NA, 81.23~
#> $ crianca_5_10_peso_x_idade_elevado            <dbl> NA, NA, NA, NA, NA, 15.48~
#> $ crianca_0_4_peso_x_altura_magreza_acentuada  <dbl> NA, NA, NA, NA, NA, 0.644~
#> $ crianca_0_4_peso_x_altura_magreza            <dbl> NA, NA, NA, NA, NA, 1.933~
#> $ crianca_0_4_peso_x_altura_adequado           <dbl> NA, NA, NA, NA, NA, 66.85~
#> $ crianca_0_4_peso_x_altura_risco_sobrepeso    <dbl> NA, NA, NA, NA, NA, 18.60~
#> $ crianca_0_4_peso_x_altura_sobrepeso          <dbl> NA, NA, NA, NA, NA, 6.445~
#> $ crianca_0_4_peso_x_altura_obesidade          <dbl> NA, NA, NA, NA, NA, 5.524~
#> $ crianca_5_10_peso_x_altura_magreza_acentuada <dbl> NA, NA, NA, NA, NA, 0.000~
#> $ crianca_5_10_peso_x_altura_magreza           <dbl> NA, NA, NA, NA, NA, 0.000~
#> $ crianca_5_10_peso_x_altura_adequado          <dbl> NA, NA, NA, NA, NA, 81.25~
#> $ crianca_5_10_peso_x_altura_risco_sobrepeso   <dbl> NA, NA, NA, NA, NA, 6.250~
#> $ crianca_5_10_peso_x_altura_sobrepeso         <dbl> NA, NA, NA, NA, NA, 6.250~
#> $ crianca_5_10_peso_x_altura_obesidade         <dbl> NA, NA, NA, NA, NA, 6.250~
#> $ crianca_0_4_altura_x_idade_muito_baixa       <dbl> NA, NA, NA, NA, NA, 3.683~
#> $ crianca_0_4_altura_x_idade_baixa             <dbl> NA, NA, NA, NA, NA, 6.169~
#> $ crianca_0_4_altura_x_idade_adequada          <dbl> NA, NA, NA, NA, NA, 90.14~
#> $ crianca_5_10_altura_x_idade_muito_baixa      <dbl> NA, NA, NA, NA, NA, 0.954~
#> $ crianca_5_10_altura_x_idade_baixa            <dbl> NA, NA, NA, NA, NA, 1.378~
#> $ crianca_5_10_altura_x_idade_adequada         <dbl> NA, NA, NA, NA, NA, 97.66~
#> $ crianca_0_4_imc_x_idade_magreza_acentuada    <dbl> NA, NA, NA, NA, NA, 1.104~
#> $ crianca_0_4_imc_x_idade_magreza              <dbl> NA, NA, NA, NA, NA, 1.012~
#> $ crianca_0_4_imc_x_idade_adequado             <dbl> NA, NA, NA, NA, NA, 65.46~
#> $ crianca_0_4_imc_x_idade_risco_sobrepeso      <dbl> NA, NA, NA, NA, NA, 19.33~
#> $ crianca_0_4_imc_x_idade_sobrepeso            <dbl> NA, NA, NA, NA, NA, 7.918~
#> $ crianca_0_4_imc_x_idade_obesidade            <dbl> NA, NA, NA, NA, NA, 5.156~
#> $ crianca_5_10_imc_x_idade_magreza_acentuada   <dbl> NA, NA, NA, NA, NA, 1.272~
#> $ crianca_5_10_imc_x_idade_magreza             <dbl> NA, NA, NA, NA, NA, 2.651~
#> $ crianca_5_10_imc_x_idade_adequado            <dbl> NA, NA, NA, NA, NA, 60.12~
#> $ crianca_5_10_imc_x_idade_risco_sobrepeso     <dbl> NA, NA, NA, NA, NA, 19.19~
#> $ crianca_5_10_imc_x_idade_sobrepeso           <dbl> NA, NA, NA, NA, NA, 9.119~
#> $ crianca_5_10_imc_x_idade_obesidade           <dbl> NA, NA, NA, NA, NA, 7.635~
#> $ adolescente_altura_x_idade_muito_baixa       <dbl> NA, NA, NA, NA, NA, 20.21~
#> $ adolescente_altura_x_idade_baixa             <dbl> NA, NA, NA, NA, NA, 76.82~
#> $ adolescente_altura_x_idade_adequada          <dbl> NA, NA, NA, NA, NA, 2.955~
#> $ adolescente_imc_x_idade_magreza_acentuada    <dbl> NA, NA, NA, NA, NA, 0.662~
#> $ adolescente_imc_x_idade_magreza              <dbl> NA, NA, NA, NA, NA, 1.920~
#> $ adolescente_imc_x_idade_adequado             <dbl> NA, NA, NA, NA, NA, 58.60~
#> $ adolescente_imc_x_idade_sobrepeso            <dbl> NA, NA, NA, NA, NA, 23.44~
#> $ adolescente_imc_x_idade_obesidade            <dbl> NA, NA, NA, NA, NA, 12.91~
#> $ adolescente_imc_x_idade_obesidade_grave      <dbl> NA, NA, NA, NA, NA, 2.450~
#> $ adulto_imc_baixo_peso                        <dbl> NA, NA, NA, NA, NA, 2.047~
#> $ adulto_imc_adequado                          <dbl> NA, NA, NA, NA, NA, 27.81~
#> $ adulto_imc_sobrepeso                         <dbl> NA, NA, NA, NA, NA, 32.82~
#> $ adulto_imc_obesidade_grau_i                  <dbl> NA, NA, NA, NA, NA, 21.44~
#> $ adulto_imc_obesidade_grau_ii                 <dbl> NA, NA, NA, NA, NA, 9.044~
#> $ adulto_imc_obesidade_grau_iii                <dbl> NA, NA, NA, NA, NA, 6.825~
#> $ idoso_imc_baixo_peso                         <dbl> NA, NA, NA, NA, NA, 9.142~
#> $ idoso_imc_adequado                           <dbl> NA, NA, NA, NA, NA, 30.44~
#> $ idoso_imc_sobrepeso                          <dbl> NA, NA, NA, NA, NA, 60.41~
```

------------------------------------------------------------------------

**DESAFIO 7**. Ampliar a disponibilidade hídrica e o acesso à água para
a população, em especial a população pobre no meio rural

Temas: Água para consumo humano; Água para a produção de alimentos;
Recursos hídricos; Saneamento básico rural

------------------------------------------------------------------------

**DESAFIO 8**. Consolidar a implementação do sistema nacional de
segurança alimentar e nutricional (SISAN), aperfeiçoando a gestão
federativa, a intersetorialidade e a participação social Diretrizes
correspondentes: 3 e 8 (envolve ainda as diretrizes da LOSAN)

Temas: Intersetorialidade nas ações federativas; Participação social;
Gestão e financiamento do Sistema; Formação, pesquisa e extensão em SAN
e DHAA; Exigibilidade e monitoramento do DHAA

------------------------------------------------------------------------

**DESAFIO 9**. Apoio às iniciativas de promoção da soberania, segurança
alimentar e nutricional, do direito humano à alimentação adequada e de
sistemas alimentares democráticos, saudáveis e sustentáveis em âmbito
internacional, por meio do diálogo e da cooperação internacional

Temas: Governança global; Cooperação internacional; Participação da
sociedade civil

------------------------------------------------------------------------

**DESAFIO 10**. Socioeconômica

10.1Índice de Vulnerabilidade Social Dimensão Capital Humano  
10.2 Índice de Vulnerabilidade Social Dimensão Renda e Trabalho  
10.3 Índice de Desenvolvimento Humano Municipal  
10.4 ndice de Desenvolvimento Humano Municipal Dimensão Longevidade  
10.5 Índice de Desenvolvimento Humano Municipal Dimensão Educação  
10.6 Índice de Desenvolvimento Humano Municipal Dimensão Renda  
10.7 Índice de Desenvolvimento Humano Municipal Dimensão Sub
Escolaridade  
10.8 Índice de Desenvolvimento Humano Municipal Dimensão Frequência
Escolar  
10.9 Índice Gini  
10.10 Renda Per capita  
10.11 Proporção de empregados com carteira assinada com 18 anos ou
mais  
10.12 Proporção de empregados sem carteira assinada com 18 anos ou
mais  
10.13 Proporção de trabalhadores do setor público com 18 anos ou mais  
10.14 Proporção de trabalhadores por conta própria com 18 anos ou mais  
10.15 Proporção de empregadores com 18 anos ou mais  
10.6 Proporção do grau formalização dos ocupados com 18 anos ou mais  
10.7 Proporção de atividade das pessoas de 10 a 14 anos de idade  
10.8 População Economicamente Ativa (PEA) com 10 anos ou mais  
10.9 População Economicamente Ativa (PEA) com 10 a 14 anos  
10.10 População Economicamente Ativa (PEA) com 15 a 17 anos  
10.11 População Economicamente Ativa (PEA) com 18 anos ou mais

------------------------------------------------------------------------
