
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
  summarise(
    familias_beneficiarias_pbf=
      sum(familias_beneficiarias_pbf,na.rm=TRUE)/12,
    pessoas_beneficiarias_pbf=
      sum(pessoas_beneficiarias_pbf,na.rm=TRUE)/12,
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

``` r
data_set_muni <- data_set %>% 
  group_by(nome) %>% 
  summarise(
    pbf_familias = mean(familias_beneficiarias_pbf, na.rm=TRUE),
    pbf_pessoas = mean(pessoas_beneficiarias_pbf, na.rm=TRUE),
    pbf_valor = mean(valor_pago_pbf, na.rm=TRUE)
  )
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

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

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
lavouras_resu <- lavouras %>% 
  group_by(nome) %>% 
  summarize(
    npper = mean(n_produtos_permanente,na.rm=TRUE),           
    nptem = mean(n_produtos_temporaria,na.rm=TRUE),         
    areapper   = mean(area_plantada_permanente,na.rm=TRUE),       
    areapltem  = mean(area_plantada_temporaria,na.rm=TRUE),      
    rendper = mean(rendimento_medio_permanente,na.rm=TRUE),     
    rendtem = mean(rendimento_medio_temporaria,na.rm=TRUE),    
    pacolhidaper = mean(prop_area_colhida_permanente,na.rm=TRUE),     
    pacolhidatem = mean(prop_area_colhida_temporaria,na.rm=TRUE),   
    pvalorpper  = mean(prop_valor_producao_permanente,na.rm=TRUE),  
    pvalortem = mean(prop_valor_producao_temporaria,na.rm=TRUE) 
  )

data_set_muni <- left_join(data_set_muni, lavouras_resu,
          by="nome")
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
```

``` r
pnae <- left_join(pnae_alunos %>% 
          select(ano:qt_alunos_pnae,nome),
        pnae_recurso,
        by =c("nome","ano","esfera_governo","etapa_ensino"))
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


pnae_resumo <- pnae %>% 
  drop_na() %>% 
  group_by(ano, nome) %>% 
  summarise(qt_alunos_pnae = sum(qt_alunos_pnae,na.rm=TRUE),
            vl_total_escolas = sum(vl_total_escolas,na.rm=TRUE)
  ) %>% filter(ano >=2010) %>% 
  group_by(nome) %>% 
  summarise(
    pnae_qa = mean(qt_alunos_pnae, na.rm=TRUE),
    pnae_vte = mean(vl_total_escolas, na.rm=TRUE)
  )
data_set_muni <- left_join(data_set_muni, pnae_resumo, by = "nome")
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
    servico_nutricao_proprio = 
      sum(indicador_servico_nutricao_proprio),
    servico_nutricao_terceirizado =  
      sum(indicador_servico_nutricao_terceirizado),
    servico_lactario_proprio = 
      sum(indicador_servico_lactario_proprio),                         
    servico_lactario_terceirizado = 
      sum(indicador_servico_lactario_terceirizado),                    
    servico_banco_leite_proprio = 
      sum(indicador_servico_banco_leite_proprio),                        
    servico_banco_leite_terceirizado=
      sum(indicador_servico_banco_leite_terceirizado),
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


sisvan %>%  names
#> [1] "municipio"                        "ano"                             
#> [3] "servico_nutricao_proprio"         "servico_nutricao_terceirizado"   
#> [5] "servico_lactario_proprio"         "servico_lactario_terceirizado"   
#> [7] "servico_banco_leite_proprio"      "servico_banco_leite_terceirizado"
#> [9] "nome"
sisvan_resumo <- sisvan %>% 
  group_by(nome) %>% 
  summarise(
    snutrip = mean(servico_nutricao_proprio,na.rm=TRUE),
    snutrit = mean(servico_nutricao_terceirizado,na.rm=TRUE), 
    slacp = mean(servico_lactario_proprio,na.rm=TRUE),
    slact = mean(servico_lactario_terceirizado,na.rm=TRUE), 
    sbleitep = mean(servico_banco_leite_proprio,na.rm=TRUE),
    sbleitet = mean(servico_banco_leite_terceirizado,na.rm=TRUE),
  )
data_set_muni <- left_join(data_set_muni, sisvan_resumo, by = "nome")
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


estado_nutricional %>% 
  filter(nome == "Adamantina")
#> # A tibble: 15 x 58
#>      ano nome       crianca_0_4_peso_x_idade_muito_baixo crianca_0_4_peso_x_id~1
#>    <dbl> <chr>                                     <dbl>                   <dbl>
#>  1  2008 Adamantina                                0.668                   0.668
#>  2  2009 Adamantina                                1.25                    0.779
#>  3  2010 Adamantina                                0.650                   2.11 
#>  4  2011 Adamantina                                0.463                   1.76 
#>  5  2012 Adamantina                                1.08                    2.42 
#>  6  2013 Adamantina                                0.416                   2.77 
#>  7  2014 Adamantina                                1.55                    1.69 
#>  8  2015 Adamantina                                1.20                    2.67 
#>  9  2016 Adamantina                                0.920                   1.66 
#> 10  2017 Adamantina                                1.26                    3.25 
#> 11  2018 Adamantina                                0.740                   1.76 
#> 12  2019 Adamantina                                0.149                   1.19 
#> 13  2020 Adamantina                                0.909                   3.90 
#> 14  2021 Adamantina                                1.32                    3.44 
#> 15  2022 Adamantina                                2.32                    3.83 
#> # i abbreviated name: 1: crianca_0_4_peso_x_idade_baixo
#> # i 54 more variables: crianca_0_4_peso_x_idade_adequado <dbl>,
#> #   crianca_0_4_peso_x_idade_elevado <dbl>,
#> #   crianca_5_10_peso_x_idade_muito_baixo <dbl>,
#> #   crianca_5_10_peso_x_idade_baixo <dbl>,
#> #   crianca_5_10_peso_x_idade_adequado <dbl>,
#> #   crianca_5_10_peso_x_idade_elevado <dbl>, ...

estado_nutri_resumo <- estado_nutricional %>% 
  group_by(nome) %>% 
  mutate(across(crianca_0_4_peso_x_idade_muito_baixo:idoso_imc_sobrepeso,mean)) %>% 
  filter(ano == 2017) %>% 
  select(-ano)

estado_nutri_resumo %>% names
#>  [1] "nome"                                        
#>  [2] "crianca_0_4_peso_x_idade_muito_baixo"        
#>  [3] "crianca_0_4_peso_x_idade_baixo"              
#>  [4] "crianca_0_4_peso_x_idade_adequado"           
#>  [5] "crianca_0_4_peso_x_idade_elevado"            
#>  [6] "crianca_5_10_peso_x_idade_muito_baixo"       
#>  [7] "crianca_5_10_peso_x_idade_baixo"             
#>  [8] "crianca_5_10_peso_x_idade_adequado"          
#>  [9] "crianca_5_10_peso_x_idade_elevado"           
#> [10] "crianca_0_4_peso_x_altura_magreza_acentuada" 
#> [11] "crianca_0_4_peso_x_altura_magreza"           
#> [12] "crianca_0_4_peso_x_altura_adequado"          
#> [13] "crianca_0_4_peso_x_altura_risco_sobrepeso"   
#> [14] "crianca_0_4_peso_x_altura_sobrepeso"         
#> [15] "crianca_0_4_peso_x_altura_obesidade"         
#> [16] "crianca_5_10_peso_x_altura_magreza_acentuada"
#> [17] "crianca_5_10_peso_x_altura_magreza"          
#> [18] "crianca_5_10_peso_x_altura_adequado"         
#> [19] "crianca_5_10_peso_x_altura_risco_sobrepeso"  
#> [20] "crianca_5_10_peso_x_altura_sobrepeso"        
#> [21] "crianca_5_10_peso_x_altura_obesidade"        
#> [22] "crianca_0_4_altura_x_idade_muito_baixa"      
#> [23] "crianca_0_4_altura_x_idade_baixa"            
#> [24] "crianca_0_4_altura_x_idade_adequada"         
#> [25] "crianca_5_10_altura_x_idade_muito_baixa"     
#> [26] "crianca_5_10_altura_x_idade_baixa"           
#> [27] "crianca_5_10_altura_x_idade_adequada"        
#> [28] "crianca_0_4_imc_x_idade_magreza_acentuada"   
#> [29] "crianca_0_4_imc_x_idade_magreza"             
#> [30] "crianca_0_4_imc_x_idade_adequado"            
#> [31] "crianca_0_4_imc_x_idade_risco_sobrepeso"     
#> [32] "crianca_0_4_imc_x_idade_sobrepeso"           
#> [33] "crianca_0_4_imc_x_idade_obesidade"           
#> [34] "crianca_5_10_imc_x_idade_magreza_acentuada"  
#> [35] "crianca_5_10_imc_x_idade_magreza"            
#> [36] "crianca_5_10_imc_x_idade_adequado"           
#> [37] "crianca_5_10_imc_x_idade_risco_sobrepeso"    
#> [38] "crianca_5_10_imc_x_idade_sobrepeso"          
#> [39] "crianca_5_10_imc_x_idade_obesidade"          
#> [40] "adolescente_altura_x_idade_muito_baixa"      
#> [41] "adolescente_altura_x_idade_baixa"            
#> [42] "adolescente_altura_x_idade_adequada"         
#> [43] "adolescente_imc_x_idade_magreza_acentuada"   
#> [44] "adolescente_imc_x_idade_magreza"             
#> [45] "adolescente_imc_x_idade_adequado"            
#> [46] "adolescente_imc_x_idade_sobrepeso"           
#> [47] "adolescente_imc_x_idade_obesidade"           
#> [48] "adolescente_imc_x_idade_obesidade_grave"     
#> [49] "adulto_imc_baixo_peso"                       
#> [50] "adulto_imc_adequado"                         
#> [51] "adulto_imc_sobrepeso"                        
#> [52] "adulto_imc_obesidade_grau_i"                 
#> [53] "adulto_imc_obesidade_grau_ii"                
#> [54] "adulto_imc_obesidade_grau_iii"               
#> [55] "idoso_imc_baixo_peso"                        
#> [56] "idoso_imc_adequado"                          
#> [57] "idoso_imc_sobrepeso"
nomes_antigos <- names(estado_nutri_resumo)
novos_nomes <- c(
  "nome"  ,                                      
 "cri04pxi_mb" ,       
 "cri04pxi_b" ,             
 "cri04pxi_a"  ,         
 "cri04pxi_e"  ,          
 "cri510pxi_mb" ,      
 "cri510pxi_b"     ,        
 "cri510pxi_a"  ,        
 "cri510pxi_e"  ,         
 "cri04pxa_magacen", 
 "cri04pxa_mag" ,          
 "cri04pxa_a",          
 "cri04pxa_rsobre",   
 "cri04pxa_sobr",         
 "cri04pxa_obes" ,        
 "cri510pxa_magacen",
 "cri510pxa_mag" ,         
 "cri510pxa_a" ,        
 "cri510pxa_rsobr" , 
 "cri510pxa_sobr" ,       
 "cri510pxa_obes",        
 "cri04axi_mb" ,     
 "cri04axi_b" ,           
 "cri04axi_a" ,        
 "cri510axi_muito_baixa",     
 "cri510axi_b" ,          
 "cri510axi_a",        
 "cri04imcxi_magacen",   
 "cri04imcxi_mag" ,            
 "cri04imcxi_a" ,           
 "cri04imcxi_r_sobr" ,    
 "cri04imcxi_sobr",           
 "cri04imcxi_obes",           
 "cri510imcxi_magacen",  
 "cri510imcxi_mag" ,           
 "cri510imcxi_a" ,          
 "cri510imcxi_rsobr",    
 "cri510imcxi_sobr",          
 "cri510imcxi_obes",          
 "adolaxi_mb",      
 "adolaxi_b",            
 "adolaxi_a" ,        
 "adolimcxi_magacen",   
 "adolimcxi_mag",             
 "adolimcxi_a",            
 "adolimcxi_sobr",           
 "adolimcxi_obes"  ,         
 "adolimcxi_obesgra",     
 "adulimc_b" ,                      
 "adulimc_a",                         
 "adulimc_sob" ,                       
 "adulimc_obesg_i",                
 "adulimc_obesg_ii"  ,              
 "adulimc_obesg_iii",               
 "idimc_b",                        
 "idimc_a" ,                         
 "idimc_sob"
)
estado_nutri_resumo <- estado_nutri_resumo %>% 
  rename_at(vars(nomes_antigos), ~novos_nomes)
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
consumo$tipo_relatorio %>%  unique()
#>  [1] "CONS_3REFEICOES"     "CONS_ASSISTETV"      "CONS_BEBIDAS"       
#>  [4] "CONS_EMBUT"          "CONS_FEIJAO"         "CONS_FRUTA"         
#>  [7] "CONS_GULOSEIMA"      "CONS_SALGADINHO"     "CONS_ULTRA"         
#> [10] "CONS_VERD"           "AMC"                 "BEBIDA"             
#> [13] "DAM"                 "DOCES"               "EMBUT"              
#> [16] "FERRO"               "FMCA"                "SALGAD"             
#> [19] "VITAA"               "IntruducaoAlimentos" "AME"                
#> [22] "ULTRA"

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

consumo_resumo <- consumo %>% 
  group_by(nome) %>% 
  summarise(
    adol_embut = mean(adolecentes_cons_embut,na.rm=TRUE),  
    adol_ultra = mean(adolecentes_cons_ultra,na.rm=TRUE), 
    adul_embut = mean(adultos_cons_embut,na.rm=TRUE), 
    adul_ultra = mean(adultos_cons_ultra,na.rm=TRUE),     
    cri24_embut = mean(criancas_2_4_cons_embut,na.rm=TRUE),
    cri24_ultra = mean(criancas_2_4_cons_ultra,na.rm=TRUE), 
    cri59_embut = mean(criancas_5_9_cons_embut,na.rm=TRUE),
    cri59_ultra = mean(criancas_5_9_cons_ultra,na.rm=TRUE),
    ido_embut = mean(idosos_cons_embut,na.rm=TRUE),      
    ido_ultra = mean(idosos_cons_ultra,na.rm=TRUE)      
  )

consumo_estado_nutricional_resumo <- left_join(estado_nutri_resumo, consumo_resumo,
          by = "nome") 
data_set_muni <- left_join(data_set_muni,
                           consumo_estado_nutricional_resumo,
                           by = "nome")
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

# Análise multivariada

``` r
library(readxl)
library(tidyverse)
library(vegan)
library(corrplot)
source("R/my-functions.R")
theme_set(theme_bw())
```

``` r
da <- data_set_muni %>% ungroup()
glimpse(da)
#> Rows: 645
#> Columns: 88
#> $ nome                  <chr> "Adamantina", "Adolfo", "Aguaí", "Agudos", "Alam~
#> $ pbf_familias          <dbl> 436.5379, 132.8106, 1406.3788, 1126.7197, 191.31~
#> $ pbf_pessoas           <dbl> 225.8409, 115.1894, 1080.9015, 759.0227, 164.257~
#> $ pbf_valor             <dbl> 580647.5, 241276.5, 2131788.0, 1731999.6, 275644~
#> $ npper                 <dbl> 9.0, 6.1, 7.5, 2.2, 2.5, 6.7, 2.4, 4.9, 4.7, 3.3~
#> $ nptem                 <dbl> 7.6, 7.4, 8.9, 5.8, 7.2, 4.7, 3.5, 8.2, 6.0, 4.3~
#> $ areapper              <dbl> 887.9, 2041.2, 9794.6, 1011.1, 443.1, 29.5, 4643~
#> $ areapltem             <dbl> 14041.6, 7094.8, 22069.4, 15418.7, 3606.3, 425.2~
#> $ rendper               <dbl> 14306.36, 20505.62, 23692.44, 17879.43, 24716.70~
#> $ rendtem               <dbl> 27582.157, 26993.338, 25704.024, 22371.008, 2484~
#> $ pacolhidaper          <dbl> 11.618140, 17.249917, 13.392304, 56.667000, 44.9~
#> $ pacolhidatem          <dbl> 13.21388, 14.02318, 11.51983, 17.67809, 14.10731~
#> $ pvalorpper            <dbl> 11.618295, 17.249375, 13.393107, 56.667333, 44.9~
#> $ pvalortem             <dbl> 13.21407, 14.02388, 11.52053, 17.67832, 14.10718~
#> $ pnae_qa               <dbl> 2656.5385, 636.0000, 4265.6154, 4010.7692, 649.5~
#> $ pnae_vte              <dbl> 322282.32, 40735.03, 462165.75, 522470.72, 77041~
#> $ snutrip               <dbl> 46.0588235, 0.0000000, 10.2352941, 12.0000000, 0~
#> $ snutrit               <dbl> 0.0000000, 0.0000000, 0.4117647, 0.0000000, 0.00~
#> $ slacp                 <dbl> 0.000000, 0.000000, 5.470588, 12.000000, 0.00000~
#> $ slact                 <dbl> 0.000000, 0.000000, 0.000000, 0.000000, 0.000000~
#> $ sbleitep              <dbl> 15.29412, 0.00000, 0.00000, 0.00000, 0.00000, 0.~
#> $ sbleitet              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
#> $ cri04pxi_mb           <dbl> 0.9929431, 0.3525272, 1.0440122, 1.3503859, 1.14~
#> $ cri04pxi_b            <dbl> 2.2597650, 1.1960755, 2.9205930, 3.2246802, 1.01~
#> $ cri04pxi_a            <dbl> 88.77147, 77.97640, 87.10713, 84.93690, 89.48770~
#> $ cri04pxi_e            <dbl> 7.975827, 20.474993, 8.928269, 10.488029, 8.3543~
#> $ cri510pxi_mb          <dbl> 0.9307421, 0.9864829, 0.8833910, 0.5671433, 2.05~
#> $ cri510pxi_b           <dbl> 1.9760752, 1.1837923, 2.8806551, 1.7478702, 1.73~
#> $ cri510pxi_a           <dbl> 81.06272, 71.41206, 83.25089, 86.56935, 84.17899~
#> $ cri510pxi_e           <dbl> 16.030466, 26.417660, 12.985059, 11.115636, 12.0~
#> $ cri04pxa_magacen      <dbl> 1.5407599, 2.4293712, 1.6608679, 0.8990326, 0.99~
#> $ cri04pxa_mag          <dbl> 2.6520022, 1.8948926, 2.6469319, 2.3548576, 0.84~
#> $ cri04pxa_a            <dbl> 69.40093, 49.74267, 64.28097, 63.88259, 63.35015~
#> $ cri04pxa_rsobre       <dbl> 16.41736, 18.93043, 17.30181, 21.72510, 21.44772~
#> $ cri04pxa_sobr         <dbl> 5.355246, 9.357953, 6.686459, 6.801356, 7.237331~
#> $ cri04pxa_obes         <dbl> 4.633707, 17.644681, 7.422963, 4.337061, 6.13016~
#> $ cri510pxa_magacen     <dbl> 1.08550068, NaN, 1.27461511, 0.08547009, NaN, Na~
#> $ cri510pxa_mag         <dbl> 2.2838779, NaN, 3.0122547, 1.5514428, NaN, NaN, ~
#> $ cri510pxa_a           <dbl> 65.40370, NaN, 74.02357, 73.82065, NaN, NaN, NaN~
#> $ cri510pxa_rsobr       <dbl> 7.397055, NaN, 7.289336, 10.694277, NaN, NaN, Na~
#> $ cri510pxa_sobr        <dbl> 4.6388073, NaN, 3.0169173, 0.2176226, NaN, NaN, ~
#> $ cri510pxa_obes        <dbl> 19.191059, NaN, 11.383305, 13.630539, NaN, NaN, ~
#> $ cri04axi_mb           <dbl> 10.583486, 3.407255, 3.522299, 3.545870, 2.58573~
#> $ cri04axi_b            <dbl> 19.705609, 4.216942, 5.968379, 10.744564, 2.4673~
#> $ cri04axi_a            <dbl> 69.71090, 92.37580, 90.50932, 85.70957, 94.94689~
#> $ cri510axi_muito_baixa <dbl> 3.1166001, 6.4190068, 1.5124167, 3.6218814, 4.21~
#> $ cri510axi_b           <dbl> 6.277196, 5.573394, 3.898615, 6.028638, 3.473670~
#> $ cri510axi_a           <dbl> 90.60620, 88.00760, 94.58897, 90.34948, 92.31598~
#> $ cri04imcxi_magacen    <dbl> 1.5918370, 3.6262531, 2.1931896, 1.2308091, 1.57~
#> $ cri04imcxi_mag        <dbl> 2.331711, 2.295646, 3.065472, 2.196628, 1.066192~
#> $ cri04imcxi_a          <dbl> 67.62946, 48.09393, 62.04480, 61.62911, 62.93601~
#> $ cri04imcxi_r_sobr     <dbl> 17.76356, 20.93877, 17.69446, 23.08856, 21.43411~
#> $ cri04imcxi_sobr       <dbl> 6.333984, 10.624717, 8.065498, 7.677431, 7.37325~
#> $ cri04imcxi_obes       <dbl> 4.349453, 14.420682, 6.936580, 4.177465, 5.61544~
#> $ cri510imcxi_magacen   <dbl> 1.0145568, 1.6008621, 1.6761426, 0.8045070, 0.83~
#> $ cri510imcxi_mag       <dbl> 2.161605, 1.886477, 2.383852, 1.657095, 1.189307~
#> $ cri510imcxi_a         <dbl> 63.71386, 49.61408, 65.40158, 70.93171, 63.94279~
#> $ cri510imcxi_rsobr     <dbl> 15.95030, 15.71876, 14.39828, 15.10013, 16.09458~
#> $ cri510imcxi_sobr      <dbl> 9.954089, 15.516265, 9.017791, 6.619272, 10.4677~
#> $ cri510imcxi_obes      <dbl> 7.205588, 15.663556, 7.122350, 4.887292, 7.47477~
#> $ adolaxi_mb            <dbl> 4.3920508, 4.4795638, 2.0691786, 2.0074307, 1.21~
#> $ adolaxi_b             <dbl> 17.825715, 2.339804, 4.495142, 8.168997, 4.39140~
#> $ adolaxi_a             <dbl> 77.78223, 93.18063, 93.43568, 89.82357, 94.38935~
#> $ adolimcxi_magacen     <dbl> 0.7643364, 1.8148723, 1.3302470, 0.6596336, 0.20~
#> $ adolimcxi_mag         <dbl> 2.743184, 1.067567, 2.394559, 2.608314, 1.217974~
#> $ adolimcxi_a           <dbl> 60.84144, 62.17501, 64.95670, 66.28651, 64.83455~
#> $ adolimcxi_sobr        <dbl> 20.22308, 20.38711, 18.30567, 18.61243, 22.62429~
#> $ adolimcxi_obes        <dbl> 12.032952, 10.041761, 9.864493, 9.418858, 9.3798~
#> $ adolimcxi_obesgra     <dbl> 3.3950006, 4.5136854, 3.1483306, 2.4142547, 1.73~
#> $ adulimc_b             <dbl> 3.647834, 2.131770, 3.026713, 3.552147, 2.952508~
#> $ adulimc_a             <dbl> 28.42171, 36.31893, 31.18899, 29.74796, 30.39134~
#> $ adulimc_sob           <dbl> 18.88469, 31.89635, 27.55629, 28.68666, 29.46555~
#> $ adulimc_obesg_i       <dbl> 21.64474, 16.98449, 22.18332, 18.55913, 21.85711~
#> $ adulimc_obesg_ii      <dbl> 16.914280, 7.817509, 9.668569, 12.509599, 8.4523~
#> $ adulimc_obesg_iii     <dbl> 10.486739, 4.850957, 6.376126, 6.944498, 6.88117~
#> $ idimc_b               <dbl> 17.03068, NaN, NaN, 10.36169, NaN, NaN, NaN, NaN~
#> $ idimc_a               <dbl> 51.50818, NaN, NaN, 34.79482, NaN, NaN, NaN, NaN~
#> $ idimc_sob             <dbl> 31.46114, NaN, NaN, 54.84349, NaN, NaN, NaN, NaN~
#> $ adol_embut            <dbl> 0.3446095, NA, 0.5274064, NaN, NA, 0.3157996, NA~
#> $ adol_ultra            <dbl> 0.9189851, NA, 0.9886364, NaN, NA, 0.7056207, NA~
#> $ adul_embut            <dbl> 0.26186351, NA, 0.42161218, 0.16666667, NA, 0.19~
#> $ adul_ultra            <dbl> 0.8228250, NA, 0.6592422, 0.1666667, NA, 0.53705~
#> $ cri24_embut           <dbl> 0.1607955, NA, 0.7071429, NaN, NA, 0.2452945, NA~
#> $ cri24_ultra           <dbl> 0.6931818, NA, 0.9911765, NaN, NA, 0.7630409, NA~
#> $ cri59_embut           <dbl> 0.03333333, NA, 0.80354090, 0.00000000, NA, 0.29~
#> $ cri59_ultra           <dbl> 0.7500000, NA, 0.9978632, 0.0000000, NA, 0.76061~
#> $ ido_embut             <dbl> 0.6888889, NA, 0.5333333, 0.3888889, NA, 0.33266~
#> $ ido_ultra             <dbl> 0.6888889, NA, 0.5333333, 0.3888889, NA, 0.35307~
```

``` r
nomes <- data_set_muni$nome
da_pad <- decostand(da[-1] , 
                      method = "standardize",
                      na.rm=TRUE)

da_pad$nome <- nomes
da_pad <- da_pad %>% 
  select(-nome)


da_pad_euc<-vegdist(da_pad,"euclidean",na.rm=TRUE) 
da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
plot(da_pad_euc_ward, 
     ylab="Distância Euclidiana",
     xlab="Acessos", hang=-1,
     col="blue", las=1,
     cex=.6,lwd=1.5);box()
```

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
grupo<-cutree(da_pad_euc_ward,3)
```

``` r
da_pad$gp <- grupo
da_pad$nomes <- nomes

da_pad_gp <- da_pad %>% 
  select(-gp, -nomes)

gp <- da_pad %>% 
  pull(gp)

nome <- da_pad %>% 
  pull(nomes)

df <- da_pad_gp %>% 
  select(pbf_familias:cri04pxa_obes) %>% 
  drop_na()

pca <-  prcomp(df,
               scale=T)
```

``` r
# Autovalores
eig<-pca$sdev^2
print(round(eig,3))
#>  [1] 17.074  4.085  2.096  1.918  1.368  1.057  0.979  0.901  0.782  0.641
#> [11]  0.607  0.557  0.447  0.361  0.308  0.264  0.232  0.221  0.202  0.183
#> [21]  0.161  0.131  0.114  0.098  0.077  0.060  0.043  0.022  0.008  0.003
#> [31]  0.000  0.000  0.000  0.000  0.000
ve<-eig/sum(eig)
print(round(ve,4))
#>  [1] 0.4878 0.1167 0.0599 0.0548 0.0391 0.0302 0.0280 0.0258 0.0223 0.0183
#> [11] 0.0174 0.0159 0.0128 0.0103 0.0088 0.0075 0.0066 0.0063 0.0058 0.0052
#> [21] 0.0046 0.0037 0.0032 0.0028 0.0022 0.0017 0.0012 0.0006 0.0002 0.0001
#> [31] 0.0000 0.0000 0.0000 0.0000 0.0000
print(round(cumsum(ve),4)*100)
#>  [1]  48.78  60.45  66.44  71.92  75.83  78.85  81.65  84.22  86.46  88.29
#> [11]  90.02  91.62  92.89  93.93  94.81  95.56  96.22  96.85  97.43  97.95
#> [21]  98.41  98.79  99.11  99.39  99.61  99.78  99.91  99.97  99.99 100.00
#> [31] 100.00 100.00 100.00 100.00 100.00
mcor<-cor(df,pca$x)
corrplot(mcor)
```

![](README_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
screeplot(pca)
abline(h=1)
```

![](README_files/figure-gfm/unnamed-chunk-22-2.png)<!-- -->

``` r
pc1V<-cor(df,pca$x)[,1]/sd(cor(df,pca$x)[,1])
pc2V<-cor(df,pca$x)[,2]/sd(cor(df,pca$x)[,2])
pc3V<-cor(df,pca$x)[,3]/sd(cor(df,pca$x)[,3])
pc1c<-pca$x[,1]/sd(pca$x[,1])
pc2c<-pca$x[,2]/sd(pca$x[,2])
pc3c<-pca$x[,3]/sd(pca$x[,3])
nv<-ncol(df)
```

``` r
bip<-data.frame(pc1c,pc2c,pc3c)#,nome)
```

``` r
texto <- data.frame(
  x = pc1V,
  y = pc2V,
  z = pc3V,
  label = names(df)
)

bip %>% 
  ggplot(aes(x=pc1c, y=pc2c))+
  geom_point() +
  coord_cartesian(xlim=c(-2,10)) +
  # geom_point(aes(shape = nomes, color = nomes), size = 3)+ theme_minimal()+
  #scale_shape_manual(values=16:19)+
  #scale_color_manual(values=c("#009E73", "#999999","#D55E00", "#A6761D"))+
  #annotate(geom="text", x=pc1V, y=pc2V, label=names(pc1V),
  #            color="black",font=3)+
  geom_vline(aes(xintercept=0),
             color="black", size=1) +
  geom_hline(aes(yintercept=0),
             color="black", size=1) +
  annotate(geom="segment",
           x=rep(0,length(df)),
           xend=texto$x,
           y=rep(0,length(df)),
           yend=texto$y,color="black",lwd=.5) +
  geom_label(data=texto,aes(x=x,y=y,label=label),
             color="black",angle=0,fontface="bold",size=4,fill="white") +
  labs(x=paste("CP1 (",round(100*ve[1],2),"%)",sep=""),
       y=paste("CP2 (",round(100*ve[2],2),"%)",sep=""),
       color="",shape="") +
  theme(legend.position = "top")
```

![](README_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
# ck<-sum(pca$sdev^2>=0.98)
# tabelapca<-vector()
# for( l in 1:ck) tabelapca<-cbind(tabelapca,mcor[,l])
# colnames(tabelapca)<-paste(rep(c("PC"),ck),1:ck,sep="")
# pcat<-round(tabelapca,3)
# tabelapca<-tabelapca[order(abs(tabelapca[,1])),]
# print(tabelapca)
```
