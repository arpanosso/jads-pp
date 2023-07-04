
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
data_set <- im %>% 
  group_by(nome, id_municipio, ano) %>% 
  summarise(familias_beneficiarias_pbf=trunc(mean(familias_beneficiarias_pbf,na.rm=TRUE)),
            pessoas_beneficiarias_pbf=trunc(mean(pessoas_beneficiarias_pbf,na.rm=TRUE)),
            valor_pago_pbf=mean(valor_pago_pbf,na.rm=TRUE))
glimpse(data_set)
#> Rows: 10,965
#> Columns: 6
#> Groups: nome, id_municipio [645]
#> $ nome                       <chr> "Adamantina", "Adamantina", "Adamantina", "~
#> $ id_municipio               <chr> "3500105", "3500105", "3500105", "3500105",~
#> $ ano                        <int> 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2~
#> $ familias_beneficiarias_pbf <dbl> 464, 1074, 1520, 1437, 1197, 931, 623, 593,~
#> $ pessoas_beneficiarias_pbf  <dbl> NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN~
#> $ valor_pago_pbf             <dbl> 27037.083, 59070.583, 68851.667, 70932.417,~
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
glimpse(lavouras)
#> Rows: 2,089,780
#> Columns: 15
#> $ ano                  <int> 1976, 1976, 1976, 1976, 1976, 1976, 1976, 1976, 1~
#> $ sigla_uf             <chr> "SP", "SP", "SP", "SP", "SP", "SP", "SP", "SP", "~
#> $ id_municipio         <chr> "3500105", "3500105", "3500105", "3500204", "3500~
#> $ produto              <chr> "Coco-da-baía", "Erva-mate (folha verde)", "Tange~
#> $ area_plantada        <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
#> $ area_colhida         <int> NA, NA, NA, NA, NA, NA, NA, NA, 877, NA, NA, NA, ~
#> $ quantidade_produzida <int> NA, NA, NA, NA, NA, NA, NA, NA, 712, NA, NA, NA, ~
#> $ rendimento_medio     <int> NA, NA, NA, NA, NA, NA, NA, NA, 812, NA, NA, NA, ~
#> $ valor_producao       <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
#> $ prop_area_plantada   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
#> $ prop_area_colhida    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, 96.16, NA, NA, NA~
#> $ prop_valor_producao  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, 94.56, NA, NA, NA~
#> $ nome                 <chr> "Adamantina", "Adamantina", "Adamantina", "Adolfo~
#> $ tipo                 <chr> "permanente", "permanente", "permanente", "perman~
#> $ na                   <int> 8, 8, 8, 8, 8, 8, 8, 8, 3, 8, 8, 8, 8, 8, 8, 8, 8~
glimpse(data_set)
#> Rows: 10,965
#> Columns: 6
#> Groups: nome, id_municipio [645]
#> $ nome                       <chr> "Adamantina", "Adamantina", "Adamantina", "~
#> $ id_municipio               <chr> "3500105", "3500105", "3500105", "3500105",~
#> $ ano                        <int> 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2~
#> $ familias_beneficiarias_pbf <dbl> 464, 1074, 1520, 1437, 1197, 931, 623, 593,~
#> $ pessoas_beneficiarias_pbf  <dbl> NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN~
#> $ valor_pago_pbf             <dbl> 27037.083, 59070.583, 68851.667, 70932.417,~
data_set<-left_join(lavouras %>% 
            filter(na != 8, ano >= 2010) %>% 
            select(-na),
          data_set, by = c("ano","id_municipio","nome")
          )

glimpse(data_set)
#> Rows: 60,722
#> Columns: 17
#> $ ano                        <int> 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2~
#> $ sigla_uf                   <chr> "SP", "SP", "SP", "SP", "SP", "SP", "SP", "~
#> $ id_municipio               <chr> "3500303", "3500303", "3500402", "3500402",~
#> $ produto                    <chr> "Café (em grão) Total", "Café (em grão) Ará~
#> $ area_plantada              <int> 530, 530, 1100, 21, 4045, 12, 4, 895, 10, 4~
#> $ area_colhida               <int> 530, 530, 1100, 21, 4045, 12, 4, 895, 10, 4~
#> $ quantidade_produzida       <int> 954, 954, 1832, 652, 121176, 504, 48, 2327,~
#> $ rendimento_medio           <int> 1800, 1800, 1665, 31048, 29957, 42000, 1200~
#> $ valor_producao             <int> 7530, 7530, 14652, 506, 45683, 762, 96, 930~
#> $ prop_area_plantada         <dbl> 6.25, 6.25, 97.52, 1.86, 89.00, 4.67, 0.26,~
#> $ prop_area_colhida          <dbl> 6.25, 6.25, 97.52, 1.86, 89.00, 4.67, 0.26,~
#> $ prop_valor_producao        <dbl> 4.99, 4.99, 95.37, 3.29, 96.12, 13.87, 0.87~
#> $ nome                       <chr> "Aguaí", "Aguaí", "Águas da Prata", "Águas ~
#> $ tipo                       <chr> "permanente", "permanente", "permanente", "~
#> $ familias_beneficiarias_pbf <dbl> 1538, 1538, 144, 144, 164, 173, 226, 158, 5~
#> $ pessoas_beneficiarias_pbf  <dbl> NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN~
#> $ valor_pago_pbf             <dbl> 208320.50, 208320.50, 21848.67, 21848.67, 2~
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
pnae <- pnae %>% 
  filter(ano >=2010 & ano <= 2019)
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
    total = trunc(n()/12)
  )
```

``` r
data_set %>%  glimpse()
#> Rows: 60,722
#> Columns: 19
#> $ ano                        <dbl> 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2~
#> $ sigla_uf                   <chr> "SP", "SP", "SP", "SP", "SP", "SP", "SP", "~
#> $ id_municipio               <chr> "3500303", "3500303", "3500402", "3500402",~
#> $ produto                    <chr> "Café (em grão) Total", "Café (em grão) Ará~
#> $ area_plantada              <int> 530, 530, 1100, 21, 4045, 12, 4, 895, 10, 4~
#> $ area_colhida               <int> 530, 530, 1100, 21, 4045, 12, 4, 895, 10, 4~
#> $ quantidade_produzida       <int> 954, 954, 1832, 652, 121176, 504, 48, 2327,~
#> $ rendimento_medio           <int> 1800, 1800, 1665, 31048, 29957, 42000, 1200~
#> $ valor_producao             <int> 7530, 7530, 14652, 506, 45683, 762, 96, 930~
#> $ prop_area_plantada         <dbl> 6.25, 6.25, 97.52, 1.86, 89.00, 4.67, 0.26,~
#> $ prop_area_colhida          <dbl> 6.25, 6.25, 97.52, 1.86, 89.00, 4.67, 0.26,~
#> $ prop_valor_producao        <dbl> 4.99, 4.99, 95.37, 3.29, 96.12, 13.87, 0.87~
#> $ nome                       <chr> "Aguaí", "Aguaí", "Águas da Prata", "Águas ~
#> $ tipo                       <chr> "permanente", "permanente", "permanente", "~
#> $ familias_beneficiarias_pbf <dbl> 1538, 1538, 144, 144, 164, 173, 226, 158, 5~
#> $ pessoas_beneficiarias_pbf  <dbl> NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN~
#> $ valor_pago_pbf             <dbl> 208320.50, 208320.50, 21848.67, 21848.67, 2~
#> $ qt_alunos_pnae             <dbl> 4158, 4158, 650, 650, 470, 358, 451, 475, 5~
#> $ vl_total_escolas           <dbl> 521800, 521800, 15504, 15504, 59900, 41600,~
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
estado_nutricional <- read_rds("data/df_final.rds")
glimpse(estado_nutricional)
#> Rows: 541,800
#> Columns: 13
#> $ ano          <dbl> 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 200~
#> $ fase_da_vida <chr> "Criança", "Criança", "Criança", "Criança", "Criança", "C~
#> $ idade        <chr> "0-4", "0-4", "0-4", "0-4", "0-4", "0-4", "0-4", "0-4", "~
#> $ indice       <chr> "Peso X Idade", "Peso X Idade", "Peso X Idade", "Peso X I~
#> $ indice_cri   <chr> "Peso X Idade", "Peso X Idade", "Peso X Idade", "Peso X I~
#> $ indice_ado   <chr> "Altura X Idade", "Altura X Idade", "Altura X Idade", "Al~
#> $ regiao       <chr> "SUDESTE", "SUDESTE", "SUDESTE", "SUDESTE", "SUDESTE", "S~
#> $ codigo_uf    <chr> "35", "35", "35", "35", "35", "35", "35", "35", "35", "35~
#> $ uf           <chr> "SP", "SP", "SP", "SP", "SP", "SP", "SP", "SP", "SP", "SP~
#> $ id_municipio <chr> "350010", "350010", "350010", "350010", "350020", "350020~
#> $ municipio    <chr> "Adamantina", "Adamantina", "Adamantina", "Adamantina", "~
#> $ classe       <chr> "muito_baixo", "baixo", "adequado", "elevado", "muito_bai~
#> $ valor        <dbl> 4, 4, 543, 48, 0, 0, 19, 1, 4, 2, 162, 16, 0, 1, 71, 2, 0~
```

7.  Consumo  
    7.1 Consumo de ultra processados **(OK)** 7.1.2 Consumo embutidos
    **(OK)** 7.1.3 Consumo processados **(OK)**

``` r
consumo <- read_rds("data/consumo/consumo.rds")
glimpse(consumo)
#> Rows: 63,004
#> Columns: 10
#> $ faixa_etaria   <chr> "2-anos-ou-mais", "2-anos-ou-mais", "2-anos-ou-mais", "~
#> $ fase_da_vida   <chr> "adolecentes", "adolecentes", "adolecentes", "adolecent~
#> $ tipo_relatorio <chr> "CONS_3REFEICOES", "CONS_3REFEICOES", "CONS_3REFEICOES"~
#> $ ano            <int> 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2~
#> $ uf             <chr> "SP", "SP", "SP", "SP", "SP", "SP", "SP", "SP", "SP", "~
#> $ codigo_ibge    <dbl> 350010, 350030, 350055, 350080, 350100, 350110, 350140,~
#> $ municipio      <chr> "ADAMANTINA", "AGUAÍ", "ÁGUAS DE SANTA BÁRBARA", "ALFRE~
#> $ total          <dbl> 5, 3, 0, 21, 4, 1, 0, 33, 10, 0, 1, 133, 92, 38, 1, 1, ~
#> $ percent        <dbl> 0.8333333, 0.7500000, 0.0000000, 0.6000000, 0.8000000, ~
#> $ monitorados    <dbl> 6, 4, 1, 35, 5, 1, 46, 47, 46, 11, 1, 161, 145, 48, 1, ~
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
