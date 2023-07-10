
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Carregando Pacotes

``` r
library(readxl)
library(tidyverse)
library(vegan)
library(corrplot)
source("R/my-functions.R")
theme_set(theme_bw())
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

### Base de dados - “im”

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
```

### Visualização de dados

``` r
tab_im <- im %>% 
  group_by(ano) %>% filter(ano >=2010, ano < 2020) %>% 
  summarise(
    nfamilias = sum(familias_beneficiarias_pbf, na.rm=TRUE),
    npessoa = sum(pessoas_beneficiarias_pbf, na.rm=TRUE),
    valorpago = sum(valor_pago_pbf, na.rm=TRUE),
  )

tab_im %>% 
  ggplot(aes(x=ano, y=nfamilias)) +
  geom_col(col="black",fill="lightgray")
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
tab_im %>% 
  ggplot(aes(x=ano, y=npessoa)) +
  geom_col(col="black",fill="lightgray")
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
tab_im %>% 
  ggplot(aes(x=ano, y=valorpago)) +
  geom_col(col="black",fill="lightgray")
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

#### Tabela de Dados

``` r
tab_im
#> # A tibble: 10 x 4
#>      ano nfamilias  npessoa  valorpago
#>    <int>     <int>    <int>      <dbl>
#>  1  2010  13346131        0 1145282091
#>  2  2011  14383203        0 1493753207
#>  3  2012  14645039        0 1785146440
#>  4  2013  15529029        0 2103544648
#>  5  2014  15951587        0 2334973894
#>  6  2015  16685980        0 2500827660
#>  7  2016  17470926        0 2682409405
#>  8  2017  17560121        0 2806387560
#>  9  2018  18627450 55840305 3047820940
#> 10  2019  18027294 52696807 3078050973
```

#### Criando a base resumo

``` r
data_set_muni <- data_set %>% filter(ano>= 2015, ano < 2020) %>% 
  group_by(nome) %>% 
  summarise(
    pbf_familias = mean(familias_beneficiarias_pbf, na.rm=TRUE),
    pbf_pessoas = mean(pessoas_beneficiarias_pbf, na.rm=TRUE),
    pbf_valor = mean(valor_pago_pbf, na.rm=TRUE)
  )
```

#### Agrupamento e Correlação

``` r
nomes <- data_set_muni$nome
da <- data_set_muni
da_pad <- decostand(da[-1] , 
                      method = "standardize",
                      na.rm=TRUE)
df <- da_pad

visdat::vis_miss(da_pad)
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
da_pad_euc<-vegdist(df,"euclidean",na.rm=TRUE) 
da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
plot(da_pad_euc_ward, 
     ylab="Distância Euclidiana",
     xlab="Acessos", hang=-1,
     col="blue", las=1,
     cex=.6,lwd=1.5);box()
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
grupo<-cutree(da_pad_euc_ward,3)
```

``` r
cor_matrix <- cor(df,use = "na.or.complete")
corrplot(cor_matrix, method="ellipse")
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

- Variáveis altamente correlacionadas, apenas uma delas representa bem
  esse banco de dados

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

### Base de dados “lavouras”

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
```

### Visualização de dados

``` r
lavouras %>% 
  filter(na !=8, ano >= 2010) %>% 
  arrange(ano) %>% 
  relocate(tipo) %>% 
  group_by(tipo,produto) %>% 
  summarise(pvp = sum(prop_valor_producao,na.rm = TRUE)) %>% 
  mutate(produto = produto %>% fct_lump(n=10, w=pvp) %>% fct_reorder(pvp)) %>% 
  ggplot(aes(x=pvp,y=produto,fill=tipo)) +
  geom_col(color="black")+
  facet_wrap(~tipo,scales = "free") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust=1))
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
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

``` r
coeff <-1
tabe_lavouras <- lavouras %>% 
  group_by(ano) %>%
  summarise(
    n_produtos_permanente = mean(n_produtos_permanente,na.rm=TRUE),
    n_produtos_temporaria = mean(n_produtos_temporaria,na.rm=TRUE),
    area_plantada_permanente = mean(area_plantada_permanente,na.rm=TRUE),
    area_plantada_temporaria = mean(area_plantada_temporaria,na.rm=TRUE),
    rendimento_medio_permanente = mean(rendimento_medio_permanente,na.rm=TRUE),
    rendimento_medio_temporaria = mean(rendimento_medio_temporaria,na.rm=TRUE),
    prop_area_colhida_permanente = mean(prop_area_colhida_permanente,na.rm=TRUE),
    prop_area_colhida_temporaria = mean(prop_area_colhida_temporaria,na.rm=TRUE),
    prop_valor_producao_permanente = mean(prop_valor_producao_permanente,na.rm=TRUE),
    prop_valor_producao_temporaria = mean(prop_valor_producao_temporaria,na.rm=TRUE)
  )
```

``` r
tabe_lavouras %>% 
  ggplot(aes(x = ano)) +
  geom_line(aes(y = prop_valor_producao_permanente), color="red") +
  geom_line(aes(y = prop_valor_producao_temporaria/coeff)) +
  scale_y_continuous(
    name = "prop_valor_producao_permanente",
    sec.axis = sec_axis(~.*coeff, name="prop_valor_producao_temporaria")
  ) +
  theme(axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red"))
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
tabe_lavouras %>% 
  ggplot(aes(x = ano)) +
  geom_line(aes(y = prop_area_colhida_permanente), color="red") +
  geom_line(aes(y = prop_area_colhida_temporaria/coeff)) +
  scale_y_continuous(
    name = "prop_area_colhida_permanente",
    sec.axis = sec_axis(~.*coeff, name="prop_area_colhida_temporaria")
  ) +
  theme(axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red"))
```

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
tabe_lavouras %>% 
  ggplot(aes(x = ano)) +
  geom_line(aes(y = rendimento_medio_permanente), color="red") +
  geom_line(aes(y = rendimento_medio_temporaria/coeff)) +
  scale_y_continuous(
    name = "rendimento_medio_permanente",
    sec.axis = sec_axis(~.*coeff, name="rendimento_medio_temporaria")
  ) +
  theme(axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red"))
```

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
tabe_lavouras %>% 
  ggplot(aes(x = ano)) +
  geom_line(aes(y = area_plantada_permanente), color="red") +
  geom_line(aes(y = area_plantada_temporaria/coeff)) +
  scale_y_continuous(
    name = "area_plantada_permanente",
    sec.axis = sec_axis(~.*coeff, name="area_plantada_temporaria")
  ) +
  theme(axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red"))
```

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
tabe_lavouras %>% 
  ggplot(aes(x = ano)) +
  geom_line(aes(y = n_produtos_permanente), color="red") +
  geom_line(aes(y = n_produtos_temporaria/coeff)) +
  scale_y_continuous(
    name = "n_produtos_permanente",
    sec.axis = sec_axis(~.*coeff, name="n_produtos_temporaria")
  ) +
  theme(axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red"))
```

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

#### Tabela de Dados

``` r
tabe_lavouras
#> # A tibble: 10 x 11
#>      ano n_produtos_permanente n_produtos_temporaria area_plantada_permanente
#>    <int>                 <dbl>                 <dbl>                    <dbl>
#>  1  2010                  4.94                  5.07                    1784.
#>  2  2011                  4.81                  5.28                    1708.
#>  3  2012                  4.56                  5.01                    1623.
#>  4  2013                  4.55                  5.01                    1499.
#>  5  2014                  4.62                  5                       1485.
#>  6  2015                  4.62                  5.19                    1426.
#>  7  2016                  4.61                  5.42                    1413.
#>  8  2017                  4.75                  5.48                    1438.
#>  9  2018                  4.81                  5.48                    1406.
#> 10  2019                  4.80                  5.58                    1412.
#> # i 7 more variables: area_plantada_temporaria <dbl>,
#> #   rendimento_medio_permanente <dbl>, rendimento_medio_temporaria <dbl>,
#> #   prop_area_colhida_permanente <dbl>, prop_area_colhida_temporaria <dbl>,
#> #   prop_valor_producao_permanente <dbl>, prop_valor_producao_temporaria <dbl>
```

#### Criando a base resumo

``` r
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

#### Agrupamento e Correlação

``` r
nomes <- lavouras_resu$nome
da <- lavouras_resu
da_pad <- decostand(da[-1] , 
                      method = "standardize",
                      na.rm=TRUE)
df <- da_pad %>% 
  drop_na()

visdat::vis_miss(da_pad)
```

![](README_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
da_pad_euc<-vegdist(df,"euclidean",na.rm=TRUE) 
da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
plot(da_pad_euc_ward, 
     ylab="Distância Euclidiana",
     xlab="Acessos", hang=-1,
     col="blue", las=1,
     cex=.6,lwd=1.5);box()
```

![](README_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
grupo<-cutree(da_pad_euc_ward,3)
```

``` r
cor_matrix <- cor(df, use = "na.or.complete")
corrplot(cor_matrix, method="ellipse")
```

![](README_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

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
# glimpse(pnae_alunos)
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
# data_set


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

# estado_nutri_resumo %>% names
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
 "cri510axi_mb",     
 "cri510axi_b" ,          
 "cri510axi_a",        
 "cri04imcxi_magacen",   
 "cri04imcxi_mag" ,            
 "cri04imcxi_a" ,           
 "cri04imcxi_rsobr" ,    
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
#consumo$tipo_relatorio %>%  unique()

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
#glimpse(data_set)

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
da <- data_set_muni %>% ungroup()
glimpse(da)
#> Rows: 645
#> Columns: 88
#> $ nome                <chr> "Adamantina", "Adolfo", "Aguaí", "Agudos", "Alamba~
#> $ pbf_familias        <dbl> 331.5667, 140.5833, 1400.9833, 996.7167, 204.4333,~
#> $ pbf_pessoas         <dbl> 363.9667, 191.3000, 1795.1833, 1254.7333, 275.1667~
#> $ pbf_valor           <dbl> 511090.8, 317909.2, 2427811.8, 1812923.6, 321099.4~
#> $ npper               <dbl> 9.0, 6.1, 7.5, 2.2, 2.5, 6.7, 2.4, 4.9, 4.7, 3.3, ~
#> $ nptem               <dbl> 7.6, 7.4, 8.9, 5.8, 7.2, 4.7, 3.5, 8.2, 6.0, 4.3, ~
#> $ areapper            <dbl> 887.9, 2041.2, 9794.6, 1011.1, 443.1, 29.5, 4643.0~
#> $ areapltem           <dbl> 14041.6, 7094.8, 22069.4, 15418.7, 3606.3, 425.2, ~
#> $ rendper             <dbl> 14306.36, 20505.62, 23692.44, 17879.43, 24716.70, ~
#> $ rendtem             <dbl> 27582.157, 26993.338, 25704.024, 22371.008, 24843.~
#> $ pacolhidaper        <dbl> 11.618140, 17.249917, 13.392304, 56.667000, 44.999~
#> $ pacolhidatem        <dbl> 13.21388, 14.02318, 11.51983, 17.67809, 14.10731, ~
#> $ pvalorpper          <dbl> 11.618295, 17.249375, 13.393107, 56.667333, 44.999~
#> $ pvalortem           <dbl> 13.21407, 14.02388, 11.52053, 17.67832, 14.10718, ~
#> $ pnae_qa             <dbl> 2656.5385, 636.0000, 4265.6154, 4010.7692, 649.500~
#> $ pnae_vte            <dbl> 322282.32, 40735.03, 462165.75, 522470.72, 77041.7~
#> $ snutrip             <dbl> 46.0588235, 0.0000000, 10.2352941, 12.0000000, 0.6~
#> $ snutrit             <dbl> 0.0000000, 0.0000000, 0.4117647, 0.0000000, 0.0000~
#> $ slacp               <dbl> 0.000000, 0.000000, 5.470588, 12.000000, 0.000000,~
#> $ slact               <dbl> 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, ~
#> $ sbleitep            <dbl> 15.29412, 0.00000, 0.00000, 0.00000, 0.00000, 0.00~
#> $ sbleitet            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
#> $ cri04pxi_mb         <dbl> 0.9929431, 0.3525272, 1.0440122, 1.3503859, 1.1442~
#> $ cri04pxi_b          <dbl> 2.2597650, 1.1960755, 2.9205930, 3.2246802, 1.0136~
#> $ cri04pxi_a          <dbl> 88.77147, 77.97640, 87.10713, 84.93690, 89.48770, ~
#> $ cri04pxi_e          <dbl> 7.975827, 20.474993, 8.928269, 10.488029, 8.354377~
#> $ cri510pxi_mb        <dbl> 0.9307421, 0.9864829, 0.8833910, 0.5671433, 2.0542~
#> $ cri510pxi_b         <dbl> 1.9760752, 1.1837923, 2.8806551, 1.7478702, 1.7394~
#> $ cri510pxi_a         <dbl> 81.06272, 71.41206, 83.25089, 86.56935, 84.17899, ~
#> $ cri510pxi_e         <dbl> 16.030466, 26.417660, 12.985059, 11.115636, 12.027~
#> $ cri04pxa_magacen    <dbl> 1.5407599, 2.4293712, 1.6608679, 0.8990326, 0.9929~
#> $ cri04pxa_mag        <dbl> 2.6520022, 1.8948926, 2.6469319, 2.3548576, 0.8417~
#> $ cri04pxa_a          <dbl> 69.40093, 49.74267, 64.28097, 63.88259, 63.35015, ~
#> $ cri04pxa_rsobre     <dbl> 16.41736, 18.93043, 17.30181, 21.72510, 21.44772, ~
#> $ cri04pxa_sobr       <dbl> 5.355246, 9.357953, 6.686459, 6.801356, 7.237331, ~
#> $ cri04pxa_obes       <dbl> 4.633707, 17.644681, 7.422963, 4.337061, 6.130162,~
#> $ cri510pxa_magacen   <dbl> 1.08550068, NaN, 1.27461511, 0.08547009, NaN, NaN,~
#> $ cri510pxa_mag       <dbl> 2.2838779, NaN, 3.0122547, 1.5514428, NaN, NaN, Na~
#> $ cri510pxa_a         <dbl> 65.40370, NaN, 74.02357, 73.82065, NaN, NaN, NaN, ~
#> $ cri510pxa_rsobr     <dbl> 7.397055, NaN, 7.289336, 10.694277, NaN, NaN, NaN,~
#> $ cri510pxa_sobr      <dbl> 4.6388073, NaN, 3.0169173, 0.2176226, NaN, NaN, Na~
#> $ cri510pxa_obes      <dbl> 19.191059, NaN, 11.383305, 13.630539, NaN, NaN, Na~
#> $ cri04axi_mb         <dbl> 10.583486, 3.407255, 3.522299, 3.545870, 2.585734,~
#> $ cri04axi_b          <dbl> 19.705609, 4.216942, 5.968379, 10.744564, 2.467377~
#> $ cri04axi_a          <dbl> 69.71090, 92.37580, 90.50932, 85.70957, 94.94689, ~
#> $ cri510axi_mb        <dbl> 3.1166001, 6.4190068, 1.5124167, 3.6218814, 4.2103~
#> $ cri510axi_b         <dbl> 6.277196, 5.573394, 3.898615, 6.028638, 3.473670, ~
#> $ cri510axi_a         <dbl> 90.60620, 88.00760, 94.58897, 90.34948, 92.31598, ~
#> $ cri04imcxi_magacen  <dbl> 1.5918370, 3.6262531, 2.1931896, 1.2308091, 1.5749~
#> $ cri04imcxi_mag      <dbl> 2.331711, 2.295646, 3.065472, 2.196628, 1.066192, ~
#> $ cri04imcxi_a        <dbl> 67.62946, 48.09393, 62.04480, 61.62911, 62.93601, ~
#> $ cri04imcxi_rsobr    <dbl> 17.76356, 20.93877, 17.69446, 23.08856, 21.43411, ~
#> $ cri04imcxi_sobr     <dbl> 6.333984, 10.624717, 8.065498, 7.677431, 7.373252,~
#> $ cri04imcxi_obes     <dbl> 4.349453, 14.420682, 6.936580, 4.177465, 5.615444,~
#> $ cri510imcxi_magacen <dbl> 1.0145568, 1.6008621, 1.6761426, 0.8045070, 0.8308~
#> $ cri510imcxi_mag     <dbl> 2.161605, 1.886477, 2.383852, 1.657095, 1.189307, ~
#> $ cri510imcxi_a       <dbl> 63.71386, 49.61408, 65.40158, 70.93171, 63.94279, ~
#> $ cri510imcxi_rsobr   <dbl> 15.95030, 15.71876, 14.39828, 15.10013, 16.09458, ~
#> $ cri510imcxi_sobr    <dbl> 9.954089, 15.516265, 9.017791, 6.619272, 10.467741~
#> $ cri510imcxi_obes    <dbl> 7.205588, 15.663556, 7.122350, 4.887292, 7.474774,~
#> $ adolaxi_mb          <dbl> 4.3920508, 4.4795638, 2.0691786, 2.0074307, 1.2192~
#> $ adolaxi_b           <dbl> 17.825715, 2.339804, 4.495142, 8.168997, 4.391400,~
#> $ adolaxi_a           <dbl> 77.78223, 93.18063, 93.43568, 89.82357, 94.38935, ~
#> $ adolimcxi_magacen   <dbl> 0.7643364, 1.8148723, 1.3302470, 0.6596336, 0.2068~
#> $ adolimcxi_mag       <dbl> 2.743184, 1.067567, 2.394559, 2.608314, 1.217974, ~
#> $ adolimcxi_a         <dbl> 60.84144, 62.17501, 64.95670, 66.28651, 64.83455, ~
#> $ adolimcxi_sobr      <dbl> 20.22308, 20.38711, 18.30567, 18.61243, 22.62429, ~
#> $ adolimcxi_obes      <dbl> 12.032952, 10.041761, 9.864493, 9.418858, 9.379893~
#> $ adolimcxi_obesgra   <dbl> 3.3950006, 4.5136854, 3.1483306, 2.4142547, 1.7364~
#> $ adulimc_b           <dbl> 3.647834, 2.131770, 3.026713, 3.552147, 2.952508, ~
#> $ adulimc_a           <dbl> 28.42171, 36.31893, 31.18899, 29.74796, 30.39134, ~
#> $ adulimc_sob         <dbl> 18.88469, 31.89635, 27.55629, 28.68666, 29.46555, ~
#> $ adulimc_obesg_i     <dbl> 21.64474, 16.98449, 22.18332, 18.55913, 21.85711, ~
#> $ adulimc_obesg_ii    <dbl> 16.914280, 7.817509, 9.668569, 12.509599, 8.452317~
#> $ adulimc_obesg_iii   <dbl> 10.486739, 4.850957, 6.376126, 6.944498, 6.881176,~
#> $ idimc_b             <dbl> 17.03068, NaN, NaN, 10.36169, NaN, NaN, NaN, NaN, ~
#> $ idimc_a             <dbl> 51.50818, NaN, NaN, 34.79482, NaN, NaN, NaN, NaN, ~
#> $ idimc_sob           <dbl> 31.46114, NaN, NaN, 54.84349, NaN, NaN, NaN, NaN, ~
#> $ adol_embut          <dbl> 0.3446095, NA, 0.5274064, NaN, NA, 0.3157996, NA, ~
#> $ adol_ultra          <dbl> 0.9189851, NA, 0.9886364, NaN, NA, 0.7056207, NA, ~
#> $ adul_embut          <dbl> 0.26186351, NA, 0.42161218, 0.16666667, NA, 0.1912~
#> $ adul_ultra          <dbl> 0.8228250, NA, 0.6592422, 0.1666667, NA, 0.5370524~
#> $ cri24_embut         <dbl> 0.1607955, NA, 0.7071429, NaN, NA, 0.2452945, NA, ~
#> $ cri24_ultra         <dbl> 0.6931818, NA, 0.9911765, NaN, NA, 0.7630409, NA, ~
#> $ cri59_embut         <dbl> 0.03333333, NA, 0.80354090, 0.00000000, NA, 0.2953~
#> $ cri59_ultra         <dbl> 0.7500000, NA, 0.9978632, 0.0000000, NA, 0.7606119~
#> $ ido_embut           <dbl> 0.6888889, NA, 0.5333333, 0.3888889, NA, 0.3326680~
#> $ ido_ultra           <dbl> 0.6888889, NA, 0.5333333, 0.3888889, NA, 0.3530762~
```
