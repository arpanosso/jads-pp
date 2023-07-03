
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
1.1.1 Pessoas Beneficiárias  
1.1.2 Famílias Beneficiárias  
1.1.3 Valor Pago

``` r
im <- read_rds("data/im.rds")
glimpse(im)
#> Rows: 129,645
#> Columns: 9
#> $ id_municipio               <chr> "3500105", "3500105", "3500105", "3500105",~
#> $ ano                        <int> 2004, 2004, 2004, 2004, 2004, 2004, 2004, 2~
#> $ mes                        <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2~
#> $ familias_beneficiarias_pbf <int> 381, 381, 381, 381, 381, 381, 381, 380, 414~
#> $ pessoas_beneficiarias_pbf  <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
#> $ valor_pago_pbf             <dbl> 23430, 23430, 23430, 23220, 23160, 23100, 2~
#> $ familias_cadastradas_cu    <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
#> $ pessoas_cadastradas_cu     <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
#> $ nome                       <chr> "Adamantina", "Adamantina", "Adamantina", "~
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

------------------------------------------------------------------------

**DESAFIO 3**. Promover a produção de alimentos saudáveis e
sustentáveis, a estruturação da agricultura familiar e o fortalecimento
de sistemas de produção de base agroecológica

Temas: Fortalecimento da agricultura familiar; Reforma agrária;
Transição agroecológica; Mulheres; Juventude; Sementes; Mudanças
climáticas.

3.1 Produção de alimentos  
3.2 Área Plantada  
3.2.1 Área Plantada Agricultura Familiar  
3.2.2 Área Plantada Agricultura não familiar  
3.3. Rendimento Médio  
3.4 Proporção Área Colhida  
3.5 Proporção Valor da Produção

``` r
lavoura_permanente <- read_rds("data/permanente.rds")
glimpse(lavoura_permanente)
#> Rows: 1,115,224
#> Columns: 13
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
```

``` r
lavoura_temporaria <- read_rds("data/temporaria.rds")
glimpse(lavoura_temporaria)
#> Rows: 974,556
#> Columns: 13
#> $ ano                  <int> 1974, 1974, 1974, 1974, 1974, 1974, 1974, 1974, 1~
#> $ sigla_uf             <chr> "SP", "SP", "SP", "SP", "SP", "SP", "SP", "SP", "~
#> $ id_municipio         <chr> "3500105", "3500105", "3500105", "3500105", "3500~
#> $ produto              <chr> "Alfafa fenada", "Cevada (em grão)", "Ervilha (em~
#> $ area_plantada        <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
#> $ area_colhida         <int> NA, NA, NA, NA, NA, NA, 2000, NA, NA, NA, NA, 180~
#> $ quantidade_produzida <int> NA, NA, NA, NA, NA, NA, 3600, NA, NA, NA, NA, 305~
#> $ rendimento_medio     <int> NA, NA, NA, NA, NA, NA, 1800, NA, NA, NA, NA, 169~
#> $ valor_producao       <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
#> $ prop_area_plantada   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
#> $ prop_area_colhida    <dbl> NA, NA, NA, NA, NA, NA, 58.82, NA, NA, NA, NA, 1.~
#> $ prop_valor_producao  <dbl> NA, NA, NA, NA, NA, NA, 48.23, NA, NA, NA, NA, 1.~
#> $ nome                 <chr> "Adamantina", "Adamantina", "Adamantina", "Adaman~
```

------------------------------------------------------------------------

**DESAFIO 4**. Promover o abastecimento e o acesso regular e permanente
da população brasileira à alimentação

Temas: Compras públicas; Abastecimento; Legislação sanitária; Economia
solidaria; Perdas e desperdícios de alimentos; Equipamentos públicos de
SAN; Agricultura urbana.

**4.1 Programa da Alimentação Escolar**  
4.1.1 Número de Alunos Beneficiários  
4.1.2 Valor gasto com Compras da Agricultura Familiar 30%  
4.1.3 Valor Formalizado  
4.1.4 Valor Executado

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
```

``` r
pnae_recurso <- read_rds("data/pnae_recurso.rds")
glimpse(pnae_recurso)
#> Rows: 2,191,590
#> Columns: 31
#> $ co_recursos_repassados    <dbl> 381758, 381759, 381760, 381761, 381762, 3817~
#> $ ano                       <dbl> 1999, 1999, 1999, 1999, 1999, 1999, 1999, 19~
#> $ estado                    <chr> "SP", "SP", "SP", "SP", "SP", "SP", "SP", "S~
#> $ municipio                 <chr> "ADAMANTINA", "ADOLFO", "AGUAI", "AGUAS DA P~
#> $ esfera_governo            <chr> "MUNICIPAL", "MUNICIPAL", "MUNICIPAL", "MUNI~
#> $ modalidade_ensino         <chr> "ENSINO FUNDAMENTAL", "ENSINO FUNDAMENTAL", ~
#> $ vl_total_escolas          <dbl> 13158985, 1817118, 12468183, 3048183, 697171~
#> $ nomin                     <chr> "ADAMANTINA", "ADOLFO", "AGUAI", "AGUAS DA P~
#> $ id_municipio              <chr> "3500105", "3500204", "3500303", "3500402", ~
#> $ id_municipio_6            <chr> "350010", "350020", "350030", "350040", "350~
#> $ id_municipio_tse          <chr> "61018", "61034", "61050", "61077", "61093",~
#> $ id_municipio_rf           <chr> "6101", "6103", "6105", "6107", "6109", "701~
#> $ id_municipio_bcb          <chr> "20248", "24677", "29517", "34117", "38553",~
#> $ nome                      <chr> "Adamantina", "Adolfo", "Aguaí", "Águas da P~
#> $ capital_uf                <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
#> $ id_comarca                <chr> "3500105", "3525706", "3500303", "3549102", ~
#> $ id_regiao_saude           <chr> "35091", "35156", "35142", "35142", "35074",~
#> $ nome_regiao_saude         <chr> "Adamantina", "José Bonifácio", "Mantiqueira~
#> $ id_regiao_imediata        <chr> "350019", "350025", "350044", "350044", "350~
#> $ nome_regiao_imediata      <chr> "Adamantina - Lucélia", "São José do Rio Pre~
#> $ id_regiao_intermediaria   <chr> "3505", "3507", "3510", "3510", "3510", "350~
#> $ nome_regiao_intermediaria <chr> "Presidente Prudente", "São José do Rio Pret~
#> $ id_microrregiao           <chr> "35035", "35004", "35029", "35030", "35033",~
#> $ nome_microrregiao         <chr> "Adamantina", "São José do Rio Preto", "Pira~
#> $ id_mesorregiao            <chr> "3508", "3501", "3507", "3507", "3507", "350~
#> $ nome_mesorregiao          <chr> "Presidente Prudente", "São José do Rio Pret~
#> $ ddd                       <chr> "18", "17", "19", "19", "19", "14", "19", "1~
#> $ id_uf                     <chr> "35", "35", "35", "35", "35", "35", "35", "3~
#> $ sigla_uf                  <chr> "SP", "SP", "SP", "SP", "SP", "SP", "SP", "S~
#> $ nome_uf                   <chr> "São Paulo", "São Paulo", "São Paulo", "São ~
#> $ nome_regiao               <chr> "Sudeste", "Sudeste", "Sudeste", "Sudeste", ~
```

``` r
pnae_escola <- read_rds("data/pnae_escolas.rds")
glimpse(pnae_escola)
#> Rows: 22,270
#> Columns: 30
#> $ cod_escolas_atendidas     <dbl> 1024713, 1024714, 1024715, 1024716, 1024717,~
#> $ ano                       <dbl> 2009, 2009, 2009, 2009, 2009, 2009, 2009, 20~
#> $ uf                        <chr> "SP", "SP", "SP", "SP", "SP", "SP", "SP", "S~
#> $ municipio                 <chr> "ADAMANTINA", "ADAMANTINA", "ADAMANTINA", "A~
#> $ esfera_governo            <chr> "ADMINISTRAÇÃO", "ADMINISTRAÇÃO", "PARTICULA~
#> $ qtd_escolas_atendidas     <dbl> 5, 14, 1, 1, 2, 1, 4, 12, 1, 2, 4, 1, 2, 11,~
#> $ nomin                     <chr> "ADAMANTINA", "ADAMANTINA", "ADAMANTINA", "A~
#> $ id_municipio              <chr> "3500105", "3500105", "3500105", "3500204", ~
#> $ id_municipio_6            <chr> "350010", "350010", "350010", "350020", "350~
#> $ id_municipio_tse          <chr> "61018", "61018", "61018", "61034", "61034",~
#> $ id_municipio_rf           <chr> "6101", "6101", "6101", "6103", "6103", "610~
#> $ id_municipio_bcb          <chr> "20248", "20248", "20248", "24677", "24677",~
#> $ nome                      <chr> "Adamantina", "Adamantina", "Adamantina", "A~
#> $ capital_uf                <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
#> $ id_comarca                <chr> "3500105", "3500105", "3500105", "3525706", ~
#> $ id_regiao_saude           <chr> "35091", "35091", "35091", "35156", "35156",~
#> $ nome_regiao_saude         <chr> "Adamantina", "Adamantina", "Adamantina", "J~
#> $ id_regiao_imediata        <chr> "350019", "350019", "350019", "350025", "350~
#> $ nome_regiao_imediata      <chr> "Adamantina - Lucélia", "Adamantina - Lucéli~
#> $ id_regiao_intermediaria   <chr> "3505", "3505", "3505", "3507", "3507", "350~
#> $ nome_regiao_intermediaria <chr> "Presidente Prudente", "Presidente Prudente"~
#> $ id_microrregiao           <chr> "35035", "35035", "35035", "35004", "35004",~
#> $ nome_microrregiao         <chr> "Adamantina", "Adamantina", "Adamantina", "S~
#> $ id_mesorregiao            <chr> "3508", "3508", "3508", "3501", "3501", "350~
#> $ nome_mesorregiao          <chr> "Presidente Prudente", "Presidente Prudente"~
#> $ ddd                       <chr> "18", "18", "18", "17", "17", "17", "19", "1~
#> $ id_uf                     <chr> "35", "35", "35", "35", "35", "35", "35", "3~
#> $ sigla_uf                  <chr> "SP", "SP", "SP", "SP", "SP", "SP", "SP", "S~
#> $ nome_uf                   <chr> "São Paulo", "São Paulo", "São Paulo", "São ~
#> $ nome_regiao               <chr> "Sudeste", "Sudeste", "Sudeste", "Sudeste", ~
```

``` r
pnae_conselho <- read_rds("data/pnae_conselho_alim_esco.rds")
glimpse(pnae_conselho)
#> Rows: 22,270
#> Columns: 30
#> $ cod_escolas_atendidas     <dbl> 1024713, 1024714, 1024715, 1024716, 1024717,~
#> $ ano                       <dbl> 2009, 2009, 2009, 2009, 2009, 2009, 2009, 20~
#> $ uf                        <chr> "SP", "SP", "SP", "SP", "SP", "SP", "SP", "S~
#> $ municipio                 <chr> "ADAMANTINA", "ADAMANTINA", "ADAMANTINA", "A~
#> $ esfera_governo            <chr> "ADMINISTRAÇÃO", "ADMINISTRAÇÃO", "PARTICULA~
#> $ qtd_escolas_atendidas     <dbl> 5, 14, 1, 1, 2, 1, 4, 12, 1, 2, 4, 1, 2, 11,~
#> $ nomin                     <chr> "ADAMANTINA", "ADAMANTINA", "ADAMANTINA", "A~
#> $ id_municipio              <chr> "3500105", "3500105", "3500105", "3500204", ~
#> $ id_municipio_6            <chr> "350010", "350010", "350010", "350020", "350~
#> $ id_municipio_tse          <chr> "61018", "61018", "61018", "61034", "61034",~
#> $ id_municipio_rf           <chr> "6101", "6101", "6101", "6103", "6103", "610~
#> $ id_municipio_bcb          <chr> "20248", "20248", "20248", "24677", "24677",~
#> $ nome                      <chr> "Adamantina", "Adamantina", "Adamantina", "A~
#> $ capital_uf                <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
#> $ id_comarca                <chr> "3500105", "3500105", "3500105", "3525706", ~
#> $ id_regiao_saude           <chr> "35091", "35091", "35091", "35156", "35156",~
#> $ nome_regiao_saude         <chr> "Adamantina", "Adamantina", "Adamantina", "J~
#> $ id_regiao_imediata        <chr> "350019", "350019", "350019", "350025", "350~
#> $ nome_regiao_imediata      <chr> "Adamantina - Lucélia", "Adamantina - Lucéli~
#> $ id_regiao_intermediaria   <chr> "3505", "3505", "3505", "3507", "3507", "350~
#> $ nome_regiao_intermediaria <chr> "Presidente Prudente", "Presidente Prudente"~
#> $ id_microrregiao           <chr> "35035", "35035", "35035", "35004", "35004",~
#> $ nome_microrregiao         <chr> "Adamantina", "Adamantina", "Adamantina", "S~
#> $ id_mesorregiao            <chr> "3508", "3508", "3508", "3501", "3501", "350~
#> $ nome_mesorregiao          <chr> "Presidente Prudente", "Presidente Prudente"~
#> $ ddd                       <chr> "18", "18", "18", "17", "17", "17", "19", "1~
#> $ id_uf                     <chr> "35", "35", "35", "35", "35", "35", "35", "3~
#> $ sigla_uf                  <chr> "SP", "SP", "SP", "SP", "SP", "SP", "SP", "S~
#> $ nome_uf                   <chr> "São Paulo", "São Paulo", "São Paulo", "São ~
#> $ nome_regiao               <chr> "Sudeste", "Sudeste", "Sudeste", "Sudeste", ~
```

**4.2 Banco de Leite **  
4.2.1 Próprio  
4.2.2 Terceirizado  
**4.3 Serviço Lactário**  
4.3.1 Próprio  
4.3.2 Terceirizado  
**4.4 Serviço Nutrição**  
4.4.1 Próprio  
4.4.2 Terceirizado

``` r
sisvan_estab <- read_rds("data/sisvan_estab.rds")
glimpse(sisvan_estab)
#> Rows: 12,622,130
#> Columns: 13
#> $ files_way                                  <chr> "1", "1", "1", "1", "1", "1~
#> $ ano                                        <dbl> 2006, 2006, 2006, 2006, 200~
#> $ mes                                        <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, ~
#> $ sigla_uf                                   <chr> "SP", "SP", "SP", "SP", "SP~
#> $ id_municipio                               <chr> "3501608", "3501608", "3504~
#> $ id_municipio_6                             <chr> "350160", "350160", "350410~
#> $ indicador_servico_nutricao_proprio         <int> 0, 1, 0, 0, 0, 1, 1, 0, 0, ~
#> $ indicador_servico_nutricao_terceirizado    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
#> $ indicador_servico_lactario_proprio         <int> 0, 0, 0, 0, 0, 1, 1, 0, 0, ~
#> $ indicador_servico_lactario_terceirizado    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
#> $ indicador_servico_banco_leite_proprio      <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
#> $ indicador_servico_banco_leite_terceirizado <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
#> $ municipio                                  <chr> "Americana", "Americana", "~
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
7. Consumo  
7.1 Consumo de ultra processados  
7.1.2 Consumo embutidos  
7.1.3 Consumo processados

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
