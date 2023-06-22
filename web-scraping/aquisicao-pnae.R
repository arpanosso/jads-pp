library(tidyverse)

# Repasses Financeiros do PNAE
'https://www.fnde.gov.br/olinda-ide/servico/PNAE_Recursos_Repassados_Pck_3/versao/v1/odata/RecursosRepassados?$filter=Ano%20eq%20%272022%27&$format=text/csv'
'https://www.fnde.gov.br/olinda-ide/servico/PNAE_Recursos_Repassados_Pck_3/versao/v1/odata/RecursosRepassados?$filter=Estado%20eq%20%27SP%27&$format=text/csv'
"https://www.fnde.gov.br/olinda-ide/servico/PNAE_Recursos_Repassados_Pck_3/versao/v1/odata/RecursosRepassados"
data <- readr::read_csv("C:\\Users\\Usuario\\Downloads\\Recursos Repassados.csv")
dplyr::glimpse(data)

readr::write_rds(data, "data\\pnae.rds")

# Escolas Atendidas pelo PNAE
'https://www.fnde.gov.br/olinda-ide/servico/PDA_Escolas_Atendidas/versao/v1/odata/EscolasAtendidas?$filter=Ano%20eq%20%272022%27&$format=text/csv'
'https://www.fnde.gov.br/olinda-ide/servico/PDA_Escolas_Atendidas/versao/v1/odata/EscolasAtendidas?$filter=UF%20eq%20%27SP%27&$format=text/csv'

# Conselho de Alimentação Escolar
'https://www.fnde.gov.br/olinda-ide/servico/PNAE_Conselho_Alimentacao_Escolar_Pck_5/versao/v1/odata/ConselhoAlimentacaoEscolar?$filter=Sg_uf%20eq%20%27SP%27&$format=text/csv'

# Alunos atendidos pelo PNAE - 2022
'https://www.fnde.gov.br/olinda-ide/servico/PNAE_Numero_Alunos_Atendidos/versao/v1/odata/Alunos_Atendidos?$filter=Estado%20eq%20%27SP%27&$format=text/csv'

# Cadastro de Nutricionistas - PNAE
