################################################

# Limpando arquivos armazenados na memória
rm(list=ls(all=TRUE))

# Definindo limite de memória para compilação do programa (útil para versão antiga do R)
aviso <- getOption("warn")
options(warn=-1)
memory.limit(size=50000)
options(warn=aviso)
rm(aviso)

# Definindo tempo de espera para obtenção de resposta do servidor (útil para acesso de redes externas ou mais lentas)
aviso <- getOption("warn")
options(warn=-1)
options(timeout=600)
options(warn=aviso)
rm(aviso)

# Definindo opção de codificação dos caracteres e linguagem (rotulação de variáveis categóricas)
aviso <- getOption("warn")
options(warn=-1)
options(encoding='latin1')
options(warn=aviso)
rm(aviso)

# Definindo opção de exibição de números sem representação exponencial
aviso <- getOption("warn")
options(warn=-1)
options(scipen=999)
options(warn=aviso)
rm(aviso)


# Definindo opção de repositório para instalação dos pacotes necessários
aviso <- getOption("warn")
options(warn=-1)
options(repos=structure(c(CRAN="https://cran.r-project.org/")))
options(warn=aviso)

# Definindo diretório de trabalho
caminho <- getwd()
setwd(dir=caminho)

# Carregando pacotes necessários para obtenção da estatística desejada
if("PNADcIBGE" %in% rownames(installed.packages())==FALSE)
{
  install.packages(pkgs="PNADcIBGE", dependencies = TRUE)
}
library(package="PNADcIBGE",verbose = TRUE)
if("survey" %in% rownames(installed.packages())==FALSE)
{
  install.packages(pkgs="survey", dependencies = TRUE)
}
library(package='survey', verbose=TRUE)


##############################################################################
##########################################################################
library(PNADcIBGE)
library(dplyr)
library(survey)
library(tidyr)


### VD4010: Setores
### V4013: CNAE
### UF
### VD4002: Ocupadas com mais de 14 anos
# VD4001: Pessoas na força de trabalho
# V2009: Idade do morador na data de referência
# V20082: Ano de nascimento (eliminar 9999)

# Estoque de capital humano da economia = Somatório qs*Lst
# qs: qualidade do trabalho dos trabalhadores nascidos no ano s
# Lst: quantidade de trabalhadores nascidos no ano s que estejam em atividade produtiva em t

### Educação
# VD3005: Educação (anos de estudo)
# V4040: Experiência (Anos de Trabalho no emprego atual)
# Variável que mede qualidade da educação (INEP) - peso, parâmetro

# VD4011: Cargo do trabalho
# V3009 (2012-2015) & V3009A (2015 - atual) : Qual foi o curso mais elevado que ... frequentou?
# Frequência em curso técnico: V3019 (entre 2016 e 2018) e V3019A (a partir de 2019)
# Pessoas que já frequentaram curso técnico: V3021 (entre 2016 e 2018) e V3021A (a partir de 2019)


### Renda (tem que deflacionar)
# V403411 (Renda Efetiva)
# V4033: rendimento bruto mensal que recebia/fazia normalmente nesse trabalho (rendimento habitual)
# V403311: faixa de rendimento bruto mensal que recebia/fazia normalmente nesse trabalho (rendimento habitual)
# VD4019: ‘Rendimento habitualmente recebido em todos os trabalhos para as pessoas de 14 anos ou mais de idade
# VDI5006: 2016-2024 (Faixa de Rendimento domiciliar per capita (habitual de todos os trabalhos e efetivo de outras fontes) (exclusive o rendimento das pessoas cuja condição na unidade domiciliar era pensionista, empregado doméstico ou parente do empregado doméstico) (Variável com imputação para moradores de domicílios que estão nas entrevistas 2, 3 ou 4))
# V403412

#### Informalidade
# VD4009:TIPO DE CONTRATO, posição na ocupação e categoria do emprego pessoas acima e 14 anos (quali)
# V4019: Esse negócio/empresa tem CNPJ (quali)

### Precarização
# VD4031: Horas habitualmente trabalhadas por semana em todos os trabalhos para pessoas de 14 anos ou mais de idade  ## não usamos horas efetivas devido ao período de tempo (começa em 2015)
# VD4036: Faixa das horas habitualmente trabalhadas por semana no trabalho principal para pessoas de 14 anos ou mais de idade
# V4029: Carteira Assinada
# V4025: Contrato temporário
# V4063: Gostaria de trabalhar mais horas?
# VD4003: Força de trabalho potencial para pessoas de 14 anos ou mais de idade
# VD4004A: Subocupação por insuficiência de horas habitualmente trabalhadas em todos os trabalhos

# PNAD Contínua Anual concentrada na primeira entrevista
dadosPNADc_anual <- get_pnadc(year=2022, interview=5, vars=c("VD4010", "V4013", "UF", "VD4002", "VD4001", "V2009", "V20082", "VD4019", "VD4020", "V403311", "V4033", "VD3005", "V4040", "VD4009", "V4019", "VD4036", "V4029", "V4025", "V4063", "VD4003"), design=TRUE, deflator=TRUE, defyear=2023)

colnames(dadosPNADc_anual)
dim(dadosPNADc_anual)

# Deflacionando Renda Efetiva (VD4020)
# CO2e: Deflator, a preços médio do último ano, utilizado para variáveis de rendimento efetivo
dadosPNADc_anual$variables <- transform(dadosPNADc_anual$variables, VD4020real=VD4020*CO2e)

dim(dadosPNADc_anual)
str(dadosPNADc_anual)
names(dadosPNADc_anual)
class(dadosPNADc_anual)

# O objeto dadosPNADc_anual não é um data frame comum, mas sim um objeto de design de amostra do pacote survey, usado para análise de amostras complexas
# Extrair os dados brutos do objeto de design antes de salvar
dados_brutos_a <- dadosPNADc_anual$variables

# Trabalhadores ocupados
# Filtrar VD4002 "Pessoas Ocupadas"
table(dados_brutos_a$VD4002, useNA="ifany")
unique(dados_brutos$VD4002)

dados_ocupados <- subset(dados_brutos_a, VD4002 == "Pessoas ocupadas")

dim(dados_ocupados)

# Filtrar CNAEs da Indústria (V4013)
table(dados_ocupados$V4013, useNA="ifany")

df_cnae_ind <- subset(dados_ocupados, substr(V4013, 1, 2) >= "10" & substr(V4013, 1, 2) <= "31")

dim(df_cnae_ind)

# Verificando se todas as pessoas estão na força de trabalho (ok)
table(df_cnae_ind$VD4001, useNA="ifany")


### Informalidade
df_informalidade <- df_cnae_ind %>%
  mutate(
    informal = as.factor(
      ifelse(
        is.na(VD4009), 
        NA, 
        ifelse(
          VD4009 %in% c(
            "Empregado no setor privado sem carteira de trabalho assinada",
            "Trabalhador doméstico sem carteira de trabalho assinada"
          ) |
            (VD4009 == "Empregador" & V4019 == "Não") |
            (VD4009 == "Conta-própria" & V4019 == "Não") |
            VD4009 == "Trabalhador familiar auxiliar",
          "Pessoas na informalidade",
          "Pessoas na formalidade"
        )
      )
    )
  )


table(df_informalidade$V4025, useNA="ifany")
names(df_informalidade)

novo_df_2022_B <- df_informalidade[, c("VD4010", "V4013", "UF", "V2009", "VD3005", "VD4020real", "informal", "V4029", "V4025")]


library(writexl)
write_xlsx(novo_df_2022_B, "C:/Users/vfmta/OneDrive/Área de Trabalho/novo_df_2022_B.xlsx")

#######################################################################################
######################################################################################################################
######################################################################################################################

