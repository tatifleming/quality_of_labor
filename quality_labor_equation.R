# Carregar pacote para manipulação de dados
library(dplyr)
library(readxl)


caminho_arquivo <- "C:/Users/vfmta/OneDrive/Área de Trabalho/df_lamb.xlsx"

# Importar o arquivo Excel
df <- read_excel(caminho_arquivo)

# Estimando os parâmetros gama2 e gama3 da função minceriana 
head(df)

df$Renda_Log <- as.numeric(df$Renda_Log)

modelo <- lm(Renda_Log ~ Estudo + V1 + V2, data=df)
summary(modelo)


######
# Equação da qualidade do trabalho (Bils e Klenow, 2001; Messa (2014))
# agora com os valores de gama2 e gama3 e com os valores da PNAD e das bases do Inep, consigo calcular o valor da qualidade do trabalho para cada trabalhador
df$q_it <- with(df,
                Quali_Ensino^(Estudo - 25) * exp(
                  (theta^(Estudo - 1 - 0)) / (1 - 0) +
                    0.016963353 * (Idade - Estudo - 6) + -0.000282853 * (Idade - Estudo - 6)^2
                )
)
