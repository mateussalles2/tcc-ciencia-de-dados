install.packages("readxl")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("DAAG")
install.packages("caret")
install.packages("glmnet")
install.packages("gt")

library(ggplot2)
library(RColorBrewer)
library(corrplot)
library(readxl)
library(dplyr)
library(DAAG)
library(caret)
library(glmnet)
library (gt)

# LIMPANDO A BASE DE CARACTERÍSTICAS DO PRODUTO

data_caracteristicas <- read.csv("C:/Users/uni31713/Documents/TCC Mateus/TCC_Mateus/caracteristicas_produtos_saude_suplementar.csv", header = TRUE, sep = ";")

data_caracteristicas <- subset(data_caracteristicas, TIPO_FINANCIAMENTO == "Preestabelecido" )
data_caracteristicas <- subset(data_caracteristicas, COBERTURA == "Médico-hospitalar" )
data_caracteristicas <- subset(data_caracteristicas, SITUACAO_PLANO == "Ativo" )
data_caracteristicas <- subset(data_caracteristicas, SGMT_ASSISTENCIAL != "Referência" )
dim(data_caracteristicas)

variaveis_para_excluir <- c("NM_PLANO", "RAZAO_SOCIAL", "VIGENCIA_PLANO",
                            "CONTRATACAO", "SGMT_ASSISTENCIAL", "COBERTURA",
                            "TIPO_FINANCIAMENTO", "SITUACAO_PLANO", "DT_SITUACAO",
                            "DT_REGISTRO_PLANO", "DT_ATUALIZACAO")
data_caracteristicas <- data_caracteristicas[, setdiff(names(data_caracteristicas), variaveis_para_excluir)]
dim(data_caracteristicas)

# LIMPANDO A BASE DE NTRP VIGENTE

data_NTRP_vigente <- read.csv("C:/Users/uni31713/Documents/TCC Mateus/TCC_Mateus/nota_tecnica_ntrp_planos_saude.csv", header = TRUE, sep = ";")

variaveis_para_excluir <- c("ID_NOTA",	"CD_OPERADORA",	"CD_PLANO",
                            "DT_NTRP",	"NM_ARQV",	"DT_ATUALIZACAO")
data_NTRP_vigente <- data_NTRP_vigente[, setdiff(names(data_NTRP_vigente), variaveis_para_excluir)]
dim(data_NTRP_vigente)

# LIMPANDO A BASE DE MENSALIDADE E CUSTO

data_mensalidade_custo_2020 <- read.csv("C:/Users/uni31713/Documents/TCC Mateus/TCC_Mateus/NOTA_TECNICA_VCM_FAIXA_ETARIA_2020.csv", header = TRUE, sep = ";")
data_mensalidade_custo_2021 <- read.csv("C:/Users/uni31713/Documents/TCC Mateus/TCC_Mateus/NOTA_TECNICA_VCM_FAIXA_ETARIA_2021.csv", header = TRUE, sep = ";")
data_mensalidade_custo_2022 <- read.csv("C:/Users/uni31713/Documents/TCC Mateus/TCC_Mateus/NOTA_TECNICA_VCM_FAIXA_ETARIA_2022.csv", header = TRUE, sep = ";")
data_mensalidade_custo_2023 <- read.csv("C:/Users/uni31713/Documents/TCC Mateus/TCC_Mateus/NOTA_TECNICA_VCM_FAIXA_ETARIA_2023.csv", header = TRUE, sep = ";")
data_mensalidade_custo <- rbind(data_mensalidade_custo_2020, data_mensalidade_custo_2021, data_mensalidade_custo_2022, data_mensalidade_custo_2023)
rm (data_mensalidade_custo_2020, data_mensalidade_custo_2021, data_mensalidade_custo_2022, data_mensalidade_custo_2023)

variaveis_para_excluir <- c("CD_OPERADORA",	"DT_NTRP","VCM_MINIMO",
                            "VCM_MAXIMO",	"DT_ATUALIZACAO")
data_mensalidade_custo <- data_mensalidade_custo[, setdiff(names(data_mensalidade_custo), variaveis_para_excluir)]
dim(data_mensalidade_custo)

# LIMPANDO A BASE UF OPERADORA

data_UF_OPS <- read.csv("C:/Users/uni31713/Documents/TCC Mateus/TCC_Mateus/Relatorio_cadop.csv", header = TRUE, sep = ";")

variaveis_para_excluir <- c("CNPJ",	"Razao_Social",	"Nome_Fantasia", "Modalidade",	
                            "Logradouro",	"Numero", "Complemento"	, "Bairro",
                            "Cidade", "CEP", "DDD", "Telefone", "Fax",
                            "Endereco_eletronico",	"Representante", "Cargo_Representante",
                            "Regiao_de_Comercializacao",	"Data_Registro_ANS")

data_UF_OPS <- data_UF_OPS[, setdiff(names(data_UF_OPS), variaveis_para_excluir)]
data_UF_OPS <- data_UF_OPS %>%
  rename(CD_OPERADORA = Registro_ANS)
dim(data_UF_OPS)


# JUNTANDO AS BASES NTRP_VIGENTE E MENSALIDADE_CUSTO

data_merge <- inner_join(data_NTRP_vigente, data_mensalidade_custo, by = c("ID_PLANO","CD_NOTA"))
dim(data_merge)

# AJUSTANDO REGIONALIZADA PARA A MÉDIA

data_merge <- data_merge %>%
  group_by(ID_ABRG.x, ID_PLANO, FAIXA_ETARIA) %>%
  mutate(
    VL_COMERCIAL_MENSALIDADE_AJUSTADA = ifelse(ID_ABRG.x == "ÚNICA", VL_COMERCIAL_MENSALIDADE, mean(VL_COMERCIAL_MENSALIDADE)),
    VL_DESP_ASSISTENCIAL_AJUSTADA = ifelse(ID_ABRG.x == "ÚNICA", VL_DESP_ASSISTENCIAL, mean(VL_DESP_ASSISTENCIAL))
  ) %>%
  mutate(
    VL_COMERCIAL_MENSALIDADE_AJUSTADA = round(VL_COMERCIAL_MENSALIDADE_AJUSTADA, 2),
    VL_DESP_ASSISTENCIAL_AJUSTADA = round(VL_DESP_ASSISTENCIAL_AJUSTADA, 2)
  )

variaveis_para_excluir <- c("ID_ABRG.x" ,	"ID_ABRG.y",	"CD_NOTA",	
                            "VL_COMERCIAL_MENSALIDADE" ,	"VL_DESP_ASSISTENCIAL")

data_merge <- data_merge[, setdiff(names(data_merge), variaveis_para_excluir)]

data_merge <- distinct(data_merge)
dim(data_merge)

# INCLUINDO AS VARIÁVEIS CATEGÓRICAS: CARACTERÍSTICAS DOS PRODUTOS

data_merge <- inner_join(data_caracteristicas, data_merge, by = c("ID_PLANO"))
dim(data_merge)

# INCLUINDO A UF DA OPERADORA

data_merge = inner_join(data_merge, data_UF_OPS, by = c("CD_OPERADORA"))
dim(data_merge)

names(data_merge)

# ANÁLISE EXPLORATÓRIA

ggplot(data_merge, aes(x = GR_CONTRATACAO)) + geom_bar(stat = "count", fill = "salmon2" ) +
  labs(title = "Frequência Tipo de Contratação",
       x = "Categoria",
       y = "Contagem") + theme_classic()

ggplot(data_merge, aes(x = GR_SGMT_ASSISTENCIAL)) + geom_bar(stat = "count", fill = "salmon2" ) +
  labs(title = "Frequência Tipo de Cobertura",
       x = "Categoria",
       y = "Contagem") + theme_classic()

ggplot(data_merge, aes(x = LG_ODONTOLOGICO)) + geom_bar(stat = "count", fill = "salmon2" ) +
  labs(title = "Frequência Cobertura Odontológica",
       x = "Categoria",
       y = "Contagem") + theme_classic()

ggplot(data_merge, aes(x = OBSTETRICIA)) + geom_bar(stat = "count", fill = "salmon2" ) +
  labs(title = "Frequência Cobertura Obstétrica",
       x = "Categoria",
       y = "Contagem") + theme_classic()

ggplot(data_merge, aes(x = ACOMODACAO_HOSPITALAR)) + geom_bar(stat = "count", fill = "salmon2" ) +
  labs(title = "Frequência Tipo de Acomodação Hospitalar",
       x = "Categoria",
       y = "Contagem") + theme_classic()

ggplot(data_merge, aes(x = FATOR_MODERADOR)) + geom_bar(stat = "count", fill = "salmon2" ) +
  labs(title = "Frequência Fator Moderador",
       x = "Categoria",
       y = "Contagem") + theme_classic()

ggplot(data_merge, aes(x = LIVRE_ESCOLHA)) + geom_bar(stat = "count", fill = "salmon2" ) +
  labs(title = "Frequência Livre Escolha",
       x = "Categoria",
       y = "Contagem") + theme_classic()

ggplot(data_merge, aes(x = ABRANGENCIA_COBERTURA)) + geom_bar(stat = "count", fill = "salmon2" ) +
  labs(title = "Frequência Abrangência Geográfica",
       x = "Categoria",
       y = "Contagem") + theme_classic()

ggplot(data_merge, aes(x = FAIXA_ETARIA, y = VL_COMERCIAL_MENSALIDADE_AJUSTADA)) +
  geom_bar(stat = "summary", fun = "mean", fill = "salmon2", alpha = 0.7) +
  labs(title = "Média da Mensalidade por Faixa Etária",
       x = "Faixa Etária",
       y = "Média da Mensalidade") + theme_classic()

media_por_faixa_etaria <- tapply(data_merge$VL_COMERCIAL_MENSALIDADE_AJUSTADA, data_merge$FAIXA_ETARIA, mean)
print(media_por_faixa_etaria)

ggplot(data_merge, aes(x = FAIXA_ETARIA, y = VL_DESP_ASSISTENCIAL_AJUSTADA)) +
  geom_bar(stat = "summary", fun = "mean", fill = "salmon2", alpha = 0.7) +
  labs(title = "Média dos Custos Assistenciais por Faixa Etária",
       x = "Faixa Etária",
       y = "Média dos custos Assistenciais") + theme_classic()

media_por_faixa_etaria <- tapply(data_merge$VL_DESP_ASSISTENCIAL_AJUSTADA, data_merge$FAIXA_ETARIA, mean)
print(media_por_faixa_etaria)

ggplot(data_merge, aes(x = UF)) + geom_point(stat = "count", color = "salmon2" ) +
  labs(title = "Frequência por UF da Matriz da Operadora",
       x = "Categoria",
       y = "Contagem") + theme_classic()

unique(data_merge$GR_CONTRATACAO)
data_merge$GR_CONTRATACAO_NUM = as.numeric(factor(data_merge$GR_CONTRATACAO,
                                                  levels = c("Individual ou Familiar", 
                                                             "Coletivo empresarial", 
                                                             "Coletivo por adesão")))
unique(data_merge$GR_CONTRATACAO_NUM)

unique(data_merge$GR_SGMT_ASSISTENCIAL)
data_merge$GR_SGMT_ASSISTENCIAL_NUM = as.numeric(factor(data_merge$GR_SGMT_ASSISTENCIAL,
                                                  levels = c("Ambulatorial + Hospitalar", 
                                                             "Hospitalar", 
                                                             "Ambulatorial")))
unique(data_merge$GR_SGMT_ASSISTENCIAL_NUM)

unique(data_merge$OBSTETRICIA)
data_merge$OBSTETRICIAL_NUM = as.numeric(factor(data_merge$OBSTETRICIA,
                                                        levels = c("Com Obstetrícia", 
                                                                   "Não se Aplica", 
                                                                   "Sem Obstetrícia")))
unique(data_merge$OBSTETRICIAL_NUM)

unique(data_merge$ABRANGENCIA_COBERTURA)
data_merge$ABRANGENCIA_COBERTURAL_NUM = as.numeric(factor(data_merge$ABRANGENCIA_COBERTURA,
                                                levels = c("Grupo de municípios", 
                                                           "Estadual", 
                                                           "Municipal", "Nacional", 
                                                           "Grupo de estados")))
unique(data_merge$ABRANGENCIA_COBERTURAL_NUM)

unique(data_merge$FATOR_MODERADOR)
data_merge$FATOR_MODERADOR_NUM = as.numeric(factor(data_merge$FATOR_MODERADOR,
                                                          levels = c("Coparticipacão", 
                                                                     "Ausente", 
                                                                     "Franquia + Coparticipacão", 
                                                                     "Franquia")))
unique(data_merge$FATOR_MODERADOR_NUM)

unique(data_merge$ACOMODACAO_HOSPITALAR)
data_merge$ACOMODACAO_HOSPITALAR_NUM = as.numeric(factor(data_merge$ACOMODACAO_HOSPITALAR,
                                                   levels = c("Individual", 
                                                              "Coletiva", 
                                                              "Não se Aplica", 
                                                              "Não Informado")))
unique(data_merge$ACOMODACAO_HOSPITALAR_NUM)

unique(data_merge$LIVRE_ESCOLHA)
data_merge$LIVRE_ESCOLHA_NUM = as.numeric(factor(data_merge$LIVRE_ESCOLHA,
                                                         levels = c("Ausente", 
                                                                    "Parcial com internação", 
                                                                    "Parcial sem internação", 
                                                                    "Total")))
unique(data_merge$LIVRE_ESCOLHA_NUM)

unique(data_merge$FAIXA_ETARIA)
data_merge$FAIXA_ETARIA_NUM = as.numeric(factor(data_merge$FAIXA_ETARIA,
                                                 levels = c("00 a 18 anos", 
                                                            "19 a 23 anos", 
                                                            "24 a 28 anos", 
                                                            "29 a 33 anos",
                                                            "34 a 38 anos",
                                                            "39 a 43 anos",
                                                            "44 a 48 anos",
                                                            "49 a 53 anos",
                                                            "54 a 58 anos",
                                                            "59 anos ou mais")))
unique(data_merge$FAIXA_ETARIA_NUM)

unique(data_merge$UF)
data_merge$UF_NUM = as.numeric(factor(data_merge$UF,
                                                levels = c("PR", "RJ", "SP", "GO", "PB", "MG", "PI",
                                                           "ES", "BA", "PA", "MA", "SC", "MS", "RS", "MT",
                                                           "TO", "DF", "CE", "SE", "AM", "PE", "RO", "AL",
                                                           "RN", "AC")))
unique(data_merge$UF_NUM)

variaveis_desejadas <- c(
  "VL_COMERCIAL_MENSALIDADE_AJUSTADA", 
  "VL_DESP_ASSISTENCIAL_AJUSTADA",
  "GR_CONTRATACAO_NUM",
  "GR_SGMT_ASSISTENCIAL_NUM",
  "OBSTETRICIAL_NUM",
  "LG_ODONTOLOGICO",
  "ABRANGENCIA_COBERTURAL_NUM",
  "FATOR_MODERADOR_NUM",
  "ACOMODACAO_HOSPITALAR_NUM",
  "LIVRE_ESCOLHA_NUM",
  "FAIXA_ETARIA_NUM",
  "UF_NUM"
)

data_merge_NUM <- data_merge[variaveis_desejadas]

ggplot(data_merge, aes(x = FAIXA_ETARIA, y = VL_COMERCIAL_MENSALIDADE_AJUSTADA)) +
  geom_boxplot(fill = "skyblue", color = "blue") +
  labs(title = "Boxplot de VL_COMERCIAL_MENSALIDADE_AJUSTADA por FAIXA_ETARIA",
       x = "FAIXA_ETARIA",
       y = "VL_COMERCIAL_MENSALIDADE_AJUSTADA") +
  theme_minimal()

matriz_cor <- cor(data_merge_NUM, use = "pairwise.complete.obs")
corrplot(matriz_cor, method = "circle", col = colorRampPalette(c("blue", "white", "red"))(100), 
         number.cex = 0.7, addCoef.col = "black", alpha = 0.7, tl.cex = 0.6, tl.col = "black")


# Regressão linear múltipla

controle <- trainControl(method = "cv", number = 5)
cv_linear <- train(VL_COMERCIAL_MENSALIDADE_AJUSTADA ~ ., data = data_merge_NUM, method = "lm", trControl = controle)
print(cv_linear)
summary(cv_linear)


# Regressão Ridge

X <- as.matrix(data_merge_NUM[, -1])
y <- data_merge_NUM$VL_COMERCIAL_MENSALIDADE_AJUSTADA

cv_ridge <- cv.glmnet(x = X, y = y, alpha = 0, nfolds = 5)
melhor_lambda <- cv_ridge$lambda.min
modelo_ridge <- glmnet(x = X, y = y, alpha = 0, lambda = melhor_lambda)
print(modelo_ridge)

# Regressão Lasso

cv_lasso <- cv.glmnet(x = X, y = y, alpha = 1, nfolds = 5)
melhor_lambda <- cv_lasso$lambda.min
modelo_lasso <- glmnet(x = X, y = y, alpha = 1, lambda = melhor_lambda)
print(modelo_lasso)

#Comparação modelos

calculate_mae <- function(y_true, y_pred) {
  mean(abs(y_true - y_pred))
}

cv_linear_pred <- predict(cv_linear, newdata = X)  
mae_cv_linear <- calculate_mae(data_merge_NUM$VL_COMERCIAL_MENSALIDADE_AJUSTADA, cv_linear_pred)

X_matrix <- as.matrix(X)
modelo_ridge_pred <- predict(modelo_ridge, newx = X_matrix)  
mae_modelo_ridge <- calculate_mae(data_merge_NUM$VL_COMERCIAL_MENSALIDADE_AJUSTADA, modelo_ridge_pred)

modelo_lasso_pred <- predict(modelo_lasso, newx = X_matrix)  
mae_modelo_lasso <- calculate_mae(data_merge_NUM$VL_COMERCIAL_MENSALIDADE_AJUSTADA, modelo_lasso_pred)

mae_resultados <- c(mae_cv_linear, mae_modelo_ridge, mae_modelo_lasso)
print(mae_resultados)

r2_linear <- cor(cv_linear_pred, y)^2

r2_ridge <- cor(modelo_ridge_pred, y)^2

r2_lasso <- cor(modelo_lasso_pred, y)^2

r2_resultados <- c(r2_linear, r2_ridge, r2_lasso)
print(r2_resultados)

# Criando Tabela de resultado.

Resultado <- data.frame(
  Modelo = c("Tradicional", "Ridge", "Lasso"),
  R2 = c(r2_linear, r2_ridge, r2_lasso),
  MAE = c(mae_cv_linear, mae_modelo_ridge, mae_modelo_lasso)
)

Resultado %>%
  gt() %>%
  tab_header(
    title = "Comparação das Modelagens"
  )
