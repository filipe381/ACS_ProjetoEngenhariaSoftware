# ============================================================
#  Treino do modelo de regressao (Gradient Boosting / gbm)
#  - Preve "Petal.Width" do dataset iris a partir das demais medidas.
#  - Treino simples: 1 modelo, split 70/30, metricas no teste.
#  - Salva tudo em "modelo.rds".
#  - Rode UMA vez antes de subir o app:
#        Rscript treinar_modelo.R
#  Dependencia obrigatoria: gbm
# ============================================================

# ---- 0. Configuracao ----
ALVO             <- "Petal.Width"          # variavel que o modelo preve (regressao)
CAMINHO_CSV      <- "dados_iris.csv"
CAMINHO_MODELO   <- "modelo.rds"
PROPORCAO_TREINO <- 0.7
SEMENTE          <- 2025

# ---- 1. Pacotes ----
if (!requireNamespace("gbm", quietly = TRUE)) {
  install.packages("gbm", repos = "https://cloud.r-project.org")
}
library(gbm)

# ---- 2. Dados ----
# O dataset iris e embutido no R. Exportamos para CSV (se faltar) para deixar
# o "dataset" visivel no repositorio e ler sempre do mesmo arquivo.
if (!file.exists(CAMINHO_CSV)) {
  write.csv(iris, CAMINHO_CSV, row.names = FALSE, fileEncoding = "UTF-8")
  cat("dados_iris.csv gerado a partir de data(iris)\n")
}
dados <- read.csv(CAMINHO_CSV, stringsAsFactors = TRUE, fileEncoding = "UTF-8")

# ---- 3. Split treino/teste (70/30, reprodutivel) ----
set.seed(SEMENTE)
idx_treino <- sample(seq_len(nrow(dados)), floor(nrow(dados) * PROPORCAO_TREINO))
treino <- dados[idx_treino, ]
teste  <- dados[-idx_treino, ]

# ---- 4. Treino do Gradient Boosting ----
# distribution = "gaussian" -> regressao (erro quadratico).
# cv.folds = 5 -> usa validacao cruzada para escolher o numero otimo de arvores.
set.seed(SEMENTE)
formula <- as.formula(paste(ALVO, "~ ."))
modelo <- gbm(
  formula,
  data              = treino,
  distribution      = "gaussian",
  n.trees           = 500,
  interaction.depth = 3,
  shrinkage         = 0.05,
  bag.fraction      = 0.8,
  cv.folds          = 5,
  n.minobsinnode    = 5,
  verbose           = FALSE
)

# Numero otimo de arvores pela validacao cruzada (evita overfitting).
best_iter <- gbm.perf(modelo, method = "cv", plot.it = FALSE)

# ---- 5. Avaliacao no teste (RMSE, MAE, R2) ----
pred  <- predict(modelo, teste, n.trees = best_iter)
real  <- teste[[ALVO]]
resid <- real - pred

rmse <- sqrt(mean(resid^2))
mae  <- mean(abs(resid))
r2   <- 1 - sum(resid^2) / sum((real - mean(real))^2)

cat("\n========== AVALIACAO DO MODELO (teste) ==========\n")
cat(sprintf("Arvores otimas : %d\n", best_iter))
cat(sprintf("RMSE           : %.4f\n", rmse))
cat(sprintf("MAE            : %.4f\n", mae))
cat(sprintf("R2             : %.4f\n\n", r2))

# ---- 6. Importancia das variaveis (influencia relativa) ----
imp <- summary(modelo, n.trees = best_iter, plotit = FALSE)
importancia <- data.frame(
  variavel        = as.character(imp$var),
  importancia_pct = round(imp$rel.inf, 2),
  row.names       = NULL
)

# ---- 7. Resumo do dataset (usado no dashboard) ----
preditores <- setdiff(names(dados), ALVO)
resumo <- list(
  n_total    = nrow(dados),
  preditores = preditores,
  alvo_vec   = dados[[ALVO]]   # para o histograma e o "onde cai" na previsao
)

# ---- 8. Bundle: tudo que o app precisa em um unico arquivo ----
bundle <- list(
  modelo      = modelo,
  best_iter   = best_iter,
  alvo        = ALVO,
  importancia = importancia,
  metricas    = list(
    rmse = round(rmse, 4),
    mae  = round(mae, 4),
    r2   = round(r2, 4)
  ),
  resumo = resumo,
  info = list(
    algoritmo   = "Gradient Boosting (gbm), distribution = gaussian",
    n_treino    = nrow(treino),
    n_teste     = nrow(teste),
    treinado_em = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
)
saveRDS(bundle, CAMINHO_MODELO)
cat("Modelo salvo em", CAMINHO_MODELO, "\n")
