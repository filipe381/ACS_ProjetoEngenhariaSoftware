# ============================================================
#  Treino da arvore de decisao de jurimetria
#  - Treina o modelo CART e salva tudo em "modelo.rds"
#  - Rode UMA vez antes de subir a API:
#        Rscript treinar_modelo.R
#  Dependencia obrigatoria: rpart  (rpart.plot e opcional, so para a imagem)
# ============================================================

# ---- 0. Configuracao ----
ALVO           <- "resultado"
CAMINHO_CSV    <- "dados_jurimetria.csv"
CAMINHO_MODELO <- "modelo.rds"
PROPORCAO_TREINO <- 0.7
SEMENTE        <- 2025

# ---- 1. Pacotes ----
if (!requireNamespace("rpart", quietly = TRUE)) {
  install.packages("rpart", repos = "https://cloud.r-project.org")
}
library(rpart)

# ---- 2. Dados ----
# O CSV ja vem com nomes de coluna limpos, entao basta ler.
dados <- read.csv(CAMINHO_CSV, stringsAsFactors = TRUE, fileEncoding = "UTF-8")

# ---- 3. "Spec" dos preditores (derivada automaticamente do dataset) ----
# Guardamos, para cada coluna preditora, o tipo e os valores aceitos.
# Isso elimina a necessidade de validar campo por campo na mao depois.
construir_spec <- function(dados, alvo) {
  preditores <- setdiff(names(dados), alvo)
  spec <- lapply(preditores, function(col) {
    x <- dados[[col]]
    if (is.factor(x)) {
      list(tipo = "fator", niveis = levels(x))
    } else {
      list(tipo = "numerico", min = min(x), max = max(x))
    }
  })
  setNames(spec, preditores)
}
spec <- construir_spec(dados, ALVO)

# ---- 4. Split treino/teste estratificado (base R, sem caret) ----
# Mantem a proporcao das classes do alvo em treino e teste.
set.seed(SEMENTE)
idx_treino <- unlist(lapply(
  split(seq_len(nrow(dados)), dados[[ALVO]]),
  function(ids) sample(ids, floor(length(ids) * PROPORCAO_TREINO))
))
treino <- dados[idx_treino, ]
teste  <- dados[-idx_treino, ]

# ---- 5. Treino + poda automatica pelo cp otimo ----
modelo_full <- rpart(
  resultado ~ .,
  data    = treino,
  method  = "class",
  parms   = list(split = "information"),  # ganho de informacao
  control = rpart.control(minsplit = 10, minbucket = 4, cp = 0.001, xval = 10)
)
# cp que minimiza o erro de validacao cruzada (xerror)
cp_otimo <- modelo_full$cptable[which.min(modelo_full$cptable[, "xerror"]), "CP"]
modelo   <- prune(modelo_full, cp = cp_otimo)

# ---- 6. Avaliacao no teste (acuracia, kappa e matriz, em base R) ----
pred <- predict(modelo, teste, type = "class")
mc   <- table(Previsto = pred, Real = teste[[ALVO]])
n        <- sum(mc)
acuracia <- sum(diag(mc)) / n
pe       <- sum(rowSums(mc) * colSums(mc)) / n^2   # concordancia esperada
kappa    <- (acuracia - pe) / (1 - pe)

cat("\n========== AVALIACAO DO MODELO ==========\n")
cat(sprintf("cp otimo : %.5f\n", cp_otimo))
cat(sprintf("Acuracia : %.4f\n", acuracia))
cat(sprintf("Kappa    : %.4f\n\n", kappa))
print(mc)

# ---- 7. Importancia das variaveis ----
imp <- modelo$variable.importance
importancia <- if (is.null(imp)) {
  data.frame()
} else {
  data.frame(
    variavel        = names(imp),
    importancia_pct = round(100 * imp / sum(imp), 2),
    row.names       = NULL
  )
}

# ---- 8. (Opcional) imagem da arvore, se rpart.plot estiver instalado ----
if (requireNamespace("rpart.plot", quietly = TRUE)) {
  png("arvore_decisao.png", width = 1400, height = 900, res = 130)
  rpart.plot::rpart.plot(
    modelo, type = 2, extra = 104, fallen.leaves = TRUE,
    box.palette = "RdYlGn", main = "Arvore de Decisao - Jurimetria"
  )
  dev.off()
  cat("\nImagem da arvore salva em arvore_decisao.png\n")
}

# ---- 9. Salva o "bundle": tudo que a API precisa em um unico arquivo ----
bundle <- list(
  modelo      = modelo,
  spec        = spec,
  classes     = levels(dados[[ALVO]]),
  importancia = importancia,
  metricas    = list(
    acuracia        = round(acuracia, 4),
    kappa           = round(kappa, 4),
    matriz_confusao = as.data.frame.matrix(mc)
  ),
  info = list(
    algoritmo   = "CART (rpart) com poda por cp otimo",
    cp_otimo    = unname(cp_otimo),
    n_treino    = nrow(treino),
    n_teste     = nrow(teste),
    treinado_em = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
)
saveRDS(bundle, CAMINHO_MODELO)
cat("\nModelo salvo em", CAMINHO_MODELO, "\n")
