# ============================================================
#  Sobe a API de Jurimetria na porta 8000
#  Uso:  Rscript iniciar_api.R
# ============================================================

if (!requireNamespace("plumber", quietly = TRUE)) {
  install.packages("plumber", repos = "https://cloud.r-project.org")
}

if (!file.exists("modelo.rds")) {
  stop("modelo.rds nao encontrado. Treine o modelo antes:  Rscript treinar_modelo.R")
}

message("Subindo API em http://localhost:8000  (docs em /__docs__/)")
plumber::plumb("plumber.R")$run(host = "0.0.0.0", port = 8000)
