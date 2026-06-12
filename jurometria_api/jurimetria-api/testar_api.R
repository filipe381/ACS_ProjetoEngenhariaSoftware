# ============================================================
#  Testes rapidos da API (rode com a API ja no ar em outra aba)
#  Uso:  Rscript testar_api.R
# ============================================================

if (!requireNamespace("httr", quietly = TRUE)) {
  install.packages("httr", repos = "https://cloud.r-project.org")
}
library(httr)

base <- "http://localhost:8000"

cat("== GET / ==\n")
print(content(GET(base)))

cat("\n== GET /info ==\n")
print(content(GET(paste0(base, "/info"))))

cat("\n== GET /metricas ==\n")
print(content(GET(paste0(base, "/metricas"))))

cat("\n== POST /prever (caso valido) ==\n")
# OBS: os valores precisam bater com os niveis do dataset, que tem acento.
r <- POST(paste0(base, "/prever"), body = list(
  area = "Consumidor", valor_acao = 15000,
  tipo_parte_autora = "Pessoa Física", ente_publico_reu = "Não",
  advogado_especializado = "Sim", foro = "Capital"
), encode = "form")
print(content(r))

cat("\n== POST /prever (caso invalido: area errada) ==\n")
r <- POST(paste0(base, "/prever"), body = list(
  area = "Penal", valor_acao = -5,
  tipo_parte_autora = "Pessoa Fisica", ente_publico_reu = "Nao",
  advogado_especializado = "Sim", foro = "Capital"
), encode = "form")
cat("status:", status_code(r), "\n")
print(content(r))
