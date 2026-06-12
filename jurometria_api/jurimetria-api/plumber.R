# ============================================================
#  API REST de Jurimetria - Arvore de Decisao (plumber)
#  Carrega o modelo treinado (modelo.rds) e expoe as previsoes.
#  Suba com:  Rscript iniciar_api.R
#  Docs Swagger: http://localhost:8000/__docs__/
# ============================================================

bundle <- readRDS("modelo.rds")

# ------------------------------------------------------------
#  Funcoes genericas guiadas pela "spec" salva com o modelo.
#  Validacao e montagem da linha funcionam para QUALQUER conjunto
#  de colunas - nao ha campo escrito na mao aqui.
# ------------------------------------------------------------
validar_entrada <- function(entrada, spec) {
  erros <- character(0)
  for (campo in names(spec)) {
    valor <- entrada[[campo]]
    regra <- spec[[campo]]

    vazio <- is.null(valor) || (length(valor) == 1 && is.na(valor)) ||
             !nzchar(as.character(valor))
    if (vazio) {
      erros <- c(erros, sprintf("Campo '%s' e obrigatorio.", campo))
      next
    }

    if (regra$tipo == "fator") {
      if (!as.character(valor) %in% regra$niveis) {
        erros <- c(erros, sprintf(
          "Valor invalido para '%s': '%s'. Aceitos: %s.",
          campo, valor, paste(regra$niveis, collapse = ", ")
        ))
      }
    } else {  # numerico
      v <- suppressWarnings(as.numeric(valor))
      if (is.na(v)) {
        erros <- c(erros, sprintf("Campo '%s' deve ser numerico.", campo))
      } else if (v < 0) {
        erros <- c(erros, sprintf("Campo '%s' nao pode ser negativo.", campo))
      }
    }
  }
  erros
}

montar_linha <- function(entrada, spec) {
  linha <- lapply(names(spec), function(campo) {
    regra <- spec[[campo]]
    if (regra$tipo == "fator") {
      factor(as.character(entrada[[campo]]), levels = regra$niveis)
    } else {
      as.numeric(entrada[[campo]])
    }
  })
  as.data.frame(setNames(linha, names(spec)), stringsAsFactors = FALSE)
}

# ============================================================
#                       ENDPOINTS
# ============================================================

#* @apiTitle API de Jurimetria - Arvore de Decisao
#* @apiDescription Preve o resultado de uma acao judicial (Procedente /
#*   Parcialmente Procedente / Improcedente) usando uma arvore CART.
#* @apiVersion 2.0.0

#* Health-check da API
#* @get /
function() {
  list(
    status    = "ok",
    mensagem  = "API de Jurimetria no ar",
    endpoints = c("/", "/info", "/metricas", "/importancia", "/prever")
  )
}

#* Informacoes do modelo e campos aceitos
#* @get /info
function() {
  c(bundle$info, list(classes = bundle$classes, campos = bundle$spec))
}

#* Metricas de desempenho no conjunto de teste
#* @get /metricas
function() {
  bundle$metricas
}

#* Importancia das variaveis na arvore
#* @get /importancia
function() {
  bundle$importancia
}

#* Faz uma previsao com base nas caracteristicas do processo
#* @param area Area do direito (Civel, Trabalhista, Consumidor)
#* @param valor_acao Valor da causa (numerico, >= 0)
#* @param tipo_parte_autora Pessoa Fisica ou Pessoa Juridica
#* @param ente_publico_reu Sim ou Nao
#* @param advogado_especializado Sim ou Nao
#* @param foro Capital ou Interior
#* @get /prever
#* @post /prever
function(res,
         area = NULL, valor_acao = NULL, tipo_parte_autora = NULL,
         ente_publico_reu = NULL, advogado_especializado = NULL, foro = NULL) {

  entrada <- list(
    area                   = area,
    valor_acao             = valor_acao,
    tipo_parte_autora      = tipo_parte_autora,
    ente_publico_reu       = ente_publico_reu,
    advogado_especializado = advogado_especializado,
    foro                   = foro
  )

  erros <- validar_entrada(entrada, bundle$spec)
  if (length(erros) > 0) {
    res$status <- 400
    return(list(erro = "Requisicao invalida", detalhes = erros))
  }

  linha  <- montar_linha(entrada, bundle$spec)
  classe <- as.character(predict(bundle$modelo, linha, type = "class"))
  probs  <- predict(bundle$modelo, linha, type = "prob")[1, ]

  list(
    previsao       = classe,
    confianca      = round(unname(max(probs)), 4),
    probabilidades = as.list(round(probs, 4)),
    entrada        = entrada
  )
}
