# ============================================================
#  App Shiny — Previsao de metricas com Gradient Boosting (gbm)
#  Dataset: iris | Alvo: Petal.Width (regressao)
#  Abas: Dashboard (graficos) | Previsao (interativa) | Sobre
#  Rodar:
#     Rscript -e "shiny::runApp('app.R', port=8080)"
#  (Se modelo.rds nao existir, o app treina na 1a execucao.)
# ============================================================

# ---- Pacotes (auto-instalacao do CRAN se faltarem) ----
pacotes <- c("shiny", "shinydashboard", "ggplot2", "DT", "gbm")
faltando <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltando) > 0) {
  install.packages(faltando, repos = "https://cloud.r-project.org")
}
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(gbm)

# ---- Modelo + dados ----
# Se o bundle nao existe, treina (gera modelo.rds e dados_iris.csv).
if (!file.exists("modelo.rds")) source("treinar_modelo.R")
bundle <- readRDS("modelo.rds")
dados  <- read.csv("dados_iris.csv", stringsAsFactors = TRUE, fileEncoding = "UTF-8")

ALVO    <- bundle$alvo
niveis_especie <- levels(dados$Species)
COR <- "#2E7D32"  # verde (tema "iris"/botanico)

# Faixas reais de cada preditor numerico (para os sliders da previsao).
faixa <- function(col) {
  v <- dados[[col]]
  list(min = floor(min(v) * 10) / 10,
       max = ceiling(max(v) * 10) / 10,
       med = round(mean(v), 1))
}
fSL <- faixa("Sepal.Length"); fSW <- faixa("Sepal.Width"); fPL <- faixa("Petal.Length")

# ============================================================
#  UI
# ============================================================
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Iris — Previsao (GBM)", titleWidth = 260),
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-column")),
      menuItem("Previsao",  tabName = "previsao",  icon = icon("wand-magic-sparkles")),
      menuItem("Sobre",     tabName = "sobre",     icon = icon("circle-info"))
    )
  ),
  dashboardBody(
    tabItems(
      # ---------- Aba 1: Dashboard ----------
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBox(bundle$resumo$n_total, "Amostras (flores)", icon = icon("seedling"), color = "green"),
          valueBox(sprintf("%.3f", bundle$metricas$r2), "R2 (teste)", icon = icon("bullseye"), color = "olive"),
          valueBox(sprintf("%.3f", bundle$metricas$rmse), "RMSE (teste)", icon = icon("ruler"), color = "teal")
        ),
        fluidRow(
          box(title = "Distribuicao do alvo (Petal.Width)", width = 6, status = "success",
              solidHeader = TRUE, plotOutput("hist_alvo", height = 280)),
          box(title = "Petal.Length x Petal.Width por especie", width = 6, status = "success",
              solidHeader = TRUE, plotOutput("disp", height = 280))
        ),
        fluidRow(
          box(title = "Importancia das variaveis (influencia relativa %)", width = 5,
              status = "success", solidHeader = TRUE, plotOutput("imp", height = 260)),
          box(title = "Dataset (iris)", width = 7, status = "success",
              solidHeader = TRUE, DT::dataTableOutput("tabela"))
        )
      ),
      # ---------- Aba 2: Previsao ----------
      tabItem(
        tabName = "previsao",
        fluidRow(
          box(title = "Caracteristicas da flor", width = 5, status = "success", solidHeader = TRUE,
              sliderInput("sl", "Sepal.Length (cm)", fSL$min, fSL$max, fSL$med, step = 0.1),
              sliderInput("sw", "Sepal.Width (cm)",  fSW$min, fSW$max, fSW$med, step = 0.1),
              sliderInput("pl", "Petal.Length (cm)", fPL$min, fPL$max, fPL$med, step = 0.1),
              selectInput("sp", "Species", choices = niveis_especie),
              actionButton("prever", "Prever Petal.Width", icon = icon("play"),
                           class = "btn-success btn-lg")
          ),
          box(title = "Resultado", width = 7, status = "success", solidHeader = TRUE,
              valueBoxOutput("vb_pred", width = 12),
              uiOutput("contexto_pred"),
              plotOutput("hist_pred", height = 240)
          )
        )
      ),
      # ---------- Aba 3: Sobre ----------
      tabItem(
        tabName = "sobre",
        box(width = 12, status = "success", solidHeader = TRUE, title = "Sobre este projeto",
            HTML(paste0(
              "<h4>Objetivo</h4>",
              "<p>Aplicacao web em <b>Shiny (R)</b> que <b>preve valores</b> de uma variavel ",
              "numerica usando um modelo de <b>Gradient Boosting</b> (pacote <code>gbm</code>).</p>",
              "<h4>Dataset</h4>",
              "<p>Usa o classico <b>iris</b> (150 flores, 4 medidas + especie). O modelo preve o ",
              "<b>Petal.Width</b> (largura da petala) a partir de <code>Sepal.Length</code>, ",
              "<code>Sepal.Width</code>, <code>Petal.Length</code> e <code>Species</code>.</p>",
              "<p><i>Observacao:</i> o iris e um dataset demonstrativo de medidas botanicas, ",
              "escolhido pela simplicidade e confiabilidade.</p>",
              "<h4>Metodologia</h4>",
              "<ul>",
              "<li>Split treino/teste 70/30 reprodutivel (<code>set.seed(2025)</code>).</li>",
              "<li>Gradient Boosting com numero de arvores escolhido por validacao cruzada (5-fold).</li>",
              "<li>Avaliacao no teste por RMSE, MAE e R2.</li>",
              "<li>Treino separado de servir: <code>treinar_modelo.R</code> gera <code>modelo.rds</code>; ",
              "o app apenas carrega o modelo.</li>",
              "</ul>",
              "<h4>Modelo treinado</h4>",
              "<p>Algoritmo: ", bundle$info$algoritmo, "<br>",
              "Arvores otimas: ", bundle$best_iter, " | ",
              "RMSE: ", bundle$metricas$rmse, " | MAE: ", bundle$metricas$mae,
              " | R2: ", bundle$metricas$r2, "<br>",
              "Treino: ", bundle$info$n_treino, " amostras | Teste: ", bundle$info$n_teste,
              " | Treinado em: ", bundle$info$treinado_em, "</p>"
            ))
        )
      )
    )
  )
)

# ============================================================
#  Server
# ============================================================
server <- function(input, output, session) {

  # ---------- Dashboard ----------
  output$hist_alvo <- renderPlot({
    ggplot(dados, aes(x = .data[[ALVO]])) +
      geom_histogram(bins = 20, fill = COR, color = "white") +
      labs(x = ALVO, y = "Frequencia") +
      theme_minimal(base_size = 13)
  })

  output$disp <- renderPlot({
    ggplot(dados, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
      geom_point(size = 2.5, alpha = 0.8) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
      labs(x = "Petal.Length", y = "Petal.Width") +
      theme_minimal(base_size = 13)
  })

  output$imp <- renderPlot({
    imp <- bundle$importancia
    imp$variavel <- factor(imp$variavel, levels = rev(imp$variavel))
    ggplot(imp, aes(x = importancia_pct, y = variavel)) +
      geom_col(fill = COR) +
      geom_text(aes(label = sprintf("%.1f%%", importancia_pct)), hjust = -0.1, size = 4) +
      xlim(0, max(imp$importancia_pct) * 1.18) +
      labs(x = "Influencia relativa (%)", y = NULL) +
      theme_minimal(base_size = 13)
  })

  output$tabela <- DT::renderDataTable({
    DT::datatable(dados, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  # ---------- Previsao ----------
  predicao <- eventReactive(input$prever, {
    novo <- data.frame(
      Sepal.Length = input$sl,
      Sepal.Width  = input$sw,
      Petal.Length = input$pl,
      Species      = factor(input$sp, levels = niveis_especie)
    )
    as.numeric(predict(bundle$modelo, novo, n.trees = bundle$best_iter))
  }, ignoreNULL = FALSE)

  output$vb_pred <- renderValueBox({
    valueBox(
      sprintf("%.2f cm", predicao()),
      "Petal.Width previsto",
      icon = icon("wand-magic-sparkles"), color = "green"
    )
  })

  output$contexto_pred <- renderUI({
    p <- predicao()
    pct <- round(mean(dados[[ALVO]] <= p) * 100)
    HTML(sprintf(
      paste0("<p style='margin-top:8px'>O valor previsto e <b>maior ou igual</b> a cerca de ",
             "<b>%d%%</b> das flores do dataset (percentil aproximado).</p>"),
      pct))
  })

  output$hist_pred <- renderPlot({
    p <- predicao()
    ggplot(dados, aes(x = .data[[ALVO]])) +
      geom_histogram(bins = 20, fill = "#A5D6A7", color = "white") +
      geom_vline(xintercept = p, color = "#1B5E20", linewidth = 1.3) +
      annotate("text", x = p, y = Inf, label = sprintf(" previsto: %.2f", p),
               hjust = 0, vjust = 1.4, color = "#1B5E20", fontface = "bold") +
      labs(x = ALVO, y = "Frequencia") +
      theme_minimal(base_size = 13)
  })
}

shinyApp(ui, server)
