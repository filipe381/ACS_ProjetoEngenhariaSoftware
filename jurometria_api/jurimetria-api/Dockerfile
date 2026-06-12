# ============================================================
#  Imagem Docker da API de Jurimetria
#  - Instala R + plumber (rpart ja vem no R base)
#  - Treina o modelo durante a build (gera modelo.rds)
#  - Sobe a API na porta 8000 ao rodar o container
# ============================================================
FROM rocker/r-ver:4.5.2

# Bibliotecas de sistema exigidas pelas dependencias do plumber
# (httpuv, sodium, curl/openssl)
RUN apt-get update && apt-get install -y --no-install-recommends \
        libsodium-dev \
        libcurl4-openssl-dev \
        libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Instala o plumber usando os binarios do Posit Package Manager (build rapida).
# rpart ja faz parte da distribuicao base do R, entao nao precisa instalar.
RUN R -e "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/noble/latest')); install.packages('plumber')"

WORKDIR /app

# Copia apenas o necessario para treinar e servir
COPY dados_jurimetria.csv treinar_modelo.R plumber.R iniciar_api.R ./

# Treina o modelo na build -> a imagem ja sobe servindo, sem retreinar
RUN Rscript treinar_modelo.R

EXPOSE 8000

CMD ["Rscript", "iniciar_api.R"]
