FROM rocker/r-ver:4.5.1

LABEL org.opencontainers.image.source="https://github.com/DiogoRibeiro7/chaotic-dynamical-systems"
LABEL org.opencontainers.image.title="chaoticds"
LABEL org.opencontainers.image.description="Extreme value analysis for chaotic dynamical systems in R"
LABEL org.opencontainers.image.licenses="MIT"

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        build-essential \
        libcurl4-openssl-dev \
        libfontconfig1-dev \
        libfreetype6-dev \
        libfribidi-dev \
        libharfbuzz-dev \
        libjpeg-dev \
        libpng-dev \
        libssl-dev \
        libtiff-dev \
        libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/chaoticds

COPY . .

RUN R -q -e 'install.packages("remotes", repos = "https://cloud.r-project.org")' \
    && R -q -e 'remotes::install_local(".", dependencies = c("Depends", "Imports", "LinkingTo"), upgrade = "never")' \
    && R -q -e 'stopifnot(requireNamespace("chaoticds", quietly = TRUE)); packageVersion("chaoticds")'

WORKDIR /workspace

CMD ["R"]
