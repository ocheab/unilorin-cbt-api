FROM rocker/r-ver:4.5.2

RUN apt-get update && apt-get install -y \
    build-essential \
    pkg-config \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libsqlite3-dev \
    zlib1g-dev \
    libsodium-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('plumber','DBI','RSQLite','jsonlite', 'httr2', 'digest'), repos='https://cloud.r-project.org')"

WORKDIR /app

COPY plumber.R /app/plumber.R

EXPOSE 10000

CMD ["R", "-e", "pr <- plumber::plumb('/app/plumber.R'); pr$run(host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', '10000')))"]
