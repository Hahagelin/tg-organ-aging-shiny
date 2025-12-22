FROM rocker/shiny:4.5.1

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y git libxml2-dev libssl-dev libcurl4-openssl-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

RUN Rscript -e 'install.packages("renv")'
COPY renv.lock /srv/shiny-server/renv.lock
RUN Rscript -e 'setwd("/srv/shiny-server"); renv::restore()'

RUN rm -rf /srv/shiny-server/*
COPY app/ /srv/shiny-server/

RUN if id shiny &>/dev/null && [ "$(id -u shiny)" -ne 999 ]; then \
      userdel -r shiny; \
      id -u 999 &>/dev/null && userdel -r "$(id -un 999)"; \
    fi; \
    useradd -u 999 -m -s /bin/bash shiny; \
    mkdir -p /var/log/shiny-server; \
    chown -R shiny:shiny /srv/shiny-server/ /var/lib/shiny-server /var/log/shiny-server

USER shiny
EXPOSE 3838
CMD ["/usr/bin/shiny-server"]