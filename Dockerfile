FROM r-base

WORKDIR /app

ADD bootstrap.sh .
ADD packages.R .

RUN chmod +x bootstrap.sh
RUN ./bootstrap.sh
RUN R -e "install.packages(\"shinycssloaders\")"

COPY . .

ENV PORT=8080

CMD ["Rscript", "app.R"]