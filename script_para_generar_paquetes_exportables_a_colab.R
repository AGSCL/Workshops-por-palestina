# Crear carpeta destino
dir.create("paquetes_tar")

# Lista de paquetes
paquetes <- c("showtext", "ggplot2", "plotly", "grid", "gridExtra", "magick", "knitr",
              "tidyverse", "rio", "psych", "parallel", "doParallel", "glca",
              "DiagrammeR", "DiagrammeRsvg", "rsvg", "htmlwidgets", "janitor")

# Descargar los paquetes en formato .tar.gz
download.packages(paquetes, destdir = "paquetes_tar", type = "source")

system("tar -czvf paquetes.tar.gz paquetes_tar")


unlink("paquetes_tar", recursive = TRUE)
