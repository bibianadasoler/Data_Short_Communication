# Leitura e processamento de dados


# persistence 
# mamiferos nativos de medio e grande porte - periodo: jan 2015 a jan 2017
persistence_data <- read.csv(here::here("data", "processed", "persistence_data.csv"), sep = ";")

# searcher efficiency
# detected e not detected se referem as carcacas vistas de auto de linha em comparacao as encontradas a pe
efficiency <- read.csv(here::here("data", "processed", "efficiency_data.csv"), sep = ";", h = T)
