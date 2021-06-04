# Leitura e processamento de dados


# persistence 
# mamiferos nativos de medio e grande porte - periodo: jan 2015 a jan 2017
persistence_data <- read.csv(here::here("data", "processed", "persistence_data.csv"), sep = ";")

# searcher efficiency
# detected e not detected se referem as carcacas vistas de auto de linha em comparacao as encontradas a pe
efficiency_data <- read.csv(here::here("data", "processed", "efficiency_data.csv"), sep = ";")

# observed fatalities
# registros feitos por Auto de linha (tipo de veiculo de inspecao ferroviario)
fatality_data_filter <- openxlsx::read.xlsx(here::here("data", "raw","fatalidades_RUMO.xlsx"), detectDates = T) %>%
  filter(malha == "Norte",
         ano == "2015" | ano == "2016",
         metodo == "Auto",
         classe == "Mammalia",
         tamanho_corporal == "Médio porte: > 500g e ??? 25kg" | tamanho_corporal == "Grande porte: > 25kg",
         cat != "1",  # exclui animais domesticos e exoticos
         duplicata == "nao") %>% # seleciona animais que nao foram considerados duplicatas
      write.xlsx(., file = here::here("data", "processed","observed_data.xlsx"), asTable = T)

observed_data <- openxlsx::read.xlsx(here::here("data", "processed", "observed_data.xlsx"), detectDates = T)