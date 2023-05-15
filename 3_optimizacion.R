library(tidyverse)
library(tidyquant)
set.seed(2022)

retornos_totales <- read.csv("datos/cartera_total.csv", sep = ",")

calculo_escenario <- function(numero_escenario){
  retornos_medios <- colMeans(retornos_totales %>% select(-fecha))
  covarianzas_anualizadas <- cov(retornos_totales %>% select(-fecha))*12
  pesos <- runif(n = length(retornos_medios))
  pesos <- pesos/sum(pesos)
  retorno_portafolio <- (sum(pesos * retornos_medios) + 1)^12 - 1
  varianza_portafolio <- t(pesos) %*% (covarianzas_anualizadas %*% pesos)
  riesgo_portafolio <- sqrt(varianza_portafolio)
  sharpe_ratio_portafolio <- retorno_portafolio/riesgo_portafolio
  pesos_lista <- as.list(pesos) 
  names(pesos_lista) <- paste0('peso_', names(retornos_medios))
  
  resultado <- as_tibble(c(pesos_lista, 
                           retorno = retorno_portafolio,
                           riesgo = riesgo_portafolio,
                           sharpe_ratio = sharpe_ratio_portafolio))
}

resultados <- map_df(1:1000, calculo_escenario)

min_riesgo <- resultados %>% 
  filter(riesgo == min(riesgo))

max_sr <- resultados %>% 
  filter(sharpe_ratio == max(sharpe_ratio))

resultados %>%
  ggplot(aes(x = riesgo, y = retorno, color = sharpe_ratio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Riesgo Anualizado',
       y = 'Retorno Anualizado',
       title = "ROP: Optimización de portafolios") +
  geom_point(aes(x = riesgo,
                 y = retorno), data = min_riesgo, color = 'red', size = 4) +
  geom_point(aes(x = riesgo,
                 y = retorno), data = max_sr, color = 'orange', size = 4)
