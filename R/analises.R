# analises

# persistence 
# estimativa da persistencia das carcassas
persistence <- carcass::persistence.prob(persistence_data$grupo, persistence_data$perstime,
                                         persistence_data$status, pers.const = TRUE) ; persistence
persistence_prob <- persistence$persistence.prob
persistence_lower <- persistence$lower
persistence_upper <- persistence$upper

# searcher efficiency
efficiency <- carcass::search.efficiency(efficiency_data)

efficiency_mean = efficiency$f.perperson$f
efficiency_lower = efficiency$f.perperson$lwr
efficiency_upper = efficiency$f.perperson$upr

# detectability 
# Equacao Etterson - considera persistencia, eficiencia e intervalo de amostragem
# intervalos de amostragens: DATAS INICIAIS DAS CAMPANHAS DE AUTO DE LINHA
# 2015 = 21 a 26 jan; 12 a 19 mar; 13 a 19 mai; 13 a 16 jul; 27 a 31 out; 30 a 04 nov-dez;
# 2016 = 29 a 04 mar-abr; 16 a 23 ago; 11 a 14 nov;
intervalo <-c(50, 62, 61, 106, 34, 120, 140, 87)
detectability <- ettersonEq14(s = persistence_prob, f = efficiency_mean, J = intervalo)
detectability_CI <- CIetterson(s = persistence_prob, s.lwr = persistence_lower, s.upr = persistence_upper, 
                               f = efficiency_mean, f.lwr = efficiency_lower, f.upr = efficiency_upper, 
                               J = intervalo)

# estimativa de fatalidades
observed <- nrow(observed_data)
fatalities <- estimateN(count = observed, p = detectability, p.lower = detectability_CI$p.lower, 
                                 p.upper = detectability_CI$p.upper, pform = "etterson", J = intervalo, 
                                 maxn = 30000, nsim = 10000, plot = T, postdist = T, arrival = "uniform")
plot(0:30000, fatalities$postdist)