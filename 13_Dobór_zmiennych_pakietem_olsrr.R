setwd("C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Wstêpna analiza")
library(olsrr)
p <- read.csv("Dane_all_fundusze.csv", sep = ",", header = T, row.names = 1, fileEncoding = "UTF-16LE")

p <- p[, c('Op³aty.bie¿¹ce.nor', 'rr16.nor', 'sr16.nor', 'sd16.nor', 'TER.nor', 'Pierwsza.wp³ata.nor', 
           'zmiana_akt_flow_16_perc.nor', 'VaR10.nor', 'VaR15.nor', 'VaR20.nor', 'VaR100.nor', 
           'VaR200.nor', 'Log_akt.nor', 'Wiek.nor', 'Typ_ad')]

model <- lm(p$Op³aty.bie¿¹ce.nor ~ p$rr16.nor + p$sr16.nor + p$sd16.nor + p$TER.nor + 
              p$Pierwsza.wp³ata.nor + p$zmiana_akt_flow_16_perc.nor + 
              p$VaR10.nor + p$VaR15.nor + p$VaR20.nor + p$VaR100.nor + p$VaR200.nor + 
              p$Wiek.nor + p$Log_akt.nor + p$Typ_ad, data = p)



