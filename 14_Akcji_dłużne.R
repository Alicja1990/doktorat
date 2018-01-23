setwd("C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Wstêpna analiza")
p <- read.csv("Dane_all_fundusze_rang.csv", sep = ",", header = T, row.names = 1)

model_sel_r <- lm(p$Op³aty.bie¿¹ce.nor ~ p$sr16.nor + p$sd16.nor + p$TER.nor + 
                    p$zmiana_akt_flow_16_perc.nor + p$Pierwsza.wp³ata.1000 +
                    p$Wiek.nor + p$Log_akt.nor + p$Typ_ad, data = p)
summary(model_sel_r)

p <- p[p$Typ_ad == 'akcjopodobne',]
model_r_akcji <- lm(p$Op³aty.bie¿¹ce.nor ~ p$rr16_rang + p$rr16.nor + p$sr16.nor + p$sd16.nor + p$TER.nor + 
                      p$Pierwsza.wp³ata.nor + p$Pierwsza.wp³ata.1000 + p$zmiana_akt_flow_16_perc.nor + 
                      p$VaR10.nor + p$VaR15.nor + p$VaR20.nor + p$VaR100.nor + p$VaR200.nor + 
                      p$Wiek.nor + p$Log_akt.nor, data = p)
ols_step_backward(model_r_akcji, details = F, prem = 0.05)
model_r_akcji_sel <- lm(p$Op³aty.bie¿¹ce.nor ~ p$sr16.nor + p$sd16.nor + p$TER.nor + 
                          p$Pierwsza.wp³ata.1000 + 
                          p$Wiek.nor + p$Log_akt.nor, data = p)
summary(model_r_akcji_sel)

p <- read.csv("Dane_all_fundusze_rang.csv", sep = ",", header = T, row.names = 1)
p <- p[p$Typ_ad == 'd³u¿nopodobne',]
model_r_d³u¿ne <- lm(p$Op³aty.bie¿¹ce.nor ~ p$rr16_rang + p$rr16.nor + p$sr16.nor + p$sd16.nor + p$TER.nor + 
                      p$Pierwsza.wp³ata.nor + p$Pierwsza.wp³ata.1000 + p$zmiana_akt_flow_16_perc.nor + 
                      p$VaR10.nor + p$VaR15.nor + p$VaR20.nor + p$VaR100.nor + p$VaR200.nor + 
                      p$Wiek.nor + p$Log_akt.nor, data = p)
ols_step_backward(model_r_d³u¿ne, details = F, prem = 0.05)
model_r_d³u¿ne_sel <- lm(p$Op³aty.bie¿¹ce.nor ~ p$TER.nor + 
                           p$zmiana_akt_flow_16_perc.nor + 
                           p$VaR200.nor, data = p)
summary(model_r_d³u¿ne_sel)
