setwd("C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Wstêpna analiza")
p <- read.csv("Dane_all_fundusze.csv", sep = ",", header = T, row.names = 1, fileEncoding = "UTF-16LE")

rang <- p[,c('Nazwa.IZFiA', 'Pierwsza.wp³ata', 'rr16')]

for (i in (2:3)) {
  rang[,i] <- as.integer(cut(rang[,i], quantile(rang[,i], 0:4/4, na.rm = T), include.lowest = T))
  colnames(rang)[i] <- paste(colnames(rang)[i], "_rang", sep = "")
}

rang_m <- merge(rang[, c('Nazwa.IZFiA', 'Pierwsza.wp³ata_rang', 'rr16_rang')], p, by = 'Nazwa.IZFiA', all.y = T)

model_r <- lm(rang_m$Op³aty.bie¿¹ce.nor ~ rang_m$rr16_ + rang_m$sr16.nor + rang_m$sd16.nor + rang_m$TER.nor + 
                rang_m$Pierwsza.wp³ata.x + rang_m$zmiana_akt_flow_16_perc.nor + 
                rang_m$VaR10.nor + rang_m$VaR15.nor + rang_m$VaR20.nor + rang_m$VaR100.nor + rang_m$VaR200.nor + 
                rang_m$Wiek.nor + rang_m$Log_akt.nor + rang_m$Typ_ad, data = rang_m)

ols_step_backward(model_r, details = T, prem = 0.05)
model_r_sel <- lm(rang_m$Op³aty.bie¿¹ce.nor ~ rang_m$sd16.nor + rang_m$TER.nor + 
                rang_m$Pierwsza.wp³ata_rang + rang_m$zmiana_akt_flow_16_perc.nor + 
                rang_m$Wiek.nor + rang_m$Log_akt.nor + rang_m$Typ_ad, data = rang_m)
summary(model_r_sel)

# Zamiana rangowych na >=1000 i < 1000

for (i in 1:dim(rang_m)[1]) {
  if (rang_m$Pierwsza.wp³ata[i] >= 1000) {
    rang_m$Pierwsza.wp³ata.1000[i] <- 2
  } else {
    rang_m$Pierwsza.wp³ata.1000[i] <- 1
  }
}

model_r2 <- lm(rang_m$Op³aty.bie¿¹ce.nor ~ rang_m$rr16_rang + rang_m$sr16.nor + rang_m$sd16.nor + rang_m$TER.nor + 
                rang_m$Pierwsza.wp³ata.1000 + rang_m$zmiana_akt_flow_16_perc.nor + 
                rang_m$VaR10.nor + rang_m$VaR15.nor + rang_m$VaR20.nor + rang_m$VaR100.nor + rang_m$VaR200.nor + 
                rang_m$Wiek.nor + rang_m$Log_akt.nor + rang_m$Typ_ad, data = rang_m)

ols_step_backward(model_r, details = T, prem = 0.05)
model_r2_sel <- lm(rang_m$Op³aty.bie¿¹ce.nor ~ rang_m$sr16.nor + rang_m$sd16.nor + rang_m$TER.nor + 
                 rang_m$Pierwsza.wp³ata.1000 + rang_m$zmiana_akt_flow_16_perc.nor + 
                 rang_m$Wiek.nor + rang_m$Log_akt.nor + rang_m$Typ_ad, data = rang_m)
