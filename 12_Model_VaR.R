setwd("C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Wstêpna analiza")
source("11_VaR.R")

r <- r[!(r$Nazwa.IZFiA == ""),]
z <- z[!(z$Nazwa.IZFiA == ""),]
z <- subset(z, select = c(Nazwa.IZFiA, rr16:sr12.6))

s <- merge(z, r, by = 'Nazwa.IZFiA')
s$TER <- as.numeric(as.character(s$TER))
s$Aktywa <- as.numeric(as.character(s$Aktywa))

s$Log_akt <- log(s$Aktywa)
s$Wiek <- 2017 - s$Rok.uruchomienia

nor <- s[,c('Nazwa.IZFiA', 'rr16', 'sr16', 'sd16', 'TER', 'Op³aty.bie¿¹ce', 'Pierwsza.wp³ata', 'zmiana_akt_flow_16_perc', 'VaR10', 'VaR15', 'VaR20', 'VaR100', 'VaR200', 'Log_akt', 'Wiek')]

for (i in 2:dim(nor)[2]) {
  for (j in 1:dim(nor)[1]){
    nor[j, i] <- (nor[j, i] - mean(nor[, i], na.rm = T)) / sd(nor [,i], na.rm = T)
  }
  colnames(nor)[i] <- paste(colnames(nor)[i], '.nor', sep = "")
}

s <- merge(nor, s, by = 'Nazwa.IZFiA')

source("12a_ Zamiana_typ_na_akcji_d³u¿ne.R")
s <- s[!is.na(s$rr16.nor) & !is.na(s$Op³aty.bie¿¹ce.nor) & !is.na(s$sr16.nor) & 
         !is.na(s$sd16.nor) & !is.na(s$TER.nor) & !is.na(s$Pierwsza.wp³ata.nor) & 
          !is.na(s$zmiana_akt_flow_16_perc.nor) & !is.na(s$VaR10.nor) & !is.na(s$Wiek.nor) &
         !is.na(s$Log_akt.nor) & !is.na(s$Typ_ad),]

write.csv(s, file = "Dane_all_fundusze.csv")

model <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
              s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
              s$VaR10.nor + s$VaR15.nor + s$VaR20.nor + s$VaR100.nor + s$VaR200.nor + 
              s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)

# Selekcja zmiennych

model.rr16 <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR10.nor + s$VaR15.nor + s$VaR20.nor + s$VaR100.nor + s$VaR200.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.sr16 <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR10.nor + s$VaR15.nor + s$VaR20.nor + s$VaR100.nor + s$VaR200.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.sd16 <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR10.nor + s$VaR15.nor + s$VaR20.nor + s$VaR100.nor + s$VaR200.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.TER <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor +
                  s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                  s$VaR10.nor + s$VaR15.nor + s$VaR20.nor + s$VaR100.nor + s$VaR200.nor + 
                  s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.Pier.wp <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
                      s$zmiana_akt_flow_16_perc.nor + 
                      s$VaR10.nor + s$VaR15.nor + s$VaR20.nor + s$VaR100.nor + s$VaR200.nor + 
                      s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.zmiana.akt <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
                         s$Pierwsza.wp³ata.nor + 
                         s$VaR10.nor + s$VaR15.nor + s$VaR20.nor + s$VaR100.nor + s$VaR200.nor + 
                         s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.VaR10 <-lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR15.nor + s$VaR20.nor + s$VaR100.nor + s$VaR200.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.VaR15 <-lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR10.nor + s$VaR20.nor + s$VaR100.nor + s$VaR200.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.VaR20 <-lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR15.nor + s$VaR10.nor + s$VaR100.nor + s$VaR200.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.VaR100 <-lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR15.nor + s$VaR20.nor + s$VaR10.nor + s$VaR200.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.VaR200 <-lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR15.nor + s$VaR20.nor + s$VaR100.nor + s$VaR10.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.Wiek <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
              s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
              s$VaR10.nor + s$VaR15.nor + s$VaR20.nor + s$VaR100.nor + s$VaR200.nor + 
              s$Log_akt.nor + s$Typ_ad)
model.log.akt <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
              s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
              s$VaR10.nor + s$VaR15.nor + s$VaR20.nor + s$VaR100.nor + s$VaR200.nor + 
              s$Wiek.nor + s$Typ_ad)
model.typ <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
              s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
              s$VaR10.nor + s$VaR15.nor + s$VaR20.nor + s$VaR100.nor + s$VaR200.nor + 
              s$Wiek.nor + s$Log_akt.nor)

sse_f <- sum((model$residuals)^2)
n <- dim(s)[[1]]
k <- 14
r <- 13
a <- sse_f/(n-k-1)

sse_rr16 <- sum((model.rr16$residuals)^2)
sse_sr16 <- sum((model.sr16$residuals)^2)
sse_sd16 <- sum((model.sd16$residuals)^2)
sse_TER <- sum((model.TER$residuals)^2)
sse_Pier.wp <- sum((model.Pier.wp$residuals)^2)
sse_zmiana.akt <- sum((model.zmiana.akt$residuals)^2)
sse_VaR10 <- sum((model.VaR10$residuals)^2)
sse_VaR15 <- sum((model.VaR15$residuals)^2)
sse_VaR20 <- sum((model.VaR20$residuals)^2)
sse_VaR100 <- sum((model.VaR100$residuals)^2)
sse_VaR200 <- sum((model.VaR200$residuals)^2)
sse_wiek <- sum((model.Wiek$residuals)^2)
sse_log.akt <- sum((model.log.akt$residuals)^2)
sse_typ <- sum((model.typ$residuals)^2)

((sse_rr16 - sse_f)/r)/a
((sse_sr16 - sse_f)/r)/a
((sse_sd16 - sse_f)/r)/a
((sse_TER - sse_f)/r)/a
((sse_Pier.wp - sse_f)/r)/a
((sse_zmiana.akt - sse_f)/r)/a
((sse_VaR10 - sse_f)/r)/a
((sse_VaR15 - sse_f)/r)/a
((sse_VaR20 - sse_f)/r)/a
((sse_VaR100 - sse_f)/r)/a
((sse_VaR200 - sse_f)/r)/a
((sse_wiek - sse_f)/r)/a
((sse_log.akt - sse_f)/r)/a
((sse_typ - sse_f)/r)/a

# Eliminujemy VaR10

model.rr16 <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR15.nor + s$VaR20.nor + s$VaR100.nor + s$VaR200.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.sr16 <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR15.nor + s$VaR20.nor + s$VaR100.nor + s$VaR200.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.sd16 <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR15.nor + s$VaR20.nor + s$VaR100.nor + s$VaR200.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.TER <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor +
                  s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                  s$VaR15.nor + s$VaR20.nor + s$VaR100.nor + s$VaR200.nor + 
                  s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.Pier.wp <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
                      s$zmiana_akt_flow_16_perc.nor + 
                      s$VaR15.nor + s$VaR20.nor + s$VaR100.nor + s$VaR200.nor + 
                      s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.zmiana.akt <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
                         s$Pierwsza.wp³ata.nor + 
                         s$VaR15.nor + s$VaR20.nor + s$VaR100.nor + s$VaR200.nor + 
                         s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.VaR15 <-lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR20.nor + s$VaR100.nor + s$VaR200.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.VaR20 <-lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR15.nor + s$VaR100.nor + s$VaR200.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.VaR100 <-lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
                    s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                    s$VaR15.nor + s$VaR20.nor + s$VaR200.nor + 
                    s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.VaR200 <-lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
                    s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                    s$VaR15.nor + s$VaR20.nor + s$VaR100.nor + 
                    s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.Wiek <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR15.nor + s$VaR20.nor + s$VaR100.nor + s$VaR200.nor + 
                   s$Log_akt.nor + s$Typ_ad)
model.log.akt <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
                      s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                      s$VaR15.nor + s$VaR20.nor + s$VaR100.nor + s$VaR200.nor + 
                      s$Wiek.nor + s$Typ_ad)
model.typ <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
                  s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                  s$VaR15.nor + s$VaR20.nor + s$VaR100.nor + s$VaR200.nor + 
                  s$Wiek.nor + s$Log_akt.nor)

sse_rr16 <- sum((model.rr16$residuals)^2)
sse_sr16 <- sum((model.sr16$residuals)^2)
sse_sd16 <- sum((model.sd16$residuals)^2)
sse_TER <- sum((model.TER$residuals)^2)
sse_Pier.wp <- sum((model.Pier.wp$residuals)^2)
sse_zmiana.akt <- sum((model.zmiana.akt$residuals)^2)
sse_VaR15 <- sum((model.VaR15$residuals)^2)
sse_VaR20 <- sum((model.VaR20$residuals)^2)
sse_VaR100 <- sum((model.VaR100$residuals)^2)
sse_VaR200 <- sum((model.VaR200$residuals)^2)
sse_wiek <- sum((model.Wiek$residuals)^2)
sse_log.akt <- sum((model.log.akt$residuals)^2)
sse_typ <- sum((model.typ$residuals)^2)

((sse_rr16 - sse_f)/r)/a
((sse_sr16 - sse_f)/r)/a
((sse_sd16 - sse_f)/r)/a
((sse_TER - sse_f)/r)/a
((sse_Pier.wp - sse_f)/r)/a
((sse_zmiana.akt - sse_f)/r)/a
((sse_VaR15 - sse_f)/r)/a
((sse_VaR20 - sse_f)/r)/a
((sse_VaR100 - sse_f)/r)/a
((sse_VaR200 - sse_f)/r)/a
((sse_wiek - sse_f)/r)/a
((sse_log.akt - sse_f)/r)/a
((sse_typ - sse_f)/r)/a

# Eliminujê VaR20

model.rr16 <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR15.nor + s$VaR100.nor + s$VaR200.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.sr16 <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR15.nor + s$VaR100.nor + s$VaR200.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.sd16 <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR15.nor + s$VaR100.nor + s$VaR200.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.TER <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor +
                  s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                  s$VaR15.nor + s$VaR100.nor + s$VaR200.nor + 
                  s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.Pier.wp <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
                      s$zmiana_akt_flow_16_perc.nor + 
                      s$VaR15.nor + s$VaR100.nor + s$VaR200.nor + 
                      s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.zmiana.akt <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
                         s$Pierwsza.wp³ata.nor + 
                         s$VaR15.nor + s$VaR100.nor + s$VaR200.nor + 
                         s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.VaR15 <-lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR100.nor + s$VaR200.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.VaR100 <-lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
                    s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                    s$VaR15.nor + s$VaR200.nor + 
                    s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.VaR200 <-lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
                    s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                    s$VaR15.nor + s$VaR100.nor + 
                    s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.Wiek <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR15.nor + s$VaR100.nor + s$VaR200.nor + 
                   s$Log_akt.nor + s$Typ_ad)
model.log.akt <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
                      s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                      s$VaR15.nor + s$VaR100.nor + s$VaR200.nor + 
                      s$Wiek.nor + s$Typ_ad)
model.typ <- lm(s$Op³aty.bie¿¹ce.nor ~ s$rr16.nor + s$sr16.nor + s$sd16.nor + s$TER.nor + 
                  s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                  s$VaR15.nor + s$VaR100.nor + s$VaR200.nor + 
                  s$Wiek.nor + s$Log_akt.nor)

sse_rr16 <- sum((model.rr16$residuals)^2)
sse_sr16 <- sum((model.sr16$residuals)^2)
sse_sd16 <- sum((model.sd16$residuals)^2)
sse_TER <- sum((model.TER$residuals)^2)
sse_Pier.wp <- sum((model.Pier.wp$residuals)^2)
sse_zmiana.akt <- sum((model.zmiana.akt$residuals)^2)
sse_VaR15 <- sum((model.VaR15$residuals)^2)
sse_VaR100 <- sum((model.VaR100$residuals)^2)
sse_VaR200 <- sum((model.VaR200$residuals)^2)
sse_wiek <- sum((model.Wiek$residuals)^2)
sse_log.akt <- sum((model.log.akt$residuals)^2)
sse_typ <- sum((model.typ$residuals)^2)

((sse_rr16 - sse_f)/r)/a
((sse_sr16 - sse_f)/r)/a
((sse_sd16 - sse_f)/r)/a
((sse_TER - sse_f)/r)/a
((sse_Pier.wp - sse_f)/r)/a
((sse_zmiana.akt - sse_f)/r)/a
((sse_VaR15 - sse_f)/r)/a
((sse_VaR100 - sse_f)/r)/a
((sse_VaR200 - sse_f)/r)/a
((sse_wiek - sse_f)/r)/a
((sse_log.akt - sse_f)/r)/a
((sse_typ - sse_f)/r)/a

# Eliminujemy rr16

model.sr16 <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR15.nor + s$VaR100.nor + s$VaR200.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.sd16 <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR15.nor + s$VaR100.nor + s$VaR200.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.TER <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor +
                  s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                  s$VaR15.nor + s$VaR100.nor + s$VaR200.nor + 
                  s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.Pier.wp <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                      s$zmiana_akt_flow_16_perc.nor + 
                      s$VaR15.nor + s$VaR100.nor + s$VaR200.nor + 
                      s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.zmiana.akt <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                         s$Pierwsza.wp³ata.nor + 
                         s$VaR15.nor + s$VaR100.nor + s$VaR200.nor + 
                         s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.VaR15 <-lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR100.nor + s$VaR200.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.VaR100 <-lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                    s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                    s$VaR15.nor + s$VaR200.nor + 
                    s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.VaR200 <-lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                    s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                    s$VaR15.nor + s$VaR100.nor + 
                    s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.Wiek <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR15.nor + s$VaR100.nor + s$VaR200.nor + 
                   s$Log_akt.nor + s$Typ_ad)
model.log.akt <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                      s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                      s$VaR15.nor + s$VaR100.nor + s$VaR200.nor + 
                      s$Wiek.nor + s$Typ_ad)
model.typ <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                  s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                  s$VaR15.nor + s$VaR100.nor + s$VaR200.nor + 
                  s$Wiek.nor + s$Log_akt.nor)

sse_sr16 <- sum((model.sr16$residuals)^2)
sse_sd16 <- sum((model.sd16$residuals)^2)
sse_TER <- sum((model.TER$residuals)^2)
sse_Pier.wp <- sum((model.Pier.wp$residuals)^2)
sse_zmiana.akt <- sum((model.zmiana.akt$residuals)^2)
sse_VaR15 <- sum((model.VaR15$residuals)^2)
sse_VaR100 <- sum((model.VaR100$residuals)^2)
sse_VaR200 <- sum((model.VaR200$residuals)^2)
sse_wiek <- sum((model.Wiek$residuals)^2)
sse_log.akt <- sum((model.log.akt$residuals)^2)
sse_typ <- sum((model.typ$residuals)^2)

((sse_sr16 - sse_f)/r)/a
((sse_sd16 - sse_f)/r)/a
((sse_TER - sse_f)/r)/a
((sse_Pier.wp - sse_f)/r)/a
((sse_zmiana.akt - sse_f)/r)/a
((sse_VaR15 - sse_f)/r)/a
((sse_VaR100 - sse_f)/r)/a
((sse_VaR200 - sse_f)/r)/a
((sse_wiek - sse_f)/r)/a
((sse_log.akt - sse_f)/r)/a
((sse_typ - sse_f)/r)/a

# Eliminujemy VaR15

model.sr16 <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR100.nor + s$VaR200.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.sd16 <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR100.nor + s$VaR200.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.TER <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor +
                  s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                  s$VaR100.nor + s$VaR200.nor + 
                  s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.Pier.wp <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                      s$zmiana_akt_flow_16_perc.nor + 
                      s$VaR100.nor + s$VaR200.nor + 
                      s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.zmiana.akt <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                         s$Pierwsza.wp³ata.nor + 
                         s$VaR100.nor + s$VaR200.nor + 
                         s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.VaR100 <-lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                    s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                    s$VaR200.nor + 
                    s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.VaR200 <-lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                    s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                    s$VaR100.nor + 
                    s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.Wiek <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR100.nor + s$VaR200.nor + 
                   s$Log_akt.nor + s$Typ_ad)
model.log.akt <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                      s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                      s$VaR100.nor + s$VaR200.nor + 
                      s$Wiek.nor + s$Typ_ad)
model.typ <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                  s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                  s$VaR100.nor + s$VaR200.nor + 
                  s$Wiek.nor + s$Log_akt.nor)

sse_sr16 <- sum((model.sr16$residuals)^2)
sse_sd16 <- sum((model.sd16$residuals)^2)
sse_TER <- sum((model.TER$residuals)^2)
sse_Pier.wp <- sum((model.Pier.wp$residuals)^2)
sse_zmiana.akt <- sum((model.zmiana.akt$residuals)^2)
sse_VaR100 <- sum((model.VaR100$residuals)^2)
sse_VaR200 <- sum((model.VaR200$residuals)^2)
sse_wiek <- sum((model.Wiek$residuals)^2)
sse_log.akt <- sum((model.log.akt$residuals)^2)
sse_typ <- sum((model.typ$residuals)^2)

((sse_sr16 - sse_f)/r)/a
((sse_sd16 - sse_f)/r)/a
((sse_TER - sse_f)/r)/a
((sse_Pier.wp - sse_f)/r)/a
((sse_zmiana.akt - sse_f)/r)/a
((sse_VaR100 - sse_f)/r)/a
((sse_VaR200 - sse_f)/r)/a
((sse_wiek - sse_f)/r)/a
((sse_log.akt - sse_f)/r)/a
((sse_typ - sse_f)/r)/a

# Eliminujemy VaR200

model.sr16 <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR100.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.sd16 <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR100.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.TER <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor +
                  s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                  s$VaR100.nor + 
                  s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.Pier.wp <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                      s$zmiana_akt_flow_16_perc.nor + 
                      s$VaR100.nor + 
                      s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.zmiana.akt <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                         s$Pierwsza.wp³ata.nor + 
                         s$VaR100.nor + 
                         s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.VaR100 <-lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                    s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                    s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.Wiek <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$VaR100.nor +
                   s$Log_akt.nor + s$Typ_ad)
model.log.akt <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                      s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                      s$VaR100.nor + 
                      s$Wiek.nor + s$Typ_ad)
model.typ <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                  s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                  s$VaR100.nor + 
                  s$Wiek.nor + s$Log_akt.nor)

sse_sr16 <- sum((model.sr16$residuals)^2)
sse_sd16 <- sum((model.sd16$residuals)^2)
sse_TER <- sum((model.TER$residuals)^2)
sse_Pier.wp <- sum((model.Pier.wp$residuals)^2)
sse_zmiana.akt <- sum((model.zmiana.akt$residuals)^2)
sse_VaR100 <- sum((model.VaR100$residuals)^2)
sse_wiek <- sum((model.Wiek$residuals)^2)
sse_log.akt <- sum((model.log.akt$residuals)^2)
sse_typ <- sum((model.typ$residuals)^2)

((sse_sr16 - sse_f)/r)/a
((sse_sd16 - sse_f)/r)/a
((sse_TER - sse_f)/r)/a
((sse_Pier.wp - sse_f)/r)/a
((sse_zmiana.akt - sse_f)/r)/a
((sse_VaR100 - sse_f)/r)/a
((sse_wiek - sse_f)/r)/a
((sse_log.akt - sse_f)/r)/a
((sse_typ - sse_f)/r)/a
qf(0.05, 9, 355)

# Eliminujemy VaR100

model.sr16 <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.sd16 <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.TER <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor +
                  s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                  s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.Pier.wp <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                      s$zmiana_akt_flow_16_perc.nor + 
                      s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.zmiana.akt <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                         s$Pierwsza.wp³ata.nor + 
                         s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.Wiek <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                   s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                   s$Log_akt.nor + s$Typ_ad)
model.log.akt <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                      s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                      s$Wiek.nor + s$Typ_ad)
model.typ <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                  s$Pierwsza.wp³ata.nor + s$zmiana_akt_flow_16_perc.nor + 
                  s$Wiek.nor + s$Log_akt.nor)

sse_sr16 <- sum((model.sr16$residuals)^2)
sse_sd16 <- sum((model.sd16$residuals)^2)
sse_TER <- sum((model.TER$residuals)^2)
sse_Pier.wp <- sum((model.Pier.wp$residuals)^2)
sse_zmiana.akt <- sum((model.zmiana.akt$residuals)^2)
sse_wiek <- sum((model.Wiek$residuals)^2)
sse_log.akt <- sum((model.log.akt$residuals)^2)
sse_typ <- sum((model.typ$residuals)^2)

((sse_sr16 - sse_f)/r)/a
((sse_sd16 - sse_f)/r)/a
((sse_TER - sse_f)/r)/a
((sse_Pier.wp - sse_f)/r)/a
((sse_zmiana.akt - sse_f)/r)/a
((sse_wiek - sse_f)/r)/a
((sse_log.akt - sse_f)/r)/a
((sse_typ - sse_f)/r)/a
qf(0.05, 8, 355)

# Eliminujemy Pierwsz¹ wp³atê

model.sr16 <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sd16.nor + s$TER.nor + 
                   s$zmiana_akt_flow_16_perc.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.sd16 <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$TER.nor + 
                   s$zmiana_akt_flow_16_perc.nor + 
                   s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.TER <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor +
                  s$zmiana_akt_flow_16_perc.nor + 
                  s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.zmiana.akt <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                         s$Wiek.nor + s$Log_akt.nor + s$Typ_ad)
model.Wiek <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                   s$zmiana_akt_flow_16_perc.nor + 
                   s$Log_akt.nor + s$Typ_ad)
model.log.akt <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                      s$zmiana_akt_flow_16_perc.nor + 
                      s$Wiek.nor + s$Typ_ad)
model.typ <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
                  s$zmiana_akt_flow_16_perc.nor + 
                  s$Wiek.nor + s$Log_akt.nor)

sse_sr16 <- sum((model.sr16$residuals)^2)
sse_sd16 <- sum((model.sd16$residuals)^2)
sse_TER <- sum((model.TER$residuals)^2)
sse_zmiana.akt <- sum((model.zmiana.akt$residuals)^2)
sse_wiek <- sum((model.Wiek$residuals)^2)
sse_log.akt <- sum((model.log.akt$residuals)^2)
sse_typ <- sum((model.typ$residuals)^2)

((sse_sr16 - sse_f)/r)/a
((sse_sd16 - sse_f)/r)/a
((sse_TER - sse_f)/r)/a
((sse_zmiana.akt - sse_f)/r)/a
((sse_wiek - sse_f)/r)/a
((sse_log.akt - sse_f)/r)/a
((sse_typ - sse_f)/r)/a
qf(0.05, 7, 355)

model <- lm(s$Op³aty.bie¿¹ce.nor ~ s$sr16.nor + s$sd16.nor + s$TER.nor + 
              s$zmiana_akt_flow_16_perc.nor + s$Wiek.nor + s$Log_akt.nor + 
              s$Typ_ad)


options(scipen = 100)
summary(model)