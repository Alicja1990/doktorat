setwd("C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Wstêpna analiza")
source("6_Wzrost_aktywów_stopa_przep³ywy.R")

f <- merge(x = b, y = c[, c(2, 32:72)], all.x = T, by.x = 'Nazwa.IZFiA', by.y = 'Nazwa.IZFiA')

f$Log_akt <- log(f$Aktywa)
f$Wiek <- 2017 - f$Rok.uruchomienia

nor <- f[,c(1, 5, 8, 9, 10, 11, 12, 22, 23, 32:37, 52, 53, 85:95)]

for (i in c(3, 4, 5, 7:23)) {
  for (j in 1:dim(nor)[1]){
    nor[j, i] <- (nor[j, i] - mean(nor[, i], na.rm = T)) / sd(nor [,i], na.rm = T)
  }
}

model <- lm(nor$Op³aty.bie¿¹ce ~ nor$zmiana_akt_flow_16_perc + nor$rr16_r + nor$Log_akt + nor$sr16 + nor$sr15 + nor$Typ + nor$Wiek + nor$TER + nor$Op³ata.za.nabycie)

sse_f <- sum((model$residuals)^2)
n <- dim(nor)[[1]]
k <- 9
r <- 8
a <- sse_f/(n-k-1)

model_rr16 <- lm(nor$Op³aty.bie¿¹ce ~ nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$sr16 + nor$sr15 + nor$Typ + nor$Wiek + nor$TER + nor$Op³ata.za.nabycie)
model_zaf16 <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$Log_akt + nor$sr16 + nor$sr15 + nor$Typ + nor$Wiek + nor$TER + nor$Op³ata.za.nabycie)
model_akt <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$zmiana_akt_flow_16_perc + nor$sr16 + nor$sr15 + nor$Typ + nor$Wiek + nor$TER + nor$Op³ata.za.nabycie)
model_sr16 <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$sr15 + nor$Typ + nor$Wiek + nor$TER + nor$Op³ata.za.nabycie)
model_typ <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$sr16 + nor$sr15 + nor$Wiek + nor$TER + nor$Op³ata.za.nabycie)
model_wiek <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$sr16 + nor$sr15 + nor$Typ + nor$TER + nor$Op³ata.za.nabycie)
model_ter <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$sr16 + nor$sr15 + nor$Typ + nor$Wiek + nor$Op³ata.za.nabycie)
model_ozn <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$sr16 + nor$sr15 + nor$Typ + nor$Wiek + nor$TER)
model_sr15 <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$sr16 + nor$Typ + nor$Wiek + nor$TER + nor$Op³ata.za.nabycie)

sse_rr16 <- sum((model_rr16$residuals)^2)
sse_zaf16 <- sum((model_zaf16$residuals)^2)
sse_akt <- sum((model_akt$residuals)^2)
sse_sr16 <- sum((model_sr16$residuals)^2)
sse_sr15 <- sum((model_sr15$residuals)^2)
sse_typ <- sum((model_typ$residuals)^2)
sse_wiek <- sum((model_wiek$residuals)^2)
sse_ter <- sum((model_ter$residuals)^2)
sse_ozn <- sum((model_ozn$residuals)^2)

((sse_rr16 - sse_f)/r)/a
((sse_zaf16 - sse_f)/r)/a
((sse_akt - sse_f)/r)/a
((sse_sr16 - sse_f)/r)/a
((sse_sr15 - sse_f)/r)/a
((sse_typ - sse_f)/r)/a
((sse_wiek - sse_f)/r)/a
((sse_ter - sse_f)/r)/a
((sse_ozn - sse_f)/r)/a

# Eliminujemy sr15

model_rr16 <- lm(nor$Op³aty.bie¿¹ce ~ nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$sr16 + nor$Typ + nor$Wiek + nor$TER + nor$Op³ata.za.nabycie)
model_zaf16 <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$Log_akt + nor$sr16 + nor$Typ + nor$Wiek + nor$TER + nor$Op³ata.za.nabycie)
model_akt <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$zmiana_akt_flow_16_perc + nor$sr16 + nor$Typ + nor$Wiek + nor$TER + nor$Op³ata.za.nabycie)
model_sr16 <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$Typ + nor$Wiek + nor$TER + nor$Op³ata.za.nabycie)
model_typ <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$sr16 + nor$Wiek + nor$TER + nor$Op³ata.za.nabycie)
model_wiek <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$sr16 + nor$Typ + nor$TER + nor$Op³ata.za.nabycie)
model_ter <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$sr16 + nor$Typ + nor$Wiek + nor$Op³ata.za.nabycie)
model_ozn <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$sr16 + nor$Typ + nor$Wiek + nor$TER)

sse_rr16 <- sum((model_rr16$residuals)^2)
sse_zaf16 <- sum((model_zaf16$residuals)^2)
sse_akt <- sum((model_akt$residuals)^2)
sse_sr16 <- sum((model_sr16$residuals)^2)
sse_typ <- sum((model_typ$residuals)^2)
sse_wiek <- sum((model_wiek$residuals)^2)
sse_ter <- sum((model_ter$residuals)^2)
sse_ozn <- sum((model_ozn$residuals)^2)

((sse_rr16 - sse_f)/r)/a
((sse_zaf16 - sse_f)/r)/a
((sse_akt - sse_f)/r)/a
((sse_sr16 - sse_f)/r)/a
((sse_typ - sse_f)/r)/a
((sse_wiek - sse_f)/r)/a
((sse_ter - sse_f)/r)/a
((sse_ozn - sse_f)/r)/a

# Eliminujemy sr16

model_rr16 <- lm(nor$Op³aty.bie¿¹ce ~ nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$Typ + nor$Wiek + nor$TER + nor$Op³ata.za.nabycie)
model_zaf16 <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$Log_akt + nor$Typ + nor$Wiek + nor$TER + nor$Op³ata.za.nabycie)
model_akt <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$zmiana_akt_flow_16_perc + nor$Typ + nor$Wiek + nor$TER + nor$Op³ata.za.nabycie)
model_typ <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$Wiek + nor$TER + nor$Op³ata.za.nabycie)
model_wiek <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$Typ + nor$TER + nor$Op³ata.za.nabycie)
model_ter <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$Typ + nor$Wiek + nor$Op³ata.za.nabycie)
model_ozn <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$Typ + nor$Wiek + nor$TER)

sse_rr16 <- sum((model_rr16$residuals)^2)
sse_zaf16 <- sum((model_zaf16$residuals)^2)
sse_akt <- sum((model_akt$residuals)^2)
sse_typ <- sum((model_typ$residuals)^2)
sse_wiek <- sum((model_wiek$residuals)^2)
sse_ter <- sum((model_ter$residuals)^2)
sse_ozn <- sum((model_ozn$residuals)^2)

((sse_rr16 - sse_f)/r)/a
((sse_zaf16 - sse_f)/r)/a
((sse_akt - sse_f)/r)/a
((sse_typ - sse_f)/r)/a
((sse_wiek - sse_f)/r)/a
((sse_ter - sse_f)/r)/a
((sse_ozn - sse_f)/r)/a


# Eliminujemy Typ

model_rr16 <- lm(nor$Op³aty.bie¿¹ce ~ nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$Wiek + nor$TER + nor$Op³ata.za.nabycie)
model_zaf16 <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$Log_akt + nor$Wiek + nor$TER + nor$Op³ata.za.nabycie)
model_akt <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$zmiana_akt_flow_16_perc + nor$Wiek + nor$TER + nor$Op³ata.za.nabycie)
model_wiek <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$TER + nor$Op³ata.za.nabycie)
model_ter <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$Wiek + nor$Op³ata.za.nabycie)
model_ozn <- lm(nor$Op³aty.bie¿¹ce ~ nor$rr16_r + nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$Wiek + nor$TER)

sse_rr16 <- sum((model_rr16$residuals)^2)
sse_zaf16 <- sum((model_zaf16$residuals)^2)
sse_akt <- sum((model_akt$residuals)^2)
sse_wiek <- sum((model_wiek$residuals)^2)
sse_ter <- sum((model_ter$residuals)^2)
sse_ozn <- sum((model_ozn$residuals)^2)

((sse_rr16 - sse_f)/r)/a
((sse_zaf16 - sse_f)/r)/a
((sse_akt - sse_f)/r)/a
((sse_wiek - sse_f)/r)/a
((sse_ter - sse_f)/r)/a
((sse_ozn - sse_f)/r)/a


# Eliminujemy rr16

model_zaf16 <- lm(nor$Op³aty.bie¿¹ce ~ nor$Log_akt + nor$Wiek + nor$TER + nor$Op³ata.za.nabycie)
model_akt <- lm(nor$Op³aty.bie¿¹ce ~ nor$zmiana_akt_flow_16_perc + nor$Wiek + nor$TER + nor$Op³ata.za.nabycie)
model_wiek <- lm(nor$Op³aty.bie¿¹ce ~ nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$TER + nor$Op³ata.za.nabycie)
model_ter <- lm(nor$Op³aty.bie¿¹ce ~ nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$Wiek + nor$Op³ata.za.nabycie)
model_ozn <- lm(nor$Op³aty.bie¿¹ce ~ nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$Wiek + nor$TER)

sse_zaf16 <- sum((model_zaf16$residuals)^2)
sse_akt <- sum((model_akt$residuals)^2)
sse_wiek <- sum((model_wiek$residuals)^2)
sse_ter <- sum((model_ter$residuals)^2)
sse_ozn <- sum((model_ozn$residuals)^2)

((sse_zaf16 - sse_f)/r)/a
((sse_akt - sse_f)/r)/a
((sse_wiek - sse_f)/r)/a
((sse_ter - sse_f)/r)/a
((sse_ozn - sse_f)/r)/a

model <- lm(nor$Op³aty.bie¿¹ce ~ nor$zmiana_akt_flow_16_perc + nor$Log_akt + nor$Wiek + nor$TER + nor$Op³ata.za.nabycie)