setwd("C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Wstêpna analiza")
source("6_Wzrost_aktywów_stopa_przep³ywy.R")

b <- b[!b$Nazwa.IZFiA == "",]
c <- c[!c$Nazwa.IZFiA == "",]
f <- merge(x = b, y = c[, c(2, 32:72)], all.x = T, by.x = 'Nazwa.IZFiA', by.y = 'Nazwa.IZFiA')

f$Log_akt <- log(f$Aktywa)
f$Wiek <- 2017 - f$Rok.uruchomienia
model <- lm(f$Op³aty.bie¿¹ce ~ f$zmiana_akt_flow_16_perc + f$rr16_r + f$Log_akt + f$sr16 + f$sr15 + f$Typ + f$Wiek + f$TER + f$Op³ata.za.nabycie)

sse_f <- sum((model$residuals)^2)
n <- dim(b)[[1]]
k <- 9
r <- 8
a <- sse_f/(n-k-1)

model_rr16 <- lm(f$Op³aty.bie¿¹ce ~ f$zmiana_akt_flow_16_perc + f$Log_akt + f$sr16 + f$sr15 + f$Typ + f$Wiek + f$TER + f$Op³ata.za.nabycie)
model_zaf16 <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$Log_akt + f$sr16 + f$sr15 + f$Typ + f$Wiek + f$TER + f$Op³ata.za.nabycie)
model_akt <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$sr16 + f$sr15 + f$Typ + f$Wiek + f$TER + f$Op³ata.za.nabycie)
model_sr16 <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Log_akt + f$sr15 + f$Typ + f$Wiek + f$TER + f$Op³ata.za.nabycie)
model_sr15 <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Log_akt + f$sr16 + f$Typ + f$Wiek + f$TER + f$Op³ata.za.nabycie)
model_typ <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Log_akt + f$sr16 + f$sr15 + f$Wiek + f$TER + f$Op³ata.za.nabycie)
model_wiek <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Log_akt + f$sr16 + f$sr15 + f$Typ + f$TER + f$Op³ata.za.nabycie)
model_ter <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Log_akt + f$sr16 + f$sr15 + f$Typ + f$Wiek + f$Op³ata.za.nabycie)
model_ozn <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Log_akt + f$sr16 + f$sr15 + f$Typ + f$Wiek + f$TER)

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

model_rr16 <- lm(f$Op³aty.bie¿¹ce ~ f$zmiana_akt_flow_16_perc + f$Log_akt + f$sr16 + f$Typ + f$Wiek + f$TER + f$Op³ata.za.nabycie)
model_zaf16 <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$Log_akt + f$sr16 + f$Typ + f$Wiek + f$TER + f$Op³ata.za.nabycie)
model_akt <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$sr16 + f$Typ + f$Wiek + f$TER + f$Op³ata.za.nabycie)
model_sr16 <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Log_akt + f$Typ + f$Wiek + f$TER + f$Op³ata.za.nabycie)
model_typ <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Log_akt + f$sr16 + f$Wiek + f$TER + f$Op³ata.za.nabycie)
model_wiek <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Log_akt + f$sr16 + f$Typ + f$TER + f$Op³ata.za.nabycie)
model_ter <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Log_akt + f$sr16 + f$Typ + f$Wiek + f$Op³ata.za.nabycie)
model_ozn <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Log_akt + f$sr16 + f$Typ + f$Wiek + f$TER)

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

model_rr16 <- lm(f$Op³aty.bie¿¹ce ~ f$zmiana_akt_flow_16_perc + f$Log_akt + f$Typ + f$Wiek + f$TER + f$Op³ata.za.nabycie)
model_zaf16 <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$Log_akt + f$Typ + f$Wiek + f$TER + f$Op³ata.za.nabycie)
model_akt <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Typ + f$Wiek + f$TER + f$Op³ata.za.nabycie)
model_typ <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Log_akt + f$Wiek + f$TER + f$Op³ata.za.nabycie)
model_wiek <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Log_akt + f$Typ + f$TER + f$Op³ata.za.nabycie)
model_ter <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Log_akt + f$Typ + f$Wiek + f$Op³ata.za.nabycie)
model_ozn <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Log_akt + f$Typ + f$Wiek + f$TER)

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

# Eliminujemy Wiek

model_rr16 <- lm(f$Op³aty.bie¿¹ce ~  f$zmiana_akt_flow_16_perc + f$Log_akt + f$Typ + f$TER + f$Op³ata.za.nabycie)
model_zaf16 <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$Log_akt + f$Typ + f$TER + f$Op³ata.za.nabycie)
model_akt <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Typ + f$TER + f$Op³ata.za.nabycie)
model_typ <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Log_akt + f$TER + f$Op³ata.za.nabycie)
model_ter <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Log_akt + f$Typ + f$Op³ata.za.nabycie)
model_ozn <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Log_akt + f$Typ + f$TER)

sse_rr16 <- sum((model_rr16$residuals)^2)
sse_zaf16 <- sum((model_zaf16$residuals)^2)
sse_akt <- sum((model_akt$residuals)^2)
sse_typ <- sum((model_typ$residuals)^2)
sse_ter <- sum((model_ter$residuals)^2)
sse_ozn <- sum((model_ozn$residuals)^2)

((sse_rr16 - sse_f)/r)/a
((sse_zaf16 - sse_f)/r)/a
((sse_akt - sse_f)/r)/a
((sse_typ - sse_f)/r)/a
((sse_ter - sse_f)/r)/a
((sse_ozn - sse_f)/r)/a

model <- lm(f$Op³aty.bie¿¹ce ~ f$zmiana_akt_flow_16_perc + f$rr16_r + f$Log_akt + f$Typ + f$TER + f$Op³ata.za.nabycie)