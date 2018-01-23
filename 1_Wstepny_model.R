setwd("C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Wstêpna analiza")
z <- read.csv("0_Log_stopy_zwrotu.csv", row.names = 1)

b <- z[z$Typ == "akcji" | z$Typ == "mieszane" | z$Typ == "d³u¿ne",]

b$Log_akt <- log(b$Aktywa)
b$Wiek <- 2017 - b$Rok.uruchomienia
model <- lm(b$Op³aty.bie¿¹ce ~ b$Log_akt + b$sr16 + b$sr15 + b$Typ + b$Rok.uruchomienia + b$TER + b$Op³ata.za.nabycie)

sse_f <- sum((model$residuals)^2)
n <- dim(b)[[1]]
k <- 7
r <- 6
a <- sse_f/(n-k-1)

model_log <- lm(b$Op³aty.bie¿¹ce ~ b$sr16 + b$sr15 + b$Typ + b$Wiek + b$TER + b$Op³ata.za.nabycie)
model_sr16 <- lm(b$Op³aty.bie¿¹ce ~ b$Log_akt + b$sr15 + b$Typ + b$Wiek + b$TER + b$Op³ata.za.nabycie)
model_sr15 <- lm(b$Op³aty.bie¿¹ce ~ b$Log_akt + b$sr16 + b$Typ + b$Wiek + b$TER + b$Op³ata.za.nabycie)
model_typ <- lm(b$Op³aty.bie¿¹ce ~ b$Log_akt + b$sr16 + b$sr15 + b$Wiek + b$TER + b$Op³ata.za.nabycie)
model_wiek <- lm(b$Op³aty.bie¿¹ce ~ b$Log_akt + b$sr16 + b$sr15 + b$Typ + b$TER + b$Op³ata.za.nabycie)
model_ter <- lm(b$Op³aty.bie¿¹ce ~ b$Log_akt + b$sr16 + b$sr15 + b$Typ + b$Wiek + b$Op³ata.za.nabycie)
model_ozn <- lm(b$Op³aty.bie¿¹ce ~ b$Log_akt + b$sr16 + b$sr15 + b$Typ + b$Wiek + b$TER)

sse_log <- sum((model_log$residuals)^2)
sse_sr16 <- sum((model_sr16$residuals)^2)
sse_sr15 <- sum((model_sr15$residuals)^2)
sse_typ <- sum((model_typ$residuals)^2)
sse_wiek <- sum((model_wiek$residuals)^2)
sse_ter <- sum((model_ter$residuals)^2)
sse_ozn <- sum((model_ozn$residuals)^2)

((sse_log - sse_f)/r)/a
((sse_sr16 - sse_f)/r)/a
((sse_sr15 - sse_f)/r)/a
((sse_typ - sse_f)/r)/a
((sse_wiek - sse_f)/r)/a
((sse_ter - sse_f)/r)/a
((sse_ozn - sse_f)/r)/a

# Eliminujemy sr16

model_log <- lm(b$Op³aty.bie¿¹ce ~ b$sr15 + b$Typ + b$Wiek + b$TER + b$Op³ata.za.nabycie)
model_sr15 <- lm(b$Op³aty.bie¿¹ce ~ b$Log_akt + b$Typ + b$Wiek + b$TER + b$Op³ata.za.nabycie)
model_typ <- lm(b$Op³aty.bie¿¹ce ~ b$Log_akt + b$sr15 + b$Wiek + b$TER + b$Op³ata.za.nabycie)
model_wiek <- lm(b$Op³aty.bie¿¹ce ~ b$Log_akt + b$sr15 + b$Typ + b$TER + b$Op³ata.za.nabycie)
model_ter <- lm(b$Op³aty.bie¿¹ce ~ b$Log_akt + b$sr15 + b$Typ + b$Wiek + b$Op³ata.za.nabycie)
model_ozn <- lm(b$Op³aty.bie¿¹ce ~ b$Log_akt + b$sr15 + b$Typ + b$Wiek + b$TER)

sse_log <- sum((model_log$residuals)^2)
sse_sr15 <- sum((model_sr15$residuals)^2)
sse_typ <- sum((model_typ$residuals)^2)
sse_wiek <- sum((model_wiek$residuals)^2)
sse_ter <- sum((model_ter$residuals)^2)
sse_ozn <- sum((model_ozn$residuals)^2)

((sse_log - sse_f)/r)/a
((sse_sr15 - sse_f)/r)/a
((sse_typ - sse_f)/r)/a
((sse_wiek - sse_f)/r)/a
((sse_ter - sse_f)/r)/a
((sse_ozn - sse_f)/r)/a

# Eliminujemy zmienn¹ Wiek

model_log <- lm(b$Op³aty.bie¿¹ce ~ b$sr15 + b$Typ + b$TER + b$Op³ata.za.nabycie)
model_sr15 <- lm(b$Op³aty.bie¿¹ce ~ b$Log_akt + b$Typ + b$TER + b$Op³ata.za.nabycie)
model_typ <- lm(b$Op³aty.bie¿¹ce ~ b$Log_akt + b$sr15 + b$TER + b$Op³ata.za.nabycie)
model_ter <- lm(b$Op³aty.bie¿¹ce ~ b$Log_akt + b$sr15 + b$Typ + b$Op³ata.za.nabycie)
model_ozn <- lm(b$Op³aty.bie¿¹ce ~ b$Log_akt + b$sr15 + b$Typ + b$TER)

sse_log <- sum((model_log$residuals)^2)
sse_sr15 <- sum((model_sr15$residuals)^2)
sse_typ <- sum((model_typ$residuals)^2)
sse_ter <- sum((model_ter$residuals)^2)
sse_ozn <- sum((model_ozn$residuals)^2)

((sse_log - sse_f)/r)/a
((sse_sr15 - sse_f)/r)/a
((sse_typ - sse_f)/r)/a
((sse_ter - sse_f)/r)/a
((sse_ozn - sse_f)/r)/a

model <- lm(b$Op³aty.bie¿¹ce ~ b$Log_akt + b$sr15 + b$Typ + b$TER + b$Op³ata.za.nabycie)

model <- lm(b$Op³aty.bie¿¹ce ~ b$Log_akt + b$Typ + b$TER + b$Op³ata.za.nabycie)
for (i in 1:dim(b)[1]) {
  if (b$Typ[i] == "d³u¿ne") {
    b$D³u¿ne[i] <- 1
  } else {
    b$D³u¿ne[i] <- 0
  }
}

model <- lm(b$Op³aty.bie¿¹ce ~ b$Log_akt + b$D³u¿ne + b$TER + b$Op³ata.za.nabycie)
