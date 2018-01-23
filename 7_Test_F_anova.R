setwd("C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Wstêpna analiza")
source("6_Wzrost_aktywów_stopa_przep³ywy.R")


f <- merge(x = b, y = c[, c(2, 31:71)], all.x = T, by.x = 'Nazwa.IZFiA', by.y = 'Nazwa.IZFiA')

f$Log_akt <- log(f$Aktywa)
f$Wiek <- 2017 - f$Rok.uruchomienia
f <- f[!is.na(f$rr16_r) & !is.na(f$zmiana_akt_flow_16_perc) & !is.na(f$Aktywa_2016) 
       & !is.na(f$sr15) & !is.na(f$sr16) & !is.na(f$Typ) & !is.na(f$Wiek) & !is.na(f$TER),]
model <- lm(f$Op³aty.bie¿¹ce ~ f$zmiana_akt_flow_16_perc + f$rr16_r + f$Log_akt + f$sr16 + f$sr15 + f$Typ + f$Wiek + f$TER + f$Op³ata.za.nabycie)

model_rr16 <- lm(f$Op³aty.bie¿¹ce ~ f$zmiana_akt_flow_16_perc + f$Log_akt + f$sr16 + f$sr15 + f$Typ + f$Wiek + f$TER + f$Op³ata.za.nabycie)
model_zaf16 <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$Log_akt + f$sr16 + f$sr15 + f$Typ + f$Wiek + f$TER + f$Op³ata.za.nabycie)
model_akt <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$sr16 + f$sr15 + f$Typ + f$Wiek + f$TER + f$Op³ata.za.nabycie)
model_sr16 <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Log_akt + f$sr15 + f$Typ + f$Wiek + f$TER + f$Op³ata.za.nabycie)
model_sr15 <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Log_akt + f$sr16 + f$Typ + f$Wiek + f$TER + f$Op³ata.za.nabycie)
model_typ <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Log_akt + f$sr16 + f$sr15 + f$Wiek + f$TER + f$Op³ata.za.nabycie)
model_wiek <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Log_akt + f$sr16 + f$sr15 + f$Typ + f$TER + f$Op³ata.za.nabycie)
model_ter <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Log_akt + f$sr16 + f$sr15 + f$Typ + f$Wiek + f$Op³ata.za.nabycie)
model_ozn <- lm(f$Op³aty.bie¿¹ce ~ f$rr16_r + f$zmiana_akt_flow_16_perc + f$Log_akt + f$sr16 + f$sr15 + f$Typ + f$Wiek + f$TER)


anova(model, model_rr16)[[6]]
anova(model, model_zaf16)[[6]]
anova(model, model_akt)[[6]]
anova(model, model_sr15)[[6]]
anova(model, model_sr16)[[6]]
anova(model, model_typ)[[6]]
anova(model, model_wiek)[[6]]
anova(model, model_ter)[[6]]
anova(model, model_ozn)[[6]]

anova(model, model_rr16, model_zaf16, model_akt, model_sr15, model_sr16, model_typ, model_wiek, model_ter, model_ozn)