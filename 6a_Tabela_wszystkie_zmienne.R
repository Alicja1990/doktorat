setwd("C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Wst�pna analiza")
source("6_Wzrost_aktyw�w_stopa_przep�ywy.R")

c$Log_akt <- log(as.numeric(as.character(c$Aktywa)))
c$Wiek <- 2017 - c$Rok.uruchomienia

dim(c)
names(c)

