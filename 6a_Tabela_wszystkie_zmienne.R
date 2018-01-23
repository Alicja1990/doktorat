setwd("C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Wstêpna analiza")
source("6_Wzrost_aktywów_stopa_przep³ywy.R")

c$Log_akt <- log(as.numeric(as.character(c$Aktywa)))
c$Wiek <- 2017 - c$Rok.uruchomienia

dim(c)
names(c)

