setwd("C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Wstêpna analiza")
source("Log_stopy_zwrotu.R")
source("2_Merge_feesdata_navizfa.R")

rr16 <- y[grep("2016", y$Date),]
rr_max <- rr16[rr16$Date == max(rr16$Date),]
rr_min <- rr16[rr16$Date == min(rr16$Date),]
rr16_r <- (rr_max - rr_min)/rr_min

c <- merge(c, t(rr16_r), by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
colnames(c)[length(colnames(c))] <- c("rr16_r")
c$rr16_r <- as.numeric(levels(c$rr16_r)[c$rr16_r])
c$zmiana_akt_zwrot_16 <- c$Aktywa_2015 * c$rr16_r
c$zmiana_akt_flow_16 <- c$zmiana_akt_val_2016 - c$zmiana_akt_zwrot_16
c$zmiana_akt_flow_16_perc <- c$zmiana_akt_flow_16 / c$Aktywa_2015




