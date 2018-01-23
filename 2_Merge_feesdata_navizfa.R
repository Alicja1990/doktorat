setwd("C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Wstêpna analiza")
c <- read.csv("0_Log_stopy_zwrotu.csv", row.names = 1, as.is = T)

setwd("C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Wstêpna analiza/IZFA aktywa baza na koniec roku")
csv <- dir(path = "C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Wstêpna analiza/IZFA aktywa baza na koniec roku", pattern = "csv")

for (i in 1:length(csv)) {

  w <- read.csv(csv[i], sep = ",", header = T)
  w$DateMast <- as.Date(w$DateMast, format = "%Y-%m-%d")
  w <- subset(w, w$DateMast == max(w$DateMast))
  w <- w[, c("Fund_Name", "Amount")]
  w$Amount <- as.character(w$Amount)
  w$Amount <- gsub(' ', '', w$Amount)
  w$Amount <- as.numeric(w$Amount)
  
  c <- merge(x=c, y=w, by.x = 'Nazwa.IZFiA', by.y = 'Fund_Name', all.x = T)
  names(c)[names(c) == "Amount"] <- paste("Aktywa_", substr(csv[i], nchar(csv[i])-7, nchar(csv[i]) - 4), sep = "")
  
}

for (i in 2006:2017) {
  for (j in dim(c)[1]) {
    if (c[j, paste("Aktywa_", i-1, sep = "")] == 0 || is.na(c[j, paste("Aktywa_", i-1, sep = "")])) {
      c[j, paste("zmiana_akt_perc_", i, sep = "")] <- NaN
    } 
  }
  c[, paste("zmiana_akt_perc_", i, sep = "")] <- (c[, paste("Aktywa_", i, sep = "")] - c[, paste("Aktywa_", i-1, sep = "")]) / c[, paste("Aktywa_", i-1, sep = "")]
  c[, paste("zmiana_akt_val_", i, sep = "")] <- (c[, paste("Aktywa_", i, sep = "")] - c[, paste("Aktywa_", i-1, sep = "")])
}

setwd("C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Wstêpna analiza")
write.csv(c, "2_Stopy_fees_aktywa_i_zmiany_aktywów.csv")
