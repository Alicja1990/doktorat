xls <- dir(path = "C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Wstêpna analiza/IZFA aktywa baza na koniec roku", pattern = "xls")
setwd("C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Wstêpna analiza/IZFA aktywa baza na koniec roku")

library(readxl)
for (i in 1:length(xls)) {
  A <- read_excel(xls[i], sheet = 'RawData')
  write.csv(A, file=paste(substr(xls[i], start = 1, nchar(xls[i]) - 4), "csv", sep="."), row.names=FALSE)
}

