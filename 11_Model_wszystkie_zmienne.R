setwd("C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Wstêpna analiza")
source("6_Wzrost_aktywów_stopa_przep³ywy.R")

# VaR

setwd("C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Wstêpna analiza")

x <- as.data.frame(read.csv("Baza_danych_aktywa_netto.csv", sep = ";", header = F), stringsAsFactors = F)
x[2,1] <- "Date"

y <- x[c(1, 2, 4405:6191),]
y[,1] <- as.Date(y[,1], "%Y-%m-%d")
colnames(y) <- apply(y[1,], 2, as.character)
colnames(y)[1] <- "Date"
y <- y[-(1:2),]
y[,2:1183] <- apply(y[,2:1183], 2, as.numeric)
yl <- y
yl[,2:1183] <- apply(y[,2:1183], 2, log)
ylr <- yl
ylr[2:1787,2:1183] <- apply(yl[,2:1183], 2, diff)
ylr <- ylr[-1, ]

ylr16 <- ylr[grep("2016", ylr$Date), ]
ylr16$weekday <- weekdays(ylr16$Date)

for (j in 2:(dim(ylr16)[2] - 1)) {
  for (i in 1:dim(ylr16)[1]){
    if (ylr16$weekday[i] == "poniedzia³ek"){
      ylr16[i,j] <- sum(ylr16[ylr16$Date >= ylr16$Date[i] & ylr16$Date <= ylr16$Date[i]+6,j])
    } else {
      ylr16[i, j] <- NA
    }
  }
}

# VaR 10%

v <- log(1.1)/52
ylr16_w <- ylr16[ylr16$weekday == "poniedzia³ek",]
j <- dim(ylr16_w)[1]

for (i in 2:dim(ylr16_w)[2]){
  ylr16_w[j+1, i] <- sum(ylr16_w[1:49,i] >= v)/dim(ylr16_w)[1]
}

ylr16_wt <- transpose(ylr16_w)
colnames(ylr16_wt) <- ylr16_w$Date
rownames(ylr16_wt) <- colnames(ylr16_w)
ylr16_wt <- ylr16_wt[-1, dim(ylr16_wt)[2], drop = F]

r <- merge(c, ylr16_wt, by.x = 'Nazwa.funduszu', by.y = 0, all.x = T)
colnames(r)[73] <- 'VaR10'
r$VaR10 <- as.numeric(r$VaR10)


# VaR 15%

v <- log(1.15)/52
ylr16_w <- ylr16[ylr16$weekday == "poniedzia³ek",]
j <- dim(ylr16_w)[1]

for (i in 2:dim(ylr16_w)[2]){
  ylr16_w[j+1, i] <- sum(ylr16_w[1:49,i] >= v)/dim(ylr16_w)[1]
}

ylr16_wt <- transpose(ylr16_w)
colnames(ylr16_wt) <- ylr16_w$Date
rownames(ylr16_wt) <- colnames(ylr16_w)
ylr16_wt <- ylr16_wt[-1, dim(ylr16_wt)[2], drop = F]

r <- merge(r, ylr16_wt, by.x = 'Nazwa.funduszu', by.y = 0, all.x = T)
colnames(r)[74] <- 'VaR15'
r$VaR15 <- as.numeric(r$VaR15)


# VaR 20%

v <- log(1.20)/52
ylr16_w <- ylr16[ylr16$weekday == "poniedzia³ek",]
j <- dim(ylr16_w)[1]

for (i in 2:dim(ylr16_w)[2]){
  ylr16_w[j+1, i] <- sum(ylr16_w[1:49,i] >= v)/dim(ylr16_w)[1]
}

ylr16_wt <- transpose(ylr16_w)
colnames(ylr16_wt) <- ylr16_w$Date
rownames(ylr16_wt) <- colnames(ylr16_w)
ylr16_wt <- ylr16_wt[-1, dim(ylr16_wt)[2], drop = F]

r <- merge(r, ylr16_wt, by.x = 'Nazwa.funduszu', by.y = 0, all.x = T)
colnames(r)[75] <- 'VaR20'
r$VaR20 <- as.numeric(r$VaR20)

# VaR 100%

v <- log(2)/52
ylr16_w <- ylr16[ylr16$weekday == "poniedzia³ek",]
j <- dim(ylr16_w)[1]

for (i in 2:dim(ylr16_w)[2]){
  ylr16_w[j+1, i] <- sum(ylr16_w[1:49,i] >= v)/dim(ylr16_w)[1]
}

ylr16_wt <- transpose(ylr16_w)
colnames(ylr16_wt) <- ylr16_w$Date
rownames(ylr16_wt) <- colnames(ylr16_w)
ylr16_wt <- ylr16_wt[-1, dim(ylr16_wt)[2], drop = F]

r <- merge(r, ylr16_wt, by.x = 'Nazwa.funduszu', by.y = 0, all.x = T)
colnames(r)[76] <- 'VaR100'
r$VaR100 <- as.numeric(r$VaR100)

