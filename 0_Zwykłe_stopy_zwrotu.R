setwd("C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Wstêpna analiza")

x <- as.data.frame(read.csv("Baza_danych_analizy.csv", sep = ";", header = F), stringsAsFactors = F)
x[2,1] <- "Date"

y <- x[c(1, 2, 4405:6191),]
y[,1] <- as.Date(y[,1], "%Y-%m-%d")
colnames(y) <- apply(y[1,], 2, as.character)
colnames(y)[1] <- "Date"
y <- y[-(1:2),]
y[,2:1183] <- apply(y[,2:1183], 2, as.numeric)

# 2016

d_2016 <- y[format(y[,1], "%Y") == "2016",1]
d1_16 <- match(max(d_2016, na.rm = T), y$Date)
d2_16 <- match(min(d_2016, na.rm = T), y$Date)
rr16 <- apply(y, 2, function(x) ((as.numeric(x[d1_16])-as.numeric(x[d2_16]))*100/as.numeric(x[d2_16])))
rr16 <- as.data.frame(rr16)
sd16 <- apply(y, 2, function(x) sd(as.numeric(x), na.rm = T))
sd16 <- as.data.frame(sd16)
sr16 <- (rr16 - 2.5)/sd16

z <- read.csv("Baza danych.csv", sep = ";", header = T)
z <- merge(z, rr16, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
z <- merge(z, sd16, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
z <- merge(z, sr16, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
colnames(z)[11] <- "rr16"
colnames(z)[13] <- "sr16"


# 2015

d_2015 <- y[format(y[,1], "%Y") == "2015",1]
d1_15 <- match(max(d_2015, na.rm = T), y$Date)
d2_15 <- match(min(d_2015, na.rm = T), y$Date)
rr15 <- apply(y, 2, function(x) ((as.numeric(x[d1_15])-as.numeric(x[d2_15]))*100/as.numeric(x[d2_15])))
sd15 <- apply(y, 2, function(x) sd(as.numeric(x), na.rm = T))
sr15 <- (rr15 - 2.5)/sd15

z <- merge(z, rr15, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
z <- merge(z, sd15, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
z <- merge(z, sr15, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
colnames(z)[14:16] <- c("rr15", "sd15", "sr15")


# 2014

d_2014 <- y[format(y[,1], "%Y") == "2014",1]
d1_14 <- match(max(d_2014, na.rm = T), y$Date)
d2_14 <- match(min(d_2014, na.rm = T), y$Date)
rr14 <- apply(y, 2, function(x) ((as.numeric(x[d1_14])-as.numeric(x[d2_14]))*100/as.numeric(x[d2_14])))
sd14 <- apply(y, 2, function(x) sd(as.numeric(x), na.rm = T))
sr14 <- (rr14 - 2.5)/sd14

z <- merge(z, rr14, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
z <- merge(z, sd14, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
z <- merge(z, sr14, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
colnames(z)[17:19] <- c("rr14", "sd14", "sr14")


# 2013

d_2013 <- y[format(y[,1], "%Y") == "2013",1]
d1_13 <- match(max(d_2013, na.rm = T), y$Date)
d2_13 <- match(min(d_2013, na.rm = T), y$Date)
rr13 <- apply(y, 2, function(x) ((as.numeric(x[d1_13])-as.numeric(x[d2_13]))*100/as.numeric(x[d2_13])))
sd13 <- apply(y, 2, function(x) sd(as.numeric(x), na.rm = T))
sr13 <- (rr13 - 2.5)/sd13

z <- merge(z, rr13, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
z <- merge(z, sd13, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
z <- merge(z, sr13, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
colnames(z)[20:22] <- c("rr13", "sd13", "sr13")


# 2012

d_2012 <- y[format(y[,1], "%Y") == "2012",1]
d1_12 <- match(max(d_2012, na.rm = T), y$Date)
d2_12 <- match(min(d_2012, na.rm = T), y$Date)
rr12 <- apply(y, 2, function(x) ((as.numeric(x[d1_12])-as.numeric(x[d2_12]))*100/as.numeric(x[d2_12])))
sd12 <- apply(y, 2, function(x) sd(as.numeric(x), na.rm = T))
sr12 <- (rr12 - 2.5)/sd12

z <- merge(z, rr12, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
z <- merge(z, sd12, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
z <- merge(z, sr12, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
colnames(z)[23:25] <- c("rr12", "sd12", "sr12")

write.csv(z, "0_Zwyk³e_stopy_zwrotu.csv")
