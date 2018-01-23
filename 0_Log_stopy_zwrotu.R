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


# 2016

rr16 <- apply(ylr[format(ylr[,1], "%Y") == "2016",2:1183], 2, mean, na.rm = TRUE)
sd16 <- apply(ylr[format(ylr[,1], "%Y") == "2016",2:1183], 2, sd, na.rm = TRUE)
sr16 <- (rr16 - (log(1.025)/365))/sd16

z <- read.csv("Baza danych_oplaty.csv", sep = ";", header = T)
z$Nazwa.funduszu <- as.character(z$Nazwa.funduszu)
z$Forma.prawna <- as.character(z$Forma.prawna)
z$Rok.uruchomienia <- as.integer(z$Rok.uruchomienia)
z$Udzia³.akcji <- as.numeric(as.character(z$Udzia³.akcji))
z$Op³ata.za.zarz¹dzanie <- as.numeric(z$Op³ata.za.zarz¹dzanie)
z$TER <- as.numeric(as.character(z$TER))
z$Op³aty.bie¿¹ce <- as.numeric(z$Op³aty.bie¿¹ce)
z$Typ <- as.character(z$Typ)
z$Pierwsza.wp³ata <- as.integer(z$Pierwsza.wp³ata)
z$Nastêpna.wp³ata <- as.integer(z$Nastêpna.wp³ata)
z$Aktywa <- as.numeric(as.character(z$Aktywa))
z$Op³ata.za.nabycie <- as.numeric(as.character(z$Op³ata.za.nabycie))
z$Nazwa.IZFiA <- as.character(z$Nazwa.IZFiA)


z <- merge(z, rr16, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
z <- merge(z, sd16, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
z <- merge(z, sr16, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
colnames(z)[(length(colnames(z)) - 2):length(colnames(z))] <- c("rr16", "sd16", "sr16")


# 2015

rr15 <- apply(ylr[format(ylr[,1], "%Y") == "2015",2:1183], 2, mean, na.rm = TRUE)
sd15 <- apply(ylr[format(ylr[,1], "%Y") == "2015",2:1183], 2, sd, na.rm = TRUE)
sr15 <- (rr15 - (log(1.025))/365)/sd15

z <- merge(z, rr15, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
z <- merge(z, sd15, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
z <- merge(z, sr15, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
colnames(z)[(length(colnames(z)) - 2):length(colnames(z))] <- c("rr15", "sd15", "sr15")

# 2014

rr14 <- apply(ylr[format(ylr[,1], "%Y") == "2014",2:1183], 2, mean, na.rm = TRUE)
sd14 <- apply(ylr[format(ylr[,1], "%Y") == "2014",2:1183], 2, sd, na.rm = TRUE)
sr14 <- (rr14 - (log(1.025))/365)/sd14

z <- merge(z, rr14, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
z <- merge(z, sd14, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
z <- merge(z, sr14, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
colnames(z)[(length(colnames(z)) - 2):length(colnames(z))] <- c("rr14", "sd14", "sr14")

# 2013

rr13 <- apply(ylr[format(ylr[,1], "%Y") == "2013",2:1183], 2, mean, na.rm = TRUE)
sd13 <- apply(ylr[format(ylr[,1], "%Y") == "2013",2:1183], 2, sd, na.rm = TRUE)
sr13 <- (rr13 - (log(1.025)-1)/365)/sd13

z <- merge(z, rr13, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
z <- merge(z, sd13, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
z <- merge(z, sr13, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
colnames(z)[(length(colnames(z)) - 2):length(colnames(z))] <- c("rr13", "sd13", "sr13")

# 2012

rr12 <- apply(ylr[format(ylr[,1], "%Y") == "2012",2:1183], 2, mean, na.rm = TRUE)
sd12 <- apply(ylr[format(ylr[,1], "%Y") == "2012",2:1183], 2, sd, na.rm = TRUE)
sr12 <- (rr12 - (log(1.025))/365)/sd12

z <- merge(z, rr12, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
z <- merge(z, sd12, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
z <- merge(z, sr12, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
colnames(z)[(length(colnames(z)) - 2):length(colnames(z))] <- c("rr12", "sd12", "sr12")


# 2014/6

rr14.6 <- apply(ylr[format(ylr[,1], "%Y") == "2014" | format(ylr[,1], "%Y") == "2015" | format(ylr[,1], "%Y") == "2016", 2:1183], 2, mean, na.rm = TRUE)
sd14.6 <- apply(ylr[format(ylr[,1], "%Y") == "2014" | format(ylr[,1], "%Y") == "2015" | format(ylr[,1], "%Y") == "2016", 2:1183], 2, sd, na.rm = TRUE)
sr14.6 <- (rr14.6 - (log(1.025)/365))/sd14.6

z <- merge(z, rr14.6, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
z <- merge(z, sd14.6, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
z <- merge(z, sr14.6, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
colnames(z)[(length(colnames(z)) - 2):length(colnames(z))] <- c("rr14.6", "sd14.6", "sr14.6")

# 2012/6

rr12.6 <- apply(ylr[format(ylr[,1], "%Y") == "2012" | format(ylr[,1], "%Y") == "2013" | format(ylr[,1], "%Y") == "2014" | format(ylr[,1], "%Y") == "2015" | format(ylr[,1], "%Y") == "2016", 2:1183], 2, mean, na.rm = TRUE)
sd12.6 <- apply(ylr[format(ylr[,1], "%Y") == "2012" | format(ylr[,1], "%Y") == "2013" | format(ylr[,1], "%Y") == "2014" | format(ylr[,1], "%Y") == "2015" | format(ylr[,1], "%Y") == "2016", 2:1183], 2, sd, na.rm = TRUE)
sr12.6 <- (rr12.6 - (log(1.025)/365))/sd12.6

z <- merge(z, rr12.6, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
z <- merge(z, sd12.6, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
z <- merge(z, sr12.6, by.x = "Nazwa.funduszu", by.y = 0, all.x = T)
colnames(z)[(length(colnames(z)) - 2):length(colnames(z))] <- c("rr12.6", "sd12.6", "sr12.6")

a <- z[z$Typ == "akcji",]
m <- z[z$Typ == "mieszane",]
d <- z[z$Typ == "d³u¿ne",]
p <- z[z$Typ == "pieniê¿ne",]
ab <- z[z$Typ == "absolutnej stopy zwrotu",]

b <- z[z$Typ == "akcji" | z$Typ == "mieszane" | z$Typ == "d³u¿ne",]

write.csv(z, "0_Log_stopy_zwrotu.csv")