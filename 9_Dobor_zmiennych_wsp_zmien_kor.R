# Wsp. zmienno�� wysoki dla wszystkich zmiennych, ale metoda eliminacji korelacj� eliminuje za ma�o zmiennych

setwd("C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Wst�pna analiza")
source("6_Wzrost_aktyw�w_stopa_przep�ywy.R")

f <- merge(x = z, y = c[, c(2, 32:72)], all.x = T, by.x = 'Nazwa.IZFiA', by.y = 'Nazwa.IZFiA')

f$Log_akt <- log(f$Aktywa)
f$Wiek <- 2017 - f$Rok.uruchomienia
g <- data.frame()

f <- f[,c('Nazwa.IZFiA', 'Nazwa.funduszu', 'Op�aty.bie��ce', 'Op�ata.za.zarz�dzanie', 'TER', 
'Pierwsza.wp�ata', 'Op�ata.za.nabycie', 'rr16', 'rr15', 'sd15', 'sd16', 'sr15', 'sr16', 'rr14.6', 
'sd14.6', 'sr14.6', 32, 33, 34, 35, 36, 46, 47, 48, 64, 52, 53, 83, 85, 87, 90, 91, 92, 93, 94)]

f <- subset(f, select = c(Nazwa.funduszu, Nazwa.IZFiA, Op�aty.bie��ce, Op�ata.za.zarz�dzanie, TER, 
                          Pierwsza.wp�ata, Op�ata.za.nabycie, rr16:sr15, rr14.6, sd14.6, sr14.6, 
                          Aktywa_2015, Aktywa_2016, Aktywa_2017, zmiana_akt_val_2016:Wiek))

for (i in c(3:28)) {
    g[i,1] <- sd(f[,i], na.rm = T) / mean(f[,i], na.rm = T)
}

cor(f[,3:28], use = "pairwise.complete.obs")

tkryt <- qt(0.05, 1154, lower.tail = F)
rkryt <- (tkryt^2/(1154 - 2 + tkryt^2))^0.5

# Eliminujemy rr15, rr16, sr16, sr15, zmiana_akt_perc_15 i 16, zmiana_akt_zwrot_16, zmiana_akt_flow_16_perc

f <- f[,c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 14, 16:21, 24, 26, 28, 29)]

cor(f[,3:21], use = "pairwise.complete.obs")

# Eliminujemy zmienne najsilniej skorelowane z najsilnij skorelowan�

f <- f[,c(1:9, 12:21)]

cor(f[,3:19], use = "pairwise.complete.obs")

f <- f[,c(1:6, 8:19)]
cor(f[,3:18], use = "pairwise.complete.obs")

model <- lm(f$Op�aty.bie��ce ~ f$Udzia�.akcji + f$Op�ata.za.zarz�dzanie + f$TER + 
              f$Op�ata.za.nabycie + f$rr16_r + f$rr14.6 + f$sd14.6 + f$sr14.6 + 
              f$Aktywa_2015 + f$Aktywa_2016 + f$Aktywa_2016 + f$Aktywa_2017 + 
              f$zmiana_akt_perc_2017 + f$zmiana_akt_flow_16 + f$Log_akt + f$Wiek)
