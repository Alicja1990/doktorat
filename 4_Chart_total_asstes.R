setwd("C:/Users/Alicja/Dropbox/Doktorat/Praca doktorska/Wstêpna analiza")
library(reshape2)
library(ggplot2)
z <- read.csv("2_Stopy_fees_aktywa_i_zmiany_aktywów.csv", row.names = 1, as.is = T)

akt <- z[, c('Nazwa.funduszu', 'Typ', 'Aktywa_2005', 'Aktywa_2006', 'Aktywa_2007', 'Aktywa_2008',             
             'Aktywa_2009', 'Aktywa_2010', 'Aktywa_2011', 'Aktywa_2012', 'Aktywa_2013',
             'Aktywa_2014', 'Aktywa_2015', 'Aktywa_2016', 'Aktywa_2017')]
akt_m <- melt(akt, c('Nazwa.funduszu', 'Typ'))
options(scipen = 100)
ggplot(data = akt_m, aes(x = variable, y = value)) +
  geom_point(aes(colour = Typ))


akt_proc <- z[, c('Nazwa.funduszu', 'Typ', "zmiana_akt_perc_2006", "zmiana_akt_perc_2007", "zmiana_akt_perc_2008",               
                  "zmiana_akt_perc_2009", "zmiana_akt_perc_2010", "zmiana_akt_perc_2011",       
                  "zmiana_akt_perc_2012", "zmiana_akt_perc_2013", "zmiana_akt_perc_2014",               
                  "zmiana_akt_perc_2015", "zmiana_akt_perc_2016", "zmiana_akt_perc_2017")]
akt_proc_m <- melt(akt_proc, c('Nazwa.funduszu', 'Typ'))
ggplot(data = akt_proc_m, aes(x = variable, y = value)) +
  geom_boxplot(aes(colour = Typ)) +
  ylim(c(0, 25))
