library(reshape2)

akt <- z[, c(3, 11, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 31, 32)]
akt_m <- melt(akt, c('Nazwa.funduszu', 'Typ'))
ggplot(data = akt_m, aes(x = variable, y = value)) +
  geom_point(aes(colour = Typ))


akt_proc <- z[, c(3, 11, 44, 46, 48, 50, 52, 54, 56, 58, 60, 62, 64, 66)]
akt_proc_m <- melt(akt_proc, c('Nazwa.funduszu', 'Typ'))
ggplot(data = akt_proc_m, aes(x = variable, y = value)) +
  geom_boxplot(aes(colour = Typ)) +
  ylim(c(0, 25))
