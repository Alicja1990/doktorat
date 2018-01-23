s$Typ_ad <- s$Typ

levels(s$Typ_ad)[levels(s$Typ_ad) == 'akcji'] <- "akcjopodobne"
levels(s$Typ_ad)[levels(s$Typ_ad) == 'absolutnej stopy zwrotu'] <- "akcjopodobne"
levels(s$Typ_ad)[levels(s$Typ_ad) == 'mieszane'] <- "akcjopodobne"
levels(s$Typ_ad)[levels(s$Typ_ad) == 'd³u¿ne'] <- "d³u¿nopodobne"
levels(s$Typ_ad)[levels(s$Typ_ad) == 'pieniê¿ne'] <- "d³u¿nopodobne"

s <- s[s$Typ_ad == "akcjopodobne" | s$Typ_ad == "d³u¿nopodobne",]

                   
                   
