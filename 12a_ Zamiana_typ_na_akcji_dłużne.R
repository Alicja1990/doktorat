s$Typ_ad <- s$Typ

levels(s$Typ_ad)[levels(s$Typ_ad) == 'akcji'] <- "akcjopodobne"
levels(s$Typ_ad)[levels(s$Typ_ad) == 'absolutnej stopy zwrotu'] <- "akcjopodobne"
levels(s$Typ_ad)[levels(s$Typ_ad) == 'mieszane'] <- "akcjopodobne"
levels(s$Typ_ad)[levels(s$Typ_ad) == 'dłużne'] <- "dłużnopodobne"
levels(s$Typ_ad)[levels(s$Typ_ad) == 'pieniężne'] <- "dłużnopodobne"

s <- s[s$Typ_ad == "akcjopodobne" | s$Typ_ad == "dłużnopodobne",]

                   
                   
