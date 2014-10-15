require("rgdal")
require("maptools")
require("ggplot2")
require("plyr")
require("XLConnect")

#kuna mul on working direcotrile viitamisega jama, siis määran selle ka:
setwd("//mts.local/dfs$/profiilid/risto.hinno/My Documents/R working directory")
#loeme sisse Eesti kaardi:
eesti= readOGR(dsn="//mts.local/dfs$/profiilid/risto.hinno/My Documents/R working directory", layer="omavalitsus_20140701")

#teeme ta ggplotile söödavaks, kasutades id-na maakonnanime muutujat (ONIMI neis andmeis, teiste andmetega tuleb see ära muuta millekski muuks).
eesti@data$id = eesti@data$ONIMI
eesti.points = fortify(eesti, region="id")
eesti.df = join(eesti.points, eesti@data, by="id")

#kuna R ei suuda täpitähti normaalselt tuvastada 
#(ja selle alusel siduda ning täpitähtedega maakondade kohale jäävad tühimikud), 
#tuleb muuta järgnevates tabelite veergudes encoding UTF-8 peale
eesti.df$ONIMI <- iconv(eesti.df$ONIMI, "UTF-8")
eesti.df$id <- iconv(eesti.df$id, "UTF-8")
eesti.points$id <- iconv(eesti.points$id, "UTF-8")

# loeme sisse exceli tabeli, mis asub working directorys:
wb <- loadWorkbook("rahvaarv_KOV.xlsx", create = FALSE)
df1 <- readWorksheet(wb, sheet = "Leht1")

#olgu meil mingi andmefail, kusjuures maakonna nimi on
#siis identne sellega, kuis ta kaardiandmetes ONIMI muutujas kirjutet
#encoding tuleb UTF-8 peale muuta, et suudaks siduda:
osavõtt<-data.frame(omavalitsus= iconv(df1$omavalitsus, "UTF-8"), osakaal = df1$rahvaarv)

#teeme vahemikud, et oleks ilus kuvada
osavõtt$osakaal.vahemik<-cut(osavõtt$osakaal, breaks=c(0, 1000, 3000, 10000, 20000, 500000), labels=c("0-1 000","1 000-3 000", "3 000-10 000", "10 000-20 000", "20 000-500 000"))

#Kuvame iga maakonna peal ta osakaalu. Esmalt on vaja teada koordinaate, kuhu numbreid kuvada. 
#Leiame maakondade tsentroidid. iconv aitab tagada, et ka täpitähtedega maakondade nr kuvatakse 
#kui ära jättas, siis ei kuva täpitähtedega maakonna peal numbrit
#kuna praegu omavalitsuste peal ei kuva arve, siis pole seda vaja
#omavalitsustetsentroidid = as.data.frame(gCentroid(eesti,byid=TRUE))
#omavalitsustetsentroidid$omavalitsus<-iconv(eesti$ONIMI, "UTF-8")
#osavõtt<-join(osavõtt, omavalitsustetsentroidid)

#ja graafik ise:
p<-ggplot(osavõtt, aes(fill=osakaal.vahemik))+ #Ã¼tleme talle, et andmed on tabelis osavÃµtt ja et vÃ¤rv valitaks lÃ¤htuvalt oskaal.vahemikust
  geom_map(aes(map_id=omavalitsus), map=eesti.df, color="black")+ #Ã¼tleme, et kaart tuleb tabelist eesti.df, osavÃµtuandmestikuga seob seda maakonna-muutuja ja maakonnapiirid tehtagu mustaks
  expand_limits(x=eesti.df$long, y=eesti.df$lat)+ #see on geom_map()'i jaoks vajalik mingil pÃµhjusel
  coord_fixed()+#hoiame oma riigi kÃµrguse ja laiuse paigas
  #  geom_map(aes(map_id=maakond), map=tlntrt.df, color="black")+
  #geom_text(aes(label=osakaal,x=x, y=y), data=osavõtt)+ #see osa kuvab iga maakonna numbri
  theme(axis.title=element_blank(), axis.text=element_blank(),line=element_blank(), panel.background=element_blank())+ #eemaldame kÃµik ebavajaliku graafikult, veame legendi alla
  ggtitle("Elanike arv ")+ #paneme juurde nime
  scale_fill_brewer("Elanike arv maakondade kaupa", palette = 1) +#veidi kenamad vÃ¤rvid kui vaikimisiantud, võib kirjutada ka värvinime (mitmuses)
  #nt palette = 1 , palette = "Blues", palette = "Set1"
  annotate("text", x = 700000, y = 6370000, label = c("kaart: Maa-amet, 01.07.2014"), size=3)

ggsave(p, file="Eestivallad.png", height=2, width=3, scale=3)