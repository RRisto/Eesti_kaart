require("rgdal")
require("maptools")
require("ggplot2")
require("plyr")
require("XLConnect")

#loeme sisse Eesti kaardi:
eesti= readOGR(dsn="//mts.local/dfs$/profiilid/risto.hinno/My Documents/R working directory", layer="maakond_20140701")

#teeme ta ggplotile s��davaks, kasutades id-na maakonnanime muutujat (MNIMI neis andmeis, teiste andmetega tuleb see �ra muuta millekski muuks).
eesti@data$id = eesti@data$MNIMI
eesti.points = fortify(eesti, region="id")
eesti.df = join(eesti.points, eesti@data, by="id")

#kuna R ei suuda t�pit�hti normaalselt tuvastada 
#(ja selle alusel siduda ning t�pit�htedega maakondade kohale j��vad t�himikud), 
#tuleb muuta j�rgnevates tabelite veergudes encoding UTF-8 peale
eesti.df$MNIMI <- iconv(eesti.df$MNIMI, "UTF-8")
eesti.df$id <- iconv(eesti.df$id, "UTF-8")
eesti.points$id <- iconv(eesti.points$id, "UTF-8")

#loeme sisse exceli tabeli, mis asub working directorys:
wb <- loadWorkbook("rahvaarv_maakond.xlsx", create = FALSE)
df1 <- readWorksheet(wb, sheet = "Leht1")

#olgu meil mingi andmefail, kusjuures maakonna nimi on
#siis identne sellega, kuis ta kaardiandmetes MNIMI muutujas kirjutet
#encoding tuleb UTF-8 peale muuta, et suudaks siduda:
osav�tt<-data.frame(maakond= iconv(df1$maakond, "UTF-8"), osakaal = df1$rahvaarv)

#teeme vahemikud, et oleks ilus kuvada
osav�tt$osakaal.vahemik<-cut(osav�tt$osakaal, breaks=c(0, 20000, 50000, 100000, 200000, 600000), labels=c("0-20 000","20 000-50 000", "50 000-100 000","100 000-200 000", "200 000-600 000"))

#Kuvame iga maakonna peal ta osakaalu. Esmalt on vaja teada koordinaate, kuhu numbreid kuvada. 
#Leiame maakondade tsentroidid. iconv aitab tagada, et ka t�pit�htedega maakondade nr kuvatakse 
#kui �ra j�ttas, siis ei kuva t�pit�htedega maakonna peal numbrit
maakondadetsentroidid = as.data.frame(gCentroid(eesti,byid=TRUE))
maakondadetsentroidid$maakond<-iconv(eesti$MNIMI, "UTF-8")
osav�tt<-join(osav�tt, maakondadetsentroidid)

#ja graafik ise:
p<-ggplot(osav�tt, aes(fill=osakaal.vahemik))+ #�tleme talle, et andmed on tabelis osav�tt ja et v�rv valitaks l�htuvalt oskaal.vahemikust
  geom_map(aes(map_id=maakond), map=eesti.df, color="black")+ #�tleme, et kaart tuleb tabelist eesti.df, osav�tuandmestikuga seob seda maakonna-muutuja ja maakonnapiirid tehtagu mustaks
  expand_limits(x=eesti.df$long, y=eesti.df$lat)+ #see on geom_map()'i jaoks vajalik mingil p�hjusel
  coord_fixed()+#hoiame oma riigi k�rguse ja laiuse paigas
  #  geom_map(aes(map_id=maakond), map=tlntrt.df, color="black")+
  geom_text(aes(label=osakaal,x=x, y=y), data=osav�tt)+ #see osa kuvab kaardil igal KOVil tema n�itaja numbri
  theme(axis.title=element_blank(), axis.text=element_blank(),line=element_blank(), panel.background=element_blank())+ #eemaldame k�ik ebavajaliku graafikult, veame legendi alla
  ggtitle("Elanike arv ")+ #paneme juurde nime
  scale_fill_brewer("Elanike arv maakondade kaupa", palette = 1) +#veidi kenamad v�rvid kui vaikimisiantud
  annotate("text", x = 700000, y = 6370000, label = c("kaart: Maa-amet, 01.07.2014"), size=3)

ggsave(p, file="Eestikov.png", height=2, width=3, scale=3)

#lisame Tallinna ja Tartu:
#loeme omavalitsused sisse
eestiov = readOGR(dsn="//mts.local/dfs$/profiilid/risto.hinno/My Documents/R working directory", layer="omavalitsus_20140701")

#teeme ta ggplotile s��davaks, kasutades id-na maakonnanime muutujat (MNIMI neis andmeis, teiste andmetega tuleb see �ra muuta millekski muuks).
eestiov@data$id = eestiov@data$ONIMI
eestiov.points = fortify(eestiov, region="id")
eestiov.df = join(eestiov.points, eestiov@data, by="id")

#j�tame alles �ksnes Tallinna ja Tartu
tlntrt.df<-subset(eestiov.df, id%in%c("Tallinna linn", "Tartu linn"))

#leiame nende asukoha (tegelikult peaks leidma nt loodenurga, praegu leiab tsentroidi)
tlntrttsentroidid = as.data.frame(gCentroid(eestiov,byid=TRUE))
tlntrttsentroidid$maakond<-eestiov$ONIMI
tlntrttsentroidid<-subset(tlntrttsentroidid, maakond%in%c("Tallinna linn", "Tartu linn"))

#lisame need andmed osav�tt-andmetabelisse
osav�tt$x[osav�tt$maakond=="Tallinna linn"]<-tlntrttsentroidid$x[tlntrttsentroidid$maakond=="Tallinna linn"]
osav�tt$y[osav�tt$maakond=="Tallinna linn"]<-tlntrttsentroidid$y[tlntrttsentroidid$maakond=="Tallinna linn"]
osav�tt$x[osav�tt$maakond=="Tartu linn"]<-tlntrttsentroidid$x[tlntrttsentroidid$maakond=="Tartu linn"]
osav�tt$y[osav�tt$maakond=="Tartu linn"]<-tlntrttsentroidid$y[tlntrttsentroidid$maakond=="Tartu linn"]

#muudame Tallinna linna ja Tartu linna Tallinnaks ja Tartuks
osav�tt$maakond<-revalue(osav�tt$maakond, replace=c("Tallinna linn"="Tallinn", "Tartu linn"="Tartu"))
tlntrt.df$id<-revalue(tlntrt.df$id, replace=c("Tallinna linn"="Tallinn", "Tartu linn"="Tartu"))

#ja teeme graafiku
p<-ggplot(osav�tt, aes(fill=osakaal.vahemik))+ #�tleme talle, et andmed on tabelis osav�tt ja et v�rv valitaks l�htuvalt oskaal.vahemikust
  geom_map(aes(map_id=maakond), map=eesti.df, color="grey")+ #�tleme, et kaart tuleb tabelist eesti.df, osav�tuandmestikuga seob seda maakonna-muutuja ja maakonnapiirid tehtagu mustaks
  geom_map(aes(map_id=maakond), map=tlntrt.df, color="black")+ #lisame Tallinna ja Tartu
  expand_limits(x=eesti.df$long, y=eesti.df$lat)+ #see on geom_map()'i jaoks vajalik mingil p�hjusel
  coord_fixed()+#hoiame oma riigi k�rguse ja laiuse paigas
  geom_text(aes(label=maakond,x=x-5000, y=y+5000), data=subset(osav�tt, maakond%in%c("Tallinn", "Tartu")), hjust=1, vjust=0)+
  theme(axis.title=element_blank(), axis.text=element_blank(),line=element_blank(), panel.background=element_blank())+ #eemaldame k�ik ebavajaliku graafikult
  ggtitle("Elanike arv")+ #paneme juurde nime
  scale_fill_brewer("Elanike arv", palette = 1) +#veidi kenamad v�rvid kui vaikimisiantud
  annotate("text", x = 700000, y = 6370000, label = c("kaart: Maa-amet, 01.07.2014"), size=3, color="grey")

ggsave(p, file="Eestikov2.png", height=2, width=3, scale=3)
