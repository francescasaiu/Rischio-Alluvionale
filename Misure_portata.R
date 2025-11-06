#############################################################
# Elaborazioni relative all'identificazione di una scala di #
# deflusso a partire da diverse campagne di misura          #
#############################################################

# Carico i dati di portata
# (4 diverse campagne di misura, accuratezza Q: dev. std. 25%)
Dati<-read.csv(file = "Misure_portata.csv")
# h.mis: tirante in m
# Q.mis: corrispondente portata in m^3/s

# Costruisco la scala di deflusso con il primo set di dati (mis1)
# 1 - ripetere la costruzione per gli altri set
# 2 - confrontare le scale di deflusso ottenute e le corrispondenti stima di Q(h=2m))
h.mis<-Dati$h.mis1
Q.mis<-Dati$Q.mis1
# # Esempio con il secondo set di dati (mis2)
# h.mis<-Dati$h.mis2
# Q.mis<-Dati$Q.mis2

# Scala di deflusso: regressione lineare tra le trasformate logaritmiche
# Uso la funzione di R "lm"
Sc.Def<-lm(formula=log(Q.mis)~log(h.mis),na.action=na.exclude)
summary(Sc.Def) # coefficienti e statistiche di interesse
# Intervallo di confidenza della scala di deflusso
# Traccio le fasce per tiranti arbitrari da 0.1m a 2m con passo 0.1m
# la funzione "predict" richiede in ingresso una variabile "data.frame"
Tiranti<-data.frame(h.mis=seq(0.1,2,.1))
Confidenza<-signif(exp(predict(Sc.Def,Tiranti,interval="confidence")),digits = 4)
# Salvo la scala di deflusso e fasce di confidenza in un file txt
# per utilizzi successivi (ad es. importazione in excel)
Sc.Def.mis1<-Tiranti
Sc.Def.mis1[,2:4]<-Confidenza
colnames(Sc.Def.mis1)<-c("h (m)","Q(m^3/s)","Int.Conf-","Int.Conf+")
write.table(Sc.Def.mis1,file = "ScalaDef_mis1.txt",sep = ";",dec = ".",row.names = F)
# ATTENZIONE AL NOME DEL FILE, RIFERIMENTO ESPLICITO A "mis1" 

# Rappresento i risultati (scala naturale)
plot(Q.mis,h.mis,
     xlim=c(0.1,240),ylim=c(0.01,2),
     xlab=expression(paste("Portata (",m^3/s,")",sep="")),
     ylab="Tirante (m)",
     main="Misure di portata e scala di deflusso",
     type="p",col="red")
lines(Confidenza[,1],Tiranti[,],col="blue",lwd=2)
lines(Confidenza[,2],Tiranti[,],col="red",lty = "dashed")
lines(Confidenza[,3],Tiranti[,],col="red",lty = "dashed")

# Da cui la portata corrispondente ad un tirante di 2m risulta
Q.2m<-signif(Confidenza[which(Tiranti$h.mis==2),],digits = 4)
Q.2m # valori di portata
Q.2m/Q.2m[1] # rapporto tra stima e estremi dell'intervallo di confidenza

# Rappresento i risultati (scala logaritmica)
plot(Q.mis,h.mis,log="xy",
     xlim=c(0.05,240),ylim=c(0.01,2),
     xlab=expression(paste("Portata (",m^3/s,")",sep="")),
     ylab="Tirante (m)",
     main="Misure di portata e scala di deflusso",
     type="p",col="red")
lines(Confidenza[,1],Tiranti[,],col="blue",lwd=2)
lines(Confidenza[,2],Tiranti[,],col="red",lty = "dashed")
lines(Confidenza[,3],Tiranti[,],col="red",lty = "dashed")
