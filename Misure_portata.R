#############################################################
# Elaborazioni relative all'identificazione di una scala di #
# deflusso a partire da diverse campagne di misura          #
#############################################################

# Carico i dati di portata
# (4 diverse campagne di misura, accuratezza Q: dev. std. 25%)
Dati<-read.csv(file = "Misure_portata.csv")
# h.mis: tirante in m
# Q.mis: corrispondente portata in m^3/s
# Misura 1
h.mis<-Dati$h.mis1
Q.mis<-Dati$Q.mis1
Sc.Def<-lm(formula=log(Q.mis)~log(h.mis),na.action=na.exclude)

# Intervallo di confidenza della scala di deflusso
Tiranti<-data.frame(h.mis=seq(0.1,2,.1))
Confidenza<-signif(exp(predict(Sc.Def,Tiranti,interval="confidence")),digits = 4)
Sc.Def.mis<-Tiranti
Sc.Def.mis[,2:4]<-Confidenza
colnames(Sc.Def.mis)<-c("h (m)","Q(m^3/s)","Int.Conf-","Int.Conf+")
write.table(Sc.Def.mis,file = "ScalaDef_mis1.txt",sep = ";",dec = ".",row.names = F)

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
# Misura 2
h.mis<-Dati$h.mis2
Q.mis<-Dati$Q.mis2
Sc.Def<-lm(formula=log(Q.mis)~log(h.mis),na.action=na.exclude)

# Intervallo di confidenza della scala di deflusso
Tiranti<-data.frame(h.mis=seq(0.1,2,.1))
Confidenza<-signif(exp(predict(Sc.Def,Tiranti,interval="confidence")),digits = 4)
Sc.Def.mis<-Tiranti
Sc.Def.mis[,2:4]<-Confidenza
colnames(Sc.Def.mis)<-c("h (m)","Q(m^3/s)","Int.Conf-","Int.Conf+")
write.table(Sc.Def.mis,file = "ScalaDef_mis2.txt",sep = ";",dec = ".",row.names = F)

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

# Misura 3
h.mis<-Dati$h.mis3
Q.mis<-Dati$Q.mis3
Sc.Def<-lm(formula=log(Q.mis)~log(h.mis),na.action=na.exclude)

# Intervallo di confidenza della scala di deflusso
Tiranti<-data.frame(h.mis=seq(0.1,2,.1))
Confidenza<-signif(exp(predict(Sc.Def,Tiranti,interval="confidence")),digits = 4)
Sc.Def.mis<-Tiranti
Sc.Def.mis[,2:4]<-Confidenza
colnames(Sc.Def.mis)<-c("h (m)","Q(m^3/s)","Int.Conf-","Int.Conf+")
write.table(Sc.Def.mis,file = "ScalaDef_mis2.txt",sep = ";",dec = ".",row.names = F)

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

# Misura 4
h.mis<-Dati$h.mis4
Q.mis<-Dati$Q.mis4
Sc.Def<-lm(formula=log(Q.mis)~log(h.mis),na.action=na.exclude)

# Intervallo di confidenza della scala di deflusso
Tiranti<-data.frame(h.mis=seq(0.1,2,.1))
Confidenza<-signif(exp(predict(Sc.Def,Tiranti,interval="confidence")),digits = 4)
Sc.Def.mis<-Tiranti
Sc.Def.mis[,2:4]<-Confidenza
colnames(Sc.Def.mis)<-c("h (m)","Q(m^3/s)","Int.Conf-","Int.Conf+")
write.table(Sc.Def.mis,file = "ScalaDef_mis2.txt",sep = ";",dec = ".",row.names = F)

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
