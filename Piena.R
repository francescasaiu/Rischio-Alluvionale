###################################
# Analisi di frequenza locale:    #
# Esempio per Savio a San Vittore #
###################################

# STEP 1 - IDENTIFICAZIONE DEL CAMPIONE (gia' fatto)
# Carico il DB delle serie AMS
AMS<-read.csv(file = "Serie_AMS_ROMA.csv")
# Salvo i nomi delle stazioni in una variabile Nomi.Staz
Nomi.Staz<-colnames(AMS)

# Memorizzo i dati del SAVIO.S_Vittore ordinati in maniera crescente 
# nella variabile "x"
# Trovo la posizione della stazione all’interno di Nomi.Staz
Sito<-which(Nomi.Staz=="SAVIO.S_Vittore") # 42 anni
# Altri esempi
# Sito<-which(Nomi.Staz=="RENO.Casalecchio") # 74 anni
# Sito<-which(Nomi.Staz=="LAMONE.Grattacoppa") # 15 anni
x<-sort(AMS[,Sito],na.last = NA); x

# STEP 2 - CALCOLO DELLA FREQUENZA CAMPIONARIA
# Calcolo le variabili per rappresentare la cdf campionaria
n<-length(x) # Lunghezza della serie
pp<-(1:n)/(n+1); pp # Weibull plotting position
ppT<-1/(1-pp); # Stime empiriche del tempo di ritorno

# Rappresentazione grafica
plot(ppT,x,type="b",
     main=Nomi.Staz[Sito],
     xlim=c(1,200), # Limite asse x
     ylim=c(0,1.5*max(x)), # Limite asse y
     xlab="Tempo di ritorno (anni)", 
     ylab=expression(paste("Portata [",m^3,"/s]")),
     log="x")

# STEP 3 - SCELTA DELLA DISTRIBUZIONE DI FREQUENZA
# Bonta' di adattamento (goodness of the fit) - distr. Gumbel
# Plotting-Position Correlation Coefficient (PPCC) test
# Carico il pacchetto "ppcc"
library("ppcc")
# Eseguo il test per
# - la distrib. di Gumbel
# - la pp di Weibull
ppccTest(x,qfn = "qgumbel",ppos = "Weibull")
# se p-value è maggiore della significativita' scelta: 0.05
# la dist. di Gumbel e' idonea
# in caso contrario si possono considerare altre distrib.

# STEP 4 - STIMA DELLA DISTRIBUZIONE DI FREQUENZA
# Carico il pacchetto "extRemes" per la stima MLE dei parametri della distribuzione di Gumbel e GEV
library("extRemes")
# il pacchetto extRemes usa oggetti "fevd"
fevd(x,type = "Gumbel")
str(fevd(x,type = "Gumbel")) # struttura: lista
# memorizzo le stime MLE dei parametri
Par_Gum<-fevd(x,type = "Gumbel")$results$par

# Aggiungo al diagramma la distribuzione teorica (Gumbel)
T_teo<-c(1+(1:19)/20,2:200) # vettore con tempi di ritorno arbitrari
q<-rlevd(period = T_teo,
         loc = Par_Gum["location"],
         scale = Par_Gum["scale"],
         type = "Gumbel")
lines(T_teo,q,col="blue",lwd=2) # aggiungo una nuova serie "linea"

# STEP 5 - CALCOLO LA PORTATA DI PROGETTO QT
# Valuto e rappresento il quantile per T = 100 anni
Q100_Gum<-q["100"]
points(100,Q100_Gum,col="blue",pch=19) # aggiungo il punto al diagramma
signif(as.numeric(Q100_Gum),4)

# Intervalli di confidenza da pacchetto  extRemes
# Tracciati attraverso ricampionamento Bootstrap
Conf.Int.GUM<-ci(fevd(x,type = "Gumbel"),
                 alpha = 0.05, type = "return.level",
                 return.period = T_teo, R = 500, method = "boot")

# Rappresento l'intervallo di confidenza
lines(T_teo,Conf.Int.GUM[,1],type="l",col="orange",lwd=2)
lines(T_teo,Conf.Int.GUM[,2],type="l",lty="dashed",col="orange",lwd=2)
lines(T_teo,Conf.Int.GUM[,3],type="l",col="orange",lwd=2)

# Confronto dei quantili centennali
signif(as.numeric(Q100_Gum),4)
signif(Conf.Int.GUM["100-year",],4)
# Rapporto
signif(Conf.Int.GUM["100-year",],4)/signif(as.numeric(Q100_Gum),4)

# ESEMPIO PER LA DISTRIBUZIONE GEV (3 PARAMETRI, PIU' FLESSIBILE, MENO ROBUSTA)
# Intervalli di confidenza dal pacchetto extRemes per la distribuzione GEV
Conf.Int.GEV<-ci(fevd(x,type = "GEV"),
                 alpha = 0.05, type = "return.level",
                 return.period = T_teo, R = 500, method = "boot")
lines(T_teo,Conf.Int.GEV[,1],type="l",col="red",lwd=2)
lines(T_teo,Conf.Int.GEV[,2],type="l",lty="dashed",col="red",lwd=2)
lines(T_teo,Conf.Int.GEV[,3],type="l",col="red",lwd=2)
