# ----------------------------------- Punto 2  ----------------------------------- 
# VALIDAZIONE DELLA SCALA ATTRAVERSO IL CALCOLO DI CR e MMR
dati <- read.csv2("~/Desktop/stat sociale/tavola.csv") #importazione dati 
colnames(dati) <- trimws(tolower(colnames(dati)))
items_scala <- c ("v26", "v35", "v42", "v15", "v37","v20", "v28", "v36", "v54", "v44", "v48", "v22") 
#scala iniziale, poi si possono poi eliminare gli item, fino ad arrivare a 8 

# Verifica delle frequenze
frequenze <- round(colMeans(dati[, items_scala]) * 100, 1)
cat("FREQUENZE ITEM (% di 1):\n")
for(i in 1:length(items_scala)) {
  cat(sprintf("  %s: %.1f%%\n", items_scala[i], frequenze[i]))
}
#errori 
dati_scala <- dati[, items_scala] ; dati_scala
n_soggetti <- nrow(dati_scala); n_soggetti
n_items <- length(items_scala); n_items
errori_totali <- 0
for(i in 1:n_soggetti){
  risposte <- as.numeric(dati_scala[i, ])
  errori_possibili <- numeric(n_items + 1)
  for(k in 0:n_items){
    pattern <- c(rep(1,k), rep(0, n_items-k))
    errori_possibili[k+1] <- sum(risposte != pattern)
  }
  errori_totali <- errori_totali + min(errori_possibili)
}
errori_totali
#coefficiente correzione
CR <- 1 - errori_totali / (n_soggetti * n_items); CR
# MMR
proporzioni_modali <- numeric(n_items)
for(j in 1:n_items){
  p1 <- sum(dati_scala[[j]]) / n_soggetti
  if(p1 >= 0.5){
    proporzioni_modali[j] <- p1
  } else {
    proporzioni_modali[j] <- 1 - p1
  }
}
MMR <- sum(proporzioni_modali) / n_items; MMR

#errore per ogni item
errori_item <- rep(0, n_items)
for(i in 1:n_soggetti){
  risposte <- as.numeric(dati_scala[i, ])
  errori_possibili <- numeric(n_items + 1)
  patterns <- matrix(0, nrow = n_items + 1, ncol = n_items)
  for(k in 0:n_items){
    pattern <- c(rep(1,k), rep(0, n_items-k))
    patterns[k+1, ] <- pattern
    errori_possibili[k+1] <- sum(risposte != pattern)
  }
  migliore <- which.min(errori_possibili)
  pattern_migliore <- patterns[migliore, ]
  errori_item <- errori_item + (risposte != pattern_migliore)
}
errori_item

#scala con otto item 
scalaitem <- read.csv2("~/Desktop/stat sociale/scalaitem.csv")
head(scalaitem)
items <- scalaitem[, c("v26","v35","v42","v15","v37","v20","v28","v48")]
# Somma per ogni individuo
scalaitem$somma <- rowSums(items, na.rm = TRUE)
head(scalaitem)

#RAGAZZI VS RAGAZZE 
# Separazione per genere 
maschi <- scalaitem$somma[scalaitem$V2 == "maschio"]
femmine <- scalaitem$somma[scalaitem$V2 == "femmina"]
# Descrizione la distribuzione
summary(maschi)
summary(femmine)
sd(maschi)    # Deviazione standard maschi
sd(femmine)  # Deviazione standard femmine

# Test statistico (t-test per gruppi indipendenti)
t.test(somma ~ V2, data = scalaitem)

# VERIFICA dell'ipotesi: femmine potrebbero manifestare forme più sottili o indirette di aggressività reattiva
# Separazione maschi e femmine
items <- c("v26","v35","v42","v15","v37","v20","v28","v48")
maschi <- scalaitem[scalaitem$V2 == "maschio", items]
femmine <- scalaitem[scalaitem$V2 == "femmina", items]
# Somma per item
somma_maschi <- colSums(maschi, na.rm = TRUE)
somma_femmine <- colSums(femmine, na.rm = TRUE)
# Media (proporzione di 1)
media_maschi <- colMeans(maschi, na.rm = TRUE)
media_femmine <- colMeans(femmine, na.rm = TRUE)

# Visione dei risultati
data.frame(
  item = items,
  somma_maschi = somma_maschi,
  somma_femmine = somma_femmine,
  media_maschi = round(media_maschi, 2),
  media_femmine = round(media_femmine, 2)
)

# Test LOT-R 
lotR <- read.csv2("~/Desktop/stat sociale/lotR.csv")
colnames(lotR) <- tolower(colnames(lotR))
head(lotR)
# Inversione degli item negativi 
lotR$v19_3_r <- 6 - lotR$v19_3
lotR$v19_6_r <- 6 - lotR$v19_6
lotR$v19_8_r <- 6 - lotR$v19_8
# Calcolo del punteggio LOT-R, sugli item (item filler evitati)
lotR$lotr_tot <- rowSums(lotR[,c("v19_1",
                                 "v19_4",
                                 "v19_10",
                                 "v19_3_r",
                                 "v19_6_r",
                                 "v19_8_r")],
                         na.rm=TRUE)

# Descrizione dei dati e deviazione standard 
summary(lotR$lotr_tot)
sd(lotR$lotr_tot)

# confronto tra maschi e femmine LOT R 
maschi_lotr <- lotR$lotr_tot[lotR$v2=="maschio"]
femmine_lotr <- lotR$lotr_tot[lotR$v2=="femmina"]
summary(maschi_lotr)
summary(femmine_lotr)
sd(maschi_lotr)
sd(femmine_lotr)

# RELAZIONE TRA AGGRESSIVITA E OTTIMISMO 
dati_finali <- cbind(scalaitem, lotR$lotr_tot)
colnames(dati_finali)[ncol(dati_finali)] <- "lotr"
cor.test(dati_finali$somma, dati_finali$lotr)


# ----------------------------------- Punto 3  -----------------------------------
#install.packages("Compind")
#install.packages("stringr")
library(Compind)
library(stringr)

mydata <- read.csv2("~/Desktop/stat sociale/indici.csv")
mydata
# Nuovi nomi alle variabili per semplicità
colnames(mydata) <- c("Territorio","X1","X2","X3","X4","X5","X6","X7")
# Tutti questi indicatori sono POSITIVI (più alto = meglio), già invertiti su excel quindi NON serve ribaltamento
mydata$x1cal <- mydata$X1
mydata$x2cal <- mydata$X2
mydata$x3cal <- mydata$X3
mydata$x4cal <- mydata$X4
mydata$x5cal <- mydata$X5
mydata$x6cal <- mydata$X6
mydata$x7cal <- mydata$X7
# Eliminazione delle righe aggregate (teniamo solo le regioni), Italia la si tiene all’ultima riga 
mydataindic <- mydata[1:20,]
mydataindic

# ----------------------------- METODO 1: RANGHI ------------------------------
# Normalizzazione del metodo dei ranghi
normalise_ci(mydataindic, c("x1cal","x2cal","x3cal","x4cal","x5cal","x6cal","x7cal"),
             polarity=rep("POS",7), method=3)
rankInd <- normalise_ci(mydataindic, c(9:15),
                        polarity=rep("POS",7),
                        method=3, ties="average")
#Estrazione dei ranghi normalizzati
mydataindic <- data.frame(mydataindic, rankInd$ci_norm)
#Nuovi nomi alle nuove colonne create (colonne 8, 9, 10) in rank1, rank2, rank3..
colnames(mydataindic)[16:22] <- paste("rank", 1:7, sep="")

# Indicatore sintetico s1 (somma ranghi)
mydataindic$s1 <- apply(mydataindic[,16:22],1,sum)

# ---------------------- METODO 2: NUMERI INDICI ------------------------------
numind <- function(x){x[1:20]/x[30]}
indiciInd <- apply(mydata[,2:8],2,numind)
# Aggiungere i numeri indici al dataset. Creazione tre nuove colonne: I1, I2, I3..
mydataindic <- data.frame(mydataindic, indiciInd)
colnames(mydataindic)[24:30] <- paste("I", 1:7, sep="")
# Indicatore sintetico s2 (media)
mydataindic$s2 <- apply(mydataindic[,24:30],1,mean)

# ---------------------- METODO 3: MIN-MAX ------------------------------------
#Normalizzazione con il metodo min-max
rangeInd <- normalise_ci(mydataindic, c(9:15),
                         polarity=rep("POS",7),
                         method=2)
#Aggiungere i valori normalizzati al dataset
mydataindic <- data.frame(mydataindic, rangeInd$ci_norm)
colnames(mydataindic)[32:38] <- paste("xcv", 1:7, sep="")
# Indicatore sintetico s3 (media)
mydataindic$s3 <- apply(mydataindic[,32:38],1,mean)

# ------------------------- METODO 4: STANDARDIZZAZIONE ------------------------
# Definizione della funzione per calcolare z-score
stzInd <- function(x){
  media <- x[length(x)]  # valore ITALIA
  sqm <- sqrt(1/(length(x)-1)*sum((x[1:(length(x)-1)]-x[length(x)])^2))
  z <- (x[1:length(x)-1]-media)/sqm
  return(z)}
# Applicazione della funzione a ogni colonna
stdInd <- apply(mydata[,2:8], 2, stzInd)
# Aggiungere gli z-score al dataset 
mydata_clean <- mydata[c(1:20, nrow(mydata)), ]
stdInd <- apply(mydata_clean[,2:8], 2, stzInd)
mydataindic <- data.frame(mydataindic, stdInd)
#Rinominare le colonne
colnames(mydataindic)[(ncol(mydataindic)-6):ncol(mydataindic)] <- paste("z",1:7,sep="")
# Indicatore sintetico s4
mydataindic$s4 <- apply(mydataindic[, grep("^z", colnames(mydataindic))],1,mean)

# ------------------------- METODO 5: PROPORZIONI ------------------------------
# Calcolare la proporzione di ogni regione sul totale
propInd <- apply(mydataindic[,2:8], 2, function(x){x/sum(x)*100})
#Aggiungere le proporzioni al dataset
mydataindic <- data.frame(mydataindic, propInd)
#Rinominare le colonne
colnames(mydataindic)[(ncol(mydataindic)-6):ncol(mydataindic)] <- paste("p",1:7,sep="")
# Indicatore sintetico s5
colnames(mydataindic)[(ncol(mydataindic)-6):ncol(mydataindic)] <- paste("p",1:7,sep="")
p_cols <- paste("p",1:7,sep="")
mydataindic$s5 <- apply(mydataindic[, p_cols], 1, sum)

# ------------------------- METODO 6: WROCLAW ------------------------
ci_wroclaw_mia <- function (x, indic_col) 
{
  options(warn = -1)
  x_num = x[, indic_col]  # selezione z1...z7
  n_indic <- ncol(x_num)  # numero indicatori = 7
  n_unit <- nrow(x_num)   # numero regioni = 20
# Controllo numerico
  for (i in 1:n_indic) {
    if (!is.numeric(x_num[, i])) {
      stop(paste("Colonna non numerica:", i))
    }
  }
# Controllo NA
  if (any(is.na(x_num))) {
    message("Attenzione: presenti NA")
  }
# UNITÀ IDEALE (massimi)
Ma <- apply(x_num, 2, max, na.rm = TRUE)
# Replica dell'unità ideale
  Ma_m <- matrix(rep(Ma, each = n_unit), nrow = n_unit)
# DISTANZA DALL'IDEALE
  diff_Ma = x_num - Ma_m
  diff_Ma_q = diff_Ma^2
# Somma per riga
  Sum_riga <- apply(diff_Ma_q, 1, sum)
# Distanza euclidea
  D_i0 <- sqrt(Sum_riga)
  media <- mean(D_i0, na.rm = TRUE)
  std <- sqrt(((n_unit - 1) / n_unit) * var(D_i0, na.rm = TRUE))
  denom <- media + 2 * std #normalizzazione
  ci_wroclaw_est <- D_i0 / denom
return(list(
    Ideale = Ma,
    ci_wroclaw_est = ci_wroclaw_est,
    mean = media,
    sqm = std
  ))
}
# Selezione delle colonne z1...z7
z_cols <- grep("^z", colnames(mydataindic))
# Applicazione metodo Wroclaw
wroclawInd <- ci_wroclaw_mia(mydataindic, z_cols)
mydataindic$ws <- round(wroclawInd$ci_wroclaw_est, 3)


# -----------CONFRONTO TRA TUTTI GLI INDICATORI SINTETICI ------------------
indicatori <- data.frame(
  s1 = mydataindic$s1,
  s2 = mydataindic$s2,
  s3 = mydataindic$s3,
  s4 = mydataindic$s4 +2,   # shift per positività 
  s5 = mydataindic$s5,
  sw = mydataindic$ws
)
indicatori 
#i ranghi vengono calcolati solo per valori di indicatori positivi, quindi aggiungiamo 2 a s4


# ------------------------- GRADUATORIA ------------------------------
gradInd <- normalise_ci(indicatori,
                        c(1:6),
                        polarity=c(rep("POS",5), "NEG"),
                        method=3,
                        ties="average")
#Aggiungere nomi regioni
rownames(gradInd$ci_norm) <- mydataindic$Territorio
#Visualizzare graduatorie in ordine alfabetico
gradInd$ci_norm[str_sort(rownames(gradInd$ci_norm), decreasing = FALSE),]
# --------------------------- ANALISI DI CONCORDANZA --------------------------
# Correlazione di Spearman tra i 6 indicatori sintetici
spearman <- cor(gradInd$ci_norm, method="spearman")
print(round(spearman, 2))
mspearman<- mean(spearman[upper.tri(spearman)])
mspearman
# ---------------------- Coefficiente di Kendall W ----------------------------
kendal <- function(k, spearman){
  1/k + (k-1)/k * mean(spearman[upper.tri(spearman)])
}
# k = numero indicatori sintetici (s1, s2, s3, s4, s5, sw → 6)
W <- kendal(6, spearman)
W # kendal è anche calcolabile come K <- 1/6 + (6-1)/6 * mspearman; K


# --------------------------- COSTRUZIONE INDICATORE ----------------------------
mydataZ<- data.frame(stdInd)
colnames(mydataZ)<- paste("z",1:7,sep="")
mydataZ$Territorio<- mydata[1:20, 1]
mydataZ
# aggregazione di Competenza alfabetica adeguata  +  Competenza numerica  adeguata /2 (media aritmetica) 
# aggregazione di Partecipazione culturale fuori + casa	Lettura di libri e quotidiani /2 (media aritmetica)
mydataZ$z23<- rowMeans(mydataZ[, c("z2", "z3")])
mydataZ$z45<- rowMeans(mydataZ[, c("z4", "z5")])
mydataZ
# ------------------- PESI ------------------------
w1 <- 0.17
w2 <- 0.25
w3 <- 0.16
w4 <- 0.25
w5 <- 0.17
# ------------------- INDICATORE SINTETICO --------
indicatore_sintetico <- 
  w1 * mydataZ$z1 +
  w2 * mydataZ$z23 +
  w3 * mydataZ$z45 +
  w4 * mydataZ$z6 +
  w5 * mydataZ$z7
# Aggiungo al dataset l'indice
mydataZ$indice <- indicatore_sintetico
# Ordinamento (decrescente, ovvero i territori con indice più alto sono in cima alla classifica)
mydataZ <- mydataZ[order(-mydataZ$indice), ]
mydataZ
# Visualizza classifica
mydataZ[, c("Territorio", "indice")]
# statistiche di base
summary(mydataZ$indice)
sd(mydataZ$indice)


# -------------------CLUSTER ANALYSIS (K-means)----------------------------------------
#Si utilizza solo l'indice sintetico
cluster_data <- mydataZ$indice
# trasformazione in matrice
cluster_data <- as.matrix(cluster_data)
# numero di cluster migliore (metodo elbow )
cluster_data <- mydataZ[, paste("z",1:7,sep="")]
wss <- numeric(10)
for (k in 1:10) {
  set.seed(123)
  km <- kmeans(cluster_data, centers = k, nstart = 10)
  wss[k] <- km$tot.withinss
}
plot(1:10, wss,
     type="b",
     pch=19,
     col="lightgreen",
     xlab="Numero cluster",
     ylab="WSS",
     main="Elbow method (z-score)")
abline(v=3, col="darkgreen", lty=2)
# Creazione della tabella con k e WSS
elbow_table <- data.frame(
  k = 1:10,
  WSS = wss
)
# Riduzione percentuale
elbow_table$diff <- c(NA, diff(elbow_table$WSS))
# Riduzione relativa (%)
elbow_table$perc_drop <- c(NA, -diff(elbow_table$WSS) / elbow_table$WSS[-length(elbow_table$WSS)] * 100)
elbow_table
# L’analisi della riduzione della WSS mostra un forte miglioramento fino a k = 3. 
# Oltre questo valore la diminuzione diventa più graduale e stabile, ciò indica la presenza del gomito. 
# Pertanto, si sceglie k = 3 come numero ottimale di cluster

set.seed(123)
k3 <- kmeans(cluster_data, centers=3, nstart=10)
mydataZ$cluster <- k3$cluster
mydataZ[order(mydataZ$cluster), c("Territorio", "indice", "cluster")]
plot(mydataZ$indice,
     col=mydataZ$cluster,
     pch=19,
     main="Cluster regioni (k=3)",
     ylab="Indice")
for(i in 1:3){
  cat("\nCluster", i, ":\n")
  print(mydataZ$Territorio[mydataZ$cluster == i])
}

# CLUSTER GERARCHICO
# Si utilizzano i 7 z-score
z_data <- mydataZ[, paste("z",1:7,sep="")]
# Calcolare la distanza euclidea
dist_matrix <- dist(z_data, method = "euclidean")
# Hierarchical clustering 
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogramma
plot(hc, labels = mydataZ$Territorio,
     main = "Dendrogramma gerarchico regioni",
     xlab = "", sub = "", cex = 0.7)
# Taglio a 3 cluster per confronto con k-means
rect.hclust(hc, k = 3, border = "red")

# ------------------- Programma aggregazione  -------------------
fusion_table <- data.frame(
  Step = 1:nrow(hc$merge),
  Cluster1 = sapply(hc$merge[,1], function(x) ifelse(x<0, mydataZ$Territorio[-x], paste0("Cluster_",x))),
  Cluster2 = sapply(hc$merge[,2], function(x) ifelse(x<0, mydataZ$Territorio[-x], paste0("Cluster_",x))),
  Distanza = hc$height
)
print(fusion_table)


#------------------------------- CONFRONTO INDICATORE E SODDISFAZIONE ------------------------------- 
library(dplyr)
sodd <- read.csv2("~/Desktop/stat sociale/soddisfazione_vita.csv")
# Si tengono solo le regioni (no province autonome)
sodd_reg <- sodd[c(1:5,8:22), c("Territorio", "Soddisfazione.per.la.propria.vita")]
sodd_reg 
# Uniformare i nomi dei due differenti dataset (indice sintetico e soddisfazione vita)
mydataZ$Territorio <- trimws(iconv(mydataZ$Territorio, from = "latin1", to = "UTF-8", sub = ""))
mydataZ$Territorio <- gsub("Valle d'Aosta/Vall.*", "Valle d'Aosta", mydataZ$Territorio)
mydataZ$Territorio <- gsub("Trentino-Alto Adige/S.*", "Trentino-Alto Adige", mydataZ$Territorio)
sodd_reg$Territorio <- trimws(sodd_reg$Territorio)
# unione indice e soddisfazione in un unica tabella con merge 
dati_finali <- merge(mydataZ[, c("Territorio","indice")],
                     sodd_reg,
                     by="Territorio")
dati_finali

# Ranghi dell'indicatore sintetico e soddisfazione vita
dati_r <- dati_finali %>%
  mutate(
    rank_ind = rank(indice, ties.method = "average"),
    rank_sodd = rank(Soddisfazione.per.la.propria.vita, ties.method = "average"),
    diff_ranks = rank_ind - rank_sodd,
    diff_ranks_sq = diff_ranks^2
  )
# Somma dei quadrati delle differenze
sum_sq <- sum(dati_r$diff_ranks_sq)
dati_r
sum_sq

#INDICE SPEARMAN
n <- nrow(dati_r)
spearman <- 1 - (6 * sum_sq) / (n * (n^2 - 1)) # ci sono ties
spearman
# INDICE KENDALL
k <- 2
kendall_w <- 1/k + (k-1)/k * spearman
kendall_w
# test significatività
cor.test(dati_finali$indice,
         dati_finali$Soddisfazione.per.la.propria.vita,
         method="spearman")


