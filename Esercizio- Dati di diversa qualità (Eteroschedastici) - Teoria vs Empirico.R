# Esercizio: Dati di diversa qualità (Eteroschedastici) - Teoria vs Empirico

# Settiamo la cartella di lavoro:
setwd("/Users/jackross/Desktop/Data Science/Esercizi_R")
# Vediamo la WD Corrente:
getwd()

set.seed(450) # Per avere sempre gli stessi "dati"

# PARAMETRI GENERALI DEL MODELLO
N <- 200
# Parametro Deterministico
y_vero <- 5
media_w <- 0

# Varianze per ricreare i due scenari di test rispetto alla soglia critica (3*sigma1^2):
# Scenario 1: Var 1 e 2 (La seconda tranche è sotto soglia -> Conviene Media Semplice)
# Scenario 2: Var 1 e 4 (La seconda tranche è sopra soglia -> Conviene il Cestino)
varianza_W1 <- 1
varianza_W2 <- 2
varianza_W3 <- 4
  
sigma_W1 <- sqrt(varianza_W1)
sigma_W2 <- sqrt(varianza_W2)
sigma_W3 <- sqrt(varianza_W3)

# CALCOLO DEI VALORI TEORICI
# Calcolo dei pesi ottimi (p*) per lo stimatore a Massima Verosimiglianza (ML).
# Si ottengono annullando la derivata prima dell'MSE teorico rispetto a p, 
# ovvero trovando il valore di p che minimizza lo Stimatore ML.
p1 <- varianza_W2 / (varianza_W1 + varianza_W2)
p2 <- varianza_W3 / (varianza_W1 + varianza_W3)

# Calcolo della Varianza (MSE perché non Polorizzato) dello Stimatore ML
MSE_TEO_ML1 <- 2/N * (p1^2 * varianza_W1 + (1-p1)^2 * varianza_W2)
MSE_TEO_ML2 <- 2/N * (p2^2 * varianza_W1 + (1-p2)^2 * varianza_W3)

# Calcolo MSE della Media Campionaria (p = 0.5)
MSE_TEO_SM1 <- 2/N * ((varianza_W1 + varianza_W2) / 4)
MSE_TEO_SM2 <- 2/N * ((varianza_W1 + varianza_W3) / 4)

# Calcolo MSE eliminando la tranche con varianza più grande (p = 1)
MSE_TEO_SENZA2 <- 2/N * varianza_W1

# CALCOLO DEGLI MSE EMPIRICI TRAMITE SIMULAZIONE MONTE CARLO VARIANZO N_MC
# Testiamo diversi valori di iterazioni
N_MC_TOTALI <- c(10, 50, 100, 200, 500, 1000, 5000, 10000)
MSE_EMP_ML1_TOTALI <- numeric(length(N_MC_TOTALI))
MSE_EMP_ML2_TOTALI <- numeric(length(N_MC_TOTALI))
MSE_EMP_SM1_TOTALI <- numeric(length(N_MC_TOTALI))
MSE_EMP_SM2_TOTALI <- numeric(length(N_MC_TOTALI))
MSE_EMP_SENZA2_TOTALI <- numeric(length(N_MC_TOTALI))

for(j in 1: length(N_MC_TOTALI)) {
  N_MC <- N_MC_TOTALI[j]
  MSE_EMP_ML1 <- numeric(N_MC)
  MSE_EMP_ML2 <- numeric(N_MC)
  MSE_EMP_SM1 <- numeric(N_MC)
  MSE_EMP_SM2 <- numeric(N_MC)
  MSE_EMP_SENZA2 <- numeric(N_MC)

  for(i in 1: N_MC) {
    # Generazione del rumore W con sigmaW1
    W1 <- rnorm(n = N/2, media_w, sd = sigma_W1)
    X1 <- y_vero + W1
    
    # Generazione del rumore W con sigmaW2
    W2 <- rnorm(n = N/2, media_w, sd = sigma_W2)
    X2 <- y_vero + W2
    
    # 2. Generazione del rumore W con sigmaW3
    W3 <- rnorm(n = N/2, media_w, sd = sigma_W3)
    X3 <- y_vero + W3
    
    # Medie campionarie delle singole tranche
    m1 <- mean(X1)
    m2 <- mean(X2)
    m3 <- mean(X3)
    
    # Stime SCENARIO 1 (X1 e X2)
    stima_ML1    <- p1 * m1 + (1 - p1) * m2
    stima_SM1    <- 0.5 * m1 + 0.5 * m2   
    stima_SENZA2 <- 1.0 * m1 + 0.0 * m2       
    
    # Errori Quadratici SCENARIO 1 (X1 e X2)
    MSE_EMP_ML1[i]      <- (stima_ML1 - y_vero)^2
    MSE_EMP_SM1[i]      <- (stima_SM1 - y_vero)^2
    MSE_EMP_SENZA2[i] <- (stima_SENZA2 - y_vero)^2
    
    # Stime SCENARIO 2 (X1 e X3) 
    stima_ML2    <- p2 * m1 + (1 - p2) * m3
    stima_SM2    <- 0.5 * m1 + 0.5 * m3   
    
    # Errori Quadratici SCENARIO 2 (X1 e X3)
    MSE_EMP_ML2[i]      <- (stima_ML2 - y_vero)^2
    MSE_EMP_SM2[i]      <- (stima_SM2 - y_vero)^2
    # Il valore dell'errore del cestino è sempre lo stesso perché leva sempre la 
    # tranche peggiore e quindi rimane solo con X1.
  }
  
  # Medie Errori Quadratici sulle N_MC prove effettuate
  MSE_EMP_ML1_TOTALI[j] <- mean(MSE_EMP_ML1)
  MSE_EMP_SM1_TOTALI[j] <- mean(MSE_EMP_SM1)
  MSE_EMP_SENZA2_TOTALI[j] <- mean(MSE_EMP_SENZA2)
  
  MSE_EMP_ML2_TOTALI[j] <- mean(MSE_EMP_ML2)
  MSE_EMP_SM2_TOTALI[j] <- mean(MSE_EMP_SM2)
}

# Confronto Scenario 1 (Varianze 1 e 2)
risultati_scenario1 <- data.frame(
  Prove_MC    = N_MC_TOTALI,
  ML_Emp      = round(MSE_EMP_ML1_TOTALI, 5),
  SM_Emp      = round(MSE_EMP_SM1_TOTALI, 5),
  Cestino_Emp = round(MSE_EMP_SENZA2_TOTALI, 5),
  ML_Teo      = round(MSE_TEO_ML1, 5),
  SM_Teo      = round(MSE_TEO_SM1, 5),
  Cestino_Teo = round(MSE_TEO_SENZA2, 5)
)
print("SCENARIO 1 (Varianze 1 e 2)")
risultati_scenario1

# Confronto Scenario 2 (Varianze 1 e 4)
risultati_scenario2 <- data.frame(
  Prove_MC    = N_MC_TOTALI,
  ML_Emp      = round(MSE_EMP_ML2_TOTALI, 5),
  SM_Emp      = round(MSE_EMP_SM2_TOTALI, 5),
  Cestino_Emp = round(MSE_EMP_SENZA2_TOTALI, 5),
  ML_Teo      = round(MSE_TEO_ML2, 5),
  SM_Teo      = round(MSE_TEO_SM2, 5),
  Cestino_Teo = round(MSE_TEO_SENZA2, 5)
)
print("SCENARIO 2 (Varianze 1 e 4)")
risultati_scenario2

# GRAFICO 1: Boxplot per confrontare la dispersione degli errori (Scenario 1)

# Creiamo un dataframe
df_errori_scen1 <- data.frame(
  ML = MSE_EMP_ML1,
  Media = MSE_EMP_SM1,
  Cestino = MSE_EMP_SENZA2
)
boxplot(df_errori_scen1, 
        main = "Confronto degli Stimatori (Scenario 1)", 
        ylab = "Valori MSE")

# GRAFICO 2: Boxplot per lo Scenario 2 (dove il Cestino diventa migliore della Media)

df_errori_scen2 <- data.frame(
  ML = MSE_EMP_ML2,
  Media = MSE_EMP_SM2,
  Cestino = MSE_EMP_SENZA2
)
boxplot(df_errori_scen2, 
        main = "Confronto degli Stimatori (Scenario 2)", 
        ylab = "Valori MSE")

# GRAFICO 3: Istogrammi affiancati dell'ML e della Media (Scenario 1)

par(mfrow = c(1, 3)) 

hist(MSE_EMP_ML1, 
     main = "Istogramma ML", 
     xlab = "Errore Quadratico", 
     ylab = "Frequenza")

hist(MSE_EMP_SM1, 
     main = "Istogramma Media Semplice", 
     xlab = "Errore Quadratico", 
     ylab = "Frequenza")

hist(MSE_EMP_SENZA2, 
     main = "Istogramma Cestino", 
     xlab = "Errore Quadratico", 
     ylab = "Frequenza")

par(mfrow = c(1, 1)) 

# GRAFICO 4: Istogrammi affiancati dell'ML e della Media (Scenario 2)

par(mfrow = c(1, 3)) 

hist(MSE_EMP_ML2, 
     main = "Istogramma ML", 
     xlab = "Errore Quadratico", 
     ylab = "Frequenza")

hist(MSE_EMP_SM2, 
     main = "Istogramma Media Semplice", 
     xlab = "Errore Quadratico", 
     ylab = "Frequenza")

hist(MSE_EMP_SENZA2, 
     main = "Istogramma Cestino", 
     xlab = "Errore Quadratico", 
     ylab = "Frequenza")

par(mfrow = c(1, 1)) 

# GRAFICO 5: Scatter plot con linee per vedere la convergenza dell'ML al variare di N_MC
# Calcoliamo i limiti dell'asse Y per farci stare comodamente tutte e tre le curve
y_min <- min(MSE_EMP_ML1_TOTALI, MSE_EMP_SM1_TOTALI)
y_max <- max(MSE_EMP_SENZA2_TOTALI)

# Plot base con la curva dell'ML (Asse X Lineare, come nel tuo grafico originale)
plot(N_MC_TOTALI, MSE_EMP_ML1_TOTALI, 
     type = "b", pch = 16, col = "red", lwd = 2,
     ylim = c(y_min, y_max),
     main = "Stabilità degli Stimatori all'aumentare delle prove (Scenario 1)",
     xlab = "Numero di Prove Monte Carlo (N_MC)",
     ylab = "MSE Medio Empirico")

# Aggiungiamo le curve empiriche di Media Semplice e Cestino
lines(N_MC_TOTALI, MSE_EMP_SM1_TOTALI, type = "b", pch = 16, col = "blue", lwd = 2)
lines(N_MC_TOTALI, MSE_EMP_SENZA2_TOTALI, type = "b", pch = 16, col = "darkslategray", lwd = 2)

# Aggiungiamo le tre linee teoriche orizzontali verso cui convergono
abline(h = MSE_TEO_ML1, col = "red", lty = 2)
abline(h = MSE_TEO_SM1, col = "blue", lty = 2)
abline(h = MSE_TEO_SENZA2, col = "darkslategray", lty = 2)

# Legenda
legend("right", 
       legend = c("ML pesato", "Media semplice", "Solo gruppo 1", "--- Valori Teorici"),
       col = c("red", "blue", "darkslategray", "black"),
       pch = c(16, 16, 16, NA), lty = c(1, 1, 1, 2), bty = "n")

# CALCOLO DEL VALORE DEGLI STIMATORE AUMENTANDO GLI N CAMPIONI
# Fissiamo le prove Monte Carlo a 1000 per avere stime stabili
N_MC_FISSO <- 1000

# Testiamo diversi valori di N (numerosità del dataset)
N_TOTALI <- c(50, 100, 200, 500, 1000, 2000, 5000, 10000)

# Vettori per salvare i risultati EMPIRICI finali
MSE_EMP_ML1_N <- numeric(length(N_TOTALI))
MSE_EMP_SM1_N <- numeric(length(N_TOTALI))
MSE_EMP_ML2_N <- numeric(length(N_TOTALI))
MSE_EMP_SM2_N <- numeric(length(N_TOTALI))
MSE_EMP_SENZA2_N <- numeric(length(N_TOTALI))

# Vettori per salvare i risultati TEORICI finali
MSE_TEO_ML1_N <- numeric(length(N_TOTALI))
MSE_TEO_SM1_N <- numeric(length(N_TOTALI))
MSE_TEO_ML2_N <- numeric(length(N_TOTALI))
MSE_TEO_SM2_N <- numeric(length(N_TOTALI))
MSE_TEO_SENZA2_N <- numeric(length(N_TOTALI))

for(j in 1:length(N_TOTALI)) {
  N <- N_TOTALI[j]
  
  # 1. CALCOLO DEI VALORI TEORICI (Dipendono da N, quindi li ricalcoliamo a ogni giro)
  MSE_TEO_ML1_N[j] <- 2/N * (p1^2 * varianza_W1 + (1-p1)^2 * varianza_W2)
  MSE_TEO_SM1_N[j] <- 2/N * ((varianza_W1 + varianza_W2) / 4)
  
  MSE_TEO_ML2_N[j] <- 2/N * (p2^2 * varianza_W1 + (1-p2)^2 * varianza_W3)
  MSE_TEO_SM2_N[j] <- 2/N * ((varianza_W1 + varianza_W3) / 4)
  
  MSE_TEO_SENZA2_N[j] <- 2/N * varianza_W1
  
  # Vettori temporanei per la simulazione Monte Carlo corrente
  MSE_EMP_ML1 <- numeric(N_MC_FISSO)
  MSE_EMP_SM1 <- numeric(N_MC_FISSO)
  MSE_EMP_ML2 <- numeric(N_MC_FISSO)
  MSE_EMP_SM2 <- numeric(N_MC_FISSO)
  MSE_EMP_SENZA2 <- numeric(N_MC_FISSO)
  
  for(i in 1:N_MC_FISSO) {
    # Generazione del rumore W e dei dati X
    W1 <- rnorm(n = N/2, media_w, sd = sigma_W1)
    X1 <- y_vero + W1
    
    W2 <- rnorm(n = N/2, media_w, sd = sigma_W2)
    X2 <- y_vero + W2
    
    W3 <- rnorm(n = N/2, media_w, sd = sigma_W3)
    X3 <- y_vero + W3
    
    # Medie campionarie delle singole tranche
    m1 <- mean(X1)
    m2 <- mean(X2)
    m3 <- mean(X3)
    
    # Stime SCENARIO 1 (X1 e X2)
    stima_ML1    <- p1 * m1 + (1 - p1) * m2
    stima_SM1    <- 0.5 * m1 + 0.5 * m2   
    stima_SENZA2 <- 1.0 * m1 + 0.0 * m2       
    
    # Errori Quadratici SCENARIO 1 (X1 e X2)
    MSE_EMP_ML1[i]    <- (stima_ML1 - y_vero)^2
    MSE_EMP_SM1[i]    <- (stima_SM1 - y_vero)^2
    MSE_EMP_SENZA2[i] <- (stima_SENZA2 - y_vero)^2
    
    # Stime SCENARIO 2 (X1 e X3) 
    stima_ML2    <- p2 * m1 + (1 - p2) * m3
    stima_SM2    <- 0.5 * m1 + 0.5 * m3   
    
    # Errori Quadratici SCENARIO 2 (X1 e X3)
    MSE_EMP_ML2[i] <- (stima_ML2 - y_vero)^2
    MSE_EMP_SM2[i] <- (stima_SM2 - y_vero)^2
  }
  
  # Medie Errori Quadratici sulle prove effettuate per il dato N
  MSE_EMP_ML1_N[j]    <- mean(MSE_EMP_ML1)
  MSE_EMP_SM1_N[j]    <- mean(MSE_EMP_SM1)
  MSE_EMP_SENZA2_N[j] <- mean(MSE_EMP_SENZA2)
  
  MSE_EMP_ML2_N[j] <- mean(MSE_EMP_ML2)
  MSE_EMP_SM2_N[j] <- mean(MSE_EMP_SM2)
}

# GRAFICI 6: MSE in scala log-log (Empirico vs Teorico)
# Dividiamo la finestra per avere i due scenari affiancati
par(mfrow = c(1, 2)) 

# Calcoliamo i logaritmi base 10 per l'asse X
log_N <- log10(N_TOTALI)

# --- GRAFICO SCENARIO 1 (Varianze 1 e 2) ---
plot(log_N, log10(MSE_EMP_ML1_N), type = "p", pch = 16, col = "red", 
     ylim = c(min(log10(MSE_EMP_ML1_N)) - 0.2, max(log10(MSE_EMP_SENZA2_N)) + 0.2),
     main = "Scenario 1: Varianze 1 e 2", xlab = "log10(N)", ylab = "log10(MSE)")
# Aggiungiamo la linea teorica del ML
lines(log_N, log10(MSE_TEO_ML1_N), col = "red")

# Aggiungiamo Media Semplice (Punti + Linea)
points(log_N, log10(MSE_EMP_SM1_N), pch = 16, col = "blue")
lines(log_N, log10(MSE_TEO_SM1_N), col = "blue")

# Aggiungiamo Cestino (Punti + Linea)
points(log_N, log10(MSE_EMP_SENZA2_N), pch = 16, col = "darkslategray")
lines(log_N, log10(MSE_TEO_SENZA2_N), col = "darkslategray")


# --- GRAFICO SCENARIO 2 (Varianze 1 e 4) ---
plot(log_N, log10(MSE_EMP_ML2_N), type = "p", pch = 16, col = "red", 
     ylim = c(min(log10(MSE_EMP_ML2_N)) - 0.2, max(log10(MSE_EMP_SM2_N)) + 0.2),
     main = "Scenario 2: Varianze 1 e 4", xlab = "log10(N)", ylab = "log10(MSE)")
# Aggiungiamo la linea teorica del ML
lines(log_N, log10(MSE_TEO_ML2_N), col = "red")

# Aggiungiamo Media Semplice (Punti + Linea)
points(log_N, log10(MSE_EMP_SM2_N), pch = 16, col = "blue")
lines(log_N, log10(MSE_TEO_SM2_N), col = "blue")

# Aggiungiamo Cestino (Punti + Linea)
points(log_N, log10(MSE_EMP_SENZA2_N), pch = 16, col = "darkslategray")
lines(log_N, log10(MSE_TEO_SENZA2_N), col = "darkslategray")

# Aggiungiamo la legenda al secondo grafico
legend("topright", legend = c("ML pesato", "Media semplice", "Solo gruppo 1", "Linea Teorica"),
       col = c("red", "blue", "darkslategray", "black"), 
       pch = c(16, 16, 16, NA), lty = c(NA, NA, NA, 1), bty = "n")

# Ripristiniamo la visualizzazione singola per i futuri grafici
par(mfrow = c(1, 1))
