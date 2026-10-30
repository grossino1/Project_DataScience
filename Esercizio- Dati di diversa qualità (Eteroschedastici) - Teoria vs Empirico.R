# Esercizio: Dati di diversa qualità (Eteroschedastici) - Teoria vs Empirico

# Settiamo la cartella di lavoro:
setwd("/Users/jackross/Desktop/Data Science/Esercizi_R")
# Vediamo la WD Corrente:
getwd()

set.seed(450) # Per 

# 1. PARAMETRI GENERALI DEL MODELLO
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

# CALCOLO DEGLI MSE EMPIRICI TRAMITE SIMULAZIONE MONTE CARLO
# Testiamo diversi valori di iterazioni
N_MC_TOTALI <- c(10, 100, 1000, 10000)
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

plot(N_MC_TOTALI, MSE_EMP_ML1_TOTALI, 
     type = "b",
     main = "Convergenza ML all'aumentare delle prove nello Scenario 1",
     xlab = "Numero di Prove Monte Carlo",
     ylab = "MSE Medio Empirico")

abline(h = MSE_TEO_ML1, col = "red", "MSE Teorico ML Scenario 1")
