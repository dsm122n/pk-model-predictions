install.packages("httpgd")
install.packages("tidyr")
install.packages("gridExtra")
install.packages("forcats")
install.packages("pracma")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("minpack.lm")

library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(forcats)
library(pracma)
library(httpgd)
library(minpack.lm)





# ChatGPT
# cargar el paquete minpack.lm
library(minpack.lm)

# datos experimentales
t <- c(0.25, 0.5, 1, 2, 4, 6, 8)
C <- c(12.2, 9.7, 6.4, 2.8, 0.9, 0.4, 0.2)
gpt <- tibble(tiempo = t, concentracion = C)
t_completa <- seq(0, 8, 0.1)
gpt_hat <- tibble(tiempo = t_completa)
    mutate(concentracion = model(tiempo, 14.8365, 0.8265))

ggplot()+
  geom_line(data = gpt, aes(x=tiempo, y=concentracion),size = 0.7, col="#0476c2")+
  # geom_point(data = gpt, aes(x=tiempo, y=concentracion),size = 2, col="#000000")+
  # geom_line(data = gpt_hat, aes(x=tiempo, y=concentracion),size = 0.7, col="#113b19")+
  theme_bw()+
  guides(title=NULL)+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.text = element_text(size = 8))


# definir el modelo de eliminación de primer orden
model <- function(t, C0, k) {
  C0 * exp(-k * t)
}

# ajustar el modelo a los datos experimentales
fit <- nlsLM(C ~ model(t, C0, k), start = list(C0 = max(C), k = 0.1))

# ver los resultados del ajuste
summary(fit)





# cargar el paquete minpack.lm

# definir el modelo farmacocinético

model_prueba <- function(a, b, c, t, t13) {
  
  c[t <= t13] <- a+b+c + t[t>t13]
  c[t > t13] <- a+b
  return(c)
}
model_prueba(a = 1, b = 2, c = 3, t = c(1:10), t13 = 5)

model(t = c(0:60), V = 4, Cl = 5, t13 = 30, R = 3)



library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(forcats)
library(pracma)
library(httpgd)
library(minpack.lm)

ejemplo_tibble <- tibble(
  tiempo = c(seq(0, 60, 5), seq(0, 60, 5), seq(0, 60, 5)),
  concentracion = c(0.1, 2, 2.5, 2.9, 3, 3, 3, 1.57, 1.15, 1.03, 1.01, 1, 1,
                    0.1+0.1, 2+0.1, 2.5+0.1, 2.9+0.1, 3+0.1, 3+0.1, 3+0.1, 1.57+0.1, 1.15+0.1, 1.03+0.1, 1.01+0.1, 1+0.1, 1+1,
                    0.1-0.2, 2-0.2, 2.5-0.2, 2.9-0.2, 3-0.2, 3-0.2, 3-0.2, 1.57-0.2, 1.15-0.2, 1.03-0.2, 1.01-0.2, 1-0.2, 1+2),
                      
)
ejemplo <- ejemplo_tibble$concentracion
model <- function(V, Cl) {
  k <- Cl / V
  t <- rep(seq(0, 60, 5), 3)
  t13 <- 30
  R <- 3
  conc <- t
  conc[t <= t13] <- (R/Cl) * (1 - exp(-k * t[t<=t13]))

  conc[t > t13] <- 
                  (R/Cl) * (1-exp(-k*(t13))) * exp(-k*((t[t > t13] - t13))) +
                  (R/3/Cl) * (1-exp(-k*(t[t > t13] - t13)))  
  return(conc)
}
model_pk <- function(V, Cl, t, t13, R) {
  k <- Cl / V

  conc <- t
  conc[t <= t13] <- (R/Cl) * (1 - exp(-k * t[t<=t13]))

  conc[t > t13] <- 
                  (R/Cl) * (1-exp(-k*(t13))) * exp(-k*((t[t > t13] - t13))) +
                  (R/3/Cl) * (1-exp(-k*(t[t > t13] - t13)))  
  return(conc)
}

# ajustar el modelo a los datos experimentales
fit <- nlsLM(ejemplo ~ model(V, Cl), start = list(V = 80, Cl = 5))
nlsLM(ejemplo ~ model(V, Cl), start = list(V = 80, Cl = 5))
nlsLM(ejemplo ~ model(V, Cl), start = list(V = 10, Cl = 1))
# ver los resultados del ajuste
summary(fit)



ct_hat <- tibble(tiempo = c(0:60)) %>%
    mutate(concentracion = model_pk(t = c(0:60), V = 4, Cl = 1, t13 = 30, R = 3))
ct_hat_2 <- tibble(tiempo = c(0:60)) %>%
    mutate(concentracion = model_pk(t = c(0:60), V = 4.613, Cl = 1.005, t13 = 30, R = 3))
View(ct_hat)
ggplot()+
  #geom_line(data = ct_hat, aes(x=tiempo, y=concentracion),size = 0.7, col="#0476c2")+
  geom_line(data = ct_hat_2, aes(x=tiempo, y=concentracion),size = 0.7, col="#c20404")+
  geom_point(data = ejemplo_tibble, aes(x=tiempo, y=concentracion),size = 2, col="#000000")+
  # geom_line(data = gpt_hat, aes(x=tiempo, y=concentracion),size = 0.7, col="#113b19")+
  theme_bw()+
  guides(title=NULL)+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.text = element_text(size = 8))



# datos de concentraciones plasmáticas y tiempos para 5 sujetos
datos <- list(conc_sujeto1, conc_sujeto2, conc_sujeto3, conc_sujeto4, conc_sujeto5)
tiempo <- tiempo_comun_a_todos_los_sujetos

# ajustar el modelo a los datos para cada sujeto
ajuste_sujetos <- list()
for (i in 1:5) {
  fit <- nlsLM(datos[[i]] ~ model(tiempo, V, Cl, t13, R),
               start = list(V = 20, Cl = 0.2, t13 = 2, R = 10),
               control = nls.lm.control(maxiter = 100))
  ajuste_sujetos[[i]] <- fit
}

# ver los resultados del ajuste para cada sujeto
for (i in 1:5) {
  print(summary(ajuste_sujetos[[i]]))
}










# mg/ml * ml/min * 60min/h / 176.12g/mol  / 1000
2250/250*3*60/176.12



modelo_ajustar <- function(V, Cl) {
  k <- Cl / V
  t <- c(0, 15, 30, 60, 90, 120, 180)
  t13 <- 90
  R <- 9.198274
  conc <- t
  conc[t <= t13] <- (R/Cl) * (1 - exp(-k * t[t<=t13]/60))

  conc[t > t13] <- 
                  (R/Cl) * (1-exp(-k*(t13/60))) * exp(-k*(t[t > t13] - t13)/60) +
                  (0/Cl) * (1-exp(-k*(t[t > t13] - t13)/60))  
  return(conc)
}


modelo_simulacion <- function(V, Cl, t, t13, R) {
  k <- Cl / V

  conc <- t
  conc[t <= t13] <- (R/Cl) * (1 - exp(-k * t[t<=t13]/60))

  conc[t > t13] <- 
                  (R/Cl) * (1-exp(-k*(t13/60))) * exp(-k*((t[t > t13] - t13)/60)) +
                  (0/Cl) * (1-exp(-k*(t[t > t13] - t13)/60))  
  return(conc)
}

modelo_ajustar_cph <- function(V, Cl, Cph) {
  k <- Cl / V
  t <- c(0, 15, 30, 60, 90, 120, 180)
  t13 <- 90
  R <- 9.198274
  conc <- t
  conc[t <= t13] <- (R/Cl) * (1 - exp(-k * t[t<=t13]/60)) + Cph

  conc[t > t13] <- 
                  (R/Cl) * (1-exp(-k*(t13/60))) * exp(-k*(t[t > t13] - t13)/60) +
                  (0/Cl) * (1-exp(-k*(t[t > t13] - t13)/60))  + Cph
  return(conc)
}


modelo_simulacion <- function(V, Cl, t, t13, R) {
  k <- Cl / V

  conc <- t
  conc[t <= t13] <- (R/Cl) * (1 - exp(-k * t[t<=t13]/60))

  conc[t > t13] <- 
                  (R/Cl) * (1-exp(-k*(t13/60))) * exp(-k*((t[t > t13] - t13)/60)) +
                  (0/Cl) * (1-exp(-k*(t[t > t13] - t13)/60))  
  return(conc)
}


read.csv("RAW_HPLC_VC_Mean.csv")

# paciente 10
(concentracion_p10 <- read.csv("RAW_HPLC_VC_Mean.csv")$P10)
concentracion_p10 <- concentracion_p10 / 1000
nlsLM(concentracion_p10 ~ modelo_ajustar(V, Cl), start = list(V = 6, Cl = 5))$coefficients("V")
fit <- nlsLM(concentracion_p10 ~ modelo_ajustar(V, Cl), start = list(V = 6, Cl = 5))
fit$coefficients[V]

summary(fit)$coefficients[1,1] # para obtener el valor de V
summary(fit)$coefficients[2,1] # para obtener el valor de Cl
summary(fit)$coefficients[1,4] # para obtener el valor de p-value para V
summary(fit)$coefficients[2,4] # para obtener el valor de p-value para Cl



summary(fit)$coefficients[4]
summary(fit)$coefficients["V"]
cat("Valor del parametro V:", fit$coef["V"], "\n")
nlsLM(concentracion_p10 ~ modelo_ajustar_cph(V, Cl, Cph), start = list(V = 6, Cl = 5, Cph = 0.6))$coef[V]
fit <- nlsLM(concentracion_p10 ~ modelo_ajustar(V, Cl), start = list(V = 50, Cl = 1))
# ver los resultados del ajuste
View(fit)

ct_hat_10 <- tibble(tiempo = c(0:180)) %>%
    mutate(concentracion = modelo_simulacion(t = c(0:180), V = 6.774, Cl = 5.909, t13 = 90, R = 9.198274))
ct_hat_10_cph <- tibble(tiempo = c(0:180)) %>%
    mutate(concentracion = modelo_simulacion(t = c(0:180), V = 7.7682, Cl = 7.3511, t13 = 90, R = 9.198274) + 0.1333)
ct_real_10 <- tibble(tiempo = c(0, 15, 30, 60, 90, 120, 180),
                  concentracion = concentracion_p10
                  )

# paciente 11
(concentracion_p11 <- read.csv("RAW_HPLC_VC_Mean.csv")$P11)
concentracion_p11 <- concentracion_p11 / 1000
nlsLM(concentracion_p11 ~ modelo_ajustar(V, Cl), start = list(V = 7, Cl = 6))
fit <- nlsLM(concentracion_p11 ~ modelo_ajustar(V, Cl), start = list(V = 7, Cl = 6))
# ver los resultados del ajuste
View(fit)

ct_hat_11 <- tibble(tiempo = c(0:180)) %>%
    mutate(concentracion = modelo_simulacion(t = c(0:180), V = 14.233, Cl = 7.534, t13 = 90, R = 9.198274))
ct_real_11 <- tibble(tiempo = c(0, 15, 30, 60, 90, 120, 180),
                  concentracion = concentracion_p11
                  )

# paciente 15
(concentracion_p15 <- read.csv("RAW_HPLC_VC_Mean.csv")$P15)
concentracion_p15 <- concentracion_p15 / 1000
nlsLM(concentracion_p15 ~ modelo_ajustar(V, Cl), start = list(V = 6, Cl = 5))
fit <- nlsLM(concentracion_p15 ~ modelo_ajustar(V, Cl), start = list(V = 50, Cl = 1))
# ver los resultados del ajuste
View(fit)

ct_hat_15 <- tibble(tiempo = c(0:180)) %>%
    mutate(concentracion = modelo_simulacion(t = c(0:180), V = 8.604, Cl = 7.354, t13 = 90, R = 9.198274))
ct_real_15 <- tibble(tiempo = c(0, 15, 30, 60, 90, 120, 180),
                  concentracion = concentracion_p15
                  )

# paciente 14
(concentracion_p14 <- read.csv("RAW_HPLC_VC_Mean.csv")$P14)
concentracion_p14 <- concentracion_p14 / 1000
nlsLM(concentracion_p14 ~ modelo_ajustar(V, Cl), start = list(V = 6, Cl = 5))
fit <- nlsLM(concentracion_p14 ~ modelo_ajustar(V, Cl), start = list(V = 50, Cl = 1))
# ver los resultados del ajuste
View(fit)

ct_hat_14 <- tibble(tiempo = c(0:180)) %>%
    mutate(concentracion = modelo_simulacion(t = c(0:180), V = 11.797, Cl = 9.369, t13 = 90, R = 9.198274))
ct_real_14 <- tibble(tiempo = c(0, 15, 30, 60, 90, 120, 180),
                  concentracion = concentracion_p14
                  )

# paciente 17
(concentracion_p17 <- read.csv("RAW_HPLC_VC_Mean.csv")$P17)
concentracion_p17 <- concentracion_p17 / 1000
nlsLM(concentracion_p17 ~ modelo_ajustar(V, Cl), start = list(V = 6, Cl = 5))
fit <- nlsLM(concentracion_p17 ~ modelo_ajustar(V, Cl), start = list(V = 50, Cl = 1))
# ver los resultados del ajuste
View(fit)

ct_hat_17 <- tibble(tiempo = c(0:180)) %>%
    mutate(concentracion = modelo_simulacion(t = c(0:180), V = 11.84, Cl = 11.94, t13 = 90, R = 9.198274))
ct_real_17 <- tibble(tiempo = c(0, 15, 30, 60, 90, 120, 180),
                  concentracion = concentracion_p17
                  )

# paciente 18
(concentracion_p18 <- read.csv("RAW_HPLC_VC_Mean.csv")$P18)
concentracion_p18 <- concentracion_p18 / 1000
nlsLM(concentracion_p18 ~ modelo_ajustar(V, Cl), start = list(V = 6, Cl = 5))
fit <- nlsLM(concentracion_p18 ~ modelo_ajustar(V, Cl), start = list(V = 50, Cl = 1))
# ver los resultados del ajuste
View(fit)

ct_hat_18 <- tibble(tiempo = c(0:180)) %>%
    mutate(concentracion = modelo_simulacion(t = c(0:180), V = 11.795, Cl = 9.557, t13 = 90, R = 9.198274))
ct_real_18 <- tibble(tiempo = c(0, 15, 30, 60, 90, 120, 180),
                  concentracion = concentracion_p18
                  )








ggplot()+
  geom_line( data =  ct_hat_10, aes(x=tiempo, y=concentracion),size = 0.7, col="#000000")+
  geom_line( data =  ct_hat_10_cph, aes(x=tiempo, y=concentracion),size = 0.7, col="#0476c2")+
  geom_point(data = ct_real_10, aes(x=tiempo, y=concentracion),size = 2, col="#0476c2", alpha = 0.8)+
  geom_line( data =  ct_hat_11, aes(x=tiempo, y=concentracion),size = 0.7, col="#104713")+
  geom_point(data = ct_real_11, aes(x=tiempo, y=concentracion),size = 2, col="#104713", alpha = 0.8)+
  geom_line( data =  ct_hat_14, aes(x=tiempo, y=concentracion),size = 0.7, col="#042dc2")+
  geom_point(data = ct_real_14, aes(x=tiempo, y=concentracion),size = 2, col="#042dc2", alpha = 0.8)+
  geom_line( data =  ct_hat_15, aes(x=tiempo, y=concentracion),size = 0.7, col="#c23a04")+
  geom_point(data = ct_real_15, aes(x=tiempo, y=concentracion),size = 2, col="#c23a04", alpha = 0.8)+
  geom_line( data =  ct_hat_17, aes(x=tiempo, y=concentracion),size = 0.7, col="#6604c2")+
  geom_point(data = ct_real_17, aes(x=tiempo, y=concentracion),size = 2, col="#6604c2", alpha = 0.8)+
  geom_line( data =  ct_hat_18, aes(x=tiempo, y=concentracion),size = 0.7, col="#c28604")+
  geom_point(data = ct_real_18, aes(x=tiempo, y=concentracion),size = 2, col="#c28604", alpha = 0.8)+
  # geom_line(data = gpt_hat, aes(x=tiempo, y=concentracion),size = 0.7, col="#113b19")+
  theme_bw()+
  guides(title=NULL)+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.text = element_text(size = 8))
ggsave("primer estimado.pdf", width = 15, height = 15, units = c("cm"))















# parámetros PK que hay que calcular
# ***Clearance (CL=k*V)
# ***Volumen de distribución
# *Constante de eliminación (k=V/CL)
# Vida media (se calcula en base a constante de eliminación)
# ABC = Dosis / CL o CL = Dosis/ABC = R*t / ABC

Simul1Comp <- function(V, CL, tD, R, Cph=0){
  
  tD0 <- c(0,tD) #adding tD instant 0 
  
  Tinf <- c(0) #infusion time
  pos0 <- 1
  time_tot <- tD [length(tD)]
  for(i in tD){
    Tinf [pos0] <- i - tD0[pos0]
    pos0 <- pos0 + 1
  }
  
  Ct <- tibble(Time=c(0:(time_tot*60)),
                Concentration=0) #defining output tibble in terms of time in [min] and concentration [mM]
  # Parameters definition
  k <- CL/V
  
  # Decay Summation terms calculation
  summation <- tibble(Time = c(0:(time_tot*60)))
  pos1 <- 1
  tD_aux <- tD [-length(tD)]
  for (i in tD_aux) {
    summation <- summation %>% 
      mutate(other=0)
    colnames(summation)[which(names(summation)=="other")] <- 
      paste("decay_D", pos1, sep = ".")
    t1 <- i*60
    t1 <- trunc(t1)
    intervalo_ti.tn <- summation$Time[t1+1]:summation$Time[length(summation$Time)]
    for (j in intervalo_ti.tn) {
      t <- j/60
      summation[j+1, pos1+1] <- R[pos1]/CL * (1 - exp(-k*Tinf[pos1])) * exp(-k*(t-i)) 
    }
    pos1 <- pos1 + 1
  }
  sigma <- summation %>%
    select(contains("decay_D")) %>%
    rowSums()
  summation <- summation %>%
    transmute(decay_accumulated=sigma) 
  
  # final curve
  pos2 <- 1
  for (i in tD) {              # intervals according to end time tD[i] of infusion R[i]
    aux <- (tD0[pos2]*60):(i*60)
    
    for (j in aux) {            # j = time [min] 
      t <- j/60              # t: time [h]
      taux <- tD0[pos2]      # taux: time since the end of previous infusion 
      
      Ct$Concentration [j+1] <- (R[pos2]/CL) * (1-exp(-k*(t-taux)))+
        summation$decay_accumulated[j+1] + Cph

    }
    pos2 <- pos2 + 1
  }
  return(Ct)
}
# mala
Simul1Comp <- function(V, CL, tD, R, Cph=0){
  
  tD0 <- c(0,tD) #adding tD instant 0 
  
  Tinf <- c(0) #infusion time
  
  time_tot <- tD [length(tD)]

  pos0 <- 1
  for(i in tD){
    Tinf [pos0] <- i - tD0[pos0]
    pos0 <- pos0 + 1
  }
  
  Ct <- tibble(Time=c(0:(time_tot*60)),
                Concentration=0) #defining output tibble in terms of time in [min] and concentration [mM]
  # Parameters definition
  k <- CL/V
  
  # Decay Summation terms calculation
  summation <- tibble(Time = c(0:(time_tot*60)))
  
  pos1 <- 1
  tD_aux <- tD [-length(tD)]
  for (i in tD_aux) {
    summation <- summation %>% 
      mutate(other=0)
    colnames(summation)[which(names(summation)=="other")] <- 
      paste("decay_D", pos1, sep = ".")
    t1 <- i*60
    t1 <- trunc(t1)
    intervalo_ti.tn <- summation$Time[t1+1]:summation$Time[length(summation$Time)]
    for (j in intervalo_ti.tn) {
      t <- j/60
      summation[j+1, pos1+1] <- R[pos1]/CL * (1 - exp(-k*Tinf[pos1])) * exp(-k*(t-i)) 
    }
    pos1 <- pos1 + 1
  }
  pos2 <- 1
  for (i in tD) {              # intervals according to end time tD[i] of infusion R[i]
    aux <- (tD0[pos2]*60):(i*60)
    
    for (j in aux) {            # j = time [min] 
      t <- j/60              # t: time [h]
      taux <- tD0[pos2]      # taux: time since the end of previous infusion 
      
      Ct$Concentration [j+1] <- (R[pos2]/CL) * (1-exp(-k*(t-taux)))+
        summation$decay_accumulated[j+1] + Cph

    }
    pos2 <- pos2 + 1
  }
  return(Ct)
}
Simul2Comp <- function(V, V2, CL, Q, tD, R, Cph=0){
  
  tD0 <- c(0,tD) 
  
  Tinf <- c(0) 
  pos0 <- 1
  time_tot <- tD [length(tD)]
  for(i in tD){
    Tinf [pos0] <- i - tD0[pos0]
    pos0 <- pos0 + 1
  }
  
  Ct <- tibble(Time=c(0:(time_tot*60)),
                   Concentration=0)

  # Parameters definition
  terms <- (Q/V) + (Q/V2) + (CL/V)
  beta <- 0.5 * (terms - sqrt(terms^2 - 4*(Q/V2)*(CL/V)))
  alpha <- ((Q/V2)*(CL/V))/beta
  A <- (1/V) * (alpha - (Q/V2))/(alpha-beta)
  B <- (1/V) * ((beta - Q/V2)/(beta-alpha))
  
  summation <- tibble(Time = c(0:(time_tot*60)))
  pos1 <- 1
  tD_aux <- tD [-length(tD)]
  for (i in tD_aux) {
    summation <- summation %>% 
      mutate(other=0)

    colnames(summation)[which(names(summation)=="other")] <- 
      paste("decay_D", pos1, sep = ".")
    t1 <- i*60
    t1 <- trunc(t1)
    intervalo_ti.tn <- summation$Time[t1+1]:summation$Time[length(summation$Time)]
    for (j in intervalo_ti.tn) {
      t <- j/60
      summation[j+1, pos1+1] <- R[pos1] * (
        (A/alpha) * (1 - exp(-alpha*Tinf[pos1])) * exp(-alpha*(t-i)) + 
          (B/beta) * (1 - exp(-beta*Tinf[pos1])) * exp(-beta*(t-i)))
    }
    pos1 <- pos1 + 1
  }
  sigma <- summation %>%
    select(contains("decay_D")) %>%
    rowSums()
  summation <- summation %>%
    transmute(decay_accumulated=sigma) 
  
  pos2 <- 1
  for (i in tD) {
    aux <- (tD0[pos2]*60):(i*60)
    
    for (j in aux) {
      t <- j/60
      taux <- tD0[pos2]
      Ct$Concentration [j+1] <- R[pos2] * 
        ((A/alpha) * (1-exp(-alpha*(t-taux))) + 
           (B/beta) * (1-exp(-beta*(t-taux)))) +
        summation$decay_accumulated[j+1] +
        Cph
    }
    pos2 <- pos2 + 1
  }
  return(Ct)
}

Simul2Comp(V=5, V2=10, CL=5, Q=5, 
        tD = c(0.5,1,2),
        R = c(0.1, 0.5, 0.4),
        Cph=0
        )


Simul3Comp <- function(V1, V2, V3, CL, Q2, Q3, tD, R, Cph=0){
  tD0 <- c(0,tD)
  
  Tinf <- c(0)
  pos0 <- 1
  time_tot <- tD [length(tD)]
  for(i in tD){
    Tinf [pos0] <- i - tD0[pos0]
    pos0 <- pos0 + 1
  }
  
  Ct <- tibble(Time=c(0:(time_tot*60)),
                   Concentration=0)
  
  # Parameters definition
  a0 <- (CL*Q2*Q3)/(V1*V2*V3)
  a1 <- (CL*Q3)/(V1*V3) + (Q2*Q3)/(V2*V3) + (Q2*Q3)/(V2*V1) + 
    (CL*Q2)/(V1*V2) + (Q3*Q2)/(V3*V1)
  a2 <- CL/V1 + Q2/V1 + Q3/V1 + Q2/V2 + Q3/V3
  
  p <- a1 - (a2^2)/3
  q <- 2*(a2^3)/27 - a1*a2/3 + a0
  r1 <- sqrt(-(p^3)/27)
  r2 <- 2*r1^(1/3)
  phi <- acos(-q/(2*r1))/3

  alpha <- -(cos(phi)*r2 - a2/3)
  beta <- -(cos(phi + 2*pi/3)*r2 - a2/3)
  gamma <- -(cos(phi + 4*pi/3)*r2 - a2/3)
  

  A <- 1/V1 * (Q2/V2 - alpha)/(alpha-beta) * (Q3/V3 - alpha)/(alpha-gamma)
  B <- 1/V1 * (Q2/V2 - beta)/(beta-alpha) * (Q3/V3 - beta)/(beta-gamma)
  C <- 1/V1 * (Q2/V2 - gamma)/(gamma-beta) * (Q3/V3 - gamma)/(gamma-alpha)
  
  summation <- tibble(Time = c(0:(time_tot*60)))
  pos1 <- 1
  tD_aux <- tD [-length(tD)]
  for (i in tD_aux) {
    summation <- summation %>% 
      mutate(other=0)
    colnames(summation)[which(names(summation)=="other")] <- 
      paste("decay_D", pos1, sep = ".")
    t1 <- i*60
    t1 <- trunc(t1)
    intervalo_ti.tn <- summation$Time[t1+1]:summation$Time[length(summation$Time)]
    for (j in intervalo_ti.tn) {
      t <- j/60
      summation[j+1, pos1+1] <- R[pos1] * 
        ((A/alpha)*(1 - exp(-alpha*Tinf[pos1]))*exp(-alpha*(t-i))+
           (B/beta)*(1 - exp(-beta*Tinf[pos1]))*exp(-beta*(t-i))+
           (C/gamma)*(1 - exp(-gamma*Tinf[pos1]))*exp(-gamma*(t-i))) 
    }
    pos1 <- pos1 + 1
  }
  sigma <- summation %>%
    select(contains("decay_D")) %>%
    rowSums()
  summation <- summation %>%
    transmute(decay_accumulated=sigma) 
  
  pos2 <- 1
  for (i in tD) {
    aux <- (tD0[pos2]*60):(i*60)
    
    for (j in aux) {
      t <- j/60
      taux <- tD0[pos2]
      Ct$Concentration [j+1] <- R[pos2] * 
        ((A/alpha) * (1-exp(-alpha*(t-taux))) + 
           (B/beta) * (1-exp(-beta*(t-taux))) + 
           (C/gamma) * (1-exp(-gamma*(t-taux)))) +
        summation$decay_accumulated[j+1] + Cph
    }
    pos2 <- pos2 + 1
  }
  
  return(Ct)
}
pos0 <- 1
   while(pos0 < 4){
    print("holis")
   pos0 <- pos0 + 1
   }



# para algoritmo de LM, x debe ser un vector con los parámetros a estimar 
CtFco1  <- function(x){
  
  V <- x[1]
  CL <- x[2]

  #parámetros secundarios
  k <- CL/V


  # definir infusiones
  # Vector que contenga los tiempos en h de cambio de velocidad de infusión
  DuracionR  <- c(2, 5.4, 6)
  # Vector que contenga las velocidades de infusión en "mmol/h" de las distintas velocidades de infusión
  R <- c(6,3,5)
  
  # tiempo a evaluar 
  Tx <- 5.5
  
  #concentración fisiológica basal
  Cph <- 0
  
  # cortando tiempos post Tx
  tiempos <- c(0, DuracionR)
  # TiemposR <- c(0, DuracionR)
  # pos0 <- 1
  # for(i in TiemposR){
  #   
  #   if(Tx <= i){
  #     TiemposR <- TiemposR[-c((pos0):(length(TiemposR)))]
  #     break
  #   }
  #   pos0 <- pos0 + 1
# 
  # }
  # TiemposR[3]
  #Concentraciones decaimientos en Tx
  Decaimiento <- 0
  pos1 <- 1
  var <- tiempos[pos1+1]
  #Revisar
  while (var < Tx){
    Decaimiento <- Decaimiento + R[pos1]/CL * (1 - exp(-k*(tiempos[pos1 + 1]-tiempos[pos1]))) * exp(-k*(Tx-tiempos[pos1 + 1])) 
    
    pos1 <- pos1 + 1
    var <- tiempos[pos1]
  }
  #     }
  # }
  # for(i in DuracionR){
  #   Decaimiento <- Decaimiento + R[pos1]/CL * (1 - exp(-k*(i-TiemposR[pos1]))) * exp(-k*(Tx-i)) 
  #   pos1 <- pos1 + 1
  # }

  #valor final (Revisar, qué pasa si R final=0)(debería sumarse 0 a la suma de decaimiento)
  CTx <- Decaimiento + R[length(R)] / CL * (1 - exp(-k*(Tx-DuracionR[length(R)-1]))) + Cph

  return(Decaimiento)
}
CtFco1(x = c(2,5))
concentracion_fco  <- function(x){
  
  V <- x[1]
  CL <- x[2]

  #parámetros secundarios
  k <- CL/V


  # definir infusiones
  # Vector que contenga los tiempos en h de cambio de velocidad de infusión
  DuracionR  <- c(2, 5.4, 6)
  # Vector que contenga las velocidades de infusión en "mmol/h" de las distintas velocidades de infusión
  R <- c(6,3,5)
  
  # tiempo a evaluar 
  Tx <- 5
  
  #concentración fisiológica basal
  Cph <- 0
  
  # cortando tiempos post Tx
  tiempos <- c(0, DuracionR)
  
  #Concentraciones decaimientos en Tx
  Decaimiento <- 0
  pos1 <- 1
  var <- tiempos[pos1+1]
  while (var < Tx){
    Decaimiento <- Decaimiento + R[pos1]/CL * (1 - exp(-k*(tiempos[pos1 + 1]-tiempos[pos1]))) * exp(-k*(Tx-tiempos[pos1 + 1])) 
    
    pos1 <- pos1 + 1
    var <- tiempos[pos1]
  }

  #valor final (Revisar, qué pasa si R final=0)(debería sumarse 0 a la suma de decaimiento)
  CTx <- Decaimiento + R[length(R)] / CL * (1 - exp(-k*(Tx-DuracionR[length(R)-1]))) + Cph

  return(CTx)
}
concentracion_fco(x = c(2,5))
View(Simul1Comp(V = 2, CL = 5, tD=c(2, 5.4, 6), R = c(6,3,5)))


i <- 1
is.true(i)

CtFco1Arg  <- function(V, CL, DuracionR, R, Tx, Cph=0){
  
  #parámetros secundarios
  k <- CL/V
 
  # cortando tiempos post Tx
  TiemposR <- c(0, DuracionR)
  pos0 <- 1
  for(i in TiemposR){
    
    if(Tx < i){
      TiemposR <- TiemposR[-c((pos0):(length(TiemposR)))]
      R <- R[-c((pos0):length(R))]
      break
    }
    pos0 <- pos0 + 1

  }

  #Concentraciones decaimientos en Tx
  Decaimiento <- 0
  #pos1 <- 1
  #Revisar ARREGLAAAAAR CRANEAAAARLAAAAAAAA ESTO ESTÁ DANDO EL ERROR EN LA LINEA 377 APROX
  pos0 <- 1
  #Revisar
  while(TiemposR[pos0 + 1] < Tx){
    Decaimiento <- Decaimiento + R[pos0]/CL * (1 - exp(-k*(TiemposR[pos0 + 1]-TiemposR[pos0]))) * exp(-k*(Tx-TiemposR[pos0 + 1])) 
    pos0 <- pos0 + 1
  }
  
  # for(i in 1:length(TiemposR)){
  #   Decaimiento <- Decaimiento + R[i]/CL * (1 - exp(-k*(TiemposR[i]-TiemposR[i-1]))) * exp(-k*(Tx-TiemposR[i]))
  #   #pos1 <- pos1 + 1
  # }

  #valor final (Revisar, qué pasa si R final=0)(debería sumarse 0 a la suma de decaimiento)
  CTx <- Decaimiento + R[length(R)] / CL * (1 - exp(-k*(Tx-TiemposR[length(TiemposR)]))) + Cph

  return(CTx)
}


View(Simul1Comp(V = 2, CL = 5, tD = c(2,5,6),R = c(6,3,0))) 

# función suma de los cuadrados de los residuos. Input: tibble con valores empíricos y tibble con valores de simulación
# tibble [[c]][f] extrae valor de columna c y fila f 
RSS <- function(DatosEmpiricos, func){
  sum <- 0
  #pos0 <- 1

  for (i in 1:length(DatosEmpiricos[[1]])){
    CHat <- func(DatosEmpiricos[[1]][i])
    sum <- sum + (DatosEmpiricos[[2]][i]-CHat)^2
    #pos0 <- pos0 + 1
  }
  return(sum)
}

  f <- function(x){
    x
    return(x)
  }
(tib <- tibble(time_min=0:10, p1=c(0.1, 0, 2.5, 3.4, 3.5, 5.1, 6.1, 6.9, 7, 9, 10)))
(tib <- tibble(time_min=0:10, p1=0:10))
RSS(tib,f)

#función de residuos para llevar a LM
(jeje <- tibble(read.csv("paraprobar.csv")))
RssAMinimizar <- function(x){
  Vx <- x[1] 
  CLx <- x[2] 
  
  # definir infusiones
  # Vector que contenga los tiempos en h de cambio de velocidad de infusión
  DuracionRx  <- c(0.5, 1.5, 3)
  # Vector que contenga las velocidades de infusión en "mmol/h" de las distintas velocidades de infusión
  Rx <- c(20.2, 3.37, 0)
  
  #Concentración fisiológica
  Cphx <- jeje[[2]][1]

  sum <- 0

  # for (i in 1:length(jeje[[1]])){
  #   j <- jeje[[1]][i]
  #   CHat <- func()
  #   sum <- sum + (jeje[[2]][i]-CHat)^2
  # }
  pos0 <- 1

  for (i in jeje[[1]]){
    
    CHat <- CtFco1Arg(V = Vx, CL = CLx, 
                  DuracionR = DuracionRx, R = Rx, 
                  Tx = i/60, Cph=Cphx)
    sum <- sum + (jeje[[2]][i]-CHat)^2
    pos0 <- pos0 + 1
  }
  return(sum)
  
}
sum <- 0
pos0 <- 1
for (i in jeje[[1]]){
  
  CHat <- CtFco1Arg(V = 5, CL = 5, 
                  DuracionR = c(0.5, 1.5, 3), R = c(20.2, 3.37, 0), 
                  Tx = i/60, Cph=0.83)
  sum <- sum + (jeje[[2]][pos0]-CHat)^2
  pos0 <- pos0 + 1
}
sum
CtFco1Arg(V = 5, CL = 5, 
                  DuracionR = c(0.5, 1.5, 3), R = c(20.2, 3.37, 0), 
                  Tx = 2, Cph=0.83)
RssAMinimizar(x=c(5, 5))

# Levenber-Marquardt
# tolerance = t, lambda = l 
# x0 is a vector containing initial arguments for function f 
LM  <- function(f, x0, t, l=10, r=10) { 
    
    
    x <- x0
    k <- 0
    while (TRUE) {
      H <- hessian(f, x)
      G <- grad(f, x)
      dk <- inv(H + l * diag(nrow(H))) %*% G  
      # dk <- solve(H + l * diag(nrow(H)), G)
      x1 <- x - dk   # update rule
      print("---------------------------------------------------", quote=FALSE)
      print(c("iteration: ", k), quote = FALSE)  # iteration
      print(c("lambda: ", l), quote = FALSE) #
      print(c("x: ", x1), quote = FALSE) # x1, x2
      print(c("grad f(x): ", G), quote = FALSE)  # grad f(x)
      print(c("d: ", dk), quote = FALSE) # d1, d2
      if (Norm(G) < t) break
      l <- ifelse(f(x1) < f(x), l / r, l * r)
      k <- k + 1
      x <- x1 # update the old point 
    }
}
rep(0,2)

LM(f = RssAMinimizar,
   x0 = c(5,5),
   t = 1e-3,
   l = 10,
   r = 10 )
hessian(f = RssAMinimizar,
   x0 = c(5,5))
LM(f = Conc1CompHat,
   x0 = c(,0,0,0,0,0),
   t = 1e-3,
   l = 10,
   r = 10 )

#probando LM
f <- function(x) {
   return (((x[1]^2+x[2]-25)^2 + (x[1]+x[2]^2-25)^2 + x[3]*x[4])*t)
}
x0 <- rep(100,4)
LM(f, x0, t=1e-3, l=400, r=2)

f <- function(x) {
   return ((x[2] - exp(x[1]-100)))
}
x0 <- c(20,100)
LM(f, x0, t=1e-3, l=40, r=2)

x0 <- rep(100,2)










###########################################################
# Graficando

(PKdata <- tibble(read.csv2("PKdata.csv")))




(inf1 <- tibble(read.csv("inf_1_data.csv")))
# adding median to inf1
(inf1 <- mutate(inf1, me = 0))

for(i in 1:7){
    data <- c(
        inf1$p1[i],
        inf1$p2[i],
        inf1$p3[i],
        inf1$p4[i],
        inf1$p5[i],
        inf1$p6[i]
        )

    inf1$me[i] <- unname(quantile(data, 0.5,na.rm = T))

}

# adding Q1 to inf1
(inf1 <- mutate(inf1, q1=0))

for(i in 1:7){
    data <- c(
        inf1$p1[i],
        inf1$p2[i],
        inf1$p3[i],
        inf1$p4[i],
        inf1$p5[i],
        inf1$p6[i]
        )

    inf1$q1[i] <- unname(quantile(data, 0.25, na.rm = T))

}

# adding Q1 to inf1
(inf1 <- mutate(inf1, q3=0))

for(i in 1:7){
    data <- c(
        inf1$p1[i],
        inf1$p2[i],
        inf1$p3[i],
        inf1$p4[i],
        inf1$p5[i],
        inf1$p6[i]
        )

    inf1$q3[i] <- unname(quantile(data, 0.75, na.rm = T))

}

inf1





ggplot() +
    geom_line(data=inf1, aes(x=time_min, y=p1), na.rm = T, col="black", size=1, alpha =1/5) + 
    geom_line(data=inf1, aes(x=time_min, y=p2), na.rm = T, col="black", size=1, alpha =1/5) + 
    geom_line(data=inf1, aes(x=time_min, y=p3), na.rm = T, col="black", size=1, alpha =1/5) +
    geom_line(data=inf1, aes(x=time_min, y=p4), na.rm = T, col="black", size=1, alpha =1/5) +
    geom_line(data=inf1, aes(x=time_min, y=p5), na.rm = T, col="black", size=1, alpha =1/5) +
#    geom_line(data=inf1, aes(x=time_min, y=p6), na.rm = T, col="black", size=1, alpha =1/10) +
    geom_line(data=inf1, aes(x=time_min, y=me), na.rm = T, col="#37ba91", size=1)+
    geom_errorbar(data=inf1, aes(x=time_min, y=me, 
                                ymin=q1, ymax=q3),
                col="#800077", size=1)+
    geom_point(data=inf1, aes(x=time_min, y=me), na.rm = T, col="black", size=4, shape="diamond")+
    labs(x="Time [min]", y="Concentration [mM]")+
    theme_bw()+
    theme(legend.position = "bottom" )



inf1_pivot <- pivot_longer(inf1, cols = 2:7, names_to = "patient", values_to = "concentration")
(inf1_pivot  <- mutate(inf1_pivot, ln_concentration = log(concentration)))

coeff <- 10
ggplot() +
    geom_boxplot(data=inf1_pivot, aes(x=time_min, y=concentration/1000, group=time_min), na.rm = T, col="#000000", fill="#81d6deda", size=0.5) +
    #geom_jitter(data=inf1_pivot, aes(x=time_min, y=concentration, group=time_min), na.rm = T, size=2, alpha = 0.5, col="blue") +
    #geom_boxplot(data=inf1_pivot, aes(x=time_min, y=ln_concentration/coeff, group=time_min), na.rm = T, col="#0000004a", fill="#949494ac", size=0.5) +
    geom_point(data=inf1_pivot, aes(x=time_min, y=concentration/1000), na.rm = T, col="#5c6bb4", size=4, shape="circle", alpha =1/2) + 
    #geom_point(data=inf1_pivot, aes(x=time_min, y=ln_concentration/coeff), na.rm = T, col="#000000", size=5, shape="circle", alpha =1/5) + 
    #geom_smooth(data=inf1_pivot, aes(x=time_min, y=concentration), na.rm = T, col="black", size=1, shape="circle", alpha =1/10) + 
    #geom_errorbar(data=inf1, aes(x=time_min, y=me, 
    #                            ymin=q1, ymax=q3),
    #            col="black", size=1)+
    #geom_point(data=inf1, aes(x=time_min, y=me), na.rm = T, col="black", size=4, shape="square")+
    #geom_line(data=inf1_pivot, aes(x=time_min, y=concentration), na.rm = T, col="#a5ebd5", size=1)+
    #geom_line(data = VcRr, aes(x=Time, y=Concentration),size = 0.7, col=rgb(127,	63,	191, maxColorValue = 255))
    
    labs(x = "Time [min]", y = "Concentration [mM]") +
    #scale_y_continuous(name = "Concentration [mM]", 
    #    sec.axis = sec_axis(~ log(.)*100, name = "ln(Concentration)", na.rm=T)) + 
    theme_bw() + 
    theme(legend.position = "bottom" ) +
    guides(title=NULL,col = guide_legend(nrow = 2))

# simulación VC
(VcRr <- Simul1Comp(PKdata$V1[1], PKdata$CL[1], 
                   tD= c(0.5, 1.5, 3),
                   R= c(20.44, 3.4, 0), 
                   0.06))
Conc1CompHat(PKdata$V1[1], PKdata$CL[1], 
                   DuracionR = c(0.5, 1.5, 3),
                   R= c(20.44, 3.4, 0), 
                   Tx = 1.111,
                   0.06)


ggplot()+
  geom_line(data = VcRr, aes(x=Time, y=Concentration),size = 0.7, col=rgb(127,	63,	191, maxColorValue = 255))+
  # geom_line(data = VcRr, aes(x=Time, y=1/5 * log(Concentration) + 0.7),size = 0.7, col="#000000")+
  geom_line(data = VcRr, aes(x=Time, y= log(Concentration*1000)/10),size = 0.7, col="#780202")+
  #scale_x_continuous(breaks = seq(0,180,30), limits = c(-15, 180), labels = seq(0,3,0.5))+
  labs(title="VC RR sol 56.78 mM", x="Tiempo [h]", y="Concentracion [mM]")+
  theme_bw()+
  guides(title=NULL)+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.text = element_text(size = 8))

VcAij <- Simul1Comp(PKdata$V1[1], PKdata$CL[1], 
          tD= c(0.5, 1.5, 3),
          R= c(7.57, 7.57, 0), 
          0.06)
VcAij <- Simul1Comp(PKdata$V1[1], PKdata$CL[1], 
          tD= c(0.5, 1.5, 3),
          R= c(7.57, 7.57, 0), 
          0.06)

ggplot()+
  geom_line(data = VcAij, aes(x=Time, y=Concentration),size = 0.7, col=rgb(127,	63,	191, maxColorValue = 255))+
  geom_line(data = VcAij, aes(x=Time, y=1/5 * log(Concentration)+0.7),size = 0.7, col="#000000")+
  scale_x_continuous(breaks = seq(0,180,30), limits = c(-15, 180), labels = seq(0,3,0.5))+
  labs(title="VC AIJ sol 45.43 mM", x="Tiempo [h]", y="Concentracion [mM]")+
  theme_bw()+
  guides(title=NULL)+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.text = element_text(size = 8))

# Simulación NAC
NacRr <- Simul3Comp(PKdata$V1[2], PKdata$V2[2], PKdata$V3[2],
                        PKdata$CL[2], PKdata$Q[2],PKdata$Q3[2],
                        tD= c(0.5, 1.5, 3),
                        R= c(17.647, 2.941, 0))



ggplot()+
  geom_line(data=NacRr, aes(x=Time, y=Concentration), size = 0.7, col=rgb(63,	191,	255, maxColorValue=255))+
  geom_line(data=NacRr, aes(x=Time, y=1/10 * (log(Concentration)+5.5)), size = 0.7, col="#004551")+
  scale_x_continuous(breaks = seq(0,180,30), limits = c(-15, 180), labels = seq(0,3,0.5))+
  labs(title="NAC RR sol 49.02 mM", x="Tiempo [h]", y="Concentracion [mM]")+
  theme_bw()+
  guides(title=NULL,col = guide_legend(nrow = 4))+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8))

NacAij <- Simul3Comp(PKdata$V1[2], PKdata$V2[2], PKdata$V3[2],
                    PKdata$CL[2], PKdata$Q[2],PKdata$Q3[2],
                    tD= c(0.5, 1.5, 3),
                    R= c(16.54, 16.54, 0))
ggplot()+
  geom_line(data=NacAij, aes(x=Time, y=Concentration), size = 0.7, col=rgb(63,	191,	255, maxColorValue=255))+
  geom_line(data=NacAij, aes(x=Time, y=1/10 * (log(Concentration)+5.5)), size = 0.7, col="#000000")+
  scale_x_continuous(breaks = seq(0,180,30), limits = c(-15, 180), labels = seq(0,3,0.5))+
  labs(title="NAC AIJ sol 99.26 mM", x="Tiempo [h]", y="Concentracion [mM]")+
  theme_bw()+
  guides(title=NULL,col = guide_legend(nrow = 4))+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8))






# Simulación funciones
tabla <- tibble(x=seq(0,10,0.001), y= exp(-x)); ggplot()+
  geom_line(data=tabla, aes(x=x, y=y))+
  theme_bw()




















  # Datos HPLC
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(gridExtra)
  library(forcats)
  library(pracma)

  (HplcVc <- tibble(read.csv("RAW_HPLC_VC.csv", check.names = FALSE, fileEncoding = 'UTF-8-BOM'), .name_repair = "minimal"))
  (HplcNac <- tibble(read.csv("RAW_HPLC_NAC.csv", check.names = FALSE, fileEncoding = 'UTF-8-BOM'), .name_repair = "minimal"))
  (HplcDfo <- tibble(read.csv("RAW_HPLC_DFO.csv", check.names = FALSE, fileEncoding = 'UTF-8-BOM'), .name_repair = "minimal"))

  hplc_vc <- HplcVc  %>%
    pivot_longer(cols = 2:18, names_to = "patient", values_to = "concentration", values_drop_na = FALSE, names_repair = "minimal")  %>%
        mutate(sol = c("")) 
  pos0 <- 1
  for (i in hplc_vc$patient){
    if(i == "Paciente 1") hplc_vc$sol[pos0]   <- "TAC 1" 
    if(i == "Paciente 2") hplc_vc$sol[pos0]   <- "P" 
    if(i == "Paciente 3") hplc_vc$sol[pos0]   <- "TAC 1" 
    if(i == "Paciente 4") hplc_vc$sol[pos0]   <- "P" 
    if(i == "Paciente 5") hplc_vc$sol[pos0]   <- "TAC 1" 
    if(i == "Paciente 6") hplc_vc$sol[pos0]   <- "TAC 1" 
    if(i == "Paciente 7") hplc_vc$sol[pos0]   <- "P" 
    if(i == "Paciente 8") hplc_vc$sol[pos0]   <- "TAC 1" 
    if(i == "Paciente 9") hplc_vc$sol[pos0]   <- "TAC 1" 
    if(i == "Paciente 10") hplc_vc$sol[pos0]  <- "TAC 2"
    if(i == "Paciente 11") hplc_vc$sol[pos0]  <- "TAC 2"
    if(i == "Paciente 12") hplc_vc$sol[pos0]  <- "P"
    if(i == "Paciente 13") hplc_vc$sol[pos0]  <- "P"
    if(i == "Paciente 14") hplc_vc$sol[pos0]  <- "TAC 2"
    if(i == "Paciente 15") hplc_vc$sol[pos0]  <- "TAC 2"
    if(i == "Paciente 17") hplc_vc$sol[pos0]  <- "TAC 2"
    if(i == "Paciente 18") hplc_vc$sol[pos0]  <- "TAC 2"
    pos0 <- pos0 + 1
  }
  # View(hplc_vc)
  hplc_nac <- HplcNac  %>%
    pivot_longer(cols = 2:13, names_to = "patient", values_to = "concentration", values_drop_na = FALSE, names_repair = "minimal")  %>%
        mutate(sol = c(""))
  pos0 <- 1
  for (i in hplc_nac$patient){
    if(i == "Paciente 1") hplc_nac$sol[pos0]   <- "TAC 1" 
    if(i == "Paciente 2") hplc_nac$sol[pos0]   <- "P" 
    if(i == "Paciente 3") hplc_nac$sol[pos0]   <- "TAC 1" 
    if(i == "Paciente 4") hplc_nac$sol[pos0]   <- "P" 
    if(i == "Paciente 5") hplc_nac$sol[pos0]   <- "TAC 1" 
    if(i == "Paciente 6") hplc_nac$sol[pos0]   <- "TAC 1" 
    if(i == "Paciente 7") hplc_nac$sol[pos0]   <- "P" 
    if(i == "Paciente 8") hplc_nac$sol[pos0]   <- "TAC 1" 
    if(i == "Paciente 9") hplc_nac$sol[pos0]   <- "TAC 1" 
    if(i == "Paciente 10") hplc_nac$sol[pos0]  <- "TAC 2"
    if(i == "Paciente 11") hplc_nac$sol[pos0]  <- "TAC 2"
    if(i == "Paciente 12") hplc_nac$sol[pos0]  <- "P"
    if(i == "Paciente 13") hplc_nac$sol[pos0]  <- "P"
    if(i == "Paciente 14") hplc_nac$sol[pos0]  <- "TAC 2"
    if(i == "Paciente 15") hplc_nac$sol[pos0]  <- "TAC 2"
    if(i == "Paciente 17") hplc_nac$sol[pos0]  <- "TAC 2"
    if(i == "Paciente 18") hplc_nac$sol[pos0]  <- "TAC 2"
    pos0 <- pos0 + 1
  }
  hplc_dfo <- HplcDfo  %>%
    pivot_longer(cols = 2:13, names_to = "patient", values_to = "concentration", values_drop_na = FALSE, names_repair = "minimal")  %>%
        mutate(sol = c(""))
  pos0 <- 1
  for (i in hplc_dfo$patient){
    if(i == "Paciente 1") hplc_dfo$sol[pos0]   <- "TAC 1" 
    if(i == "Paciente 2") hplc_dfo$sol[pos0]   <- "P" 
    if(i == "Paciente 3") hplc_dfo$sol[pos0]   <- "TAC 1" 
    if(i == "Paciente 4") hplc_dfo$sol[pos0]   <- "P" 
    if(i == "Paciente 5") hplc_dfo$sol[pos0]   <- "TAC 1" 
    if(i == "Paciente 6") hplc_dfo$sol[pos0]   <- "TAC 1" 
    if(i == "Paciente 7") hplc_dfo$sol[pos0]   <- "P" 
    if(i == "Paciente 8") hplc_dfo$sol[pos0]   <- "TAC 1" 
    if(i == "Paciente 9") hplc_dfo$sol[pos0]   <- "TAC 1" 
    if(i == "Paciente 10") hplc_dfo$sol[pos0]  <- "TAC 2"
    if(i == "Paciente 11") hplc_dfo$sol[pos0]  <- "TAC 2"
    if(i == "Paciente 12") hplc_dfo$sol[pos0]  <- "P"
    if(i == "Paciente 13") hplc_dfo$sol[pos0]  <- "P"
    if(i == "Paciente 14") hplc_dfo$sol[pos0]  <- "TAC 2"
    if(i == "Paciente 15") hplc_dfo$sol[pos0]  <- "TAC 2"
    if(i == "Paciente 17") hplc_dfo$sol[pos0]  <- "TAC 2"
    if(i == "Paciente 18") hplc_dfo$sol[pos0]  <- "TAC 2"
    pos0 <- pos0 + 1
  }


  # promedios hplc
  (HplcVcMean <- tibble(read.csv("RAW_HPLC_VC_Mean.csv", check.names = FALSE, fileEncoding = 'UTF-8-BOM'), .name_repair = "minimal"))

  hplc_vc_mean <- HplcVcMean  %>%
    pivot_longer(cols = 2:18, names_to = "patient", values_to = "concentration", values_drop_na = FALSE, names_repair = "minimal")  %>%
        mutate(sol = c("")) 
  pos0 <- 1
  for (i in hplc_vc_mean$patient){
    if(i == "P1") hplc_vc_mean$sol[pos0]   <- "TAC 1" 
    if(i == "P2") hplc_vc_mean$sol[pos0]   <- "P" 
    if(i == "P3") hplc_vc_mean$sol[pos0]   <- "TAC 1" 
    if(i == "P4") hplc_vc_mean$sol[pos0]   <- "P" 
    if(i == "P5") hplc_vc_mean$sol[pos0]   <- "TAC 1" 
    if(i == "P6") hplc_vc_mean$sol[pos0]   <- "TAC 1" 
    if(i == "P7") hplc_vc_mean$sol[pos0]   <- "P" 
    if(i == "P8") hplc_vc_mean$sol[pos0]   <- "TAC 1" 
    if(i == "P9") hplc_vc_mean$sol[pos0]   <- "TAC 1" 
    if(i == "P10") hplc_vc_mean$sol[pos0]  <- "TAC 2"
    if(i == "P11") hplc_vc_mean$sol[pos0]  <- "TAC 2"
    if(i == "P12") hplc_vc_mean$sol[pos0]  <- "P"
    if(i == "P13") hplc_vc_mean$sol[pos0]  <- "P"
    if(i == "P14") hplc_vc_mean$sol[pos0]  <- "TAC 2"
    if(i == "P15") hplc_vc_mean$sol[pos0]  <- "TAC 2"
    if(i == "P17") hplc_vc_mean$sol[pos0]  <- "TAC 2"
    if(i == "P18") hplc_vc_mean$sol[pos0]  <- "TAC 2"
    pos0 <- pos0 + 1
  }
  # View(hplc_vc_mean)



  # (vc_tac1 <- tibble(t_min=c(1:119), concentration=c(1:119)))
  # pos0 <- 1
  # for(i in hplc_vc_mean$sol){
  #   if(i == "TAC 1"){
  #     vc_tac1$t_min[pos0] <- hplc_vc_mean$t_min[pos0]
  #     vc_tac1$concentration[pos0] <- hplc_vc_mean$concentration[pos0]
  #   }
  #   pos0 <- pos0 + 1
  # }
  # View(vc_tac1)




  (HplcNacMean <- tibble(read.csv("RAW_HPLC_NAC_Mean.csv", check.names = FALSE, fileEncoding = 'UTF-8-BOM'), .name_repair = "minimal"))
  hplc_nac_mean <- HplcNacMean  %>%
    pivot_longer(cols = 2:13, names_to = "patient", values_to = "concentration", values_drop_na = FALSE, names_repair = "minimal")  %>%
        mutate(sol = c(""))
  pos0 <- 1
  for (i in hplc_nac_mean$patient){
    if(i == "P1") hplc_nac_mean$sol[pos0]   <- "TAC 1" 
    if(i == "P2") hplc_nac_mean$sol[pos0]   <- "P" 
    if(i == "P3") hplc_nac_mean$sol[pos0]   <- "TAC 1" 
    if(i == "P4") hplc_nac_mean$sol[pos0]   <- "P" 
    if(i == "P5") hplc_nac_mean$sol[pos0]   <- "TAC 1" 
    if(i == "P6") hplc_nac_mean$sol[pos0]   <- "TAC 1" 
    if(i == "P7") hplc_nac_mean$sol[pos0]   <- "P" 
    if(i == "P8") hplc_nac_mean$sol[pos0]   <- "TAC 1" 
    if(i == "P9") hplc_nac_mean$sol[pos0]   <- "TAC 1" 
    if(i == "P10") hplc_nac_mean$sol[pos0]  <- "TAC 2"
    if(i == "P11") hplc_nac_mean$sol[pos0]  <- "TAC 2"
    if(i == "P12") hplc_nac_mean$sol[pos0]  <- "P"
    if(i == "P13") hplc_nac_mean$sol[pos0]  <- "P"
    if(i == "P14") hplc_nac_mean$sol[pos0]  <- "TAC 2"
    if(i == "P15") hplc_nac_mean$sol[pos0]  <- "TAC 2"
    if(i == "P17") hplc_nac_mean$sol[pos0]  <- "TAC 2"
    if(i == "P18") hplc_nac_mean$sol[pos0]  <- "TAC 2"
    pos0 <- pos0 + 1
  }
  # View(hplc_nac_mean)


  (HplcDfoMean <- tibble(read.csv("RAW_HPLC_DFO_Mean.csv", check.names = FALSE, fileEncoding = 'UTF-8-BOM'), .name_repair = "minimal"))
  hplc_dfo_mean <- HplcDfoMean  %>%
    pivot_longer(cols = 2:13, names_to = "patient", values_to = "concentration", values_drop_na = FALSE, names_repair = "minimal")  %>%
        mutate(sol = c(""))
  pos0 <- 1
  for (i in hplc_dfo_mean$patient){
    if(i == "P1") hplc_dfo_mean$sol[pos0]   <- "TAC 1" 
    if(i == "P2") hplc_dfo_mean$sol[pos0]   <- "P" 
    if(i == "P3") hplc_dfo_mean$sol[pos0]   <- "TAC 1" 
    if(i == "P4") hplc_dfo_mean$sol[pos0]   <- "P" 
    if(i == "P5") hplc_dfo_mean$sol[pos0]   <- "TAC 1" 
    if(i == "P6") hplc_dfo_mean$sol[pos0]   <- "TAC 1" 
    if(i == "P7") hplc_dfo_mean$sol[pos0]   <- "P" 
    if(i == "P8") hplc_dfo_mean$sol[pos0]   <- "TAC 1" 
    if(i == "P9") hplc_dfo_mean$sol[pos0]   <- "TAC 1" 
    if(i == "P10") hplc_dfo_mean$sol[pos0]  <- "TAC 2"
    if(i == "P11") hplc_dfo_mean$sol[pos0]  <- "TAC 2"
    if(i == "P12") hplc_dfo_mean$sol[pos0]  <- "P"
    if(i == "P13") hplc_dfo_mean$sol[pos0]  <- "P"
    if(i == "P14") hplc_dfo_mean$sol[pos0]  <- "TAC 2"
    if(i == "P15") hplc_dfo_mean$sol[pos0]  <- "TAC 2"
    if(i == "P17") hplc_dfo_mean$sol[pos0]  <- "TAC 2"
    if(i == "P18") hplc_dfo_mean$sol[pos0]  <- "TAC 2"
    pos0 <- pos0 + 1
  }
  # View(hplc_dfo_mean)


# Graficando promedios

# Vitamina C
ggplot() +
    geom_line(data=hplc_vc_mean, aes(x=t_min, y=concentration, group=interaction(patient, sol), col = sol), alpha=0.5, na.rm = T, size=1)+
    scale_colour_manual(values=c("#5a5959", "#2b4f89", "#858f00"))+
    geom_boxplot(data=hplc_vc_mean, aes(x=t_min, y=concentration, group=interaction(t_min, sol), fill = sol), na.rm = T, alpha=1, size=0.5 ) +
    # geom_line(data = VcRr, aes(x=Time, y=Concentration),size = 0.7, col=rgb(127,	63,	191, maxColorValue = 255))
    scale_fill_manual(values=c("#848484", "#699df1", "#aab429"))+
    labs(x = "Tiempo [min]", y = "Concentración [μM]", ) +
    #scale_y_continuous(name = "Concentration [mM]", 
    #    sec.axis = sec_axis(~ log(.)*100, name = "ln(Concentration)", na.rm=T)) + 
    theme_bw() + 
    theme(legend.position = "bottom", 
          axis.title = element_text(size=20), 
          axis.text = element_text(size=16), 
          legend.title = element_text(size=20), 
          legend.text = element_text(size=20) 
          ) +
    guides(title=NULL,col = guide_legend(nrow = 2))
ggsave("00 vc para leyenda.pdf", width = 15, height = 15, units = c("cm"))
ggsave("00 vc.png", width = 15, height = 15, units = c("cm"))
ggsave("00 vc.pdf", width = 15, height = 15, units = c("cm"))


# NAC

ggplot() +
    geom_line(data=hplc_nac_mean, aes(x=t_min, y=concentration, group=interaction(patient, sol), col = sol), alpha=0.5, na.rm = T, size=1)+
    scale_colour_manual(values=c("#42cfd2", "#767d1a"))+
    geom_boxplot(data=hplc_nac_mean, aes(x=t_min, y=concentration, group=interaction(t_min, sol), fill = sol), na.rm = T, alpha=1, size=0.5 ) +
    scale_fill_manual(values=c("#42cfd2", "#767d1a"))+
    labs(x = "Tiempo [min]", y = "Concentración [μM]", ) +
    theme_bw() + 
    theme(legend.position = "bottom", 
          axis.title = element_text(size=20), 
          axis.text = element_text(size=16), 
          legend.title = element_text(size=20), 
          legend.text = element_text(size=20) 
          ) +
    guides(title=NULL,col = guide_legend(nrow = 2))
ggsave("00 nac.pdf", width = 15, height = 15, units = c("cm"))
ggsave("00 nac.png", width = 15, height = 15, units = c("cm"))

# DFO

ggplot() +
    geom_line(data=hplc_dfo_mean, aes(x=t_min, y=concentration, group=interaction(patient, sol), col = sol), alpha=0.5, na.rm = T, size=1)+
    scale_colour_manual(values=c("#42cfd2", "#767d1a"))+
    geom_boxplot(data=hplc_dfo_mean, aes(x=t_min, y=concentration, group=interaction(t_min, sol), fill = sol), na.rm = T, alpha=1, size=0.5 ) +
    scale_fill_manual(values=c("#699df1", "#aab429"))+
    labs(x = "Tiempo [min]", y = "Concentración [μM]", ) +
    theme_bw() + 
    theme(legend.position = "bottom", 
          axis.title = element_text(size=20), 
          axis.text = element_text(size=16), 
          legend.title = element_text(size=20), 
          legend.text = element_text(size=20) 
          ) +
    guides(title=NULL,col = guide_legend(nrow = 2))
ggsave("00 dfo.pdf", width = 15, height = 15, units = c("cm"))
ggsave("00 dfo.png", width = 15, height = 15, units = c("cm"))


# sin leyendas
(hplc_vc_mean_2 <- hplc_vc_mean  %>%
    group_by(t_min, sol) %>%
        summarise(qmin = quantile(concentration,0.25), qmax = quantile(concentration,0.75), concentration = mean(concentration)))

(hplc_vc_mean_2 <- hplc_vc_mean  %>%
    group_by(t_min, sol) %>%
        summarise(qmin = quantile(concentration,0.25), 
        qmax = quantile(concentration,0.75), 
        Me = quantile(concentration,0.5), 
        concentration = mean(concentration)))
(hplc_vc_mean_2 <- hplc_vc_mean  %>%
    group_by(t_min, sol) %>%
        summarise(sdmin = mean(concentration)-sd(concentration), 
                  sdmax = mean(concentration) + sd(concentration), 
                  concentration = mean(concentration)))
# group_by(hplc_vc_mean, )

# summarise(group_by(hplc_vc_mean), )

# Vitamina C
ggplot() +
    geom_line(data=hplc_vc_mean, aes(x=t_min, y=concentration/1000, group=interaction(patient, sol), col = sol), 
              alpha=0.25, na.rm = T, size=0.5, linetype = "solid")+
    scale_colour_manual(values=c("#5a5959", "#2b4f89", "#858f00"))+
    # geom_boxplot(data=hplc_vc_mean, aes(x=t_min, y=concentration, group=interaction(t_min, sol), fill = sol), na.rm = T, alpha=1, size=0.6 ) +
    # scale_fill_manual(values=c("#848484", "#699df1", "#aab429"))+
    geom_line(data=hplc_vc_mean_2, aes(x=t_min, y=concentration/1000, group=sol, col = sol), alpha=1, na.rm = T, size=1.5)+    
    # geom_point(data=hplc_vc_mean_2, aes(x=t_min, y=concentration/1000, group=sol, col = sol), alpha=1, na.rm = T, size=5, shape = 18)+    
    geom_errorbar(data=hplc_vc_mean_2, aes(x=t_min, ymax=sdmax/1000, ymin = sdmin/1000, group=sol, col = sol), alpha=1, na.rm = T, size=0.5)+    
    # geom_linerange(data=hplc_vc_mean_2, aes(x=t_min, ymax=sdmax/1000, ymin = sdmin/1000, group=sol, col = sol), alpha=1, na.rm = T, size=0.75)+    
    # stat_summary(data=hplc_vc_mean, geom = "line", fun = mean, aes(x = t_min, y = concentration/1000, group = sol, col=sol), size = 1.5)+
    # stat_summary(data=hplc_vc_mean, geom = "errorbar", fun = mean_se, aes(x = t_min, y = concentration/1000, group = sol, col=sol), size = 3)+
    labs(title = "AA") +
    scale_x_continuous(breaks = c(0, 15, 30, 60, 90, 120, 180))+
    scale_y_continuous(breaks = c(0.1, 0.25, 0.5, 0.75, 1))+
    theme_bw() + 
    theme(legend.position = "none", 
          axis.title = element_blank(), 
          plot.title = element_text(size = 22, hjust = 0.5),
          axis.text = element_text(size = 16)
          ) +
    guides(title=NULL,col = guide_legend(nrow = 2))
ggsave("00 vc se indiv.pdf", width = 13.5, height = 11, units = c("cm"))

# Facet
ggplot() +
    geom_line(data=hplc_vc_mean, aes(x=t_min, y=concentration/1000, group=interaction(patient, sol), col = sol), 
              alpha=0.3, na.rm = T, size=0.75, linetype = "solid")+
    scale_colour_manual(values=c("#5a5959", "#2b4f89", "#858f00"))+
    # geom_boxplot(data=hplc_vc_mean, aes(x=t_min, y=concentration, group=interaction(t_min, sol), fill = sol), na.rm = T, alpha=1, size=0.6 ) +
    # scale_fill_manual(values=c("#848484", "#699df1", "#aab429"))+
    geom_line(data=hplc_vc_mean_2, aes(x=t_min, y=concentration/1000, group=sol, col = sol), alpha=1, na.rm = T, size=1.5)+    
    # geom_point(data=hplc_vc_mean_2, aes(x=t_min, y=concentration/1000, group=sol, col = sol), alpha=1, na.rm = T, size=5, shape = 18)+    
    geom_errorbar(data=hplc_vc_mean_2, aes(x=t_min, ymax=sdmax/1000, ymin = sdmin/1000, group=sol, col = sol), alpha=1, na.rm = T, size=0.5)+    
    # geom_linerange(data=hplc_vc_mean_2, aes(x=t_min, ymax=sdmax/1000, ymin = sdmin/1000, group=sol, col = sol), alpha=1, na.rm = T, size=0.75)+    
    # stat_summary(data=hplc_vc_mean, geom = "line", fun = mean, aes(x = t_min, y = concentration/1000, group = sol, col=sol), size = 1.5)+
    # stat_summary(data=hplc_vc_mean, geom = "errorbar", fun = mean_se, aes(x = t_min, y = concentration/1000, group = sol, col=sol), size = 3)+
    # labs(title = "AA") +
    scale_x_continuous(breaks = c(0, 15, 30, 60, 90, 120, 180))+
    scale_y_continuous(breaks = c(0.1, 0.25, 0.5, 0.75, 1))+
    facet_grid(rows = vars(sol))+
    theme_bw() + 
    theme(legend.position = "none", 
          axis.title = element_blank(), 
          plot.title = element_text(size = 22, hjust = 0.5),
          axis.text = element_text(size = 16),
          strip.text.y = element_blank(),
          strip.background = element_blank()
          ) +
    guides(title=NULL,col = guide_legend(nrow = 2))
ggsave("00 vc se indiv facet.pdf", width = 13.5, height = 24, units = c("cm"))

ggplot() +
    # geom_line(data=hplc_vc_mean, aes(x=t_min, y=concentration/1000, group=interaction(patient, sol), col = sol), 
    #           alpha=0.25, na.rm = T, size=0.5, linetype = "solid")+
    scale_colour_manual(values=c("#5a5959", "#2b4f89", "#858f00"))+
    # geom_boxplot(data=hplc_vc_mean, aes(x=t_min, y=concentration, group=interaction(t_min, sol), fill = sol), na.rm = T, alpha=1, size=0.6 ) +
    # scale_fill_manual(values=c("#848484", "#699df1", "#aab429"))+
    geom_line(data=hplc_vc_mean_2, aes(x=t_min, y=concentration/1000, group=sol, col = sol), alpha=1, na.rm = T, size=1.5)+    
    # geom_point(data=hplc_vc_mean_2, aes(x=t_min, y=concentration/1000, group=sol, col = sol), alpha=1, na.rm = T, size=5, shape = 18)+    
    geom_errorbar(data=hplc_vc_mean_2, aes(x=t_min, ymax=sdmax/1000, ymin = sdmin/1000, group=sol, col = sol), alpha=1, na.rm = T, size=0.5)+    
    # geom_linerange(data=hplc_vc_mean_2, aes(x=t_min, ymax=sdmax/1000, ymin = sdmin/1000, group=sol, col = sol), alpha=1, na.rm = T, size=0.75)+    
    labs(title = "AA") +
    scale_x_continuous(breaks = c(0, 15, 30, 60, 90, 120, 180))+
    scale_y_continuous(breaks = c(0.1, 0.25, 0.5, 0.75, 1))+
    theme_bw() + 
    theme(legend.position = "none", 
          axis.title = element_blank(), 
          plot.title = element_text(size = 22, hjust = 0.5),
          axis.text = element_text(size = 16)
          ) +
    guides(title=NULL,col = guide_legend(nrow = 2))

ggsave("00 vc se.pdf", width = 13.5, height = 24, units = c("cm"))


# NAC
(hplc_nac_mean_2 <- hplc_nac_mean  %>%
    group_by(t_min, sol) %>%
        summarise(concentration = mean(concentration)))
(hplc_nac_mean_2 <- hplc_nac_mean  %>%
    group_by(t_min, sol) %>%
        summarise(sdmin = mean(concentration, na.rm = TRUE)-sd(concentration, na.rm = TRUE), 
                  sdmax = mean(concentration, na.rm = TRUE) + sd(concentration, na.rm = TRUE), 
                  concentration = mean(concentration, na.rm = TRUE)))
ggplot() +
    geom_line(data=hplc_nac_mean, aes(x=t_min, y=concentration/1000, group=interaction(patient, sol), col = sol), 
    alpha=0.3, na.rm = T, size=0.75, linetype = "solid")+
    scale_colour_manual(values=c("#2b4f89", "#858f00"))+
    # geom_boxplot(data=hplc_nac_mean, aes(x=t_min, y=concentration/1000, group=interaction(t_min, sol), fill = sol), na.rm = T, alpha=1, size=0.6 ) +
    # scale_fill_manual(values=c("#699df1", "#aab429"))+
    geom_line(data=hplc_nac_mean_2, aes(x=t_min, y=concentration/1000, group=sol, col = sol), alpha=1, na.rm = T, size=1.5)+    
    geom_errorbar(data=hplc_nac_mean_2, aes(x=t_min, ymax=sdmax/1000, ymin = sdmin/1000, group=sol, col = sol), alpha=1, na.rm = T, size=0.5)+    
    # labs(title = "NAC") +
    scale_x_continuous(breaks = c(0, 15, 30, 60, 90, 120, 180))+
    scale_y_continuous(breaks = c(0.1, 0.25, 0.5, 0.75, 1))+
    facet_grid(rows = vars(sol))+
    theme_bw() + 
    theme(legend.position = "none", 
          axis.title = element_blank(), 
          plot.title = element_text(size = 22, hjust = 0.5),
          axis.text = element_text(size = 16)
          ,
          strip.text.y = element_blank(),
          strip.background = element_blank()
          ) +
    guides(title=NULL,col = guide_legend(nrow = 2))
ggsave("00 nac se indiv facet.pdf", width = 13.5, height = 18, units = c("cm"))

ggplot() +
    # geom_line(data=hplc_nac_mean, aes(x=t_min, y=concentration/1000, group=interaction(patient, sol), col = sol), 
    # alpha=0.25, na.rm = T, size=0.5, linetype = "solid")+
    scale_colour_manual(values=c("#2b4f89", "#858f00"))+
    # geom_boxplot(data=hplc_nac_mean, aes(x=t_min, y=concentration/1000, group=interaction(t_min, sol), fill = sol), na.rm = T, alpha=1, size=0.6 ) +
    # scale_fill_manual(values=c("#699df1", "#aab429"))+
    geom_line(data=hplc_nac_mean_2, aes(x=t_min, y=concentration/1000, group=sol, col = sol), alpha=1, na.rm = T, size=1.5)+    
    geom_errorbar(data=hplc_nac_mean_2, aes(x=t_min, ymax=sdmax/1000, ymin = sdmin/1000, group=sol, col = sol), alpha=1, na.rm = T, size=0.5)+    
    labs(title = "NAC") +
    scale_x_continuous(breaks = c(0, 15, 30, 60, 90, 120, 180))+
    scale_y_continuous(breaks = c(0.1, 0.25, 0.5, 0.75, 1))+
    theme_bw() + 
    theme(legend.position = "none", 
          axis.title = element_blank(), 
          plot.title = element_text(size = 22, hjust = 0.5),
          axis.text = element_text(size = 16)
          ) +
    guides(title=NULL,col = guide_legend(nrow = 2))
ggsave("00 nac se.pdf", width = 13.5, height = 11, units = c("cm"))

# DFO
(hplc_dfo_mean_2 <- hplc_dfo_mean  %>%
    group_by(t_min, sol) %>%
        summarise(concentration = mean(concentration)))

(hplc_dfo_mean_2 <- hplc_dfo_mean  %>%
    group_by(t_min, sol) %>%
        summarise(sdmin = mean(concentration, na.rm = TRUE) - sd(concentration, na.rm = TRUE), 
                  sdmax = mean(concentration, na.rm = TRUE) + sd(concentration, na.rm = TRUE), 
                  concentration = mean(concentration, na.rm = TRUE)))




ggplot() +
    geom_line(data=hplc_dfo_mean, aes(x=t_min, y=concentration/1000, group=interaction(patient, sol), col = sol), 
            alpha=0.3, na.rm = T, size=0.75, linetype = "solid")+
    scale_colour_manual(values=c("#2b4f89", "#858f00"))+
    # geom_boxplot(data=hplc_dfo_mean, aes(x=t_min, y=concentration/1000, group=interaction(t_min, sol), fill = sol), na.rm = T, alpha=1, size=0.6) +
    # scale_fill_manual(values=c("#699df1", "#aab429"))+
    geom_line(data=hplc_dfo_mean_2, aes(x=t_min, y=concentration/1000, group=sol, col = sol), alpha=1, na.rm = T, size=1.5)+    
    geom_errorbar(data=hplc_dfo_mean_2, aes(x=t_min, ymax=sdmax/1000, ymin = sdmin/1000, group=sol, col = sol), alpha=1, na.rm = T, size=0.5)+    
    # labs(title = "DFO") +
    scale_x_continuous(breaks = c(0, 15, 30, 60, 90, 120, 180))+
    scale_y_continuous(breaks = seq(0, 0.012, 0.002))+
    facet_grid(rows = vars(sol))+
    theme_bw() + 
    theme(legend.position = "none", 
          axis.title = element_blank(), 
          plot.title = element_text(size = 22, hjust = 0.5),
          axis.text = element_text(size = 16),
          strip.text.y = element_blank(),
          strip.background = element_blank()
          ) +
    guides(title=NULL,col = guide_legend(nrow = 2))
ggsave("00 dfo se indiv facet.pdf", width = 13.5, height = 18, units = c("cm"))

ggplot() +
    # geom_line(data=hplc_dfo_mean, aes(x=t_min, y=concentration/1000, group=interaction(patient, sol), col = sol), 
    #         alpha=0.5, na.rm = T, size=0.75, linetype = "solid")+
    scale_colour_manual(values=c("#2b4f89", "#858f00"))+
    # geom_boxplot(data=hplc_dfo_mean, aes(x=t_min, y=concentration/1000, group=interaction(t_min, sol), fill = sol), na.rm = T, alpha=1, size=0.6) +
    # scale_fill_manual(values=c("#699df1", "#aab429"))+
    geom_line(data=hplc_dfo_mean_2, aes(x=t_min, y=concentration/1000, group=sol, col = sol), alpha=1, na.rm = T, size=1.5)+    
    geom_errorbar(data=hplc_dfo_mean_2, aes(x=t_min, ymax=sdmax/1000, ymin = sdmin/1000, group=sol, col = sol), alpha=1, na.rm = T, size=0.5)+    
    labs(title = "DFO") +
    scale_x_continuous(breaks = c(0, 15, 30, 60, 90, 120, 180))+
    scale_y_continuous(breaks = seq(0, 0.012, 0.002))+
    theme_bw() + 
    theme(legend.position = "none", 
          axis.title = element_blank(), 
          plot.title = element_text(size = 22, hjust = 0.5),
          axis.text = element_text(size = 16)
          ) +
    guides(title=NULL,col = guide_legend(nrow = 2))
ggsave("00 dfo se.pdf", width = 13.5, height = 11, units = c("cm"))
