install.packages("dplyr")
install.packages("ggplot2")
install.packages("forcats")
install.packages("svglite")

library(dplyr)
library(ggplot2)
library(forcats)
library(svglite)

# función para pasar minutos a horas
trans <- function(x){
  x <- x/60
  return(x)
}

# unidades de medida utilizadas para input
# Volumen: L
# Tiempo: h
# Cantidad de moléculas: mmol
# velocidad de infusión: mmol / h

# función para transformar velocidad de infusión de mg/min -> mmol/h
TransR <- function(InfusionMgMin, MM){

  R <- InfusionMgMin * 60 / MM 
  return(R)
}

# Argumentos de las funciones:
# tD: vector con tiempos en que termina la velocidad de infusión "n"
# R: vector con velocidades de infusión R1, R2,..., Rn
# Cph: concentración fisiológica. 0 por defecto
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


# Parámetros farmacocinéticos utilizados:
(PKdata <- tibble(read.csv("PKdata.csv")))
# Nota: Los parámetros de los papers de referencia que se ajustan por peso son:
# - VC: solo volumen de distribución
# - NAC: volumen de distribución y clearance
# para estas simulaciones se utilizarán parámetros ajustados para persona de 70 kg


#infusiones fase 1

# calculo velocidades de infusión en mmol/h

TransRVol  <- function(tD, Infmlmin, ConcmM){
  num <- length(tD)
  R <- tibble (Tiempo_fin = c(1:num), Infusion_mmolh = c(1:num))
  length <- length(tD)
  pos0 <- 1
  for(i in tD){
    R$Tiempo_fin[pos0] <- i
    R$Infusion_mmolh[pos0] <- Infmlmin[pos0] * 60 * ConcmM / 1000
    pos0 <- pos0 + 1
  }

  return(R)
}

# Solución RR (6ml/min  por 30 minutos, luego 1 ml/min por 1 h)
"****AA_AA_AA_AA_AA_AA_AA_AA_AA_AA_AA_AA_AA_AA_AA_AA_AA_****"; 
(InfVcRr <- TransRVol(tD = c(0.5, 1.5, 3),
    Infmlmin = c(6,1,0),
    ConcmM = 56.21))
"****NAC_NAC_NAC_NAC_NAC_NAC_NAC_NAC_NAC_NAC_NAC_NAC_NAC_NAC_NAC_NAC_NAC_****"; 
(InfNacRr <- TransRVol(tD = c(0.5, 1.5, 3),
    Infmlmin = c(6,1,0),
    ConcmM = 49.02))
"****DFO_DFO_DFO_DFO_DFO_DFO_DFO_DFO_DFO_DFO_DFO_DFO_DFO_DFO_DFO_DFO_DFO_****"; 
(InfDfoRr <- TransRVol(tD = c(0.5, 1.5, 3),
    Infmlmin = c(6,1,0),
    ConcmM = 6.09))

# Solución AIJ (3ml/min por 1.5 h)
"****AA_AA_AA_AA_AA_AA_AA_AA_AA_AA_AA_AA_AA_AA_AA_AA_AA_****"; 
(InfVcAij <- TransRVol(tD = c(1.5, 3),
    Infmlmin = c(3,0),
    ConcmM = 51.10))
"****NAC_NAC_NAC_NAC_NAC_NAC_NAC_NAC_NAC_NAC_NAC_NAC_NAC_NAC_NAC_NAC_NAC_****"; 
(InfNacAij <- TransRVol(tD = c(1.5, 3),
    Infmlmin = c(3,0),
    ConcmM = 98.04))
"****DFO_DFO_DFO_DFO_DFO_DFO_DFO_DFO_DFO_DFO_DFO_DFO_DFO_DFO_DFO_DFO_DFO_****"; 
(InfDfoAij <- TransRVol(tD = c(1.5, 3),
    Infmlmin = c(3,0),
    ConcmM = 9.74))


#Vitamina C

VcRr <- Simul1Comp(PKdata$V1[1], PKdata$CL[1], 
          tD= c(0.5, 1.5, 3),
          R= c(20.2, 3.37, 0), 
          0.06)
VcAij <- Simul1Comp(PKdata$V1[1], PKdata$CL[1], 
          tD= c(0.5, 1.5, 3),
          R= c(9.20, 9.20, 0), 
          0.06)


ggplot()+
  geom_line(data = VcRr, aes(x=Time, y=Concentration),size = 0.7, col=rgb(127,	63,	191, maxColorValue = 255))+
  scale_x_continuous(breaks = seq(0,180,30), limits = c(-15, 180), labels = seq(0,3,0.5))+
  labs(title="VC RR sol 56.21 mM", x="Tiempo [h]", y="Concentracion [mM]")+
  theme_bw()+
  guides(title=NULL)+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.text = element_text(size = 8))
ggsave("01 Graf RR  Vc.svg", width = 6, height = 6, units = c("cm"))
ggsave("01 Graf RR  Vc.png", width = 6, height = 6, units = c("cm"))

ggplot()+
  geom_line(data = VcAij, aes(x=Time, y=Concentration),size = 0.7, col=rgb(127,	63,	191, maxColorValue = 255))+
  scale_x_continuous(breaks = seq(0,180,30), limits = c(-15, 180), labels = seq(0,3,0.5))+
  labs(title="VC AIJ sol 51.10 mM", x="Tiempo [h]", y="Concentracion [mM]")+
  theme_bw()+
  guides(title=NULL)+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.text = element_text(size = 8))
ggsave("01 Graf AIJ Vc.svg", width = 6, height = 6, units = c("cm"))
ggsave("01 Graf AIJ Vc.png", width = 6, height = 6, units = c("cm"))


#N-Acetilcisteina

NacRr <- Simul3Comp(PKdata$V1[2], PKdata$V2[2], PKdata$V3[2],
                    PKdata$CL[2], PKdata$Q[2],PKdata$Q3[2],
                    tD= c(0.5, 1.5, 3),
                    R= c(17.6, 2.94, 0))
NacAij <- Simul3Comp(PKdata$V1[2], PKdata$V2[2], PKdata$V3[2],
                    PKdata$CL[2], PKdata$Q[2],PKdata$Q3[2],
                    tD= c(0.5, 1.5, 3),
                    R= c(17.6, 17.6, 0))

ggplot()+
  geom_line(data=NacRr, aes(x=Time, y=Concentration), 
            size = 0.7, col=rgb(63,	191,	255, maxColorValue=255))+
  scale_x_continuous(breaks = seq(0,180,30), limits = c(-15, 180), labels = seq(0,3,0.5))+
  labs(title="NAC RR sol 49.02 mM", x="Tiempo [h]", y="Concentracion [mM]")+
  theme_bw()+
  guides(title=NULL,col = guide_legend(nrow = 4))+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8))
ggsave("01 Graf RR Nac.svg", width = 6, height = 6, units = c("cm"))
ggsave("01 Graf RR Nac.png", width = 6, height = 6, units = c("cm"))

ggplot()+
  geom_line(data=NacAij, aes(x=Time, y=Concentration), 
            size = 0.7, col=rgb(63,	191,	255, maxColorValue=255))+
  scale_x_continuous(breaks = seq(0,180,30), limits = c(-15, 180), labels = seq(0,3,0.5))+
  labs(title="NAC AIJ sol 98.04 mM", x="Tiempo [h]", y="Concentracion [mM]")+
  theme_bw()+
  guides(title=NULL,col = guide_legend(nrow = 4))+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8))
ggsave("01 Graf AIJ Nac.svg", width = 6, height = 6, units = c("cm"))
ggsave("01 Graf AIJ Nac.png", width = 6, height = 6, units = c("cm"))



# Deferoxamina
DfoRr <- Simul2Comp(V=PKdata$V1[3], V2=PKdata$V2[3], 
                  CL=PKdata$CL[3], Q=PKdata$Q[3],
                  tD=c(0.5, 1.5, 3), 
                  R=c(2.19, 0.365, 0))
DfoAij <- Simul2Comp(V=PKdata$V1[3], V2=PKdata$V2[3], 
                  CL=PKdata$CL[3], Q=PKdata$Q[3],
                  tD=c(0.5, 1.5, 3), 
                  R=c(1.75, 1.75, 0))

ggplot()+
  geom_line(data=DfoRr, aes(x=Time, y=Concentration), 
            size = 0.7, col=rgb(255,	191,	63, maxColorValue=255))+
  scale_x_continuous(breaks = seq(0,180,30), limits = c(-15, 180), labels = seq(0,3,0.5))+
  labs(title="DFO RR sol 6.09 mM", x="Tiempo [h]", y="Concentracion [mM]")+
  theme_bw()+
  guides(title=NULL,col = guide_legend(nrow = 4))+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8))
ggsave("01 Graf RR Dfo.svg", width = 6, height = 6, units = c("cm"))
ggsave("01 Graf RR Dfo.png", width = 6, height = 6, units = c("cm"))


ggplot()+
  geom_line(data=DfoAij, aes(x=Time, y=Concentration), 
            size = 0.7, col=rgb(255,	191,	63, maxColorValue=255))+
  scale_x_continuous(breaks = seq(0,180,30), limits = c(-15, 180), labels = seq(0,3,0.5))+
  labs(title="DFO AIJ sol 9.74 mM", x="Tiempo [h]", y="Concentracion [mM]")+
  theme_bw()+
  guides(title=NULL,col = guide_legend(nrow = 4))+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8))
ggsave("01 Graf AIJ Dfo.svg", width = 6, height = 6, units = c("cm"))
ggsave("01 Graf AIJ Dfo.png", width = 6, height = 6, units = c("cm"))
