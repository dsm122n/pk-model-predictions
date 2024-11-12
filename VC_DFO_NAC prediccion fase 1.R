install.packages("dplyr")
install.packages("ggplot2")
install.packages("forcats")
install.packages("svglite")

library(dplyr)
library(ggplot2)
library(forcats)
library(svglite)


trans <- function(x){
  x <- x/60
  return(x)
}

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

R1CompCssTd <- function(V, CL, Css, td, Cph=0){
  
  C0 <- Css - Cph
  
  Rss <- C0 * CL # infusion rate [mili mol/h] at a desired steady state
  
  k <- (CL / V) #eliminarion rate [1/h]
  
  R0 <- (C0*CL)/(1- exp(-k*td)) # initial infusion rate [mili mol / h] to achieve Css in at td
  
  vel.infusions <- tibble(unidades=c("mmol/h", "mmol/min"), 
                               R0=c(R0, trans(R0)),
                               Rss=c(Rss, trans(Rss)))
  return(vel.infusions)
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



R2CompCssTd <- function(V, V2, CL, Q, Css, td, Cph=0){
  C0 <- Css - Cph
  Rss <- C0 * CL
  
 
  terms <- (Q/V) + (Q/V2) + (CL/V)
  beta <- 0.5 * (terms - sqrt(terms^2 - 4*(Q/V2)*(CL/V)))
  alpha <- ((Q/V2)*(CL/V))/beta
  A <- (1/V) * (alpha - (Q/V2))/(alpha-beta)
  B <- (1/V) * ((beta - Q/V2)/(beta-alpha))
  

  R0 <- C0 / ((A/alpha)*(1-exp(-alpha*td))+
                (B/beta)*(1-exp(-beta*td)))
  
  vel.infusions.2 <- tibble(unidades=c("mmol/h", "mmol/min"),
                                 R0=c(R0, trans(R0)),
                                 Rss=c(Rss, trans(Rss)))
  return(vel.infusions.2)
}

R2CompCssTdSumCl <- function(V, V2, CL, Q, Css, td, Cph=0){
  C0 <- Css - Cph

  terms <- (Q/V) + (Q/V2) + (CL/V)
  beta <- 0.5 * (terms - sqrt(terms^2 - 4*(Q/V2)*(CL/V)))
  alpha <- ((Q/V2)*(CL/V))/beta
  A <- (1/V) * (alpha - (Q/V2))/(alpha-beta)
  B <- (1/V) * ((beta - Q/V2)/(beta-alpha))
  Rss <- C0 * (CL+Q)
  R0 <- C0 / ((A/alpha)*(1-exp(-alpha*td))+
                (B/beta)*(1-exp(-beta*td)))
  
  vel.infusions.2 <- tibble(unidades=c("mmol/h", "mmol/min"),
                             R0=c(R0, trans(R0)),
                             Rss=c(Rss, trans(Rss)))
  return(vel.infusions.2)
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


R3CompCssTd <- function(V1, V2, V3, CL, Q2, Q3, Css, td, Cph=0){
  C0 <- Css - Cph
  Rss <- C0 * CL
  
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
  
  R0 <- C0/(A/(alpha)*(1-exp(-alpha*td))+
              (B/beta)*(1-exp(-beta*td))+
              (C/gamma)*(1-exp(-gamma*td)))
  vel.infusions <- tibble(unidades=c("mmol/h", "mmol/min"), 
                               R0=c(R0, trans(R0)),
                               Rss=c(Rss, trans(Rss)))
  return(vel.infusions)
}

(PKdata <- tibble(read.csv2("PKdata (NAC corregida).csv")))
PKdataVC <- tibble(read.csv2("PK VitaminC.csv"))
knitr::kable(PKdata)
knitr::kable(PKdataVC)

Ramossimul1comp <- Simul1Comp(PKdataVC$V1[2], PKdataVC$CL[2], 
                     tD= c(1, 3,24),
                     R= c(0.32*10*60, 0.32*3*60, 0), 
                     0.06)
Ramossimul2comp <- Simul2Comp(PKdataVC$V1[1], PKdataVC$V2[1],
                          PKdataVC$CL[1], PKdataVC$Q[1],
                          tD= c(1, 3,24),
                          R= c(0.32*10*60, 0.32*3*60, 0), 
                          0.06)
VC_compartments <- bind_rows("One compartment" = Ramossimul1comp, 
                            "Two compartments"= Ramossimul2comp, 
                            .id = "Studies")
Ramos <- tibble (read.csv2("RamosVC.csv"))
knitr::kable(Ramos)

ggplot()+
  geom_line(data=VC_compartments, aes(x=Time, y=Concentration, linetype=Studies), 
            size = 0.7, col=rgb(127,	63,	191, maxColorValue = 255))+
  # scale_color_manual(values = c(rgb(1,0.5,0.5), rgb(1,0.8,0.9)))+
  geom_errorbar(data=Ramos, aes(x=Time.min, y=M_conc.mM, 
                                ymin=Qmin.mM, ymax=Qmax.mM),
                col="black", size=1)+
  geom_point(data=Ramos, aes(x=Time.min, y=M_conc.mM), na.rm = T, col="black", size=4, shape="square")+
  scale_x_continuous(breaks=seq(from=0,to=1440, by=240), label = seq(from=0, to=24, by=4))+
  labs(x="Time [h]", y="Concentration [mM]")+
  theme_classic()+
  theme(legend.position = "bottom" )+
  guides(title=NULL,col = guide_legend(nrow = 2))+
  theme(legend.position = c(0.7,0.7), legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.text = element_text(size = 8),
        axis.title = element_text(size=8))

ggsave("ComparisonVC_2vs1.pdf", width = 6, height = 6, units = c("cm"))
ggsave("ComparisonVC_2vs1.svg", width = 6, height = 6, units = c("cm"))

  Ramos <- tibble (read.csv2("RamosVC.csv"))
  Guan <- Simul1Comp(PKdata$V1[1], PKdata$CL[1], 
                        tD= c(1/6, 24),
                        R= c((2000/PKdata$MM[1])*6, (20/PKdata$MM[1])), 
                        0.06)
  Gasparetto <- Simul1Comp(PKdata$V1[1], PKdata$CL[1], 
                            tD=c(1/6, 24),
                            R= c(1000/PKdata$MM[1]*6, 0), 
                            0.06)
  
  Valls_Ramos <- Simul1Comp(PKdata$V1[1], PKdata$CL[1], 
                             tD= c(1, 3, 24),
                             R= c(0.32*10*60, 0.32*3*60, 0), 
                             0.06)
  
  Shafaei <- Simul1Comp(PKdata$V1[1], PKdata$CL[1],
                        tD= c(0.1, 0.1+(1/60), 12),
                        R= c(500*60/PKdata$MM[1], 100/PKdata$MM[1]*60, 0), 
                        0.06)
  
  ComparisonVC <- bind_rows("Shafaei-Bajestani et al."=Shafaei, 
                             # "Valls et al.and Ramos et al."=Valls_Ramos, 
                             "Gasparetto et al."=Gasparetto, 
                             "Guan et al." =Guan, 
                         .id = "Studies")
  
  ggplot()+
    geom_line(data=ComparisonVC, aes(x=Time, y=Concentration, linetype=Studies), 
              size = 0.7, col=rgb(127,	63,	191, maxColorValue = 255))+
    scale_linetype_manual(values = c("solid", "dashed", "dotted"))+
    # geom_errorbar(data=Ramos, aes(x=Time.min, y=M_conc.mM, 
    #                               ymin=Qmin.mM, ymax=Qmax.mM),
    #               col="black", size=1)+
    # geom_point(data=Ramos, aes(x=Time.min, y=M_conc.mM), na.rm = T, col="black", size=4, shape="square")+
    # scale_y_break(breaks=c(2.5,5), scales=c(.3))+
    scale_x_continuous(breaks = seq(0,300,60), limits = c(-15, 300), labels = seq(0,5,1))+
    scale_y_continuous(breaks = seq(0,1.4, 0.2), limits = c(0,1.4))+
    labs(x=NULL, y=NULL)+
    theme_classic()+
    guides(title=NULL,col = guide_legend(ncol = 1))+
    theme(legend.position = "bottom", legend.title = element_blank(), 
          legend.text = element_text(size=8),
          axis.text = element_text(size = 8))

  ggsave("ComparisonVC.pdf", width = 5, height = 6, units = c("cm"))
  
  # ggsave("ComparisonVC.svg", width = 5, height = 6, units = c("cm"))
  
    ggplot()+
        geom_line(data=Valls_Ramos, aes(x=Time, y=Concentration), 
                size = 0.7, col=rgb(127,	63,	191, maxColorValue = 255))+
        # scale_linetype_manual(values = c("solid", "dashed", "dotted"))+
        geom_errorbar(data=Ramos, aes(x=Time.min, y=M_conc.mM,
                                    ymin=Qmin.mM, ymax=Qmax.mM),
                    col="black", size=1)+
        geom_point(data=Ramos, aes(x=Time.min, y=M_conc.mM), na.rm = T, col="black", size=4, shape="square")+
        # scale_y_break(breaks=c(2.5,5), scales=c(.3))+
        scale_x_continuous(breaks = seq(0,420,60), limits = c(-15, 500), labels = seq(0,7,1))+
        
        # scale_y_continuous(n.breaks = 3, breaks = c(seq(from=0,to=2, by=0.4),6,8,10))+
        labs(x=NULL, y=NULL)+
        theme_classic()+
        guides(title=NULL,col = guide_legend(ncol = 1))+
        theme(legend.position = "bottom", legend.title = element_blank(), 
            legend.text = element_text(size=8),
            axis.text = element_text(size = 8))

  ggsave("ComparisonVC02.pdf", width = 5, height = 5, units=c("cm"))

naciam <- Simul3Comp(PKdata$V1[2], PKdata$V2[2], PKdata$V3[2],
                        PKdata$CL[2], PKdata$Q[2],PKdata$Q3[2],
                      tD= c(1, 48),
                      R= c((20/PKdata$MM[2])*60, (10/PKdata$MM[2])*60))
lipsia <- Simul3Comp(PKdata$V1[2], PKdata$V2[2], PKdata$V3[2],
                    PKdata$CL[2], PKdata$Q[2],PKdata$Q3[2],
                    tD= c(1/6, 12, 24, 36),
                    R= c((1200/PKdata$MM[2])*6, 0, (1200/PKdata$MM[2])*6, 0))
lipsia <- mutate(lipsia, Time=Time+50)
Nozari <- Simul3Comp(PKdata$V1[2], PKdata$V2[2], PKdata$V3[2],
                    PKdata$CL[2], PKdata$Q[2],PKdata$Q3[2],
                    tD= c(1/6, 1/6 + 1/60, 12, 12+1/6,24),
                    R= c((100*80/PKdata$MM[2])*6, 
                            (480/PKdata$MM[2])*60, 0, 
                            (10*80/PKdata$MM[2])*6,0))
Ozaydin <- Simul3Comp(PKdata$V1[2], PKdata$V2[2], PKdata$V3[2],
                    PKdata$CL[2], PKdata$Q[2],PKdata$Q3[2],
                    tD= c(1, 49),
                    R= c((50*80/PKdata$MM[2]), 
                            (50*80/(24*PKdata$MM[2]))))

ComparisonNAC <- bind_rows("Pasupathy et al."=naciam, 
                            "Thiele et al."=lipsia, 
                            # "Nozari et al."=Nozari, 
                            "Ozaydin et al."=Ozaydin,
                             .id = "Studies")
  
  ggplot()+
    geom_line(data=ComparisonNAC, aes(x=Time, y=Concentration, linetype=Studies), 
              size = 0.7, col=rgb(63,	191,	255, maxColorValue=255))+
     scale_linetype_manual(values = c("dashed", "dotted", "solid"))+
    # scale_color_manual(values = c("red", "blue","yellow", "orange"))+
    # scale_y_break(breaks=c(1.5,5), scales=c(.3))+
    scale_x_continuous(breaks = seq(0,300,60), limits = c(-15, 300), labels = seq(0,5,1))+
    # scale_y_continuous(n.breaks = 3, breaks = c(seq(from=0,to=2, by=0.4),c(seq(from=5.5,to=6.5, by=0.5))))+
    scale_y_continuous(breaks = seq(0,1.25, 0.25), limits = c(0,1.25))+
    labs(x=NULL, y=NULL)+
    theme_classic()+
    guides(title=NULL,col = guide_legend(nrow = 4))+
    theme(legend.position = "bottom", legend.title = element_blank(), 
          legend.text = element_text(size = 8),
          axis.text = element_text(size = 8))
  
  
  ggsave("ComparisonNAC.pdf", width = 5, height = 6, units = c("cm"))
  
ggplot()+
    geom_line(data=Nozari, aes(x=Time, y=Concentration), 
              size = 0.7, col=rgb(63,	191,	255, maxColorValue=255))+
    # scale_linetype_manual(values = c("solid", "dashed", "dotted"))+
    # scale_color_manual(values = c("red", "blue","yellow", "orange"))+
    # scale_y_break(breaks=c(1.5,5), scales=c(.3))+
    scale_x_continuous(breaks = seq(0,300,60), limits = c(-15, 300), labels = seq(0,5,1))+
    # scale_y_continuous(n.breaks = 3, breaks = c(seq(from=0,to=2, by=0.4),c(seq(from=5.5,to=6.5, by=0.5))))+
    scale_y_continuous(breaks = seq(0,7, 1))+
    labs(x=NULL, y=NULL)+
    theme_classic()+
    guides(title=NULL,col = guide_legend(nrow = 4))+
    theme(legend.position = "bottom", legend.title = element_blank(), 
          legend.text = element_text(size = 8),
          axis.text = element_text(size = 8))
  
  
  ggsave("ComparisonNAC02.pdf", width = 5, height = 5, units = c("cm"))
  
  # ggsave("ComparisonNAC.svg", width = 5, height = 8, units = c("cm"))

Chan_DFO <- tibble(Units=c("mg  (/h)", "mmol  (/h)"),
                  Bolus=c(500, 500/PKdata$MM[3]),
                  Infusion=c(50*80/12, 50*80/(12*PKdata$MM[3])))

simul_DFO.Chan <- Simul2Comp(V=PKdata$V1[3], V2=PKdata$V2[3], 
                              CL=PKdata$CL[3], Q=PKdata$Q[3],
                              tD=c(1/6,12,24), 
                              R=c(Chan_DFO$Bolus[2]*6, Chan_DFO$Infusion[2],0))
ggplot()+
  geom_line(data = simul_DFO.Chan, aes(x=Time, y=Concentration, linetype="Chan et al."),size = 0.7, col=rgb(255,	191,	63, maxColorValue=255))+
  scale_x_continuous(breaks = seq(0,300,60), limits = c(-15, 300), labels = seq(0,5,1))+
  labs(x=NULL, y=NULL)+
  theme_classic()+
  guides(title=NULL)+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.text = element_text(size = 8))
ggsave("ComparisonDFO.pdf", width = 5, height = 6, units = c("cm"))






















#infusiones fase 1

#Vitamina C

VcRr <- Simul1Comp(PKdata$V1[1], PKdata$CL[1], 
          tD= c(0.5, 1.5, 3),
          R= c(18.9, 3.15, 0), 
          0.06)
VcAij <- Simul1Comp(PKdata$V1[1], PKdata$CL[1], 
          tD= c(0.5, 1.5, 3),
          R= c(7.57, 7.57, 0), 
          0.06)


ggplot()+
  geom_line(data = VcRr, aes(x=Time, y=Concentration),size = 0.7, col=rgb(127,	63,	191, maxColorValue = 255))+
  scale_x_continuous(breaks = seq(0,180,30), limits = c(-15, 180), labels = seq(0,3,0.5))+
  labs(title="VC RR sol 52.5 mM", x="Tiempo [h]", y="Concentracion [mM]")+
  theme_bw()+
  guides(title=NULL)+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.text = element_text(size = 8))
ggsave("01 Graf RR  Vc.svg", width = 6, height = 6, units = c("cm"))

ggplot()+
  geom_line(data = VcAij, aes(x=Time, y=Concentration),size = 0.7, col=rgb(127,	63,	191, maxColorValue = 255))+
  scale_x_continuous(breaks = seq(0,180,30), limits = c(-15, 180), labels = seq(0,3,0.5))+
  labs(title="VC AIJ sol 45.43 mM", x="Tiempo [h]", y="Concentracion [mM]")+
  theme_bw()+
  guides(title=NULL)+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.text = element_text(size = 8))
ggsave("01 Graf AIJ Vc.svg", width = 6, height = 6, units = c("cm"))


#N-Acetilcisteina
NacRr38mM <- Simul3Comp(PKdata$V1[2], PKdata$V2[2], PKdata$V3[2],
                    PKdata$CL[2], PKdata$Q[2],PKdata$Q3[2],
                    tD= c(0.5, 1.5, 3),
                    R= c(13.86, 2.31, 0))
NacRr50mM <- Simul3Comp(PKdata$V1[2], PKdata$V2[2], PKdata$V3[2],
                    PKdata$CL[2], PKdata$Q[2],PKdata$Q3[2],
                    tD= c(0.5, 1.5, 3),
                    R= c(18, 3, 0))
NacAij <- Simul3Comp(PKdata$V1[2], PKdata$V2[2], PKdata$V3[2],
                    PKdata$CL[2], PKdata$Q[2],PKdata$Q3[2],
                    tD= c(0.5, 1.5, 3),
                    R= c(16.54, 16.54, 0))

ggplot()+
  geom_line(data=NacRr38mM, aes(x=Time, y=Concentration), 
            size = 0.7, col=rgb(63,	191,	255, maxColorValue=255))+
  scale_x_continuous(breaks = seq(0,180,30), limits = c(-15, 180), labels = seq(0,3,0.5))+
  labs(title="NAC RR sol 38.5mM", x="Tiempo [h]", y="Concentracion [mM]")+
  theme_bw()+
  guides(title=NULL,col = guide_legend(nrow = 4))+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8))
ggsave("01 Graf RR Nac 38.5mM.svg", width = 6, height = 6, units = c("cm"))
ggplot()+
  geom_line(data=NacRr50mM, aes(x=Time, y=Concentration), 
            size = 0.7, col=rgb(63,	191,	255, maxColorValue=255))+
  scale_x_continuous(breaks = seq(0,180,30), limits = c(-15, 180), labels = seq(0,3,0.5))+
  labs(title="NAC RR sol 50 mM", x="Tiempo [h]", y="Concentracion [mM]")+
  theme_bw()+
  guides(title=NULL,col = guide_legend(nrow = 4))+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8))
ggsave("01 Graf RR Nac 50mM.svg", width = 6, height = 6, units = c("cm"))

ggplot()+
  geom_line(data=NacAij, aes(x=Time, y=Concentration), 
            size = 0.7, col=rgb(63,	191,	255, maxColorValue=255))+
  scale_x_continuous(breaks = seq(0,180,30), limits = c(-15, 180), labels = seq(0,3,0.5))+
  labs(title="NAC AIJ sol 99.26 mM", x="Tiempo [h]", y="Concentracion [mM]")+
  theme_bw()+
  guides(title=NULL,col = guide_legend(nrow = 4))+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8))
ggsave("01 Graf AIJ Nac.svg", width = 6, height = 6, units = c("cm"))



# Deferoxamina
DfoRr57 <- Simul2Comp(V=PKdata$V1[3], V2=PKdata$V2[3], 
                  CL=PKdata$CL[3], Q=PKdata$Q[3],
                  tD=c(0.5, 1.5, 3), 
                  R=c(2.052, 0.342, 0))
DfoRr42 <- Simul2Comp(V=PKdata$V1[3], V2=PKdata$V2[3], 
                  CL=PKdata$CL[3], Q=PKdata$Q[3],
                  tD=c(0.5, 1.5, 3), 
                  R=c(1.512, 0.252, 0))
DfoAij <- Simul2Comp(V=PKdata$V1[3], V2=PKdata$V2[3], 
                  CL=PKdata$CL[3], Q=PKdata$Q[3],
                  tD=c(0.5, 1.5, 3), 
                  R=c(1.522533, 1.522533, 0))

ggplot()+
  geom_line(data=DfoRr57, aes(x=Time, y=Concentration), 
            size = 0.7, col=rgb(255,	191,	63, maxColorValue=255))+
  scale_x_continuous(breaks = seq(0,180,30), limits = c(-15, 180), labels = seq(0,3,0.5))+
  labs(title="DFO RR sol 5.7 mM", x="Tiempo [h]", y="Concentracion [mM]")+
  theme_bw()+
  guides(title=NULL,col = guide_legend(nrow = 4))+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8))
ggsave("01 Graf RR Dfo con 5_7 mM.svg", width = 6, height = 6, units = c("cm"))
ggplot()+
  geom_line(data=DfoRr42, aes(x=Time, y=Concentration), 
            size = 0.7, col=rgb(255,	191,	63, maxColorValue=255))+
  scale_x_continuous(breaks = seq(0,180,30), limits = c(-15, 180), labels = seq(0,3,0.5))+
  labs(title="DFO RR sol 4.2 mM", x="Tiempo [h]", y="Concentracion [mM]")+
  theme_bw()+
  guides(title=NULL,col = guide_legend(nrow = 4))+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8))
ggsave("Graf RR Dfo con 4_2 mM.svg", width = 6, height = 6, units = c("cm"))

ggplot()+
  geom_line(data=DfoAij, aes(x=Time, y=Concentration), 
            size = 0.7, col=rgb(255,	191,	63, maxColorValue=255))+
  scale_x_continuous(breaks = seq(0,180,30), limits = c(-15, 180), labels = seq(0,3,0.5))+
  labs(title="DFO AIJ sol 1500 mg", x="Tiempo [h]", y="Concentracion [mM]")+
  theme_bw()+
  guides(title=NULL,col = guide_legend(nrow = 4))+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8))
ggsave("Graf RR Dfo con 4_2 mM.svg", width = 6, height = 6, units = c("cm"))






#Infusiones fase 1 RR corregidas 20/06/2022
#Vitamina C

VcRr <- Simul1Comp(PKdata$V1[1], PKdata$CL[1], 
                   tD= c(0.5, 1.5, 3),
                   R= c(20.44, 3.4, 0), 
                   0.06)


ggplot()+
  geom_line(data = VcRr, aes(x=Time, y=Concentration),size = 0.7, col=rgb(127,	63,	191, maxColorValue = 255))+
  scale_x_continuous(breaks = seq(0,180,30), limits = c(-15, 180), labels = seq(0,3,0.5))+
  labs(title="VC RR sol 56.78 mM", x="Tiempo [h]", y="Concentracion [mM]")+
  theme_bw()+
  guides(title=NULL)+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.text = element_text(size = 8))
ggsave("01 Graf RR  Vc 2022-06-20.svg", width = 6, height = 6, units = c("cm"))


#N-Acetilcisteina
NacRr <- Simul3Comp(PKdata$V1[2], PKdata$V2[2], PKdata$V3[2],
                        PKdata$CL[2], PKdata$Q[2],PKdata$Q3[2],
                        tD= c(0.5, 1.5, 3),
                        R= c(17.647, 2.941, 0))



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
ggsave("01 Graf RR Nac 2022-06-20.svg", width = 6, height = 6, units = c("cm"))



# Deferoxamina
DfoRr <- Simul2Comp(V=PKdata$V1[3], V2=PKdata$V2[3], 
                      CL=PKdata$CL[3], Q=PKdata$Q[3],
                      tD=c(0.5, 1.5, 3), 
                      R=c(2.1924, 0.3654, 0))


ggplot()+
  geom_line(data=DfoRr, aes(x=Time, y=Concentration), 
            size = 0.7, col=rgb(255,	191,	63, maxColorValue=255))+
  scale_x_continuous(breaks = seq(0,180,30), limits = c(-15, 180), labels = seq(0,3,0.5))+
  labs(title="DFO RR sol 5.7 mM", x="Tiempo [h]", y="Concentracion [mM]")+
  theme_bw()+
  guides(title=NULL,col = guide_legend(nrow = 4))+
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8))
ggsave("01 Graf RR Dfo con 2022-06-20 mM.svg", width = 6, height = 6, units = c("cm"))
