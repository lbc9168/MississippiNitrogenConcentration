
## ILLI_VC

ILLI_VC = read.csv("ILLI_VC.csv")

ILLI_VC.lm = with(ILLI_VC, lm(log(N_Conc) ~ log(N_Price) + log(Flow) + log(CSratio) + log(NP_3mAvg)+ as.factor(Month)))
summary(ILLI_VC.lm)


ILLI_VC.lm.1m = with(ILLI_VC, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(Flow) + 
                                   log(CSratio) + log(CSR1) + log(NP_3mAvg)+ as.factor(Month)))
summary(ILLI_VC.lm.1m)


ILLI_VC.lm.2m = with(ILLI_VC, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(Flow) + 
                                   log(CSratio) + log(CSR1) + log(CSR2) + log(NP_3mAvg)+ as.factor(Month)))
summary(ILLI_VC.lm.2m)


ILLI_VC.lm.3m = with(ILLI_VC[ILLI_VC$NP3 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(Flow) + 
                                   log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(NP_3mAvg)+ as.factor(Month)))
summary(ILLI_VC.lm.3m)


ILLI_VC.lm.4m = with(ILLI_VC[ILLI_VC$NP4 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) +
                                                      log(NP_3mAvg)+ as.factor(Month)))
summary(ILLI_VC.lm.4m)


ILLI_VC.lm.5m = with(ILLI_VC[ILLI_VC$NP5 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(NP_3mAvg)+ as.factor(Month)))
summary(ILLI_VC.lm.5m)


ILLI_VC.lm.6m = with(ILLI_VC[ILLI_VC$NP6 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(NP_3mAvg)+ as.factor(Month)))
summary(ILLI_VC.lm.6m)


ILLI_VC.lm.7m = with(ILLI_VC[ILLI_VC$NP7 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(NP7) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(CSR7) + log(NP_3mAvg)+ as.factor(Month)))
summary(ILLI_VC.lm.7m)


ILLI_VC.lm.8m = with(ILLI_VC[ILLI_VC$NP8 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(NP7) + log(NP8) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(CSR7) + log(CSR8) + log(NP_3mAvg)+ as.factor(Month)))
summary(ILLI_VC.lm.8m)


ILLI_VC.lm.9m = with(ILLI_VC[ILLI_VC$NP9 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + log(NP_3mAvg)+ as.factor(Month)))
summary(ILLI_VC.lm.9m)


ILLI_VC.lm.10m = with(ILLI_VC[ILLI_VC$NP10 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                      log(CSR10) + log(NP_3mAvg)+ as.factor(Month)))
summary(ILLI_VC.lm.10m)


ILLI_VC.lm.11m = with(ILLI_VC[ILLI_VC$NP11 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                        log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + 
                                                        log(NP11) + log(Flow) + 
                                                        log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                        log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                        log(CSR10) + log(CSR11) + log(NP_3mAvg)+ as.factor(Month)))
summary(ILLI_VC.lm.11m)


ILLI_VC.lm.12m = with(ILLI_VC[ILLI_VC$NP12 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                        log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + 
                                                        log(NP11) + log(NP12) + log(Flow) + 
                                                        log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                        log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                        log(CSR10) + log(CSR11) + log(CSR12) + log(NP_3mAvg)+ as.factor(Month)))
summary(ILLI_VC.lm.12m)


## IOWA_WAP

IOWA_WAP = read.csv("IOWA_WAP.csv")

IOWA_WAP.lm = with(IOWA_WAP, lm(log(N_Conc) ~ log(N_Price) + log(Flow) + log(CSratio) + log(NP_3mAvg)+ as.factor(Month)))
summary(IOWA_WAP.lm)


IOWA_WAP.lm.1m = with(IOWA_WAP, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(Flow) + 
                                   log(CSratio) + log(CSR1) + log(NP_3mAvg)+ as.factor(Month)))
summary(IOWA_WAP.lm.1m)


IOWA_WAP.lm.2m = with(IOWA_WAP, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(Flow) + 
                                   log(CSratio) + log(CSR1) + log(CSR2) + log(NP_3mAvg)+ as.factor(Month)))
summary(IOWA_WAP.lm.2m)


IOWA_WAP.lm.3m = with(IOWA_WAP[IOWA_WAP$NP3 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(NP_3mAvg)+ as.factor(Month)))
summary(IOWA_WAP.lm.3m)


IOWA_WAP.lm.4m = with(IOWA_WAP[IOWA_WAP$NP4 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) +
                                                      log(NP_3mAvg)+ as.factor(Month)))
summary(IOWA_WAP.lm.4m)


IOWA_WAP.lm.5m = with(IOWA_WAP[IOWA_WAP$NP5 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(NP_3mAvg)+ as.factor(Month)))
summary(IOWA_WAP.lm.5m)


IOWA_WAP.lm.6m = with(IOWA_WAP[IOWA_WAP$NP6 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(NP_3mAvg)+ as.factor(Month)))
summary(IOWA_WAP.lm.6m)


IOWA_WAP.lm.7m = with(IOWA_WAP[IOWA_WAP$NP7 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(NP7) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(CSR7) + log(NP_3mAvg)+ as.factor(Month)))
summary(IOWA_WAP.lm.7m)


IOWA_WAP.lm.8m = with(IOWA_WAP[IOWA_WAP$NP8 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(NP7) + log(NP8) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(CSR7) + log(CSR8) + log(NP_3mAvg)+ as.factor(Month)))
summary(IOWA_WAP.lm.8m)


IOWA_WAP.lm.9m = with(IOWA_WAP[IOWA_WAP$NP9 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + log(NP_3mAvg)+ as.factor(Month)))
summary(IOWA_WAP.lm.9m)


IOWA_WAP.lm.10m = with(IOWA_WAP[IOWA_WAP$NP10 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                        log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + log(Flow) + 
                                                        log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                        log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                        log(CSR10) + log(NP_3mAvg)+ as.factor(Month)))
summary(IOWA_WAP.lm.10m)


IOWA_WAP.lm.11m = with(IOWA_WAP[IOWA_WAP$NP11 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                        log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + 
                                                        log(NP11) + log(Flow) + 
                                                        log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                        log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                        log(CSR10) + log(CSR11) + log(NP_3mAvg)+ as.factor(Month)))
summary(IOWA_WAP.lm.11m)


IOWA_WAP.lm.12m = with(IOWA_WAP[IOWA_WAP$NP12 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                        log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + 
                                                        log(NP11) + log(NP12) + log(Flow) + 
                                                        log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                        log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                        log(CSR10) + log(CSR11) + log(CSR12) + log(NP_3mAvg)+ as.factor(Month)))
summary(IOWA_WAP.lm.12m)



## MIZZ_HE

MIZZ_HE = read.csv("MIZZ_HE.csv")

MIZZ_HE.lm = with(MIZZ_HE, lm(log(N_Conc) ~ log(N_Price) + log(Flow) + log(CSratio) + log(NP_3mAvg)+ as.factor(Month)))
summary(MIZZ_HE.lm)


MIZZ_HE.lm.1m = with(MIZZ_HE, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(Flow) + 
                                     log(CSratio) + log(CSR1) + log(NP_3mAvg)+ as.factor(Month)))
summary(MIZZ_HE.lm.1m)


MIZZ_HE.lm.2m = with(MIZZ_HE, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(Flow) + 
                                     log(CSratio) + log(CSR1) + log(CSR2) + log(NP_3mAvg)+ as.factor(Month)))
summary(MIZZ_HE.lm.2m)


MIZZ_HE.lm.3m = with(MIZZ_HE[MIZZ_HE$NP3 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(Flow) + 
                                                         log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(NP_3mAvg)+ as.factor(Month)))
summary(MIZZ_HE.lm.3m)


MIZZ_HE.lm.4m = with(MIZZ_HE[MIZZ_HE$NP4 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                         log(Flow) + 
                                                         log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) +
                                                         log(NP_3mAvg)+ as.factor(Month)))
summary(MIZZ_HE.lm.4m)


MIZZ_HE.lm.5m = with(MIZZ_HE[MIZZ_HE$NP5 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                         log(NP5) + log(Flow) + 
                                                         log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                         log(NP_3mAvg)+ as.factor(Month)))
summary(MIZZ_HE.lm.5m)


MIZZ_HE.lm.6m = with(MIZZ_HE[MIZZ_HE$NP6 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                         log(NP5) + log(NP6) + log(Flow) + 
                                                         log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                         log(CSR6) + log(NP_3mAvg)+ as.factor(Month)))
summary(MIZZ_HE.lm.6m)


MIZZ_HE.lm.7m = with(MIZZ_HE[MIZZ_HE$NP7 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                         log(NP5) + log(NP6) + log(NP7) + log(Flow) + 
                                                         log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                         log(CSR6) + log(CSR7) + log(NP_3mAvg)+ as.factor(Month)))
summary(MIZZ_HE.lm.7m)


MIZZ_HE.lm.8m = with(MIZZ_HE[MIZZ_HE$NP8 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                         log(NP5) + log(NP6) + log(NP7) + log(NP8) + log(Flow) + 
                                                         log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                         log(CSR6) + log(CSR7) + log(CSR8) + log(NP_3mAvg)+ as.factor(Month)))
summary(MIZZ_HE.lm.8m)


MIZZ_HE.lm.9m = with(MIZZ_HE[MIZZ_HE$NP9 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                         log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(Flow) + 
                                                         log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                         log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + log(NP_3mAvg)+ as.factor(Month)))
summary(MIZZ_HE.lm.9m)


MIZZ_HE.lm.10m = with(MIZZ_HE[MIZZ_HE$NP10 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                           log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + log(Flow) + 
                                                           log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                           log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                           log(CSR10) + log(NP_3mAvg)+ as.factor(Month)))
summary(MIZZ_HE.lm.10m)


MIZZ_HE.lm.11m = with(MIZZ_HE[MIZZ_HE$NP11 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                           log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + 
                                                           log(NP11) + log(Flow) + 
                                                           log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                           log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                           log(CSR10) + log(CSR11) + log(NP_3mAvg)+ as.factor(Month)))
summary(MIZZ_HE.lm.11m)


MIZZ_HE.lm.12m = with(MIZZ_HE[MIZZ_HE$NP12 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                           log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + 
                                                           log(NP11) + log(NP12) + log(Flow) + 
                                                           log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                           log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                           log(CSR10) + log(CSR11) + log(CSR12) + log(NP_3mAvg)+ as.factor(Month)))
summary(MIZZ_HE.lm.12m)
