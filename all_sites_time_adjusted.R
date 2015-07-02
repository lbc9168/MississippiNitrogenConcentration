
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


