setwd("C:/Users/Bingcai/OSU/AED Research/Mississipi/2015 Summer_All sites/All_data/Time adjusted added")

## ILLI_VC

ILLI_VC = read.csv("ILLI_VC.csv")

ILLI_VC.lm = with(ILLI_VC, lm(log(N_Conc) ~ log(N_Price) + log(Flow) + log(CSratio) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(ILLI_VC.lm)


ILLI_VC.lm.1m = with(ILLI_VC, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(Flow) + 
                                   log(CSratio) + log(CSR1) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(ILLI_VC.lm.1m)


ILLI_VC.lm.2m = with(ILLI_VC, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(Flow) + 
                                   log(CSratio) + log(CSR1) + log(CSR2) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(ILLI_VC.lm.2m)


ILLI_VC.lm.3m = with(ILLI_VC[ILLI_VC$NP3 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(ILLI_VC.lm.3m)


ILLI_VC.lm.4m = with(ILLI_VC[ILLI_VC$NP4 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) +
                                                      log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(ILLI_VC.lm.4m)


ILLI_VC.lm.5m = with(ILLI_VC[ILLI_VC$NP5 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(ILLI_VC.lm.5m)


ILLI_VC.lm.6m = with(ILLI_VC[ILLI_VC$NP6 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(ILLI_VC.lm.6m)


ILLI_VC.lm.7m = with(ILLI_VC[ILLI_VC$NP7 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(NP7) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(CSR7) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(ILLI_VC.lm.7m)


ILLI_VC.lm.8m = with(ILLI_VC[ILLI_VC$NP8 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(NP7) + log(NP8) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(CSR7) + log(CSR8) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(ILLI_VC.lm.8m)


ILLI_VC.lm.9m = with(ILLI_VC[ILLI_VC$NP9 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(ILLI_VC.lm.9m)


ILLI_VC.lm.10m = with(ILLI_VC[ILLI_VC$NP10 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                        log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + log(Flow) + 
                                                        log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                        log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                        log(CSR10) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(ILLI_VC.lm.10m)


ILLI_VC.lm.11m = with(ILLI_VC[ILLI_VC$NP11 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                        log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + 
                                                        log(NP11) + log(Flow) + 
                                                        log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                        log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                        log(CSR10) + log(CSR11) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(ILLI_VC.lm.11m)


ILLI_VC.lm.12m = with(ILLI_VC[ILLI_VC$NP12 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                        log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + 
                                                        log(NP11) + log(NP12) + log(Flow) + 
                                                        log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                        log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                        log(CSR10) + log(CSR11) + log(CSR12) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(ILLI_VC.lm.12m)


## IOWA_WAP

IOWA_WAP = read.csv("IOWA_WAP.csv")

IOWA_WAP.lm = with(IOWA_WAP, lm(log(N_Conc) ~ log(N_Price) + log(Flow) + log(CSratio) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(IOWA_WAP.lm)


IOWA_WAP.lm.1m = with(IOWA_WAP, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(Flow) + 
                                     log(CSratio) + log(CSR1) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(IOWA_WAP.lm.1m)


IOWA_WAP.lm.2m = with(IOWA_WAP, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(Flow) + 
                                     log(CSratio) + log(CSR1) + log(CSR2) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(IOWA_WAP.lm.2m)


IOWA_WAP.lm.3m = with(IOWA_WAP[IOWA_WAP$NP3 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(Flow) + 
                                                         log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(IOWA_WAP.lm.3m)


IOWA_WAP.lm.4m = with(IOWA_WAP[IOWA_WAP$NP4 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                         log(Flow) + 
                                                         log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) +
                                                         log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(IOWA_WAP.lm.4m)


IOWA_WAP.lm.5m = with(IOWA_WAP[IOWA_WAP$NP5 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                         log(NP5) + log(Flow) + 
                                                         log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                         log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(IOWA_WAP.lm.5m)


IOWA_WAP.lm.6m = with(IOWA_WAP[IOWA_WAP$NP6 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                         log(NP5) + log(NP6) + log(Flow) + 
                                                         log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                         log(CSR6) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(IOWA_WAP.lm.6m)


IOWA_WAP.lm.7m = with(IOWA_WAP[IOWA_WAP$NP7 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                         log(NP5) + log(NP6) + log(NP7) + log(Flow) + 
                                                         log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                         log(CSR6) + log(CSR7) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(IOWA_WAP.lm.7m)


IOWA_WAP.lm.8m = with(IOWA_WAP[IOWA_WAP$NP8 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                         log(NP5) + log(NP6) + log(NP7) + log(NP8) + log(Flow) + 
                                                         log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                         log(CSR6) + log(CSR7) + log(CSR8) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(IOWA_WAP.lm.8m)


IOWA_WAP.lm.9m = with(IOWA_WAP[IOWA_WAP$NP9 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                         log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(Flow) + 
                                                         log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                         log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(IOWA_WAP.lm.9m)


IOWA_WAP.lm.10m = with(IOWA_WAP[IOWA_WAP$NP10 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                           log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + log(Flow) + 
                                                           log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                           log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                           log(CSR10) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(IOWA_WAP.lm.10m)


IOWA_WAP.lm.11m = with(IOWA_WAP[IOWA_WAP$NP11 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                           log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + 
                                                           log(NP11) + log(Flow) + 
                                                           log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                           log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                           log(CSR10) + log(CSR11) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(IOWA_WAP.lm.11m)


IOWA_WAP.lm.12m = with(IOWA_WAP[IOWA_WAP$NP12 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                           log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + 
                                                           log(NP11) + log(NP12) + log(Flow) + 
                                                           log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                           log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                           log(CSR10) + log(CSR11) + log(CSR12) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(IOWA_WAP.lm.12m)



## MIZZ_HE

MIZZ_HE = read.csv("MIZZ_HE.csv")

MIZZ_HE.lm = with(MIZZ_HE[MIZZ_HE$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(N_Price) + log(Flow) + log(CSratio) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MIZZ_HE.lm)


MIZZ_HE.lm.1m = with(MIZZ_HE[MIZZ_HE$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(Flow) + 
                                                           log(CSratio) + log(CSR1) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MIZZ_HE.lm.1m)


MIZZ_HE.lm.2m = with(MIZZ_HE[MIZZ_HE$NP2 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MIZZ_HE.lm.2m)


MIZZ_HE.lm.3m = with(MIZZ_HE[MIZZ_HE$NP3 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MIZZ_HE.lm.3m)


MIZZ_HE.lm.4m = with(MIZZ_HE[MIZZ_HE$NP4 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) +
                                                      log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MIZZ_HE.lm.4m)


MIZZ_HE.lm.5m = with(MIZZ_HE[MIZZ_HE$NP5 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MIZZ_HE.lm.5m)


MIZZ_HE.lm.6m = with(MIZZ_HE[MIZZ_HE$NP6 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MIZZ_HE.lm.6m)


MIZZ_HE.lm.7m = with(MIZZ_HE[MIZZ_HE$NP7 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(NP7) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(CSR7) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MIZZ_HE.lm.7m)


MIZZ_HE.lm.8m = with(MIZZ_HE[MIZZ_HE$NP8 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(NP7) + log(NP8) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(CSR7) + log(CSR8) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MIZZ_HE.lm.8m)


MIZZ_HE.lm.9m = with(MIZZ_HE[MIZZ_HE$NP9 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MIZZ_HE.lm.9m)


MIZZ_HE.lm.10m = with(MIZZ_HE[MIZZ_HE$NP10 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                        log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + log(Flow) + 
                                                        log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                        log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                        log(CSR10) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MIZZ_HE.lm.10m)


MIZZ_HE.lm.11m = with(MIZZ_HE[MIZZ_HE$NP11 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                        log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + 
                                                        log(NP11) + log(Flow) + 
                                                        log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                        log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                        log(CSR10) + log(CSR11) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MIZZ_HE.lm.11m)


MIZZ_HE.lm.12m = with(MIZZ_HE[MIZZ_HE$NP12 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                        log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + 
                                                        log(NP11) + log(NP12) + log(Flow) + 
                                                        log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                        log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                        log(CSR10) + log(CSR11) + log(CSR12) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MIZZ_HE.lm.12m)


## MSSP_CL

MSSP_CL = read.csv("MSSP_CL.csv")

MSSP_CL.lm = with(MSSP_CL[MSSP_CL$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(N_Price) + log(Flow) + log(CSratio) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_CL.lm)


MSSP_CL.lm.1m = with(MSSP_CL[MSSP_CL$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(Flow) + 
                                                           log(CSratio) + log(CSR1) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_CL.lm.1m)


MSSP_CL.lm.2m = with(MSSP_CL[MSSP_CL$NP2 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_CL.lm.2m)


MSSP_CL.lm.3m = with(MSSP_CL[MSSP_CL$NP3 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_CL.lm.3m)


MSSP_CL.lm.4m = with(MSSP_CL[MSSP_CL$NP4 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) +
                                                      log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_CL.lm.4m)


MSSP_CL.lm.5m = with(MSSP_CL[MSSP_CL$NP5 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_CL.lm.5m)


MSSP_CL.lm.6m = with(MSSP_CL[MSSP_CL$NP6 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_CL.lm.6m)


MSSP_CL.lm.7m = with(MSSP_CL[MSSP_CL$NP7 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(NP7) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(CSR7) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_CL.lm.7m)


MSSP_CL.lm.8m = with(MSSP_CL[MSSP_CL$NP8 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(NP7) + log(NP8) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(CSR7) + log(CSR8) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_CL.lm.8m)


MSSP_CL.lm.9m = with(MSSP_CL[MSSP_CL$NP9 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_CL.lm.9m)


MSSP_CL.lm.10m = with(MSSP_CL[MSSP_CL$NP10 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                        log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + log(Flow) + 
                                                        log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                        log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                        log(CSR10) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_CL.lm.10m)


MSSP_CL.lm.11m = with(MSSP_CL[MSSP_CL$NP11 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                        log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + 
                                                        log(NP11) + log(Flow) + 
                                                        log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                        log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                        log(CSR10) + log(CSR11) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_CL.lm.11m)


MSSP_CL.lm.12m = with(MSSP_CL[MSSP_CL$NP12 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                        log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + 
                                                        log(NP11) + log(NP12) + log(Flow) + 
                                                        log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                        log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                        log(CSR10) + log(CSR11) + log(CSR12) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_CL.lm.12m)




## MSSP_GR

MSSP_GR = read.csv("MSSP_GR.csv")

MSSP_GR.lm = with(MSSP_GR[MSSP_GR$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(N_Price) + log(Flow) + log(CSratio) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_GR.lm)


MSSP_GR.lm.1m = with(MSSP_GR[MSSP_GR$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(Flow) + 
                                                           log(CSratio) + log(CSR1) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_GR.lm.1m)


MSSP_GR.lm.2m = with(MSSP_GR[MSSP_GR$NP2 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_GR.lm.2m)


MSSP_GR.lm.3m = with(MSSP_GR[MSSP_GR$NP3 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_GR.lm.3m)


MSSP_GR.lm.4m = with(MSSP_GR[MSSP_GR$NP4 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) +
                                                      log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_GR.lm.4m)


MSSP_GR.lm.5m = with(MSSP_GR[MSSP_GR$NP5 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_GR.lm.5m)


MSSP_GR.lm.6m = with(MSSP_GR[MSSP_GR$NP6 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_GR.lm.6m)


MSSP_GR.lm.7m = with(MSSP_GR[MSSP_GR$NP7 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(NP7) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(CSR7) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_GR.lm.7m)


MSSP_GR.lm.8m = with(MSSP_GR[MSSP_GR$NP8 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(NP7) + log(NP8) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(CSR7) + log(CSR8) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_GR.lm.8m)


MSSP_GR.lm.9m = with(MSSP_GR[MSSP_GR$NP9 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_GR.lm.9m)


MSSP_GR.lm.10m = with(MSSP_GR[MSSP_GR$NP10 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                        log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + log(Flow) + 
                                                        log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                        log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                        log(CSR10) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_GR.lm.10m)


MSSP_GR.lm.11m = with(MSSP_GR[MSSP_GR$NP11 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                        log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + 
                                                        log(NP11) + log(Flow) + 
                                                        log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                        log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                        log(CSR10) + log(CSR11) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_GR.lm.11m)


MSSP_GR.lm.12m = with(MSSP_GR[MSSP_GR$NP12 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                        log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + 
                                                        log(NP11) + log(NP12) + log(Flow) + 
                                                        log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                        log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                        log(CSR10) + log(CSR11) + log(CSR12) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_GR.lm.12m)




## MSSP_OUT

MSSP_OUT = read.csv("MSSP_OUT.csv")

MSSP_OUT.lm = with(MSSP_OUT[MSSP_OUT$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(N_Price) + log(Flow) + log(CSratio) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_OUT.lm)


MSSP_OUT.lm.1m = with(MSSP_OUT[MSSP_OUT$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(Flow) + 
                                                              log(CSratio) + log(CSR1) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_OUT.lm.1m)


MSSP_OUT.lm.2m = with(MSSP_OUT[MSSP_OUT$NP2 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(Flow) + 
                                                         log(CSratio) + log(CSR1) + log(CSR2) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_OUT.lm.2m)


MSSP_OUT.lm.3m = with(MSSP_OUT[MSSP_OUT$NP3 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(Flow) + 
                                                         log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_OUT.lm.3m)


MSSP_OUT.lm.4m = with(MSSP_OUT[MSSP_OUT$NP4 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                         log(Flow) + 
                                                         log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) +
                                                         log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_OUT.lm.4m)


MSSP_OUT.lm.5m = with(MSSP_OUT[MSSP_OUT$NP5 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                         log(NP5) + log(Flow) + 
                                                         log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                         log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_OUT.lm.5m)


MSSP_OUT.lm.6m = with(MSSP_OUT[MSSP_OUT$NP6 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                         log(NP5) + log(NP6) + log(Flow) + 
                                                         log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                         log(CSR6) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_OUT.lm.6m)


MSSP_OUT.lm.7m = with(MSSP_OUT[MSSP_OUT$NP7 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                         log(NP5) + log(NP6) + log(NP7) + log(Flow) + 
                                                         log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                         log(CSR6) + log(CSR7) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_OUT.lm.7m)


MSSP_OUT.lm.8m = with(MSSP_OUT[MSSP_OUT$NP8 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                         log(NP5) + log(NP6) + log(NP7) + log(NP8) + log(Flow) + 
                                                         log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                         log(CSR6) + log(CSR7) + log(CSR8) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_OUT.lm.8m)


MSSP_OUT.lm.9m = with(MSSP_OUT[MSSP_OUT$NP9 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                         log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(Flow) + 
                                                         log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                         log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_OUT.lm.9m)


MSSP_OUT.lm.10m = with(MSSP_OUT[MSSP_OUT$NP10 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                           log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + log(Flow) + 
                                                           log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                           log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                           log(CSR10) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_OUT.lm.10m)


MSSP_OUT.lm.11m = with(MSSP_OUT[MSSP_OUT$NP11 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                           log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + 
                                                           log(NP11) + log(Flow) + 
                                                           log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                           log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                           log(CSR10) + log(CSR11) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_OUT.lm.11m)


MSSP_OUT.lm.12m = with(MSSP_OUT[MSSP_OUT$NP12 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                           log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + 
                                                           log(NP11) + log(NP12) + log(Flow) + 
                                                           log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                           log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                           log(CSR10) + log(CSR11) + log(CSR12) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_OUT.lm.12m)




## MSSP_TH

MSSP_TH = read.csv("MSSP_TH.csv")

MSSP_TH.lm = with(MSSP_TH[MSSP_TH$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(N_Price) + log(Flow) + log(CSratio) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_TH.lm)


MSSP_TH.lm.1m = with(MSSP_TH[MSSP_TH$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(Flow) + 
                                                           log(CSratio) + log(CSR1) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_TH.lm.1m)


MSSP_TH.lm.2m = with(MSSP_TH[MSSP_TH$NP2 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_TH.lm.2m)


MSSP_TH.lm.3m = with(MSSP_TH[MSSP_TH$NP3 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_TH.lm.3m)


MSSP_TH.lm.4m = with(MSSP_TH[MSSP_TH$NP4 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) +
                                                      log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_TH.lm.4m)


MSSP_TH.lm.5m = with(MSSP_TH[MSSP_TH$NP5 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_TH.lm.5m)


MSSP_TH.lm.6m = with(MSSP_TH[MSSP_TH$NP6 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_TH.lm.6m)


MSSP_TH.lm.7m = with(MSSP_TH[MSSP_TH$NP7 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(NP7) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(CSR7) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_TH.lm.7m)


MSSP_TH.lm.8m = with(MSSP_TH[MSSP_TH$NP8 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(NP7) + log(NP8) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(CSR7) + log(CSR8) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_TH.lm.8m)


MSSP_TH.lm.9m = with(MSSP_TH[MSSP_TH$NP9 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                      log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(Flow) + 
                                                      log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                      log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_TH.lm.9m)


MSSP_TH.lm.10m = with(MSSP_TH[MSSP_TH$NP10 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                        log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + log(Flow) + 
                                                        log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                        log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                        log(CSR10) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_TH.lm.10m)


MSSP_TH.lm.11m = with(MSSP_TH[MSSP_TH$NP11 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                        log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + 
                                                        log(NP11) + log(Flow) + 
                                                        log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                        log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                        log(CSR10) + log(CSR11) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_TH.lm.11m)


MSSP_TH.lm.12m = with(MSSP_TH[MSSP_TH$NP12 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                        log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + 
                                                        log(NP11) + log(NP12) + log(Flow) + 
                                                        log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                        log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                        log(CSR10) + log(CSR11) + log(CSR12) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(MSSP_TH.lm.12m)




## OHIO_GRCH

OHIO_GRCH = read.csv("OHIO_GRCH.csv")

OHIO_GRCH.lm = with(OHIO_GRCH[OHIO_GRCH$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(N_Price) + log(Flow) + log(CSratio) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(OHIO_GRCH.lm)


OHIO_GRCH.lm.1m = with(OHIO_GRCH[OHIO_GRCH$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(Flow) + 
                                                                 log(CSratio) + log(CSR1) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(OHIO_GRCH.lm.1m)


OHIO_GRCH.lm.2m = with(OHIO_GRCH[OHIO_GRCH$NP2 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(Flow) + 
                                                            log(CSratio) + log(CSR1) + log(CSR2) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(OHIO_GRCH.lm.2m)


OHIO_GRCH.lm.3m = with(OHIO_GRCH[OHIO_GRCH$NP3 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(Flow) + 
                                                            log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(OHIO_GRCH.lm.3m)


OHIO_GRCH.lm.4m = with(OHIO_GRCH[OHIO_GRCH$NP4 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                            log(Flow) + 
                                                            log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) +
                                                            log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(OHIO_GRCH.lm.4m)


OHIO_GRCH.lm.5m = with(OHIO_GRCH[OHIO_GRCH$NP5 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                            log(NP5) + log(Flow) + 
                                                            log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                            log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(OHIO_GRCH.lm.5m)


OHIO_GRCH.lm.6m = with(OHIO_GRCH[OHIO_GRCH$NP6 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                            log(NP5) + log(NP6) + log(Flow) + 
                                                            log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                            log(CSR6) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(OHIO_GRCH.lm.6m)


OHIO_GRCH.lm.7m = with(OHIO_GRCH[OHIO_GRCH$NP7 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                            log(NP5) + log(NP6) + log(NP7) + log(Flow) + 
                                                            log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                            log(CSR6) + log(CSR7) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(OHIO_GRCH.lm.7m)


OHIO_GRCH.lm.8m = with(OHIO_GRCH[OHIO_GRCH$NP8 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                            log(NP5) + log(NP6) + log(NP7) + log(NP8) + log(Flow) + 
                                                            log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                            log(CSR6) + log(CSR7) + log(CSR8) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(OHIO_GRCH.lm.8m)


OHIO_GRCH.lm.9m = with(OHIO_GRCH[OHIO_GRCH$NP9 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                            log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(Flow) + 
                                                            log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                            log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(OHIO_GRCH.lm.9m)


OHIO_GRCH.lm.10m = with(OHIO_GRCH[OHIO_GRCH$NP10 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                              log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + log(Flow) + 
                                                              log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                              log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                              log(CSR10) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(OHIO_GRCH.lm.10m)


OHIO_GRCH.lm.11m = with(OHIO_GRCH[OHIO_GRCH$NP11 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                              log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + 
                                                              log(NP11) + log(Flow) + 
                                                              log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                              log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                              log(CSR10) + log(CSR11) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(OHIO_GRCH.lm.11m)


OHIO_GRCH.lm.12m = with(OHIO_GRCH[OHIO_GRCH$NP12 != 0,], lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) + log(NP4) +
                                                              log(NP5) + log(NP6) + log(NP7) + log(NP8) +log(NP9) + log(NP10) + 
                                                              log(NP11) + log(NP12) + log(Flow) + 
                                                              log(CSratio) + log(CSR1) + log(CSR2) + log(CSR3) + log(CSR4) + log(CSR5) +
                                                              log(CSR6) + log(CSR7) + log(CSR8) + log(CSR9) + 
                                                              log(CSR10) + log(CSR11) + log(CSR12) + log(NP_3mAvg)+ as.factor(Month) + as.factor(Year)))
summary(OHIO_GRCH.lm.12m)

