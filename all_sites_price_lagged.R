##ILLI_VC_3to8
ILLI_VC_3to8 = read.csv("ILLI_VC_3to8.csv")
ILLI_VC_3to8.lm.no.lag = with(ILLI_VC_3to8, lm(log(N_Conc) ~ log(N_Price) + log(Flow) + log(Precip) 
                                               + log(Corn_Price) + log(SB_Price)))
summary(ILLI_VC_3to8.lm.no.lag)                              

ILLI_VC_3to8.lm.1month = with(ILLI_VC_3to8, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(Flow) + log(Precip) 
                                               + log(Corn_Price) + log(CP1) + log(SB_Price) + log(SBP1)))
summary(ILLI_VC_3to8.lm.1month)

ILLI_VC_3to8.lm.2month = with(ILLI_VC_3to8, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(Flow) + log(Precip) 
                                               + log(Corn_Price) + log(CP1) + log(CP2) + log(SB_Price) + log(SBP1)
                                               + log(SBP2)))
summary(ILLI_VC_3to8.lm.2month)

ILLI_VC_3to8.lm.3month = with(ILLI_VC_3to8, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) +
                                                 log(Flow) + log(Precip) +
                                                 log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + 
                                                 log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3)))
summary(ILLI_VC_3to8.lm.3month)

ILLI_VC_3to8.lm.4month = with(ILLI_VC_3to8, lm(log(N_Conc) ~ log(N_Price) + 
                                                 log(NP1) + log(NP2) + log(NP3) + log(NP4)+
                                                 log(Flow) + log(Precip) +
                                                 log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + log(CP4) + 
                                                 log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3) + log(SBP4)))
summary(ILLI_VC_3to8.lm.4month)

ILLI_VC_3to8.lm.5month = with(ILLI_VC_3to8, lm(log(N_Conc) ~ log(N_Price) + 
                                                 log(NP1) + log(NP2) + log(NP3) + log(NP4) + log(NP5) +
                                                 log(Flow) + log(Precip) +
                                                 log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + log(CP4) + log(CP5) +
                                                 log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3) + log(SBP4) + log(SBP5)))
summary(ILLI_VC_3to8.lm.5month)

ILLI_VC_3to8.lm.6month = with(ILLI_VC_3to8, lm(log(N_Conc) ~ log(N_Price) + 
                                                 log(NP1) + log(NP2) + log(NP3) + log(NP4) + log(NP5) + log(NP6) +
                                                 log(Flow) + log(Precip) +
                                                 log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + log(CP4) + log(CP5) + log(CP6) +
                                                 log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3) + log(SBP4) + log(SBP5) + log(SBP6)))
summary(ILLI_VC_3to8.lm.6month)


## MSSP_CL_3to8
MSSP_CL_3to8 = read.csv("MSSP_CL_3to8.csv")

MSSP_CL_3to8.lm.no.lag = with(MSSP_CL_3to8, lm(log(N_Conc) ~ log(N_Price) + log(Flow) + log(Precip) 
                                               + log(Corn_Price) + log(SB_Price)))
summary(MSSP_CL_3to8.lm.no.lag)  

MSSP_CL_3to8.lm.1month = with(MSSP_CL_3to8, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(Flow) + log(Precip) 
                                               + log(Corn_Price) + log(CP1) + log(SB_Price) + log(SBP1)))
summary(MSSP_CL_3to8.lm.1month)

MSSP_CL_3to8.lm.2month = with(MSSP_CL_3to8, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(Flow) + log(Precip) 
                                               + log(Corn_Price) + log(CP1) + log(CP2) + log(SB_Price) + log(SBP1)
                                               + log(SBP2)))
summary(MSSP_CL_3to8.lm.2month)

MSSP_CL_3to8.lm.3month = with(MSSP_CL_3to8, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) +
                                                 log(Flow) + log(Precip) +
                                                 log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + 
                                                 log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3)))
summary(MSSP_CL_3to8.lm.3month)

MSSP_CL_3to8.lm.4month = with(MSSP_CL_3to8, lm(log(N_Conc) ~ log(N_Price) + 
                                                 log(NP1) + log(NP2) + log(NP3) + log(NP4)+
                                                 log(Flow) + log(Precip) +
                                                 log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + log(CP4) + 
                                                 log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3) + log(SBP4)))
summary(MSSP_CL_3to8.lm.4month)

MSSP_CL_3to8.lm.5month = with(MSSP_CL_3to8, lm(log(N_Conc) ~ log(N_Price) + 
                                                 log(NP1) + log(NP2) + log(NP3) + log(NP4) + log(NP5) +
                                                 log(Flow) + log(Precip) +
                                                 log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + log(CP4) + log(CP5) +
                                                 log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3) + log(SBP4) + log(SBP5)))
summary(MSSP_CL_3to8.lm.5month)

MSSP_CL_3to8.lm.6month = with(MSSP_CL_3to8, lm(log(N_Conc) ~ log(N_Price) + 
                                                 log(NP1) + log(NP2) + log(NP3) + log(NP4) + log(NP5) + log(NP6) +
                                                 log(Flow) + log(Precip) +
                                                 log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + log(CP4) + log(CP5) + log(CP6) +
                                                 log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3) + log(SBP4) + log(SBP5) + log(SBP6)))
summary(MSSP_CL_3to8.lm.6month)
