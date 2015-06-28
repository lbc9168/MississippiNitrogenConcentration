setwd("C:/Users/Bingcai/OSU/AED Research/Mississipi/2015 Summer_All sites/All_data")
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

## Make a test
MSSP_CL_3to8.lm.test = with(MSSP_CL_3to8, lm(log(N_Conc) ~ log(NP4) + log(Flow) + log(Precip) + log(Corn_Price)))
summary(MSSP_CL_3to8.lm.test)


## IOWA_WAP_3to8
IOWA_WAP_3to8 = read.csv("IOWA_WAP_3to8.csv")

IOWA_WAP_3to8.lm.no.lag = with(IOWA_WAP_3to8, lm(log(N_Conc) ~ log(N_Price) + log(Flow) + log(Precip) 
                                               + log(Corn_Price) + log(SB_Price)))
summary(IOWA_WAP_3to8.lm.no.lag)  

IOWA_WAP_3to8.lm.1month = with(IOWA_WAP_3to8, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(Flow) + log(Precip) 
                                               + log(Corn_Price) + log(CP1) + log(SB_Price) + log(SBP1)))
summary(IOWA_WAP_3to8.lm.1month)

IOWA_WAP_3to8.lm.2month = with(IOWA_WAP_3to8, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(Flow) + log(Precip) 
                                               + log(Corn_Price) + log(CP1) + log(CP2) + log(SB_Price) + log(SBP1)
                                               + log(SBP2)))
summary(IOWA_WAP_3to8.lm.2month)

IOWA_WAP_3to8.lm.3month = with(IOWA_WAP_3to8, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) +
                                                 log(Flow) + log(Precip) +
                                                 log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + 
                                                 log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3)))
summary(IOWA_WAP_3to8.lm.3month)

IOWA_WAP_3to8.lm.4month = with(IOWA_WAP_3to8, lm(log(N_Conc) ~ log(N_Price) + 
                                                 log(NP1) + log(NP2) + log(NP3) + log(NP4)+
                                                 log(Flow) + log(Precip) +
                                                 log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + log(CP4) + 
                                                 log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3) + log(SBP4)))
summary(IOWA_WAP_3to8.lm.4month)

IOWA_WAP_3to8.lm.5month = with(IOWA_WAP_3to8, lm(log(N_Conc) ~ log(N_Price) + 
                                                 log(NP1) + log(NP2) + log(NP3) + log(NP4) + log(NP5) +
                                                 log(Flow) + log(Precip) +
                                                 log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + log(CP4) + log(CP5) +
                                                 log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3) + log(SBP4) + log(SBP5)))
summary(IOWA_WAP_3to8.lm.5month)

IOWA_WAP_3to8.lm.6month = with(IOWA_WAP_3to8, lm(log(N_Conc) ~ log(N_Price) + 
                                                 log(NP1) + log(NP2) + log(NP3) + log(NP4) + log(NP5) + log(NP6) +
                                                 log(Flow) + log(Precip) +
                                                 log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + log(CP4) + log(CP5) + log(CP6) +
                                                 log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3) + log(SBP4) + log(SBP5) + log(SBP6)))
summary(IOWA_WAP_3to8.lm.6month)


## MIZZ_HE_3to8
MIZZ_HE_3to8 = read.csv("IOWA_WAP_3to8.csv")

MIZZ_HE_3to8.lm.no.lag = with(MIZZ_HE_3to8, lm(log(N_Conc) ~ log(N_Price) + log(Flow) + log(Precip) 
                                                 + log(Corn_Price) + log(SB_Price)))
summary(MIZZ_HE_3to8.lm.no.lag)  

MIZZ_HE_3to8.lm.1month = with(MIZZ_HE_3to8, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(Flow) + log(Precip) 
                                                 + log(Corn_Price) + log(CP1) + log(SB_Price) + log(SBP1)))
summary(MIZZ_HE_3to8.lm.1month)

MIZZ_HE_3to8.lm.2month = with(MIZZ_HE_3to8, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(Flow) + log(Precip) 
                                                 + log(Corn_Price) + log(CP1) + log(CP2) + log(SB_Price) + log(SBP1)
                                                 + log(SBP2)))
summary(MIZZ_HE_3to8.lm.2month)

MIZZ_HE_3to8.lm.3month = with(MIZZ_HE_3to8, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) +
                                                   log(Flow) + log(Precip) +
                                                   log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + 
                                                   log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3)))
summary(MIZZ_HE_3to8.lm.3month)

MIZZ_HE_3to8.lm.4month = with(MIZZ_HE_3to8, lm(log(N_Conc) ~ log(N_Price) + 
                                                   log(NP1) + log(NP2) + log(NP3) + log(NP4)+
                                                   log(Flow) + log(Precip) +
                                                   log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + log(CP4) + 
                                                   log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3) + log(SBP4)))
summary(MIZZ_HE_3to8.lm.4month)

MIZZ_HE_3to8.lm.5month = with(MIZZ_HE_3to8, lm(log(N_Conc) ~ log(N_Price) + 
                                                   log(NP1) + log(NP2) + log(NP3) + log(NP4) + log(NP5) +
                                                   log(Flow) + log(Precip) +
                                                   log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + log(CP4) + log(CP5) +
                                                   log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3) + log(SBP4) + log(SBP5)))
summary(MIZZ_HE_3to8.lm.5month)

MIZZ_HE_3to8.lm.6month = with(MIZZ_HE_3to8, lm(log(N_Conc) ~ log(N_Price) + 
                                                   log(NP1) + log(NP2) + log(NP3) + log(NP4) + log(NP5) + log(NP6) +
                                                   log(Flow) + log(Precip) +
                                                   log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + log(CP4) + log(CP5) + log(CP6) +
                                                   log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3) + log(SBP4) + log(SBP5) + log(SBP6)))
summary(MIZZ_HE_3to8.lm.6month)


## MSSP_GR_3to8
MSSP_GR_3to8 = read.csv("MSSP_GR_3to8.csv")

MSSP_GR_3to8.lm.no.lag = with(MSSP_GR_3to8, lm(log(N_Conc) ~ log(N_Price) + log(Flow) + log(Precip) 
                                               + log(Corn_Price) + log(SB_Price)))
summary(MSSP_GR_3to8.lm.no.lag)  

MSSP_GR_3to8.lm.1month = with(MSSP_GR_3to8, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(Flow) + log(Precip) 
                                               + log(Corn_Price) + log(CP1) + log(SB_Price) + log(SBP1)))
summary(MSSP_GR_3to8.lm.1month)

MSSP_GR_3to8.lm.2month = with(MSSP_GR_3to8, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(Flow) + log(Precip) 
                                               + log(Corn_Price) + log(CP1) + log(CP2) + log(SB_Price) + log(SBP1)
                                               + log(SBP2)))
summary(MSSP_GR_3to8.lm.2month)

MSSP_GR_3to8.lm.3month = with(MSSP_GR_3to8, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) +
                                                 log(Flow) + log(Precip) +
                                                 log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + 
                                                 log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3)))
summary(MSSP_GR_3to8.lm.3month)

MSSP_GR_3to8.lm.4month = with(MSSP_GR_3to8, lm(log(N_Conc) ~ log(N_Price) + 
                                                 log(NP1) + log(NP2) + log(NP3) + log(NP4)+
                                                 log(Flow) + log(Precip) +
                                                 log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + log(CP4) + 
                                                 log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3) + log(SBP4)))
summary(MSSP_GR_3to8.lm.4month)

MSSP_GR_3to8.lm.5month = with(MSSP_GR_3to8, lm(log(N_Conc) ~ log(N_Price) + 
                                                 log(NP1) + log(NP2) + log(NP3) + log(NP4) + log(NP5) +
                                                 log(Flow) + log(Precip) +
                                                 log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + log(CP4) + log(CP5) +
                                                 log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3) + log(SBP4) + log(SBP5)))
summary(MSSP_GR_3to8.lm.5month)

MSSP_GR_3to8.lm.6month = with(MSSP_GR_3to8, lm(log(N_Conc) ~ log(N_Price) + 
                                                 log(NP1) + log(NP2) + log(NP3) + log(NP4) + log(NP5) + log(NP6) +
                                                 log(Flow) + log(Precip) +
                                                 log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + log(CP4) + log(CP5) + log(CP6) +
                                                 log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3) + log(SBP4) + log(SBP5) + log(SBP6)))
summary(MSSP_GR_3to8.lm.6month)


## MSSP_OUT_3to8
MSSP_OUT_3to8 = read.csv("MSSP_OUT_3to8.csv")

MSSP_OUT_3to8.lm.no.lag = with(MSSP_OUT_3to8, lm(log(N_Conc) ~ log(N_Price) + log(Flow) + log(Precip) 
                                               + log(Corn_Price) + log(SB_Price)))
summary(MSSP_OUT_3to8.lm.no.lag)  

MSSP_OUT_3to8.lm.1month = with(MSSP_OUT_3to8, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(Flow) + log(Precip) 
                                               + log(Corn_Price) + log(CP1) + log(SB_Price) + log(SBP1)))
summary(MSSP_OUT_3to8.lm.1month)

MSSP_OUT_3to8.lm.2month = with(MSSP_OUT_3to8, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(Flow) + log(Precip) 
                                               + log(Corn_Price) + log(CP1) + log(CP2) + log(SB_Price) + log(SBP1)
                                               + log(SBP2)))
summary(MSSP_OUT_3to8.lm.2month)

MSSP_OUT_3to8.lm.3month = with(MSSP_OUT_3to8, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) +
                                                 log(Flow) + log(Precip) +
                                                 log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + 
                                                 log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3)))
summary(MSSP_OUT_3to8.lm.3month)

MSSP_OUT_3to8.lm.4month = with(MSSP_OUT_3to8, lm(log(N_Conc) ~ log(N_Price) + 
                                                 log(NP1) + log(NP2) + log(NP3) + log(NP4)+
                                                 log(Flow) + log(Precip) +
                                                 log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + log(CP4) + 
                                                 log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3) + log(SBP4)))
summary(MSSP_OUT_3to8.lm.4month)

MSSP_OUT_3to8.lm.5month = with(MSSP_OUT_3to8, lm(log(N_Conc) ~ log(N_Price) + 
                                                 log(NP1) + log(NP2) + log(NP3) + log(NP4) + log(NP5) +
                                                 log(Flow) + log(Precip) +
                                                 log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + log(CP4) + log(CP5) +
                                                 log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3) + log(SBP4) + log(SBP5)))
summary(MSSP_OUT_3to8.lm.5month)

MSSP_OUT_3to8.lm.6month = with(MSSP_OUT_3to8, lm(log(N_Conc) ~ log(N_Price) + 
                                                 log(NP1) + log(NP2) + log(NP3) + log(NP4) + log(NP5) + log(NP6) +
                                                 log(Flow) + log(Precip) +
                                                 log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + log(CP4) + log(CP5) + log(CP6) +
                                                 log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3) + log(SBP4) + log(SBP5) + log(SBP6)))
summary(MSSP_OUT_3to8.lm.6month)



## MSSP_TH_3to8
MSSP_TH_3to8 = read.csv("MSSP_TH_3to8.csv")

MSSP_TH_3to8.lm.no.lag = with(MSSP_TH_3to8, lm(log(N_Conc) ~ log(N_Price) + log(Flow) + log(Precip) 
                                                 + log(Corn_Price) + log(SB_Price)))
summary(MSSP_TH_3to8.lm.no.lag)  

MSSP_TH_3to8.lm.1month = with(MSSP_TH_3to8, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(Flow) + log(Precip) 
                                                 + log(Corn_Price) + log(CP1) + log(SB_Price) + log(SBP1)))
summary(MSSP_OUT_3to8.lm.1month)

MSSP_TH_3to8.lm.2month = with(MSSP_TH_3to8, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(Flow) + log(Precip) 
                                                 + log(Corn_Price) + log(CP1) + log(CP2) + log(SB_Price) + log(SBP1)
                                                 + log(SBP2)))
summary(MSSP_TH_3to8.lm.2month)

MSSP_TH_3to8.lm.3month = with(MSSP_TH_3to8, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) +
                                                   log(Flow) + log(Precip) +
                                                   log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + 
                                                   log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3)))
summary(MSSP_TH_3to8.lm.3month)

MSSP_TH_3to8.lm.4month = with(MSSP_TH_3to8, lm(log(N_Conc) ~ log(N_Price) + 
                                                   log(NP1) + log(NP2) + log(NP3) + log(NP4)+
                                                   log(Flow) + log(Precip) +
                                                   log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + log(CP4) + 
                                                   log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3) + log(SBP4)))
summary(MSSP_TH_3to8.lm.4month)

MSSP_TH_3to8.lm.5month = with(MSSP_TH_3to8, lm(log(N_Conc) ~ log(N_Price) + 
                                                   log(NP1) + log(NP2) + log(NP3) + log(NP4) + log(NP5) +
                                                   log(Flow) + log(Precip) +
                                                   log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + log(CP4) + log(CP5) +
                                                   log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3) + log(SBP4) + log(SBP5)))
summary(MSSP_TH_3to8.lm.5month)

MSSP_TH_3to8.lm.6month = with(MSSP_TH_3to8, lm(log(N_Conc) ~ log(N_Price) + 
                                                   log(NP1) + log(NP2) + log(NP3) + log(NP4) + log(NP5) + log(NP6) +
                                                   log(Flow) + log(Precip) +
                                                   log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + log(CP4) + log(CP5) + log(CP6) +
                                                   log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3) + log(SBP4) + log(SBP5) + log(SBP6)))
summary(MSSP_TH_3to8.lm.6month)


## OHIO_GRCH_3to8
OHIO_GRCH_3to8 = read.csv("OHIO_GRCH_3to8.csv")

OHIO_GRCH_3to8.lm.no.lag = with(OHIO_GRCH_3to8, lm(log(N_Conc) ~ log(N_Price) + log(Flow) + log(Precip) 
                                               + log(Corn_Price) + log(SB_Price)))
summary(OHIO_GRCH_3to8.lm.no.lag)  

OHIO_GRCH_3to8.lm.1month = with(OHIO_GRCH_3to8, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(Flow) + log(Precip) 
                                               + log(Corn_Price) + log(CP1) + log(SB_Price) + log(SBP1)))
summary(OHIO_GRCH_3to8.lm.1month)

OHIO_GRCH_3to8.lm.2month = with(OHIO_GRCH_3to8, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(Flow) + log(Precip) 
                                               + log(Corn_Price) + log(CP1) + log(CP2) + log(SB_Price) + log(SBP1)
                                               + log(SBP2)))
summary(OHIO_GRCH_3to8.lm.2month)

OHIO_GRCH_3to8.lm.3month = with(OHIO_GRCH_3to8, lm(log(N_Conc) ~ log(N_Price) + log(NP1) + log(NP2) + log(NP3) +
                                                 log(Flow) + log(Precip) +
                                                 log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + 
                                                 log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3)))
summary(OHIO_GRCH_3to8.lm.3month)

OHIO_GRCH_3to8.lm.4month = with(OHIO_GRCH_3to8, lm(log(N_Conc) ~ log(N_Price) + 
                                                 log(NP1) + log(NP2) + log(NP3) + log(NP4)+
                                                 log(Flow) + log(Precip) +
                                                 log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + log(CP4) + 
                                                 log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3) + log(SBP4)))
summary(OHIO_GRCH_3to8.lm.4month)

OHIO_GRCH_3to8.lm.5month = with(OHIO_GRCH_3to8, lm(log(N_Conc) ~ log(N_Price) + 
                                                 log(NP1) + log(NP2) + log(NP3) + log(NP4) + log(NP5) +
                                                 log(Flow) + log(Precip) +
                                                 log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + log(CP4) + log(CP5) +
                                                 log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3) + log(SBP4) + log(SBP5)))
summary(OHIO_GRCH_3to8.lm.5month)

OHIO_GRCH_3to8.lm.6month = with(OHIO_GRCH_3to8, lm(log(N_Conc) ~ log(N_Price) + 
                                                 log(NP1) + log(NP2) + log(NP3) + log(NP4) + log(NP5) + log(NP6) +
                                                 log(Flow) + log(Precip) +
                                                 log(Corn_Price) + log(CP1) + log(CP2) +log(CP3) + log(CP4) + log(CP5) + log(CP6) +
                                                 log(SB_Price) + log(SBP1) + log(SBP2) + log(SBP3) + log(SBP4) + log(SBP5) + log(SBP6)))
summary(OHIO_GRCH_3to8.lm.6month)



