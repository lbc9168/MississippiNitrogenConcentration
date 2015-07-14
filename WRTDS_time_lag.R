setwd("C:/Users/Bingcai/OSU/AED Research/Mississipi/2015 Summer_All sites/WRTDS model")

## ILLI_VC
ILLI_VC = read.csv("ILLI_VC.csv")

Q_Qbar2sq = with(ILLI_VC[ILLI_VC$NP3 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(ILLI_VC[ILLI_VC$NP3 != 0,], T_Tbar2^2)
ILLI_VC.lm = with(ILLI_VC[ILLI_VC$NP3 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                             +log(NP)+log(NP1)+log(NP2)+log(NP3)
                             +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)))

ILLI_VC.lm.2 = with(ILLI_VC[ILLI_VC$NP3 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                               +log(NP)+log(NP1)+log(NP2)+log(NP3)
                               +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)))
summary(ILLI_VC.lm)
summary(ILLI_VC.lm.2)


Q_Qbar2sq = with(ILLI_VC[ILLI_VC$NP6 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(ILLI_VC[ILLI_VC$NP6 != 0,], T_Tbar2^2)
ILLI_VC.lm.6m = with(ILLI_VC[ILLI_VC$NP6 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)))

ILLI_VC.lm.2.6m = with(ILLI_VC[ILLI_VC$NP6 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                     +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                     +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)))
summary(ILLI_VC.lm.6m)
summary(ILLI_VC.lm.2.6m)


Q_Qbar2sq = with(ILLI_VC[ILLI_VC$NP9 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(ILLI_VC[ILLI_VC$NP9 != 0,], T_Tbar2^2)
ILLI_VC.lm.9m = with(ILLI_VC[ILLI_VC$NP9 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                   +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                   +log(NP7)+log(NP8)+log(NP9)
                                                   +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                   +log(CSR7)+log(CSR8)+log(CSR9)))

ILLI_VC.lm.2.9m = with(ILLI_VC[ILLI_VC$NP9 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                     +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                     +log(NP7)+log(NP8)+log(NP9)
                                                     +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                     +log(CSR7)+log(CSR8)+log(CSR9)))
summary(ILLI_VC.lm.9m)
summary(ILLI_VC.lm.2.9m)


Q_Qbar2sq = with(ILLI_VC[ILLI_VC$NP12 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(ILLI_VC[ILLI_VC$NP12 != 0,], T_Tbar2^2)
ILLI_VC.lm.12m = with(ILLI_VC[ILLI_VC$NP12 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                   +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                   +log(NP7)+log(NP8)+log(NP9)+log(NP10)+log(NP11)+log(NP12)
                                                   +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                   +log(CSR7)+log(CSR8)+log(CSR9)+log(CSR10)+log(CSR11)+log(CSR12)))

ILLI_VC.lm.2.12m = with(ILLI_VC[ILLI_VC$NP12 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                   +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                   +log(NP7)+log(NP8)+log(NP9)+log(NP10)+log(NP11)+log(NP12)
                                                   +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                   +log(CSR7)+log(CSR8)+log(CSR9)+log(CSR10)+log(CSR11)+log(CSR12)))
summary(ILLI_VC.lm.12m)
summary(ILLI_VC.lm.2.12m)


## IOWA_WAP
IOWA_WAP = read.csv("IOWA_WAP.csv")

Q_Qbar2sq = with(IOWA_WAP[IOWA_WAP$NP3 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(IOWA_WAP[IOWA_WAP$NP3 != 0,], T_Tbar2^2)
IOWA_WAP.lm = with(IOWA_WAP[IOWA_WAP$NP3 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                   +log(NP)+log(NP1)+log(NP2)+log(NP3)
                                                   +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)))

IOWA_WAP.lm.2 = with(IOWA_WAP[IOWA_WAP$NP3 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                     +log(NP)+log(NP1)+log(NP2)+log(NP3)
                                                     +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)))
summary(IOWA_WAP.lm)
summary(IOWA_WAP.lm.2)


Q_Qbar2sq = with(IOWA_WAP[IOWA_WAP$NP6 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(IOWA_WAP[IOWA_WAP$NP6 != 0,], T_Tbar2^2)
IOWA_WAP.lm.6m = with(IOWA_WAP[IOWA_WAP$NP6 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                      +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                      +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)))

IOWA_WAP.lm.2.6m = with(IOWA_WAP[IOWA_WAP$NP6 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                        +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                        +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)))
summary(IOWA_WAP.lm.6m)
summary(IOWA_WAP.lm.2.6m)


Q_Qbar2sq = with(IOWA_WAP[IOWA_WAP$NP9 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(IOWA_WAP[IOWA_WAP$NP9 != 0,], T_Tbar2^2)
IOWA_WAP.lm.9m = with(IOWA_WAP[IOWA_WAP$NP9 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                      +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                      +log(NP7)+log(NP8)+log(NP9)
                                                      +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                      +log(CSR7)+log(CSR8)+log(CSR9)))

IOWA_WAP.lm.2.9m = with(IOWA_WAP[IOWA_WAP$NP9 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                        +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                        +log(NP7)+log(NP8)+log(NP9)
                                                        +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                        +log(CSR7)+log(CSR8)+log(CSR9)))
summary(IOWA_WAP.lm.9m)
summary(IOWA_WAP.lm.2.9m)


Q_Qbar2sq = with(IOWA_WAP[IOWA_WAP$NP12 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(IOWA_WAP[IOWA_WAP$NP12 != 0,], T_Tbar2^2)
IOWA_WAP.lm.12m = with(IOWA_WAP[IOWA_WAP$NP12 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                        +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                        +log(NP7)+log(NP8)+log(NP9)+log(NP10)+log(NP11)+log(NP12)
                                                        +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                        +log(CSR7)+log(CSR8)+log(CSR9)+log(CSR10)+log(CSR11)+log(CSR12)))

IOWA_WAP.lm.2.12m = with(IOWA_WAP[IOWA_WAP$NP12 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                          +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                          +log(NP7)+log(NP8)+log(NP9)+log(NP10)+log(NP11)+log(NP12)
                                                          +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                          +log(CSR7)+log(CSR8)+log(CSR9)+log(CSR10)+log(CSR11)+log(CSR12)))
summary(IOWA_WAP.lm.12m)
summary(IOWA_WAP.lm.2.12m)



## MIZZ_HE
MIZZ_HE = read.csv("MIZZ_HE.csv")

Q_Qbar2sq = with(MIZZ_HE[MIZZ_HE$NP3 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MIZZ_HE[MIZZ_HE$NP3 != 0,], T_Tbar2^2)
MIZZ_HE.lm = with(MIZZ_HE[MIZZ_HE$NP3 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                +log(NP)+log(NP1)+log(NP2)+log(NP3)
                                                +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)))

MIZZ_HE.lm.2 = with(MIZZ_HE[MIZZ_HE$NP3 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                  +log(NP)+log(NP1)+log(NP2)+log(NP3)
                                                  +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)))
summary(MIZZ_HE.lm)
summary(MIZZ_HE.lm.2)


Q_Qbar2sq = with(MIZZ_HE[MIZZ_HE$NP6 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MIZZ_HE[MIZZ_HE$NP6 != 0,], T_Tbar2^2)
MIZZ_HE.lm.6m = with(MIZZ_HE[MIZZ_HE$NP6 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                   +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                   +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)))

MIZZ_HE.lm.2.6m = with(MIZZ_HE[MIZZ_HE$NP6 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                     +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                     +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)))
summary(MIZZ_HE.lm.6m)
summary(MIZZ_HE.lm.2.6m)


Q_Qbar2sq = with(MIZZ_HE[MIZZ_HE$NP9 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MIZZ_HE[MIZZ_HE$NP9 != 0,], T_Tbar2^2)
MIZZ_HE.lm.9m = with(MIZZ_HE[MIZZ_HE$NP9 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                   +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                   +log(NP7)+log(NP8)+log(NP9)
                                                   +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                   +log(CSR7)+log(CSR8)+log(CSR9)))

MIZZ_HE.lm.2.9m = with(MIZZ_HE[MIZZ_HE$NP9 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                     +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                     +log(NP7)+log(NP8)+log(NP9)
                                                     +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                     +log(CSR7)+log(CSR8)+log(CSR9)))
summary(MIZZ_HE.lm.9m)
summary(MIZZ_HE.lm.2.9m)


Q_Qbar2sq = with(MIZZ_HE[MIZZ_HE$NP12 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MIZZ_HE[MIZZ_HE$NP12 != 0,], T_Tbar2^2)
MIZZ_HE.lm.12m = with(MIZZ_HE[MIZZ_HE$NP12 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                     +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                     +log(NP7)+log(NP8)+log(NP9)+log(NP10)+log(NP11)+log(NP12)
                                                     +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                     +log(CSR7)+log(CSR8)+log(CSR9)+log(CSR10)+log(CSR11)+log(CSR12)))

MIZZ_HE.lm.2.12m = with(MIZZ_HE[MIZZ_HE$NP12 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                       +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                       +log(NP7)+log(NP8)+log(NP9)+log(NP10)+log(NP11)+log(NP12)
                                                       +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                       +log(CSR7)+log(CSR8)+log(CSR9)+log(CSR10)+log(CSR11)+log(CSR12)))
summary(MIZZ_HE.lm.12m)
summary(MIZZ_HE.lm.2.12m)



## MSSP_CL
MSSP_CL = read.csv("MSSP_CL.csv")

Q_Qbar2sq = with(MSSP_CL[MSSP_CL$NP3 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_CL[MSSP_CL$NP3 != 0,], T_Tbar2^2)
MSSP_CL.lm = with(MSSP_CL[MSSP_CL$NP3 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                +log(NP)+log(NP1)+log(NP2)+log(NP3)
                                                +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)))

MSSP_CL.lm.2 = with(MSSP_CL[MSSP_CL$NP3 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                  +log(NP)+log(NP1)+log(NP2)+log(NP3)
                                                  +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)))
summary(MSSP_CL.lm)
summary(MSSP_CL.lm.2)


Q_Qbar2sq = with(MSSP_CL[MSSP_CL$NP6 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_CL[MSSP_CL$NP6 != 0,], T_Tbar2^2)
MSSP_CL.lm.6m = with(MSSP_CL[MSSP_CL$NP6 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                   +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                   +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)))

MSSP_CL.lm.2.6m = with(MSSP_CL[MSSP_CL$NP6 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                     +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                     +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)))
summary(MSSP_CL.lm.6m)
summary(MSSP_CL.lm.2.6m)


Q_Qbar2sq = with(MSSP_CL[MSSP_CL$NP9 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_CL[MSSP_CL$NP9 != 0,], T_Tbar2^2)
MSSP_CL.lm.9m = with(MSSP_CL[MSSP_CL$NP9 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                   +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                   +log(NP7)+log(NP8)+log(NP9)
                                                   +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                   +log(CSR7)+log(CSR8)+log(CSR9)))

MSSP_CL.lm.2.9m = with(MSSP_CL[MSSP_CL$NP9 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                     +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                     +log(NP7)+log(NP8)+log(NP9)
                                                     +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                     +log(CSR7)+log(CSR8)+log(CSR9)))
summary(MSSP_CL.lm.9m)
summary(MSSP_CL.lm.2.9m)


Q_Qbar2sq = with(MSSP_CL[MSSP_CL$NP12 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_CL[MSSP_CL$NP12 != 0,], T_Tbar2^2)
MSSP_CL.lm.12m = with(MSSP_CL[MSSP_CL$NP12 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                     +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                     +log(NP7)+log(NP8)+log(NP9)+log(NP10)+log(NP11)+log(NP12)
                                                     +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                     +log(CSR7)+log(CSR8)+log(CSR9)+log(CSR10)+log(CSR11)+log(CSR12)))

MSSP_CL.lm.2.12m = with(MSSP_CL[MSSP_CL$NP12 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                       +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                       +log(NP7)+log(NP8)+log(NP9)+log(NP10)+log(NP11)+log(NP12)
                                                       +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                       +log(CSR7)+log(CSR8)+log(CSR9)+log(CSR10)+log(CSR11)+log(CSR12)))
summary(MSSP_CL.lm.12m)
summary(MSSP_CL.lm.2.12m)


## MSSP_GR
MSSP_GR = read.csv("MSSP_GR.csv")

Q_Qbar2sq = with(MSSP_GR[MSSP_GR$NP3 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_GR[MSSP_GR$NP3 != 0,], T_Tbar2^2)
MSSP_GR.lm = with(MSSP_GR[MSSP_GR$NP3 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                +log(NP)+log(NP1)+log(NP2)+log(NP3)
                                                +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)))

MSSP_GR.lm.2 = with(MSSP_GR[MSSP_GR$NP3 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                  +log(NP)+log(NP1)+log(NP2)+log(NP3)
                                                  +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)))
summary(MSSP_GR.lm)
summary(MSSP_GR.lm.2)


Q_Qbar2sq = with(MSSP_GR[MSSP_GR$NP6 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_GR[MSSP_GR$NP6 != 0,], T_Tbar2^2)
MSSP_GR.lm.6m = with(MSSP_GR[MSSP_GR$NP6 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                   +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                   +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)))

MSSP_GR.lm.2.6m = with(MSSP_GR[MSSP_GR$NP6 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                     +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                     +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)))
summary(MSSP_GR.lm.6m)
summary(MSSP_GR.lm.2.6m)


Q_Qbar2sq = with(MSSP_GR[MSSP_GR$NP9 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_GR[MSSP_GR$NP9 != 0,], T_Tbar2^2)
MSSP_GR.lm.9m = with(MSSP_GR[MSSP_GR$NP9 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                   +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                   +log(NP7)+log(NP8)+log(NP9)
                                                   +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                   +log(CSR7)+log(CSR8)+log(CSR9)))

MSSP_GR.lm.2.9m = with(MSSP_GR[MSSP_GR$NP9 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                     +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                     +log(NP7)+log(NP8)+log(NP9)
                                                     +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                     +log(CSR7)+log(CSR8)+log(CSR9)))
summary(MSSP_GR.lm.9m)
summary(MSSP_GR.lm.2.9m)


Q_Qbar2sq = with(MSSP_GR[MSSP_GR$NP12 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_GR[MSSP_GR$NP12 != 0,], T_Tbar2^2)
MSSP_GR.lm.12m = with(MSSP_GR[MSSP_GR$NP12 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                     +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                     +log(NP7)+log(NP8)+log(NP9)+log(NP10)+log(NP11)+log(NP12)
                                                     +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                     +log(CSR7)+log(CSR8)+log(CSR9)+log(CSR10)+log(CSR11)+log(CSR12)))

MSSP_GR.lm.2.12m = with(MSSP_GR[MSSP_GR$NP12 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                       +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                       +log(NP7)+log(NP8)+log(NP9)+log(NP10)+log(NP11)+log(NP12)
                                                       +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                       +log(CSR7)+log(CSR8)+log(CSR9)+log(CSR10)+log(CSR11)+log(CSR12)))
summary(MSSP_GR.lm.12m)
summary(MSSP_GR.lm.2.12m)


## MSSP_OUT
MSSP_OUT = read.csv("MSSP_OUT.csv")

Q_Qbar2sq = with(MSSP_OUT[MSSP_OUT$NP3 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_OUT[MSSP_OUT$NP3 != 0,], T_Tbar2^2)
MSSP_OUT.lm = with(MSSP_OUT[MSSP_OUT$NP3 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                   +log(NP)+log(NP1)+log(NP2)+log(NP3)
                                                   +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)))

MSSP_OUT.lm.2 = with(MSSP_OUT[MSSP_OUT$NP3 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                     +log(NP)+log(NP1)+log(NP2)+log(NP3)
                                                     +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)))
summary(MSSP_OUT.lm)
summary(MSSP_OUT.lm.2)


Q_Qbar2sq = with(MSSP_OUT[MSSP_OUT$NP6 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_OUT[MSSP_OUT$NP6 != 0,], T_Tbar2^2)
MSSP_OUT.lm.6m = with(MSSP_OUT[MSSP_OUT$NP6 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                      +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                      +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)))

MSSP_OUT.lm.2.6m = with(MSSP_OUT[MSSP_OUT$NP6 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                        +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                        +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)))
summary(MSSP_OUT.lm.6m)
summary(MSSP_OUT.lm.2.6m)


Q_Qbar2sq = with(MSSP_OUT[MSSP_OUT$NP9 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_OUT[MSSP_OUT$NP9 != 0,], T_Tbar2^2)
MSSP_OUT.lm.9m = with(MSSP_OUT[MSSP_OUT$NP9 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                      +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                      +log(NP7)+log(NP8)+log(NP9)
                                                      +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                      +log(CSR7)+log(CSR8)+log(CSR9)))

MSSP_OUT.lm.2.9m = with(MSSP_OUT[MSSP_OUT$NP9 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                        +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                        +log(NP7)+log(NP8)+log(NP9)
                                                        +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                        +log(CSR7)+log(CSR8)+log(CSR9)))
summary(MSSP_OUT.lm.9m)
summary(MSSP_OUT.lm.2.9m)


Q_Qbar2sq = with(MSSP_OUT[MSSP_OUT$NP12 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_OUT[MSSP_OUT$NP12 != 0,], T_Tbar2^2)
MSSP_OUT.lm.12m = with(MSSP_OUT[MSSP_OUT$NP12 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                        +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                        +log(NP7)+log(NP8)+log(NP9)+log(NP10)+log(NP11)+log(NP12)
                                                        +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                        +log(CSR7)+log(CSR8)+log(CSR9)+log(CSR10)+log(CSR11)+log(CSR12)))

MSSP_OUT.lm.2.12m = with(MSSP_OUT[MSSP_OUT$NP12 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                          +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                          +log(NP7)+log(NP8)+log(NP9)+log(NP10)+log(NP11)+log(NP12)
                                                          +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                          +log(CSR7)+log(CSR8)+log(CSR9)+log(CSR10)+log(CSR11)+log(CSR12)))
summary(MSSP_OUT.lm.12m)
summary(MSSP_OUT.lm.2.12m)



## MSSP_TH
MSSP_TH = read.csv("MSSP_TH.csv")

Q_Qbar2sq = with(MSSP_TH[MSSP_TH$NP3 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_TH[MSSP_TH$NP3 != 0,], T_Tbar2^2)
MSSP_TH.lm = with(MSSP_TH[MSSP_TH$NP3 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                +log(NP)+log(NP1)+log(NP2)+log(NP3)
                                                +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)))

MSSP_TH.lm.2 = with(MSSP_TH[MSSP_TH$NP3 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                  +log(NP)+log(NP1)+log(NP2)+log(NP3)
                                                  +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)))
summary(MSSP_TH.lm)
summary(MSSP_TH.lm.2)


Q_Qbar2sq = with(MSSP_TH[MSSP_TH$NP6 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_TH[MSSP_TH$NP6 != 0,], T_Tbar2^2)
MSSP_TH.lm.6m = with(MSSP_TH[MSSP_TH$NP6 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                   +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                   +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)))

MSSP_TH.lm.2.6m = with(MSSP_TH[MSSP_TH$NP6 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                     +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                     +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)))
summary(MSSP_TH.lm.6m)
summary(MSSP_TH.lm.2.6m)


Q_Qbar2sq = with(MSSP_TH[MSSP_TH$NP9 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_TH[MSSP_TH$NP9 != 0,], T_Tbar2^2)
MSSP_TH.lm.9m = with(MSSP_TH[MSSP_TH$NP9 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                   +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                   +log(NP7)+log(NP8)+log(NP9)
                                                   +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                   +log(CSR7)+log(CSR8)+log(CSR9)))

MSSP_TH.lm.2.9m = with(MSSP_TH[MSSP_TH$NP9 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                     +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                     +log(NP7)+log(NP8)+log(NP9)
                                                     +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                     +log(CSR7)+log(CSR8)+log(CSR9)))
summary(MSSP_TH.lm.9m)
summary(MSSP_TH.lm.2.9m)


Q_Qbar2sq = with(MSSP_TH[MSSP_TH$NP12 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_TH[MSSP_TH$NP12 != 0,], T_Tbar2^2)
MSSP_TH.lm.12m = with(MSSP_TH[MSSP_TH$NP12 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                     +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                     +log(NP7)+log(NP8)+log(NP9)+log(NP10)+log(NP11)+log(NP12)
                                                     +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                     +log(CSR7)+log(CSR8)+log(CSR9)+log(CSR10)+log(CSR11)+log(CSR12)))

MSSP_TH.lm.2.12m = with(MSSP_TH[MSSP_TH$NP12 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                       +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                       +log(NP7)+log(NP8)+log(NP9)+log(NP10)+log(NP11)+log(NP12)
                                                       +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                       +log(CSR7)+log(CSR8)+log(CSR9)+log(CSR10)+log(CSR11)+log(CSR12)))
summary(MSSP_TH.lm.12m)
summary(MSSP_TH.lm.2.12m)



## OHIO_GRCH
OHIO_GRCH = read.csv("OHIO_GRCH.csv")

Q_Qbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP3 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP3 != 0,], T_Tbar2^2)
OHIO_GRCH.lm = with(OHIO_GRCH[OHIO_GRCH$NP3 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                      +log(NP)+log(NP1)+log(NP2)+log(NP3)
                                                      +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)))

OHIO_GRCH.lm.2 = with(OHIO_GRCH[OHIO_GRCH$NP3 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                        +log(NP)+log(NP1)+log(NP2)+log(NP3)
                                                        +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)))
summary(OHIO_GRCH.lm)
summary(OHIO_GRCH.lm.2)


Q_Qbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP6 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP6 != 0,], T_Tbar2^2)
OHIO_GRCH.lm.6m = with(OHIO_GRCH[OHIO_GRCH$NP6 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                         +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                         +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)))

OHIO_GRCH.lm.2.6m = with(OHIO_GRCH[OHIO_GRCH$NP6 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                           +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                           +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)))
summary(OHIO_GRCH.lm.6m)
summary(OHIO_GRCH.lm.2.6m)


Q_Qbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP9 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP9 != 0,], T_Tbar2^2)
OHIO_GRCH.lm.9m = with(OHIO_GRCH[OHIO_GRCH$NP9 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                         +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                         +log(NP7)+log(NP8)+log(NP9)
                                                         +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                         +log(CSR7)+log(CSR8)+log(CSR9)))

OHIO_GRCH.lm.2.9m = with(OHIO_GRCH[OHIO_GRCH$NP9 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                           +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                           +log(NP7)+log(NP8)+log(NP9)
                                                           +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                           +log(CSR7)+log(CSR8)+log(CSR9)))
summary(OHIO_GRCH.lm.9m)
summary(OHIO_GRCH.lm.2.9m)


Q_Qbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP12 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP12 != 0,], T_Tbar2^2)
OHIO_GRCH.lm.12m = with(OHIO_GRCH[OHIO_GRCH$NP12 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)
                                                           +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                           +log(NP7)+log(NP8)+log(NP9)+log(NP10)+log(NP11)+log(NP12)
                                                           +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                           +log(CSR7)+log(CSR8)+log(CSR9)+log(CSR10)+log(CSR11)+log(CSR12)))

OHIO_GRCH.lm.2.12m = with(OHIO_GRCH[OHIO_GRCH$NP12 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)
                                                             +log(NP)+log(NP1)+log(NP2)+log(NP3)+log(NP4)+log(NP5)+log(NP6)
                                                             +log(NP7)+log(NP8)+log(NP9)+log(NP10)+log(NP11)+log(NP12)
                                                             +log(CSR)+log(CSR1)+log(CSR2)+log(CSR3)+log(CSR4)+log(CSR5)+log(CSR6)
                                                             +log(CSR7)+log(CSR8)+log(CSR9)+log(CSR10)+log(CSR11)+log(CSR12)))
summary(OHIO_GRCH.lm.12m)
summary(OHIO_GRCH.lm.2.12m)
