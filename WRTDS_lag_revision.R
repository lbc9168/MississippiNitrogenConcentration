setwd("C:/Users/Bingcai/OSU/AED Research/Mississipi/2015 Summer_All sites/Writing/supplentment materials")

#ILLI_VC

ILLI_VC = read.csv("ILLI_VC.csv")

Q_Qbar2sq = with(ILLI_VC[ILLI_VC$NP1 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(ILLI_VC[ILLI_VC$NP1 != 0,], T_Tbar2^2)
ILLI_VC.lm.lag1 = with(ILLI_VC[ILLI_VC$NP1 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP1)+log(CSR1)))
summary(ILLI_VC.lm.lag1)

Q_Qbar2sq = with(ILLI_VC[ILLI_VC$NP2 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(ILLI_VC[ILLI_VC$NP2 != 0,], T_Tbar2^2)
ILLI_VC.lm.lag2 = with(ILLI_VC[ILLI_VC$NP2 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP2)+log(CSR2)))
summary(ILLI_VC.lm.lag2)

Q_Qbar2sq = with(ILLI_VC[ILLI_VC$NP3 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(ILLI_VC[ILLI_VC$NP3 != 0,], T_Tbar2^2)
ILLI_VC.lm.lag3 = with(ILLI_VC[ILLI_VC$NP3 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP3)+log(CSR3)))
summary(ILLI_VC.lm.lag3)

Q_Qbar2sq = with(ILLI_VC[ILLI_VC$NP4 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(ILLI_VC[ILLI_VC$NP4 != 0,], T_Tbar2^2)
ILLI_VC.lm.lag4 = with(ILLI_VC[ILLI_VC$NP4 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP4)+log(CSR4)))
summary(ILLI_VC.lm.lag4)

Q_Qbar2sq = with(ILLI_VC[ILLI_VC$NP5 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(ILLI_VC[ILLI_VC$NP5 != 0,], T_Tbar2^2)
ILLI_VC.lm.lag5 = with(ILLI_VC[ILLI_VC$NP5 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP5)+log(CSR5)))
summary(ILLI_VC.lm.lag5)

Q_Qbar2sq = with(ILLI_VC[ILLI_VC$NP6 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(ILLI_VC[ILLI_VC$NP6 != 0,], T_Tbar2^2)
ILLI_VC.lm.lag6 = with(ILLI_VC[ILLI_VC$NP6 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP6)+log(CSR6)))
summary(ILLI_VC.lm.lag6)

Q_Qbar2sq = with(ILLI_VC[ILLI_VC$NP7 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(ILLI_VC[ILLI_VC$NP7 != 0,], T_Tbar2^2)
ILLI_VC.lm.lag7 = with(ILLI_VC[ILLI_VC$NP7 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP7)+log(CSR7)))
summary(ILLI_VC.lm.lag7)

Q_Qbar2sq = with(ILLI_VC[ILLI_VC$NP8 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(ILLI_VC[ILLI_VC$NP8 != 0,], T_Tbar2^2)
ILLI_VC.lm.lag8 = with(ILLI_VC[ILLI_VC$NP8 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP8)+log(CSR8)))
summary(ILLI_VC.lm.lag8)

Q_Qbar2sq = with(ILLI_VC[ILLI_VC$NP9 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(ILLI_VC[ILLI_VC$NP9 != 0,], T_Tbar2^2)
ILLI_VC.lm.lag9 = with(ILLI_VC[ILLI_VC$NP9 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP9)+log(CSR9)))
summary(ILLI_VC.lm.lag9)

Q_Qbar2sq = with(ILLI_VC[ILLI_VC$NP10 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(ILLI_VC[ILLI_VC$NP10 != 0,], T_Tbar2^2)
ILLI_VC.lm.lag10 = with(ILLI_VC[ILLI_VC$NP10 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP10)+log(CSR10)))
summary(ILLI_VC.lm.lag10)

Q_Qbar2sq = with(ILLI_VC[ILLI_VC$NP11 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(ILLI_VC[ILLI_VC$NP11 != 0,], T_Tbar2^2)
ILLI_VC.lm.lag11 = with(ILLI_VC[ILLI_VC$NP11 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP11)+log(CSR11)))
summary(ILLI_VC.lm.lag11)

Q_Qbar2sq = with(ILLI_VC[ILLI_VC$NP12 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(ILLI_VC[ILLI_VC$NP12 != 0,], T_Tbar2^2)
ILLI_VC.lm.lag12 = with(ILLI_VC[ILLI_VC$NP12 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP12)+log(CSR12)))
summary(ILLI_VC.lm.lag12)



#IOWA_WAP

IOWA_WAP = read.csv("IOWA_WAP.csv")

Q_Qbar2sq = with(IOWA_WAP[IOWA_WAP$NP1 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(IOWA_WAP[IOWA_WAP$NP1 != 0,], T_Tbar2^2)
IOWA_WAP.lm.lag1 = with(IOWA_WAP[IOWA_WAP$NP1 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP1)+log(CSR1)))
summary(IOWA_WAP.lm.lag1)

Q_Qbar2sq = with(IOWA_WAP[IOWA_WAP$NP2 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(IOWA_WAP[IOWA_WAP$NP2 != 0,], T_Tbar2^2)
IOWA_WAP.lm.lag2 = with(IOWA_WAP[IOWA_WAP$NP2 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP2)+log(CSR2)))
summary(IOWA_WAP.lm.lag2)

Q_Qbar2sq = with(IOWA_WAP[IOWA_WAP$NP3 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(IOWA_WAP[IOWA_WAP$NP3 != 0,], T_Tbar2^2)
IOWA_WAP.lm.lag3 = with(IOWA_WAP[IOWA_WAP$NP3 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP3)+log(CSR3)))
summary(IOWA_WAP.lm.lag3)

Q_Qbar2sq = with(IOWA_WAP[IOWA_WAP$NP4 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(IOWA_WAP[IOWA_WAP$NP4 != 0,], T_Tbar2^2)
IOWA_WAP.lm.lag4 = with(IOWA_WAP[IOWA_WAP$NP4 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP4)+log(CSR4)))
summary(IOWA_WAP.lm.lag4)

Q_Qbar2sq = with(IOWA_WAP[IOWA_WAP$NP5 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(IOWA_WAP[IOWA_WAP$NP5 != 0,], T_Tbar2^2)
IOWA_WAP.lm.lag5 = with(IOWA_WAP[IOWA_WAP$NP5 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP5)+log(CSR5)))
summary(IOWA_WAP.lm.lag5)

Q_Qbar2sq = with(IOWA_WAP[IOWA_WAP$NP6 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(IOWA_WAP[IOWA_WAP$NP6 != 0,], T_Tbar2^2)
IOWA_WAP.lm.lag6 = with(IOWA_WAP[IOWA_WAP$NP6 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP6)+log(CSR6)))
summary(IOWA_WAP.lm.lag6)

Q_Qbar2sq = with(IOWA_WAP[IOWA_WAP$NP7 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(IOWA_WAP[IOWA_WAP$NP7 != 0,], T_Tbar2^2)
IOWA_WAP.lm.lag7 = with(IOWA_WAP[IOWA_WAP$NP7 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP7)+log(CSR7)))
summary(IOWA_WAP.lm.lag7)

Q_Qbar2sq = with(IOWA_WAP[IOWA_WAP$NP8 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(IOWA_WAP[IOWA_WAP$NP8 != 0,], T_Tbar2^2)
IOWA_WAP.lm.lag8 = with(IOWA_WAP[IOWA_WAP$NP8 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP8)+log(CSR8)))
summary(IOWA_WAP.lm.lag8)

Q_Qbar2sq = with(IOWA_WAP[IOWA_WAP$NP9 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(IOWA_WAP[IOWA_WAP$NP9 != 0,], T_Tbar2^2)
IOWA_WAP.lm.lag9 = with(IOWA_WAP[IOWA_WAP$NP9 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP9)+log(CSR9)))
summary(IOWA_WAP.lm.lag9)

Q_Qbar2sq = with(IOWA_WAP[IOWA_WAP$NP10 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(IOWA_WAP[IOWA_WAP$NP10 != 0,], T_Tbar2^2)
IOWA_WAP.lm.lag10 = with(IOWA_WAP[IOWA_WAP$NP10 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP10)+log(CSR10)))
summary(IOWA_WAP.lm.lag10)

Q_Qbar2sq = with(IOWA_WAP[IOWA_WAP$NP11 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(IOWA_WAP[IOWA_WAP$NP11 != 0,], T_Tbar2^2)
IOWA_WAP.lm.lag11 = with(IOWA_WAP[IOWA_WAP$NP11 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP11)+log(CSR11)))
summary(IOWA_WAP.lm.lag11)

Q_Qbar2sq = with(IOWA_WAP[IOWA_WAP$NP12 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(IOWA_WAP[IOWA_WAP$NP12 != 0,], T_Tbar2^2)
IOWA_WAP.lm.lag12 = with(IOWA_WAP[IOWA_WAP$NP12 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP12)+log(CSR12)))
summary(IOWA_WAP.lm.lag12)



#MIZZ_HE

MIZZ_HE = read.csv("MIZZ_HE.csv")

Q_Qbar2sq = with(MIZZ_HE[MIZZ_HE$NP1 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MIZZ_HE[MIZZ_HE$NP1 != 0,], T_Tbar2^2)
MIZZ_HE.lm.lag1 = with(MIZZ_HE[MIZZ_HE$NP1 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP1)+log(CSR1)))
summary(MIZZ_HE.lm.lag1)

Q_Qbar2sq = with(MIZZ_HE[MIZZ_HE$NP2 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MIZZ_HE[MIZZ_HE$NP2 != 0,], T_Tbar2^2)
MIZZ_HE.lm.lag2 = with(MIZZ_HE[MIZZ_HE$NP2 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP2)+log(CSR2)))
summary(MIZZ_HE.lm.lag2)

Q_Qbar2sq = with(MIZZ_HE[MIZZ_HE$NP3 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MIZZ_HE[MIZZ_HE$NP3 != 0,], T_Tbar2^2)
MIZZ_HE.lm.lag3 = with(MIZZ_HE[MIZZ_HE$NP3 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP3)+log(CSR3)))
summary(MIZZ_HE.lm.lag3)

Q_Qbar2sq = with(MIZZ_HE[MIZZ_HE$NP4 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MIZZ_HE[MIZZ_HE$NP4 != 0,], T_Tbar2^2)
MIZZ_HE.lm.lag4 = with(MIZZ_HE[MIZZ_HE$NP4 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP4)+log(CSR4)))
summary(MIZZ_HE.lm.lag4)

Q_Qbar2sq = with(MIZZ_HE[MIZZ_HE$NP5 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MIZZ_HE[MIZZ_HE$NP5 != 0,], T_Tbar2^2)
MIZZ_HE.lm.lag5 = with(MIZZ_HE[MIZZ_HE$NP5 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP5)+log(CSR5)))
summary(MIZZ_HE.lm.lag5)

Q_Qbar2sq = with(MIZZ_HE[MIZZ_HE$NP6 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MIZZ_HE[MIZZ_HE$NP6 != 0,], T_Tbar2^2)
MIZZ_HE.lm.lag6 = with(MIZZ_HE[MIZZ_HE$NP6 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP6)+log(CSR6)))
summary(MIZZ_HE.lm.lag6)

Q_Qbar2sq = with(MIZZ_HE[MIZZ_HE$NP7 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MIZZ_HE[MIZZ_HE$NP7 != 0,], T_Tbar2^2)
MIZZ_HE.lm.lag7 = with(MIZZ_HE[MIZZ_HE$NP7 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP7)+log(CSR7)))
summary(MIZZ_HE.lm.lag7)

Q_Qbar2sq = with(MIZZ_HE[MIZZ_HE$NP8 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MIZZ_HE[MIZZ_HE$NP8 != 0,], T_Tbar2^2)
MIZZ_HE.lm.lag8 = with(MIZZ_HE[MIZZ_HE$NP8 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP8)+log(CSR8)))
summary(MIZZ_HE.lm.lag8)

Q_Qbar2sq = with(MIZZ_HE[MIZZ_HE$NP9 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MIZZ_HE[MIZZ_HE$NP9 != 0,], T_Tbar2^2)
MIZZ_HE.lm.lag9 = with(MIZZ_HE[MIZZ_HE$NP9 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP9)+log(CSR9)))
summary(MIZZ_HE.lm.lag9)

Q_Qbar2sq = with(MIZZ_HE[MIZZ_HE$NP10 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MIZZ_HE[MIZZ_HE$NP10 != 0,], T_Tbar2^2)
MIZZ_HE.lm.lag10 = with(MIZZ_HE[MIZZ_HE$NP10 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP10)+log(CSR10)))
summary(MIZZ_HE.lm.lag10)

Q_Qbar2sq = with(MIZZ_HE[MIZZ_HE$NP11 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MIZZ_HE[MIZZ_HE$NP11 != 0,], T_Tbar2^2)
MIZZ_HE.lm.lag11 = with(MIZZ_HE[MIZZ_HE$NP11 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP11)+log(CSR11)))
summary(MIZZ_HE.lm.lag11)

Q_Qbar2sq = with(MIZZ_HE[MIZZ_HE$NP12 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MIZZ_HE[MIZZ_HE$NP12 != 0,], T_Tbar2^2)
MIZZ_HE.lm.lag12 = with(MIZZ_HE[MIZZ_HE$NP12 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP12)+log(CSR12)))
summary(MIZZ_HE.lm.lag12)



#MSSP_CL

MSSP_CL = read.csv("MSSP_CL.csv")

Q_Qbar2sq = with(MSSP_CL[MSSP_CL$NP1 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_CL[MSSP_CL$NP1 != 0,], T_Tbar2^2)
MSSP_CL.lm.lag1 = with(MSSP_CL[MSSP_CL$NP1 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP1)+log(CSR1)))
summary(MSSP_CL.lm.lag1)

Q_Qbar2sq = with(MSSP_CL[MSSP_CL$NP2 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_CL[MSSP_CL$NP2 != 0,], T_Tbar2^2)
MSSP_CL.lm.lag2 = with(MSSP_CL[MSSP_CL$NP2 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP2)+log(CSR2)))
summary(MSSP_CL.lm.lag2)

Q_Qbar2sq = with(MSSP_CL[MSSP_CL$NP3 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_CL[MSSP_CL$NP3 != 0,], T_Tbar2^2)
MSSP_CL.lm.lag3 = with(MSSP_CL[MSSP_CL$NP3 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP3)+log(CSR3)))
summary(MSSP_CL.lm.lag3)

Q_Qbar2sq = with(MSSP_CL[MSSP_CL$NP4 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_CL[MSSP_CL$NP4 != 0,], T_Tbar2^2)
MSSP_CL.lm.lag4 = with(MSSP_CL[MSSP_CL$NP4 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP4)+log(CSR4)))
summary(MSSP_CL.lm.lag4)

Q_Qbar2sq = with(MSSP_CL[MSSP_CL$NP5 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_CL[MSSP_CL$NP5 != 0,], T_Tbar2^2)
MSSP_CL.lm.lag5 = with(MSSP_CL[MSSP_CL$NP5 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP5)+log(CSR5)))
summary(MSSP_CL.lm.lag5)

Q_Qbar2sq = with(MSSP_CL[MSSP_CL$NP6 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_CL[MSSP_CL$NP6 != 0,], T_Tbar2^2)
MSSP_CL.lm.lag6 = with(MSSP_CL[MSSP_CL$NP6 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP6)+log(CSR6)))
summary(MSSP_CL.lm.lag6)

Q_Qbar2sq = with(MSSP_CL[MSSP_CL$NP7 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_CL[MSSP_CL$NP7 != 0,], T_Tbar2^2)
MSSP_CL.lm.lag7 = with(MSSP_CL[MSSP_CL$NP7 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP7)+log(CSR7)))
summary(MSSP_CL.lm.lag7)

Q_Qbar2sq = with(MSSP_CL[MSSP_CL$NP8 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_CL[MSSP_CL$NP8 != 0,], T_Tbar2^2)
MSSP_CL.lm.lag8 = with(MSSP_CL[MSSP_CL$NP8 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP8)+log(CSR8)))
summary(MSSP_CL.lm.lag8)

Q_Qbar2sq = with(MSSP_CL[MSSP_CL$NP9 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_CL[MSSP_CL$NP9 != 0,], T_Tbar2^2)
MSSP_CL.lm.lag9 = with(MSSP_CL[MSSP_CL$NP9 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP9)+log(CSR9)))
summary(MSSP_CL.lm.lag9)

Q_Qbar2sq = with(MSSP_CL[MSSP_CL$NP10 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_CL[MSSP_CL$NP10 != 0,], T_Tbar2^2)
MSSP_CL.lm.lag10 = with(MSSP_CL[MSSP_CL$NP10 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP10)+log(CSR10)))
summary(MSSP_CL.lm.lag10)

Q_Qbar2sq = with(MSSP_CL[MSSP_CL$NP11 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_CL[MSSP_CL$NP11 != 0,], T_Tbar2^2)
MSSP_CL.lm.lag11 = with(MSSP_CL[MSSP_CL$NP11 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP11)+log(CSR11)))
summary(MSSP_CL.lm.lag11)

Q_Qbar2sq = with(MSSP_CL[MSSP_CL$NP12 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_CL[MSSP_CL$NP12 != 0,], T_Tbar2^2)
MSSP_CL.lm.lag12 = with(MSSP_CL[MSSP_CL$NP12 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP12)+log(CSR12)))
summary(MSSP_CL.lm.lag12)



#MSSP_GR

MSSP_GR = read.csv("MSSP_GR.csv")

Q_Qbar2sq = with(MSSP_GR[MSSP_GR$NP1 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_GR[MSSP_GR$NP1 != 0,], T_Tbar2^2)
MSSP_GR.lm.lag1 = with(MSSP_GR[MSSP_GR$NP1 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP1)+log(CSR1)))
summary(MSSP_GR.lm.lag1)

Q_Qbar2sq = with(MSSP_GR[MSSP_GR$NP2 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_GR[MSSP_GR$NP2 != 0,], T_Tbar2^2)
MSSP_GR.lm.lag2 = with(MSSP_GR[MSSP_GR$NP2 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP2)+log(CSR2)))
summary(MSSP_GR.lm.lag2)

Q_Qbar2sq = with(MSSP_GR[MSSP_GR$NP3 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_GR[MSSP_GR$NP3 != 0,], T_Tbar2^2)
MSSP_GR.lm.lag3 = with(MSSP_GR[MSSP_GR$NP3 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP3)+log(CSR3)))
summary(MSSP_GR.lm.lag3)

Q_Qbar2sq = with(MSSP_GR[MSSP_GR$NP4 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_GR[MSSP_GR$NP4 != 0,], T_Tbar2^2)
MSSP_GR.lm.lag4 = with(MSSP_GR[MSSP_GR$NP4 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP4)+log(CSR4)))
summary(MSSP_GR.lm.lag4)

Q_Qbar2sq = with(MSSP_GR[MSSP_GR$NP5 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_GR[MSSP_GR$NP5 != 0,], T_Tbar2^2)
MSSP_GR.lm.lag5 = with(MSSP_GR[MSSP_GR$NP5 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP5)+log(CSR5)))
summary(MSSP_GR.lm.lag5)

Q_Qbar2sq = with(MSSP_GR[MSSP_GR$NP6 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_GR[MSSP_GR$NP6 != 0,], T_Tbar2^2)
MSSP_GR.lm.lag6 = with(MSSP_GR[MSSP_GR$NP6 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP6)+log(CSR6)))
summary(MSSP_GR.lm.lag6)

Q_Qbar2sq = with(MSSP_GR[MSSP_GR$NP7 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_GR[MSSP_GR$NP7 != 0,], T_Tbar2^2)
MSSP_GR.lm.lag7 = with(MSSP_GR[MSSP_GR$NP7 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP7)+log(CSR7)))
summary(MSSP_GR.lm.lag7)

Q_Qbar2sq = with(MSSP_GR[MSSP_GR$NP8 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_GR[MSSP_GR$NP8 != 0,], T_Tbar2^2)
MSSP_GR.lm.lag8 = with(MSSP_GR[MSSP_GR$NP8 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP8)+log(CSR8)))
summary(MSSP_GR.lm.lag8)

Q_Qbar2sq = with(MSSP_GR[MSSP_GR$NP9 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_GR[MSSP_GR$NP9 != 0,], T_Tbar2^2)
MSSP_GR.lm.lag9 = with(MSSP_GR[MSSP_GR$NP9 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP9)+log(CSR9)))
summary(MSSP_GR.lm.lag9)

Q_Qbar2sq = with(MSSP_GR[MSSP_GR$NP10 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_GR[MSSP_GR$NP10 != 0,], T_Tbar2^2)
MSSP_GR.lm.lag10 = with(MSSP_GR[MSSP_GR$NP10 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP10)+log(CSR10)))
summary(MSSP_GR.lm.lag10)

Q_Qbar2sq = with(MSSP_GR[MSSP_GR$NP11 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_GR[MSSP_GR$NP11 != 0,], T_Tbar2^2)
MSSP_GR.lm.lag11 = with(MSSP_GR[MSSP_GR$NP11 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP11)+log(CSR11)))
summary(MSSP_GR.lm.lag11)

Q_Qbar2sq = with(MSSP_GR[MSSP_GR$NP12 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_GR[MSSP_GR$NP12 != 0,], T_Tbar2^2)
MSSP_GR.lm.lag12 = with(MSSP_GR[MSSP_GR$NP12 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP12)+log(CSR12)))
summary(MSSP_GR.lm.lag12)



#MSSP_OUT

MSSP_OUT = read.csv("MSSP_OUT.csv")

Q_Qbar2sq = with(MSSP_OUT[MSSP_OUT$NP1 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_OUT[MSSP_OUT$NP1 != 0,], T_Tbar2^2)
MSSP_OUT.lm.lag1 = with(MSSP_OUT[MSSP_OUT$NP1 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP1)+log(CSR1)))
summary(MSSP_OUT.lm.lag1)

Q_Qbar2sq = with(MSSP_OUT[MSSP_OUT$NP2 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_OUT[MSSP_OUT$NP2 != 0,], T_Tbar2^2)
MSSP_OUT.lm.lag2 = with(MSSP_OUT[MSSP_OUT$NP2 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP2)+log(CSR2)))
summary(MSSP_OUT.lm.lag2)

Q_Qbar2sq = with(MSSP_OUT[MSSP_OUT$NP3 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_OUT[MSSP_OUT$NP3 != 0,], T_Tbar2^2)
MSSP_OUT.lm.lag3 = with(MSSP_OUT[MSSP_OUT$NP3 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP3)+log(CSR3)))
summary(MSSP_OUT.lm.lag3)

Q_Qbar2sq = with(MSSP_OUT[MSSP_OUT$NP4 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_OUT[MSSP_OUT$NP4 != 0,], T_Tbar2^2)
MSSP_OUT.lm.lag4 = with(MSSP_OUT[MSSP_OUT$NP4 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP4)+log(CSR4)))
summary(MSSP_OUT.lm.lag4)

Q_Qbar2sq = with(MSSP_OUT[MSSP_OUT$NP5 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_OUT[MSSP_OUT$NP5 != 0,], T_Tbar2^2)
MSSP_OUT.lm.lag5 = with(MSSP_OUT[MSSP_OUT$NP5 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP5)+log(CSR5)))
summary(MSSP_OUT.lm.lag5)

Q_Qbar2sq = with(MSSP_OUT[MSSP_OUT$NP6 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_OUT[MSSP_OUT$NP6 != 0,], T_Tbar2^2)
MSSP_OUT.lm.lag6 = with(MSSP_OUT[MSSP_OUT$NP6 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP6)+log(CSR6)))
summary(MSSP_OUT.lm.lag6)

Q_Qbar2sq = with(MSSP_OUT[MSSP_OUT$NP7 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_OUT[MSSP_OUT$NP7 != 0,], T_Tbar2^2)
MSSP_OUT.lm.lag7 = with(MSSP_OUT[MSSP_OUT$NP7 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP7)+log(CSR7)))
summary(MSSP_OUT.lm.lag7)

Q_Qbar2sq = with(MSSP_OUT[MSSP_OUT$NP8 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_OUT[MSSP_OUT$NP8 != 0,], T_Tbar2^2)
MSSP_OUT.lm.lag8 = with(MSSP_OUT[MSSP_OUT$NP8 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP8)+log(CSR8)))
summary(MSSP_OUT.lm.lag8)

Q_Qbar2sq = with(MSSP_OUT[MSSP_OUT$NP9 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_OUT[MSSP_OUT$NP9 != 0,], T_Tbar2^2)
MSSP_OUT.lm.lag9 = with(MSSP_OUT[MSSP_OUT$NP9 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP9)+log(CSR9)))
summary(MSSP_OUT.lm.lag9)

Q_Qbar2sq = with(MSSP_OUT[MSSP_OUT$NP10 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_OUT[MSSP_OUT$NP10 != 0,], T_Tbar2^2)
MSSP_OUT.lm.lag10 = with(MSSP_OUT[MSSP_OUT$NP10 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP10)+log(CSR10)))
summary(MSSP_OUT.lm.lag10)

Q_Qbar2sq = with(MSSP_OUT[MSSP_OUT$NP11 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_OUT[MSSP_OUT$NP11 != 0,], T_Tbar2^2)
MSSP_OUT.lm.lag11 = with(MSSP_OUT[MSSP_OUT$NP11 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP11)+log(CSR11)))
summary(MSSP_OUT.lm.lag11)

Q_Qbar2sq = with(MSSP_OUT[MSSP_OUT$NP12 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_OUT[MSSP_OUT$NP12 != 0,], T_Tbar2^2)
MSSP_OUT.lm.lag12 = with(MSSP_OUT[MSSP_OUT$NP12 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP12)+log(CSR12)))
summary(MSSP_OUT.lm.lag12)



#MSSP_TH

MSSP_TH = read.csv("MSSP_TH.csv")

Q_Qbar2sq = with(MSSP_TH[MSSP_TH$NP1 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_TH[MSSP_TH$NP1 != 0,], T_Tbar2^2)
MSSP_TH.lm.lag1 = with(MSSP_TH[MSSP_TH$NP1 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP1)+log(CSR1)))
summary(MSSP_TH.lm.lag1)

Q_Qbar2sq = with(MSSP_TH[MSSP_TH$NP2 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_TH[MSSP_TH$NP2 != 0,], T_Tbar2^2)
MSSP_TH.lm.lag2 = with(MSSP_TH[MSSP_TH$NP2 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP2)+log(CSR2)))
summary(MSSP_TH.lm.lag2)

Q_Qbar2sq = with(MSSP_TH[MSSP_TH$NP3 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_TH[MSSP_TH$NP3 != 0,], T_Tbar2^2)
MSSP_TH.lm.lag3 = with(MSSP_TH[MSSP_TH$NP3 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP3)+log(CSR3)))
summary(MSSP_TH.lm.lag3)

Q_Qbar2sq = with(MSSP_TH[MSSP_TH$NP4 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_TH[MSSP_TH$NP4 != 0,], T_Tbar2^2)
MSSP_TH.lm.lag4 = with(MSSP_TH[MSSP_TH$NP4 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP4)+log(CSR4)))
summary(MSSP_TH.lm.lag4)

Q_Qbar2sq = with(MSSP_TH[MSSP_TH$NP5 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_TH[MSSP_TH$NP5 != 0,], T_Tbar2^2)
MSSP_TH.lm.lag5 = with(MSSP_TH[MSSP_TH$NP5 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP5)+log(CSR5)))
summary(MSSP_TH.lm.lag5)

Q_Qbar2sq = with(MSSP_TH[MSSP_TH$NP6 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_TH[MSSP_TH$NP6 != 0,], T_Tbar2^2)
MSSP_TH.lm.lag6 = with(MSSP_TH[MSSP_TH$NP6 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP6)+log(CSR6)))
summary(MSSP_TH.lm.lag6)

Q_Qbar2sq = with(MSSP_TH[MSSP_TH$NP7 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_TH[MSSP_TH$NP7 != 0,], T_Tbar2^2)
MSSP_TH.lm.lag7 = with(MSSP_TH[MSSP_TH$NP7 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP7)+log(CSR7)))
summary(MSSP_TH.lm.lag7)

Q_Qbar2sq = with(MSSP_TH[MSSP_TH$NP8 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_TH[MSSP_TH$NP8 != 0,], T_Tbar2^2)
MSSP_TH.lm.lag8 = with(MSSP_TH[MSSP_TH$NP8 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP8)+log(CSR8)))
summary(MSSP_TH.lm.lag8)

Q_Qbar2sq = with(MSSP_TH[MSSP_TH$NP9 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_TH[MSSP_TH$NP9 != 0,], T_Tbar2^2)
MSSP_TH.lm.lag9 = with(MSSP_TH[MSSP_TH$NP9 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP9)+log(CSR9)))
summary(MSSP_TH.lm.lag9)

Q_Qbar2sq = with(MSSP_TH[MSSP_TH$NP10 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_TH[MSSP_TH$NP10 != 0,], T_Tbar2^2)
MSSP_TH.lm.lag10 = with(MSSP_TH[MSSP_TH$NP10 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP10)+log(CSR10)))
summary(MSSP_TH.lm.lag10)

Q_Qbar2sq = with(MSSP_TH[MSSP_TH$NP11 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_TH[MSSP_TH$NP11 != 0,], T_Tbar2^2)
MSSP_TH.lm.lag11 = with(MSSP_TH[MSSP_TH$NP11 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP11)+log(CSR11)))
summary(MSSP_TH.lm.lag11)

Q_Qbar2sq = with(MSSP_TH[MSSP_TH$NP12 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_TH[MSSP_TH$NP12 != 0,], T_Tbar2^2)
MSSP_TH.lm.lag12 = with(MSSP_TH[MSSP_TH$NP12 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP12)+log(CSR12)))
summary(MSSP_TH.lm.lag12)



#OHIO_GRCH

OHIO_GRCH = read.csv("OHIO_GRCH.csv")

Q_Qbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP1 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP1 != 0,], T_Tbar2^2)
OHIO_GRCH.lm.lag1 = with(OHIO_GRCH[OHIO_GRCH$NP1 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP1)+log(CSR1)))
summary(OHIO_GRCH.lm.lag1)

Q_Qbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP2 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP2 != 0,], T_Tbar2^2)
OHIO_GRCH.lm.lag2 = with(OHIO_GRCH[OHIO_GRCH$NP2 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP2)+log(CSR2)))
summary(OHIO_GRCH.lm.lag2)

Q_Qbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP3 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP3 != 0,], T_Tbar2^2)
OHIO_GRCH.lm.lag3 = with(OHIO_GRCH[OHIO_GRCH$NP3 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP3)+log(CSR3)))
summary(OHIO_GRCH.lm.lag3)

Q_Qbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP4 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP4 != 0,], T_Tbar2^2)
OHIO_GRCH.lm.lag4 = with(OHIO_GRCH[OHIO_GRCH$NP4 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP4)+log(CSR4)))
summary(OHIO_GRCH.lm.lag4)

Q_Qbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP5 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP5 != 0,], T_Tbar2^2)
OHIO_GRCH.lm.lag5 = with(OHIO_GRCH[OHIO_GRCH$NP5 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP5)+log(CSR5)))
summary(OHIO_GRCH.lm.lag5)

Q_Qbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP6 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP6 != 0,], T_Tbar2^2)
OHIO_GRCH.lm.lag6 = with(OHIO_GRCH[OHIO_GRCH$NP6 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP6)+log(CSR6)))
summary(OHIO_GRCH.lm.lag6)

Q_Qbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP7 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP7 != 0,], T_Tbar2^2)
OHIO_GRCH.lm.lag7 = with(OHIO_GRCH[OHIO_GRCH$NP7 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP7)+log(CSR7)))
summary(OHIO_GRCH.lm.lag7)

Q_Qbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP8 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP8 != 0,], T_Tbar2^2)
OHIO_GRCH.lm.lag8 = with(OHIO_GRCH[OHIO_GRCH$NP8 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP8)+log(CSR8)))
summary(OHIO_GRCH.lm.lag8)

Q_Qbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP9 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP9 != 0,], T_Tbar2^2)
OHIO_GRCH.lm.lag9 = with(OHIO_GRCH[OHIO_GRCH$NP9 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP9)+log(CSR9)))
summary(OHIO_GRCH.lm.lag9)

Q_Qbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP10 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP10 != 0,], T_Tbar2^2)
OHIO_GRCH.lm.lag10 = with(OHIO_GRCH[OHIO_GRCH$NP10 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP10)+log(CSR10)))
summary(OHIO_GRCH.lm.lag10)

Q_Qbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP11 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP11 != 0,], T_Tbar2^2)
OHIO_GRCH.lm.lag11 = with(OHIO_GRCH[OHIO_GRCH$NP11 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP11)+log(CSR11)))
summary(OHIO_GRCH.lm.lag11)

Q_Qbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP12 != 0,], log(Q_Qbar2)^2)
T_Tbar2sq = with(OHIO_GRCH[OHIO_GRCH$NP12 != 0,], T_Tbar2^2)
OHIO_GRCH.lm.lag12 = with(OHIO_GRCH[OHIO_GRCH$NP12 != 0,],lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP12)+log(CSR12)))
summary(OHIO_GRCH.lm.lag12)

