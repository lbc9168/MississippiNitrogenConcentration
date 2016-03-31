setwd("C:/Users/Bingcai/OSU/AED Research/Mississipi/2015 Summer_All sites/WRTDS model")

ILLI_VC = read.csv("ILLI_VC.csv")
Q_Qbar2sq = with(ILLI_VC, log(Q_Qbar2)^2)
T_Tbar2sq = with(ILLI_VC, T_Tbar2^2)
ILLI_VC.lm = with(ILLI_VC,lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP)+log(CSR)))
ILLI_VC.lm.2 = with(ILLI_VC,lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)+log(NP)+log(CSR)))
summary(ILLI_VC.lm)
summary(ILLI_VC.lm.2)

Var_T = with(ILLI_VC, var(T_Tbar2, na.rm = TRUE))
Var_Tsq = var(T_Tbar2sq, na.rm = TRUE)
Theta1_woP = summary(ILLI_VC.lm2)$coefficients["T_Tbar2","Estimate"]
Theta2_woP = summary(ILLI_VC.lm2)$coefficients["T_Tbar2sq","Estimate"]

## test original model
ILLI_VC.lm.3 = with(ILLI_VC,lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)))
summary(ILLI_VC.lm.3)
## test end

IOWA_WAP = read.csv("IOWA_WAP.csv")
lnQ_Qbar2sq = with(IOWA_WAP, log(Q_Qbar2)^2)
T_Tbar2sq = with(IOWA_WAP, T_Tbar2^2)
IOWA_WAP.lm = with(IOWA_WAP,lm(log(Conc)~log(Q_Qbar2)+lnQ_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP)+log(CSR)))
IOWA_WAP.lm.2 = with(IOWA_WAP,lm(log(Conc)~log(Q_Qbar2)+lnQ_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)+log(NP)+log(CSR)))
summary(IOWA_WAP.lm)
summary(IOWA_WAP.lm.2)

Var_T = with(IOWA_WAP, var(T_Tbar2, na.rm = TRUE))


MIZZ_HE = read.csv("MIZZ_HE.csv")
lnQ_Qbar2sq = with(MIZZ_HE, log(Q_Qbar2)^2)
T_Tbar2sq = with(MIZZ_HE, T_Tbar2^2)
MIZZ_HE.lm = with(MIZZ_HE,lm(log(Conc)~log(Q_Qbar2)+lnQ_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP)+log(CSR)))
MIZZ_HE.lm.2 = with(MIZZ_HE,lm(log(Conc)~log(Q_Qbar2)+lnQ_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)+log(NP)+log(CSR)))
summary(MIZZ_HE.lm)
summary(MIZZ_HE.lm.2)

MSSP_CL = read.csv("MSSP_CL.csv")
lnQ_Qbar2sq = with(MSSP_CL, log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_CL, T_Tbar2^2)
MSSP_CL.lm = with(MSSP_CL,lm(log(Conc)~log(Q_Qbar2)+lnQ_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP)+log(CSR)))
MSSP_CL.lm.2 = with(MSSP_CL,lm(log(Conc)~log(Q_Qbar2)+lnQ_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)+log(NP)+log(CSR)))
summary(MSSP_CL.lm)
summary(MSSP_CL.lm.2)

MSSP_GR = read.csv("MSSP_GR.csv")
lnQ_Qbar2sq = with(MSSP_GR, log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_GR, T_Tbar2^2)
MSSP_GR.lm = with(MSSP_GR,lm(log(Conc)~log(Q_Qbar2)+lnQ_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP)+log(CSR)))
MSSP_GR.lm.2 = with(MSSP_GR,lm(log(Conc)~log(Q_Qbar2)+lnQ_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)+log(NP)+log(CSR)))
summary(MSSP_GR.lm)
summary(MSSP_GR.lm.2)

MSSP_OUT = read.csv("MSSP_OUT.csv")
lnQ_Qbar2sq = with(MSSP_OUT, log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_OUT, T_Tbar2^2)
MSSP_OUT.lm = with(MSSP_OUT,lm(log(Conc)~log(Q_Qbar2)+lnQ_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP)+log(CSR)))
MSSP_OUT.lm.2 = with(MSSP_OUT,lm(log(Conc)~log(Q_Qbar2)+lnQ_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)+log(NP)+log(CSR)))
summary(MSSP_OUT.lm)
summary(MSSP_OUT.lm.2)

MSSP_TH = read.csv("MSSP_TH.csv")
lnQ_Qbar2sq = with(MSSP_TH, log(Q_Qbar2)^2)
T_Tbar2sq = with(MSSP_TH, T_Tbar2^2)
MSSP_TH.lm = with(MSSP_TH,lm(log(Conc)~log(Q_Qbar2)+lnQ_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP)+log(CSR)))
MSSP_TH.lm.2 = with(MSSP_TH,lm(log(Conc)~log(Q_Qbar2)+lnQ_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)+log(NP)+log(CSR)))
summary(MSSP_TH.lm)
summary(MSSP_TH.lm.2)

OHIO_GRCH = read.csv("OHIO_GRCH.csv")
lnQ_Qbar2sq = with(OHIO_GRCH, log(Q_Qbar2)^2)
T_Tbar2sq = with(OHIO_GRCH, T_Tbar2^2)
OHIO_GRCH.lm = with(OHIO_GRCH,lm(log(Conc)~log(Q_Qbar2)+lnQ_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP)+log(CSR)))
OHIO_GRCH.lm.2 = with(OHIO_GRCH,lm(log(Conc)~log(Q_Qbar2)+lnQ_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)+log(NP)+log(CSR)))
summary(OHIO_GRCH.lm)
summary(OHIO_GRCH.lm.2)

