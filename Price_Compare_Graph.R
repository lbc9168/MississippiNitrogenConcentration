setwd("C:/Users/Bingcai/OSU/AED Research/Mississipi/2015 Summer_All sites/WRTDS model/Price Comparation")
load("C:/Users/Bingcai/OSU/AED Research/Mississipi/2015 Summer_All sites/WRTDS model/Price Comparation/.RData")

## ILLI_VC

Pdct_ILLI_VC = read.csv("Pdct_ILLI_VC.csv")
Qbar = with(Pdct_ILLI_VC, mean(Q))
Qwave = with(Pdct_ILLI_VC, Qbar + sum((Q - Qbar)^3)/(2*sum((Q - Qbar)^2)))

Tbar = with(Pdct_ILLI_VC, mean(T))
Twave = with(Pdct_ILLI_VC, Tbar + sum((T - Tbar)^3)/(2*sum((T - Tbar)^2)))

LNC = with(Pdct_ILLI_VC, as.matrix(2.315 + 0.055*log(Q/Qwave) - 0.14*Q_Qbar2 -0.0009*(T-Twave) + 0.303*sin(2*pi*T)
                                   + 0.16*cos(2*pi*T) - 0.12*log(NP) + 0.25*log(CSR)))
C = exp(LNC)         

LNCWOP = with(Pdct_ILLI_VC, as.matrix(1.569 + 0.06*log(Q/Qwave) - 0.13*Q_Qbar2 -0.0028*(T-Twave) + 0.0007* (T-Twave)^2 + 0.31*sin(2*pi*T)
                                      + 0.162*cos(2*pi*T)))
C03 = exp(LNCWOP)

#graphic part begins
with(Pdct_ILLI_VC, matplot(T,C03,type = "l",lty = 2, xaxt = "n", ylab = "N Concentration",xlab = "Year", main = "ILLI_VC"))
axis(1, at=with(Pdct_ILLI_VC,T), labels=with(Pdct_ILLI_VC,Date))
with(Pdct_ILLI_VC, matlines(T,C,type="l"))

VC = with(ILLI_VC,lm(C~T))
VC.2 = with(ILLI_VC,lm(C03~T))
abline(VC)
abline(VC.2,lty = 2)

legend('topright', c("with Price","without Price"), lty = c(1,2), bty='o')


#graphic part ends


## IOWA_WAP

Pdct_IOWA_WAP = read.csv("Pdct_IOWA_WAP.csv")
Qbar = with(Pdct_IOWA_WAP, mean(Q))
Qwave = with(Pdct_IOWA_WAP, Qbar + sum((Q - Qbar)^3)/(2*sum((Q - Qbar)^2)))

Tbar = with(Pdct_IOWA_WAP, mean(T))
Twave = with(Pdct_IOWA_WAP, Tbar + sum((T - Tbar)^3)/(2*sum((T - Tbar)^2)))

LNC = with(Pdct_IOWA_WAP, as.matrix(2.12 - 0.18*log(Q/Qwave) - 0.309*Q_Qbar2 -0.0042*(T-Twave) + 0.3*sin(2*pi*T)
                                    + 0.45*cos(2*pi*T) - 0.11*log(NP) -0.66*log(CSR)))
C = exp(LNC)         

LNCWOP = with(Pdct_IOWA_WAP, as.matrix(2.20 - 0.188*log(Q/Qwave) - 0.313*Q_Qbar2 -0.002*(T-Twave)- 0.0004 *(T-Twave)^2 + 0.287*sin(2*pi*T)
                                       + 0.447*cos(2*pi*T)))
C03 = exp(LNCWOP)


with(Pdct_IOWA_WAP, matplot(T,C03,type = "l",lty = 2,xaxt = "n", ylab = "N Concentration", xlab = "Year", main = "IOWA_WAP"))
axis(1, at=with(Pdct_IOWA_WAP,T), labels=with(Pdct_IOWA_WAP,Date))
with(Pdct_IOWA_WAP, matlines(T,C,type="l"))

IOWA_WAP = with(Pdct_IOWA_WAP, data.frame(T,C,C03))

WAP = with(IOWA_WAP,lm(C~T))
WAP.2 = with(IOWA_WAP,lm(C03~T))
abline(WAP)
abline(WAP.2,lty = 2)

legend('topright', c("with Price","without Price"), lty = c(1,2), bty='o')
#graphic part ends

## MIZZ_HE

Pdct_MIZZ_HE = read.csv("Pdct_MIZZ_HE.csv")
Qbar = with(Pdct_MIZZ_HE, mean(Q))
Qwave = with(Pdct_MIZZ_HE, Qbar + sum((Q - Qbar)^3)/(2*sum((Q - Qbar)^2)))

Tbar = with(Pdct_MIZZ_HE, mean(T))
Twave = with(Pdct_MIZZ_HE, Tbar + sum((T - Tbar)^3)/(2*sum((T - Tbar)^2)))

LNC = with(Pdct_MIZZ_HE, as.matrix(1.1 + 0.122*log(Q/Qwave) - 0.16*Q_Qbar2 -0.01*(T-Twave) + 0.452*sin(2*pi*T)
                                   + 0.035*cos(2*pi*T) - 0.19*log(NP) -0.122*log(CSR)))
C = exp(LNC)         

LNCWOP = with(Pdct_MIZZ_HE, as.matrix(0.29 + 0.121*log(Q/Qwave) - 0.163*Q_Qbar2 -0.007*(T-Twave)- 0.0004* (T-Twave)^2 + 0.446*sin(2*pi*T)
                                      + 0.028*cos(2*pi*T)))
C03 = exp(LNCWOP)


with(Pdct_MIZZ_HE, matplot(T,C03,type = "l",lty = 2,xaxt = "n", ylab = "N Concentration", xlab = "Year", main = "MIZZ_HE"))
axis(1, at=with(Pdct_MIZZ_HE,T), labels=with(Pdct_MIZZ_HE,Date))
with(Pdct_MIZZ_HE, matlines(T,C,type="l"))

MIZZ_HE = with(Pdct_MIZZ_HE, data.frame(T,C,C03))

HE = with(MIZZ_HE,lm(C~T))
HE.2 = with(MIZZ_HE,lm(C03~T))
abline(HE)
abline(HE.2,lty = 2)

legend('topright', c("with Price","without Price"), lty = c(1,2), bty='o')
#graphic part ends


## MSSP_CL

Pdct_MSSP_CL = read.csv("Pdct_MSSP_CL.csv")
Qbar = with(Pdct_MSSP_CL, mean(Q))
Qwave = with(Pdct_MSSP_CL, Qbar + sum((Q - Qbar)^3)/(2*sum((Q - Qbar)^2)))

Tbar = with(Pdct_MSSP_CL, mean(T))
Twave = with(Pdct_MSSP_CL, Tbar + sum((T - Tbar)^3)/(2*sum((T - Tbar)^2)))

LNC = with(Pdct_MSSP_CL, as.matrix(2.398 + 0.112*log(Q/Qwave) - 0.102*Q_Qbar2 -0.0002*(T-Twave) + 0.294*sin(2*pi*T)
                                   + 0.164*cos(2*pi*T) - 0.132*log(NP) + 0.294*log(CSR)))
C = exp(LNC)         

LNCWOP = with(Pdct_MSSP_CL, as.matrix(1.56 + 0.121*log(Q/Qwave) - 0.1*Q_Qbar2 -0.002*(T-Twave)-0.0008*(T-Twave)^2 + 0.302*sin(2*pi*T)
                                      + 0.168*cos(2*pi*T)))
C03 = exp(LNCWOP)


with(Pdct_MSSP_CL, matplot(T,C03,type = "l",lty = 2,xaxt = "n", ylab = "N Concentration", xlab = "Year", main = "MSSP_CL"))
axis(1, at=with(Pdct_MSSP_CL,T), labels=with(Pdct_MSSP_CL,Date))
with(Pdct_MSSP_CL, matlines(T,C,type="l"))

MSSP_CL = with(Pdct_MSSP_CL, data.frame(T,C,C03))
CL = with(MSSP_CL,lm(C~T))
CL.2 = with(MSSP_CL,lm(C03~T))
abline(CL)
abline(CL.2,lty = 2)

legend('topright', c("with Price","without Price"), lty = c(1,2), bty='o')
#graphic part ends


## MSSP_GR

Pdct_MSSP_GR = read.csv("Pdct_MSSP_GR.csv")
Qbar = with(Pdct_MSSP_GR, mean(Q))
Qwave = with(Pdct_MSSP_GR, Qbar + sum((Q - Qbar)^3)/(2*sum((Q - Qbar)^2)))

Tbar = with(Pdct_MSSP_GR, mean(T))
Twave = with(Pdct_MSSP_GR, Tbar + sum((T - Tbar)^3)/(2*sum((T - Tbar)^2)))

LNC = with(Pdct_MSSP_GR, as.matrix(2.135 + 0.174*log(Q/Qwave) - 0.148*Q_Qbar2 -0.011*(T-Twave) + 0.283*sin(2*pi*T)
                                   + 0.139*cos(2*pi*T) - 0.2*log(NP) -0.119*log(CSR)))
C = exp(LNC)         

LNCWOP = with(Pdct_MSSP_GR, as.matrix(1.32 + 0.212*log(Q/Qwave) - 0.133*Q_Qbar2 -0.001*(T-Twave)-0.002*(T-Twave)^2 + 0.285*sin(2*pi*T)
                                      + 0.127*cos(2*pi*T)))
C03 = exp(LNCWOP)


with(Pdct_MSSP_GR, matplot(T,C03,type = "l",lty = 2,xaxt = "n", ylab = "N Concentration",xlab = "Year", main = "MSSP_GR"))
axis(1, at=with(Pdct_MSSP_GR,T), labels=with(Pdct_MSSP_GR,Date))
with(Pdct_MSSP_GR, matlines(T,C,type="l"))

MSSP_GR = with(Pdct_MSSP_GR, data.frame(T,C,C03))
GR = with(MSSP_GR,lm(C~T))
GR.2 = with(MSSP_GR,lm(C03~T))
abline(GR)
abline(GR.2,lty = 2)

legend('topright', c("with Price","without Price"), lty = c(1,2), bty='o')
#graphic part ends


## MSSP_OUT

Pdct_MSSP_OUT = read.csv("Pdct_MSSP_OUT.csv")
Qbar = with(Pdct_MSSP_OUT, mean(Q))
Qwave = with(Pdct_MSSP_OUT, Qbar + sum((Q - Qbar)^3)/(2*sum((Q - Qbar)^2)))

Tbar = with(Pdct_MSSP_OUT, mean(T))
Twave = with(Pdct_MSSP_OUT, Tbar + sum((T - Tbar)^3)/(2*sum((T - Tbar)^2)))

LNC = with(Pdct_MSSP_OUT, as.matrix(0.65 - 0.024*log(Q/Qwave) - 0.21*Q_Qbar2 -0.0017*(T-Twave) + 0.156*sin(2*pi*T)
                                    - 0.174*cos(2*pi*T) - 0.014*log(NP) + 0.29*log(CSR)))
C = exp(LNC)         

LNCWOP = with(Pdct_MSSP_OUT, as.matrix(0.32 - 0.014*log(Q/Qwave) - 0.213*Q_Qbar2 -0.0015*(T-Twave)+ 0*(T-Twave)^2 + 0.155*sin(2*pi*T)
                                       - 0.175*cos(2*pi*T)))
C03 = exp(LNCWOP)


with(Pdct_MSSP_OUT, matplot(T,C03,type = "l",lty = 2,xaxt = "n", ylab = "N Concentration",xlab = "Year", main = "MSSP_OUT"))
axis(1, at=with(Pdct_MSSP_OUT,T), labels=with(Pdct_MSSP_OUT,Date))
with(Pdct_MSSP_OUT, matlines(T,C,type="l"))

MSSP_OUT= with(Pdct_MSSP_OUT,data.frame(T,C,C03))
OUT = with(MSSP_OUT,lm(C~T))
OUT.2 = with(MSSP_OUT,lm(C03~T))
abline(OUT)
abline(OUT.2,lty = 2)

legend('topright', c("with Price","without Price"), lty = c(1,2), bty='o')
#graphic part ends


## MSSP_TH

Pdct_MSSP_TH = read.csv("Pdct_MSSP_TH.csv")
Qbar = with(Pdct_MSSP_TH, mean(Q))
Qwave = with(Pdct_MSSP_TH, Qbar + sum((Q - Qbar)^3)/(2*sum((Q - Qbar)^2)))

Tbar = with(Pdct_MSSP_TH, mean(T))
Twave = with(Pdct_MSSP_TH, Tbar + sum((T - Tbar)^3)/(2*sum((T - Tbar)^2)))

LNC = with(Pdct_MSSP_TH, as.matrix(2 + 0.118*log(Q/Qwave) - 0.295*Q_Qbar2 -0.0011*(T-Twave) + 0.253*sin(2*pi*T)
                                   -0.174*cos(2*pi*T) - 0.167*log(NP) + 0.285*log(CSR)))
C = exp(LNC)         

LNCWOP = with(Pdct_MSSP_TH, as.matrix(0.96 + 0.123*log(Q/Qwave) - 0.3*Q_Qbar2 -0.0012*(T-Twave) - 0.0006*(T-Twave)^2 + 0.253*sin(2*pi*T)
                                      -0.17*cos(2*pi*T)))
C03 = exp(LNCWOP)


with(Pdct_MSSP_TH, matplot(T,C03,type = "l",lty = 2,xaxt = "n", ylab = "N Concentration",xlab = "Year", main = "MSSP_TH"))
axis(1, at=with(Pdct_MSSP_TH,T), labels=with(Pdct_MSSP_TH,Date))
with(Pdct_MSSP_TH, matlines(T,C,type="l"))

MSSP_TH= with(Pdct_MSSP_TH,data.frame(T,C,C03))
TH = with(MSSP_TH,lm(C~T))
TH.2 = with(MSSP_TH,lm(C03~T))
abline(TH)
abline(TH.2,lty = 2)

legend('topright', c("with Price","without Price"), lty = c(1,2), bty='o')
#graphic part ends




## OHIO_GRCH

Pdct_OHIO_GRCH = read.csv("Pdct_OHIO_GRCH.csv")
Qbar = with(Pdct_OHIO_GRCH, mean(Q))
Qwave = with(Pdct_OHIO_GRCH, Qbar + sum((Q - Qbar)^3)/(2*sum((Q - Qbar)^2)))

Tbar = with(Pdct_OHIO_GRCH, mean(T))
Twave = with(Pdct_OHIO_GRCH, Tbar + sum((T - Tbar)^3)/(2*sum((T - Tbar)^2)))

LNC = with(Pdct_OHIO_GRCH, as.matrix(-0.097 - 0.1*log(Q/Qwave) - 0.23*Q_Qbar2 -0.0002*(T-Twave) + 0.26*sin(2*pi*T)
                                     + 0.007*cos(2*pi*T) - 0.015*log(NP) - 0.12*log(CSR)))
C = exp(LNC)         

LNCWOP = with(Pdct_OHIO_GRCH, as.matrix(-0.086 - 0.099*log(Q/Qwave) - 0.236*Q_Qbar2 -0.0001*(T-Twave)+ 0*(T-Twave)^2 + 0.257*sin(2*pi*T)
                                        + 0.005*cos(2*pi*T)))
C03 = exp(LNCWOP)


with(Pdct_OHIO_GRCH, matplot(T,C03,type = "l",lty = 2,xaxt = "n", ylab = "N Concentration",xlab = "Year", main = "OHIO_GRCH"))
axis(1, at=with(Pdct_OHIO_GRCH,T), labels=with(Pdct_OHIO_GRCH,Date))
with(Pdct_OHIO_GRCH, matlines(T,C,type="l"))

OHIO_GRCH= with(Pdct_OHIO_GRCH,data.frame(T,C,C03))
GRCH = with(OHIO_GRCH,lm(C~T))
GRCH.2 = with(OHIO_GRCH,lm(C03~T))
abline(GRCH)
abline(GRCH.2,lty = 2)

legend('topright', c("with Price","without Price"), lty = c(1,2), bty='o')
#graphic part ends


