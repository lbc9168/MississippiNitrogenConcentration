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

LNC03 = with(Pdct_ILLI_VC, as.matrix(2.315 + 0.055*log(Q/Qwave) - 0.14*Q_Qbar2 -0.0009*(T-Twave) + 0.303*sin(2*pi*T)
                + 0.16*cos(2*pi*T) - 0.12*log(NP03) + 0.25*log(CSR)))
C03 = exp(LNC03)

LNC09 = with(Pdct_ILLI_VC, as.matrix(2.315 + 0.055*log(Q/Qwave) - 0.14*Q_Qbar2 -0.0009*(T-Twave) + 0.303*sin(2*pi*T)
                                     + 0.16*cos(2*pi*T) - 0.12*log(NP09) + 0.25*log(CSR)))
C09 = exp(LNC09)

with(Pdct_ILLI_VC, matplot(T,C03,type = "l",col = 2))
with(Pdct_ILLI_VC, matlines(T,C09,type = "l",col = 4))
with(Pdct_ILLI_VC, matlines(T,C,type="l"))


## IOWA_WAP

Pdct_IOWA_WAP = read.csv("Pdct_IOWA_WAP.csv")
Qbar = with(Pdct_IOWA_WAP, mean(Q))
Qwave = with(Pdct_IOWA_WAP, Qbar + sum((Q - Qbar)^3)/(2*sum((Q - Qbar)^2)))

Tbar = with(Pdct_IOWA_WAP, mean(T))
Twave = with(Pdct_IOWA_WAP, Tbar + sum((T - Tbar)^3)/(2*sum((T - Tbar)^2)))

LNC = with(Pdct_IOWA_WAP, as.matrix(2.12 - 0.18*log(Q/Qwave) - 0.309*Q_Qbar2 -0.0042*(T-Twave) + 0.3*sin(2*pi*T)
                                    + 0.45*cos(2*pi*T) - 0.11*log(NP) -0.66*log(CSR)))
C = exp(LNC)         

LNC03 = with(Pdct_IOWA_WAP, as.matrix(2.12 - 0.18*log(Q/Qwave) - 0.309*Q_Qbar2 -0.0042*(T-Twave) + 0.3*sin(2*pi*T)
                                      + 0.45*cos(2*pi*T) - 0.11*log(NP03) -0.66*log(CSR)))
C03 = exp(LNC03)

LNC09 = with(Pdct_IOWA_WAP, as.matrix(2.12 - 0.18*log(Q/Qwave) - 0.309*Q_Qbar2 -0.0042*(T-Twave) + 0.3*sin(2*pi*T)
                                      + 0.45*cos(2*pi*T) - 0.11*log(NP09) -0.66*log(CSR)))
C09 = exp(LNC09)

with(Pdct_IOWA_WAP, matplot(T,C03,type = "l",col = 2))
with(Pdct_IOWA_WAP, matlines(T,C09,type = "l",col = 4))
with(Pdct_IOWA_WAP, matlines(T,C,type="l"))



## MIZZ_HE

Pdct_MIZZ_HE = read.csv("Pdct_MIZZ_HE.csv")
Qbar = with(Pdct_MIZZ_HE, mean(Q))
Qwave = with(Pdct_MIZZ_HE, Qbar + sum((Q - Qbar)^3)/(2*sum((Q - Qbar)^2)))

Tbar = with(Pdct_MIZZ_HE, mean(T))
Twave = with(Pdct_MIZZ_HE, Tbar + sum((T - Tbar)^3)/(2*sum((T - Tbar)^2)))

LNC = with(Pdct_MIZZ_HE, as.matrix(1.1 + 0.122*log(Q/Qwave) - 0.16*Q_Qbar2 -0.01*(T-Twave) + 0.452*sin(2*pi*T)
                                   + 0.035*cos(2*pi*T) - 0.19*log(NP) -0.122*log(CSR)))
C = exp(LNC)         

LNC03 = with(Pdct_MIZZ_HE, as.matrix(1.1 + 0.122*log(Q/Qwave) - 0.16*Q_Qbar2 -0.01*(T-Twave) + 0.452*sin(2*pi*T)
                                     + 0.035*cos(2*pi*T) - 0.19*log(NP03) -0.122*log(CSR)))
C03 = exp(LNC03)

LNC09 = with(Pdct_MIZZ_HE, as.matrix(1.1 + 0.122*log(Q/Qwave) - 0.16*Q_Qbar2 -0.01*(T-Twave) + 0.452*sin(2*pi*T)
                                     + 0.035*cos(2*pi*T) - 0.19*log(NP09) -0.122*log(CSR)))
C09 = exp(LNC09)

with(Pdct_MIZZ_HE, matplot(T,C03,type = "l",col = 2))
with(Pdct_MIZZ_HE, matlines(T,C09,type = "l",col = 4))
with(Pdct_MIZZ_HE, matlines(T,C,type="l"))


## MSSP_CL

Pdct_MSSP_CL = read.csv("Pdct_MSSP_CL.csv")
Qbar = with(Pdct_MSSP_CL, mean(Q))
Qwave = with(Pdct_MSSP_CL, Qbar + sum((Q - Qbar)^3)/(2*sum((Q - Qbar)^2)))

Tbar = with(Pdct_MSSP_CL, mean(T))
Twave = with(Pdct_MSSP_CL, Tbar + sum((T - Tbar)^3)/(2*sum((T - Tbar)^2)))

LNC = with(Pdct_MSSP_CL, as.matrix(2.398 + 0.112*log(Q/Qwave) - 0.102*Q_Qbar2 -0.0002*(T-Twave) + 0.294*sin(2*pi*T)
                                   + 0.164*cos(2*pi*T) - 0.132*log(NP) + 0.294*log(CSR)))
C = exp(LNC)         

LNC03 = with(Pdct_MSSP_CL, as.matrix(2.398 + 0.112*log(Q/Qwave) - 0.102*Q_Qbar2 -0.0002*(T-Twave) + 0.294*sin(2*pi*T)
                                     + 0.164*cos(2*pi*T) - 0.132*log(NP03) + 0.294*log(CSR)))
C03 = exp(LNC03)

LNC09 = with(Pdct_MSSP_CL, as.matrix(2.398 + 0.112*log(Q/Qwave) - 0.102*Q_Qbar2 -0.0002*(T-Twave) + 0.294*sin(2*pi*T)
                                     + 0.164*cos(2*pi*T) - 0.132*log(NP09) + 0.294*log(CSR)))
C09 = exp(LNC09)

with(Pdct_MSSP_CL, matplot(T,C03,type = "l",col = 2))
with(Pdct_MSSP_CL, matlines(T,C09,type = "l",col = 4))
with(Pdct_MSSP_CL, matlines(T,C,type="l"))


## MSSP_GR

Pdct_MSSP_GR = read.csv("Pdct_MSSP_GR.csv")
Qbar = with(Pdct_MSSP_GR, mean(Q))
Qwave = with(Pdct_MSSP_GR, Qbar + sum((Q - Qbar)^3)/(2*sum((Q - Qbar)^2)))

Tbar = with(Pdct_MSSP_GR, mean(T))
Twave = with(Pdct_MSSP_GR, Tbar + sum((T - Tbar)^3)/(2*sum((T - Tbar)^2)))

LNC = with(Pdct_MSSP_GR, as.matrix(2.135 + 0.174*log(Q/Qwave) - 0.148*Q_Qbar2 -0.011*(T-Twave) + 0.283*sin(2*pi*T)
                                   + 0.139*cos(2*pi*T) - 0.2*log(NP) -0.119*log(CSR)))
C = exp(LNC)         

LNC03 = with(Pdct_MSSP_GR, as.matrix(2.135 + 0.174*log(Q/Qwave) - 0.148*Q_Qbar2 -0.011*(T-Twave) + 0.283*sin(2*pi*T)
                                     + 0.139*cos(2*pi*T) - 0.2*log(NP03) -0.119*log(CSR)))
C03 = exp(LNC03)

LNC09 = with(Pdct_MSSP_GR, as.matrix(2.135 + 0.174*log(Q/Qwave) - 0.148*Q_Qbar2 -0.011*(T-Twave) + 0.283*sin(2*pi*T)
                                     + 0.139*cos(2*pi*T) - 0.2*log(NP09) -0.119*log(CSR)))
C09 = exp(LNC09)

with(Pdct_MSSP_GR, matplot(T,C03,type = "l",col = 2))
with(Pdct_MSSP_GR, matlines(T,C09,type = "l",col = 4))
with(Pdct_MSSP_GR, matlines(T,C,type="l"))


## MSSP_OUT

Pdct_MSSP_OUT = read.csv("Pdct_MSSP_OUT.csv")
Qbar = with(Pdct_MSSP_OUT, mean(Q))
Qwave = with(Pdct_MSSP_OUT, Qbar + sum((Q - Qbar)^3)/(2*sum((Q - Qbar)^2)))

Tbar = with(Pdct_MSSP_OUT, mean(T))
Twave = with(Pdct_MSSP_OUT, Tbar + sum((T - Tbar)^3)/(2*sum((T - Tbar)^2)))

LNC = with(Pdct_MSSP_OUT, as.matrix(0.65 - 0.024*log(Q/Qwave) - 0.21*Q_Qbar2 -0.0017*(T-Twave) + 0.156*sin(2*pi*T)
                                    - 0.174*cos(2*pi*T) - 0.014*log(NP) + 0.29*log(CSR)))
C = exp(LNC)         

LNC03 = with(Pdct_MSSP_OUT, as.matrix(0.65 - 0.024*log(Q/Qwave) - 0.21*Q_Qbar2 -0.0017*(T-Twave) + 0.156*sin(2*pi*T)
                                      - 0.174*cos(2*pi*T) - 0.014*log(NP03) + 0.29*log(CSR)))
C03 = exp(LNC03)

LNC09 = with(Pdct_MSSP_OUT, as.matrix(0.65 - 0.024*log(Q/Qwave) - 0.21*Q_Qbar2 -0.0017*(T-Twave) + 0.156*sin(2*pi*T)
                                      - 0.174*cos(2*pi*T) - 0.014*log(NP09) + 0.29*log(CSR)))
C09 = exp(LNC09)

with(Pdct_MSSP_OUT, matplot(T,C03,type = "l",col = 2))
with(Pdct_MSSP_OUT, matlines(T,C09,type = "l",col = 4))
with(Pdct_MSSP_OUT, matlines(T,C,type="l"))


## MSSP_TH

Pdct_MSSP_TH = read.csv("Pdct_MSSP_TH.csv")
Qbar = with(Pdct_MSSP_TH, mean(Q))
Qwave = with(Pdct_MSSP_TH, Qbar + sum((Q - Qbar)^3)/(2*sum((Q - Qbar)^2)))

Tbar = with(Pdct_MSSP_TH, mean(T))
Twave = with(Pdct_MSSP_TH, Tbar + sum((T - Tbar)^3)/(2*sum((T - Tbar)^2)))

LNC = with(Pdct_MSSP_TH, as.matrix(2 + 0.118*log(Q/Qwave) - 0.295*Q_Qbar2 -0.0011*(T-Twave) + 0.253*sin(2*pi*T)
                                   -0.174*cos(2*pi*T) - 0.167*log(NP) + 0.285*log(CSR)))
C = exp(LNC)         

LNC03 = with(Pdct_MSSP_TH, as.matrix(2 + 0.118*log(Q/Qwave) - 0.295*Q_Qbar2 -0.0011*(T-Twave) + 0.253*sin(2*pi*T)
                                     -0.174*cos(2*pi*T) - 0.167*log(NP03) + 0.285*log(CSR)))
C03 = exp(LNC03)

LNC09 = with(Pdct_MSSP_TH, as.matrix(2 + 0.118*log(Q/Qwave) - 0.295*Q_Qbar2 -0.0011*(T-Twave) + 0.253*sin(2*pi*T)
                                     -0.174*cos(2*pi*T) - 0.167*log(NP09) + 0.285*log(CSR)))
C09 = exp(LNC09)

with(Pdct_MSSP_TH, matplot(T,C03,type = "l",col = 2))
with(Pdct_MSSP_TH, matlines(T,C09,type = "l",col = 4))
with(Pdct_MSSP_TH, matlines(T,C,type="l"))


## OHIO_GRCH

Pdct_OHIO_GRCH = read.csv("Pdct_OHIO_GRCH.csv")
Qbar = with(Pdct_OHIO_GRCH, mean(Q))
Qwave = with(Pdct_OHIO_GRCH, Qbar + sum((Q - Qbar)^3)/(2*sum((Q - Qbar)^2)))

Tbar = with(Pdct_OHIO_GRCH, mean(T))
Twave = with(Pdct_OHIO_GRCH, Tbar + sum((T - Tbar)^3)/(2*sum((T - Tbar)^2)))

LNC = with(Pdct_OHIO_GRCH, as.matrix(-0.097 - 0.1*log(Q/Qwave) - 0.23*Q_Qbar2 -0.0002*(T-Twave) + 0.26*sin(2*pi*T)
                                     + 0.007*cos(2*pi*T) - 0.015*log(NP) - 0.12*log(CSR)))
C = exp(LNC)         

LNC03 = with(Pdct_OHIO_GRCH, as.matrix(-0.097 - 0.1*log(Q/Qwave) - 0.23*Q_Qbar2 -0.0002*(T-Twave) + 0.26*sin(2*pi*T)
                                       + 0.007*cos(2*pi*T) - 0.015*log(NP03) - 0.12*log(CSR)))
C03 = exp(LNC03)

LNC09 = with(Pdct_OHIO_GRCH, as.matrix(-0.097 - 0.1*log(Q/Qwave) - 0.23*Q_Qbar2 -0.0002*(T-Twave) + 0.26*sin(2*pi*T)
                                       + 0.007*cos(2*pi*T) - 0.015*log(NP09) - 0.12*log(CSR)))
C09 = exp(LNC09)

with(Pdct_OHIO_GRCH, matplot(T,C03,type = "l",col = 2))
with(Pdct_OHIO_GRCH, matlines(T,C09,type = "l",col = 4))
with(Pdct_OHIO_GRCH, matlines(T,C,type="l"))
