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
plot(C, type = "l")

LNC03 = with(Pdct_ILLI_VC, as.matrix(2.315 + 0.055*log(Q/Qwave) - 0.14*Q_Qbar2 -0.0009*(T-Twave) + 0.303*sin(2*pi*T)
                + 0.16*cos(2*pi*T) - 0.12*log(NP03) + 0.25*log(CSR)))
C03 = exp(LNC03)
plot(C03, type = "l")

LNC09 = with(Pdct_ILLI_VC, as.matrix(2.315 + 0.055*log(Q/Qwave) - 0.14*Q_Qbar2 -0.0009*(T-Twave) + 0.303*sin(2*pi*T)
                                     + 0.16*cos(2*pi*T) - 0.12*log(NP09) + 0.25*log(CSR)))
C09 = exp(LNC09)
plot(C09, type = "l")

with(Pdct_ILLI_VC, matplot(T,C,type = "l"))
with(Pdct_ILLI_VC, matlines(T,C03,type = "c",col = 2))
with(Pdct_ILLI_VC, matlines(T,C09,type = "c",col = 3))
