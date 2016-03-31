setwd("C:/Users/Bingcai/OSU/AED Research/Mississipi/2015 Summer_All sites/WRTDS model")

Tenn = read.csv("Tennessee_River.csv")
Q_Qbar2sq = with(Tenn, log(Q_Qbar2)^2)
T_Tbar2sq = with(Tenn, T_Tbar2^2)
Tenn.lm = with(Tenn,lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP)+log(CSR)))
Tenn.lm.2 = with(Tenn,lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)+log(NP)+log(CSR)))
Tenn.lm.3 = with(Tenn,lm(log(Conc)~log(Q_Qbar2)+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP)+log(CSR)))
summary(Tenn.lm)
summary(Tenn.lm.2)
summary(Tenn.lm.3)

Wab = read.csv("Wabash_River.csv")
Q_Qbar2sq = with(Wab, log(Q_Qbar2)^2)
T_Tbar2sq = with(Wab, T_Tbar2^2)
Wab.lm = with(Wab,lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP)+log(CSR)))
Wab.lm.2 = with(Wab,lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+T_Tbar2sq+sin(2*pi*T)+cos(2*pi*T)+log(NP)+log(CSR)))
Wab.lm.3 = with(Wab,lm(log(Conc)~log(Q_Qbar2)+T_Tbar2+sin(2*pi*T)+cos(2*pi*T)+log(NP)+log(CSR)))

wab.lm.4 = with(Wab,lm(log(Conc)~log(Q_Qbar2)+Q_Qbar2sq+T_Tbar2+sin(2*pi*T)+cos(2*pi*T) + log(NP)+ as.factor(after)
                       +log(NP)*as.factor(after)))
summary(Wab.lm)
summary(Wab.lm.2)
summary(Wab.lm.3)

summary(wab.lm.4)

with(Wab,plot(T,Conc))
with(Wab,matlines(T,Conc))
