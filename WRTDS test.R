setwd("C:/Users/Bingcai/OSU/AED Research/Mississipi/2015 Summer_All sites/WRTDS model")
load("C:/Users/Bingcai/OSU/AED Research/Mississipi/2015 Summer_All sites/WRTDS model/.RData")

res.ILLI_VC.lm = resid(ILLI_VC.lm)
summary(res.ILLI_VC.lm)
plot(res.ILLI_VC.lm)

res.MIZZ_HE.lm.0to2 = resid(MIZZ_HE.lm.0to2)
plot(res.MIZZ_HE.lm.0to2)
res.MIZZ_HE.lm.2.0to2 = resid(MIZZ_HE.lm.2.0to2)
plot(res.MIZZ_HE.lm.2.0to2)

res.MSSP_CL.lm.0to2 = resid(MSSP_CL.lm.0to2)
plot(res.MSSP_CL.lm.0to2)
res.MSSP_CL.lm.2.0to2 = resid(MSSP_CL.lm.2.0to2)
plot(res.MSSP_CL.lm.2.0to2)
abline(h =0)
abline(h = 0.5)
abline(h= -0.5)

anova(MIZZ_HE.lm.0to2,MIZZ_HE.lm.2.0to2)
anova(MSSP_GR.lm.0to2,MSSP_GR.lm.2.0to2)