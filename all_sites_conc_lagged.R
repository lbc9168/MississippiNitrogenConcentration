## concentration, flow, and precipitation lagged

## ILLI_VC (Precipitation wrong)
ILLI_VC_3to8 = read.csv("ILLI_VC_3to8.csv")

ILLI_VC_3to8.lm.no.lag = with(ILLI_VC_3to8, lm(log(N_Conc) ~ log(NP) + log(Flow) + log(Precip) + log(CP) + log(SBP)))
summary(ILLI_VC_3to8.lm.no.lag)

ILLI_VC_3to8.lm.1month = with(ILLI_VC_3to8, lm(log(NC1) ~ log(NP) + log(F1) + log(P1) + log(CP) + log(SBP)))


## IOWA_WAP
IOWA_WAP_3to8 = read.csv("IOWA_WAP_3to8.csv")

IOWA_WAP_3to8.lm.no.lag = with(IOWA_WAP_3to8, lm(log(N_Conc) ~ log(NP) + log(Flow) + log(Precip) + log(CP) + log(SBP)))
summary(ILLI_VC_3to8.lm.no.lag)

IOWA_WAP_3to8.lm.1month = with(IOWA_WAP_3to8, lm(log(NC1) ~ log(NP) + log(F1) + log(P1) + log(CP) + log(SBP)))
summary(IOWA_WAP_3to8.lm.1month)

IOWA_WAP_3to8.lm.2month = with(IOWA_WAP_3to8, lm(log(NC2) ~ log(NP) + log(F2) + log(P2) + log(CP) + log(SBP)))
summary(IOWA_WAP_3to8.lm.2month)

IOWA_WAP_3to8.lm.3month = with(IOWA_WAP_3to8, lm(log(NC3) ~ log(NP) + log(F3) + log(P3) + log(CP) + log(SBP)))
summary(IOWA_WAP_3to8.lm.3month)

IOWA_WAP_3to8.lm.4month = with(IOWA_WAP_3to8, lm(log(NC4) ~ log(NP) + log(F4) + log(P4) + log(CP) + log(SBP)))
summary(IOWA_WAP_3to8.lm.4month)

IOWA_WAP_3to8.lm.5month = with(IOWA_WAP_3to8, lm(log(NC5) ~ log(NP) + log(F5) + log(P5) + log(CP) + log(SBP)))
summary(IOWA_WAP_3to8.lm.5month)

IOWA_WAP_3to8.lm.6month = with(IOWA_WAP_3to8, lm(log(NC6) ~ log(NP) + log(F6) + log(P6) + log(CP) + log(SBP)))
summary(IOWA_WAP_3to8.lm.6month)


## MIZZ_HE
MIZZ_HE_3to8 = read.csv("MIZZ_HE_3to8.csv")

MIZZ_HE_3to8.lm.no.lag = with(MIZZ_HE_3to8, lm(log(N_Conc) ~ log(NP) + log(Flow) + log(Precip) + log(CP) + log(SBP)))
summary(MIZZ_HE_3to8.lm.no.lag)

MIZZ_HE_3to8.lm.1month = with(MIZZ_HE_3to8, lm(log(NC1) ~ log(NP) + log(F1) + log(P1) + log(CP) + log(SBP)))
summary(MIZZ_HE_3to8.lm.1month)

MIZZ_HE_3to8.lm.2month = with(MIZZ_HE_3to8, lm(log(NC2) ~ log(NP) + log(F2) + log(P2) + log(CP) + log(SBP)))
summary(MIZZ_HE_3to8.lm.2month)

MIZZ_HE_3to8.lm.3month = with(MIZZ_HE_3to8, lm(log(NC3) ~ log(NP) + log(F3) + log(P3) + log(CP) + log(SBP)))
summary(MIZZ_HE_3to8.lm.3month)

MIZZ_HE_3to8.lm.4month = with(MIZZ_HE_3to8, lm(log(NC4) ~ log(NP) + log(F4) + log(P4) + log(CP) + log(SBP)))
summary(MIZZ_HE_3to8.lm.4month)

MIZZ_HE_3to8.lm.5month = with(MIZZ_HE_3to8, lm(log(NC5) ~ log(NP) + log(F5) + log(P5) + log(CP) + log(SBP)))
summary(MIZZ_HE_3to8.lm.5month)

MIZZ_HE_3to8.lm.6month = with(MIZZ_HE_3to8, lm(log(NC6) ~ log(NP) + log(F6) + log(P6) + log(CP) + log(SBP)))
summary(MIZZ_HE_3to8.lm.6month)

## MSSP_CL
MSSP_CL_3to8 = read.csv("MSSP_CL_3to8.csv")

MSSP_CL_3to8.lm.no.lag = with(MSSP_CL_3to8, lm(log(N_Conc) ~ log(NP) + log(Flow) + log(Precip) + log(CP) + log(SBP)))
summary(MSSP_CL_3to8.lm.no.lag)

MSSP_CL_3to8.lm.1month = with(MSSP_CL_3to8, lm(log(NC1) ~ log(NP) + log(F1) + log(P1) + log(CP) + log(SBP)))
summary(MSSP_CL_3to8.lm.1month)

MSSP_CL_3to8.lm.2month = with(MSSP_CL_3to8, lm(log(NC2) ~ log(NP) + log(F2) + log(P2) + log(CP) + log(SBP)))
summary(MSSP_CL_3to8.lm.2month)

MSSP_CL_3to8.lm.3month = with(MSSP_CL_3to8, lm(log(NC3) ~ log(NP) + log(F3) + log(P3) + log(CP) + log(SBP)))
summary(MSSP_CL_3to8.lm.3month)

MSSP_CL_3to8.lm.4month = with(MSSP_CL_3to8, lm(log(NC4) ~ log(NP) + log(F4) + log(P4) + log(CP) + log(SBP)))
summary(MSSP_CL_3to8.lm.4month)

MSSP_CL_3to8.lm.5month = with(MSSP_CL_3to8, lm(log(NC5) ~ log(NP) + log(F5) + log(P5) + log(CP) + log(SBP)))
summary(MSSP_CL_3to8.lm.5month)

MSSP_CL_3to8.lm.6month = with(MSSP_CL_3to8, lm(log(NC6) ~ log(NP) + log(F6) + log(P6) + log(CP) + log(SBP)))
summary(MSSP_CL_3to8.lm.6month)

## test
MSSP_CL_3to8.lm.4month.test = with(MSSP_CL_3to8, lm(log(NC4) ~ log(NP) + log(F4) + log(P4) + log(CP)))
summary(MSSP_CL_3to8.lm.4month.test)


## MSSP_OUT
MSSP_OUT_3to8 = read.csv("MSSP_OUT_3to8.csv")

MSSP_OUT_3to8.lm.no.lag = with(MSSP_OUT_3to8, lm(log(N_Conc) ~ log(NP) + log(Flow) + log(Precip) + log(CP) + log(SBP)))
summary(MSSP_OUT_3to8.lm.no.lag)

MSSP_OUT_3to8.lm.1month = with(MSSP_OUT_3to8, lm(log(NC1) ~ log(NP) + log(F1) + log(P1) + log(CP) + log(SBP), subset=(P1 != 0)))
summary(MSSP_OUT_3to8.lm.1month)

MSSP_OUT_3to8.lm.2month = with(MSSP_OUT_3to8, lm(log(NC2) ~ log(NP) + log(F2) + log(P2) + log(CP) + log(SBP), subset=(P2 != 0)))
summary(MSSP_OUT_3to8.lm.2month)

MSSP_OUT_3to8.lm.3month = with(MSSP_OUT_3to8, lm(log(NC3) ~ log(NP) + log(F3) + log(P3) + log(CP) + log(SBP), subset=(P3 != 0)))
summary(MSSP_OUT_3to8.lm.3month)

MSSP_OUT_3to8.lm.4month = with(MSSP_OUT_3to8, lm(log(NC4) ~ log(NP) + log(F4) + log(P4) + log(CP) + log(SBP), subset=(P4 != 0)))
summary(MSSP_OUT_3to8.lm.4month)

MSSP_OUT_3to8.lm.5month = with(MSSP_OUT_3to8, lm(log(NC5) ~ log(NP) + log(F5) + log(P5) + log(CP) + log(SBP), subset=(P5 != 0)))
summary(MSSP_OUT_3to8.lm.5month)

MSSP_OUT_3to8.lm.6month = with(MSSP_OUT_3to8, lm(log(NC6) ~ log(NP) + log(F6) + log(P6) + log(CP) + log(SBP), subset=(P6 != 0)))
summary(MSSP_OUT_3to8.lm.6month)


## OHIO_GRCH
OHIO_GRCH_3to8 = read.csv("OHIO_GRCH_3to8.csv")

OHIO_GRCH_3to8.lm.no.lag = with(OHIO_GRCH_3to8, lm(log(N_Conc) ~ log(NP) + log(Flow) + log(Precip) + log(CP) + log(SBP)))
summary(OHIO_GRCH_3to8.lm.no.lag)

OHIO_GRCH_3to8.lm.1month = with(OHIO_GRCH_3to8, lm(log(NC1) ~ log(NP) + log(F1) + log(P1) + log(CP) + log(SBP), subset=(P1 != 0)))
summary(OHIO_GRCH_3to8.lm.1month)

OHIO_GRCH_3to8.lm.2month = with(OHIO_GRCH_3to8, lm(log(NC2) ~ log(NP) + log(F2) + log(P2) + log(CP) + log(SBP), subset=(P2 != 0)))
summary(OHIO_GRCH_3to8.lm.2month)

OHIO_GRCH_3to8.lm.3month = with(OHIO_GRCH_3to8, lm(log(NC3) ~ log(NP) + log(F3) + log(P3) + log(CP) + log(SBP), subset=(P3 != 0)))
summary(OHIO_GRCH_3to8.lm.3month)

OHIO_GRCH_3to8.lm.4month = with(OHIO_GRCH_3to8, lm(log(NC4) ~ log(NP) + log(F4) + log(P4) + log(CP) + log(SBP), subset=(P4 != 0)))
summary(OHIO_GRCH_3to8.lm.4month)

OHIO_GRCH_3to8.lm.5month = with(OHIO_GRCH_3to8, lm(log(NC5) ~ log(NP) + log(F5) + log(P5) + log(CP) + log(SBP), subset=(P5 != 0)))
summary(OHIO_GRCH_3to8.lm.5month)

OHIO_GRCH_3to8.lm.6month = with(OHIO_GRCH_3to8, lm(log(NC6) ~ log(NP) + log(F6) + log(P6) + log(CP) + log(SBP), subset=(P6 != 0)))
summary(OHIO_GRCH_3to8.lm.6month)

