setwd("C:/Users/Bingcai/OSU/AED Research/Mississipi/2015 Summer_All sites/All_data/Time adjusted added/3 month average")

# ILLI_VC
ILLI_VC = read.csv("ILLI_VC.csv")

ILLI_VC.lm = with(ILLI_VC[ILLI_VC$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow) + as.factor(Month)
                                                      + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                      + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                      + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                      + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                      + X2013 + X2014 + X2015))
ILLI_VC.lm.test = with(ILLI_VC[ILLI_VC$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow) + as.factor(Month) + as.factor(Year) ))
summary(ILLI_VC.lm)
summary(ILLI_VC.lm.test)


ILLI_VC.lm.1m = with(ILLI_VC[ILLI_VC$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                      +log(NPA1)
                                                      +log(CSRA1)
                                                      + as.factor(Month)
                                                      + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                      + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                      + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                      + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                      + X2013 + X2014 + X2015))
ILLI_VC.lm.1m.test = with(ILLI_VC[ILLI_VC$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSratio) + log(Flow)
                                                           +log(NPA1)
                                                           +log(CSR1)
                                                           + as.factor(Month) + as.factor(Year)))
summary(ILLI_VC.lm.1m)
summary(ILLI_VC.lm.1m.test)


ILLI_VC.lm.2m = with(ILLI_VC[ILLI_VC$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                         +log(NPA1) +log(NPA2)
                                                         +log(CSRA1) +log(CSRA2) 
                                                         + as.factor(Month)
                                                         + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                         + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                         + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                         + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                         + X2013 + X2014 + X2015))
ILLI_VC.lm.2m.test = with(ILLI_VC[ILLI_VC$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSratio) + log(Flow)
                                                              +log(NPA1) +log(NPA2)
                                                              +log(CSR1) +log(CSR2)
                                                              + as.factor(Month) + as.factor(Year) ))
summary(ILLI_VC.lm.2m)
summary(ILLI_VC.lm.2m.test)


ILLI_VC.lm.3m = with(ILLI_VC[ILLI_VC$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                         +log(NPA1) +log(NPA2) +log(NPA3)
                                                         +log(CSRA1) +log(CSRA2) +log(CSRA3)
                                                         + as.factor(Month)
                                                         + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                         + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                         + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                         + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                         + X2013 + X2014 + X2015))
summary(ILLI_VC.lm.3m)


ILLI_VC.lm.4m = with(ILLI_VC[ILLI_VC$NPA4 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                         +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4)
                                                         +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4)
                                                         + as.factor(Month)
                                                         + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                         + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                         + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                         + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                         + X2013 + X2014 + X2015))
summary(ILLI_VC.lm.4m)


ILLI_VC.lm.5m = with(ILLI_VC[ILLI_VC$NPA5 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(ILLI_VC.lm.5m)


ILLI_VC.lm.6m = with(ILLI_VC[ILLI_VC$NPA6 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CRSA6)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(ILLI_VC.lm.6m)


ILLI_VC.lm.7m = with(ILLI_VC[ILLI_VC$NPA7 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                     +log(NPA7)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CRSA6)
                                                     +log(CSRA7)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(ILLI_VC.lm.7m)


ILLI_VC.lm.8m = with(ILLI_VC[ILLI_VC$NPA8 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                     +log(NPA7) +log(NPA8)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CRSA6)
                                                     +log(CSRA7) +log(CSRA8)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(ILLI_VC.lm.8m)


ILLI_VC.lm.9m = with(ILLI_VC[ILLI_VC$NPA9 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                     +log(NPA7) +log(NPA8) +log(NPA9)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CRSA6)
                                                     +log(CSRA7) +log(CSRA8) +log(CSRA9)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(ILLI_VC.lm.9m)


ILLI_VC.lm.10m = with(ILLI_VC[ILLI_VC$NPA10 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                     +log(NPA7) +log(NPA8) +log(NPA9) +log(NPA10)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CRSA6)
                                                     +log(CSRA7) +log(CSRA8) +log(CSRA9) +log(CSRA10)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(ILLI_VC.lm.10m)