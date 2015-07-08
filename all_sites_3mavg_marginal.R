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


ILLI_VC.lm.1m = with(ILLI_VC[ILLI_VC$NPA1 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
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


ILLI_VC.lm.2m = with(ILLI_VC[ILLI_VC$NPA2 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
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


ILLI_VC.lm.3m = with(ILLI_VC[ILLI_VC$NPA3 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
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
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
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
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
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
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
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
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
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
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                     +log(CSRA7) +log(CSRA8) +log(CSRA9) +log(CSRA10)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(ILLI_VC.lm.10m)



# IOWA_WAP
IOWA_WAP = read.csv("IOWA_WAP.csv")

IOWA_WAP.lm = with(IOWA_WAP[IOWA_WAP$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow) + as.factor(Month)
                                                         + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                         + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                         + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                         + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                         + X2013 + X2014 + X2015))
IOWA_WAP.lm.test = with(IOWA_WAP[IOWA_WAP$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow) + as.factor(Month) + as.factor(Year) ))
summary(IOWA_WAP.lm)
summary(IOWA_WAP.lm.test)


IOWA_WAP.lm.1m = with(IOWA_WAP[IOWA_WAP$NPA1 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                        +log(NPA1)
                                                        +log(CSRA1)
                                                        + as.factor(Month)
                                                        + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                        + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                        + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                        + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                        + X2013 + X2014 + X2015))
IOWA_WAP.lm.1m.test = with(IOWA_WAP[IOWA_WAP$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSratio) + log(Flow)
                                                                 +log(NPA1)
                                                                 +log(CSR1)
                                                                 + as.factor(Month) + as.factor(Year)))
summary(IOWA_WAP.lm.1m)
summary(IOWA_WAP.lm.1m.test)


IOWA_WAP.lm.2m = with(IOWA_WAP[IOWA_WAP$NPA2 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                        +log(NPA1) +log(NPA2)
                                                        +log(CSRA1) +log(CSRA2) 
                                                        + as.factor(Month)
                                                        + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                        + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                        + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                        + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                        + X2013 + X2014 + X2015))
IOWA_WAP.lm.2m.test = with(IOWA_WAP[IOWA_WAP$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSratio) + log(Flow)
                                                                 +log(NPA1) +log(NPA2)
                                                                 +log(CSR1) +log(CSR2)
                                                                 + as.factor(Month) + as.factor(Year) ))
summary(IOWA_WAP.lm.2m)
summary(IOWA_WAP.lm.2m.test)


IOWA_WAP.lm.3m = with(IOWA_WAP[IOWA_WAP$NPA3 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                        +log(NPA1) +log(NPA2) +log(NPA3)
                                                        +log(CSRA1) +log(CSRA2) +log(CSRA3)
                                                        + as.factor(Month)
                                                        + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                        + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                        + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                        + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                        + X2013 + X2014 + X2015))
summary(IOWA_WAP.lm.3m)


IOWA_WAP.lm.4m = with(IOWA_WAP[IOWA_WAP$NPA4 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                        +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4)
                                                        +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4)
                                                        + as.factor(Month)
                                                        + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                        + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                        + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                        + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                        + X2013 + X2014 + X2015))
summary(IOWA_WAP.lm.4m)


IOWA_WAP.lm.5m = with(IOWA_WAP[IOWA_WAP$NPA5 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                        +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5)
                                                        +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5)
                                                        + as.factor(Month)
                                                        + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                        + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                        + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                        + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                        + X2013 + X2014 + X2015))
summary(IOWA_WAP.lm.5m)


IOWA_WAP.lm.6m = with(IOWA_WAP[IOWA_WAP$NPA6 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                        +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                        +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                        + as.factor(Month)
                                                        + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                        + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                        + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                        + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                        + X2013 + X2014 + X2015))
summary(IOWA_WAP.lm.6m)


IOWA_WAP.lm.7m = with(IOWA_WAP[IOWA_WAP$NPA7 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                        +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                        +log(NPA7)
                                                        +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                        +log(CSRA7)
                                                        + as.factor(Month)
                                                        + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                        + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                        + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                        + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                        + X2013 + X2014 + X2015))
summary(IOWA_WAP.lm.7m)


IOWA_WAP.lm.8m = with(IOWA_WAP[IOWA_WAP$NPA8 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                        +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                        +log(NPA7) +log(NPA8)
                                                        +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                        +log(CSRA7) +log(CSRA8)
                                                        + as.factor(Month)
                                                        + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                        + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                        + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                        + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                        + X2013 + X2014 + X2015))
summary(IOWA_WAP.lm.8m)


IOWA_WAP.lm.9m = with(IOWA_WAP[IOWA_WAP$NPA9 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                        +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                        +log(NPA7) +log(NPA8) +log(NPA9)
                                                        +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                        +log(CSRA7) +log(CSRA8) +log(CSRA9)
                                                        + as.factor(Month)
                                                        + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                        + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                        + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                        + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                        + X2013 + X2014 + X2015))
summary(IOWA_WAP.lm.9m)


IOWA_WAP.lm.10m = with(IOWA_WAP[IOWA_WAP$NPA10 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                          +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                          +log(NPA7) +log(NPA8) +log(NPA9) +log(NPA10)
                                                          +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                          +log(CSRA7) +log(CSRA8) +log(CSRA9) +log(CSRA10)
                                                          + as.factor(Month)
                                                          + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                          + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                          + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                          + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                          + X2013 + X2014 + X2015))
summary(IOWA_WAP.lm.10m)



# MIZZ_HE
MIZZ_HE = read.csv("MIZZ_HE.csv")

MIZZ_HE.lm = with(MIZZ_HE[MIZZ_HE$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow) + as.factor(Month)
                                                      + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                      + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                      + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                      + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                      + X2013 + X2014 + X2015))
MIZZ_HE.lm.test = with(MIZZ_HE[MIZZ_HE$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow) + as.factor(Month) + as.factor(Year) ))
summary(MIZZ_HE.lm)
summary(MIZZ_HE.lm.test)


MIZZ_HE.lm.1m = with(MIZZ_HE[MIZZ_HE$NPA1 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1)
                                                     +log(CSRA1)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
MIZZ_HE.lm.1m.test = with(MIZZ_HE[MIZZ_HE$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSratio) + log(Flow)
                                                              +log(NPA1)
                                                              +log(CSR1)
                                                              + as.factor(Month) + as.factor(Year)))
summary(MIZZ_HE.lm.1m)
summary(MIZZ_HE.lm.1m.test)


MIZZ_HE.lm.2m = with(MIZZ_HE[MIZZ_HE$NPA2 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2)
                                                     +log(CSRA1) +log(CSRA2) 
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
MIZZ_HE.lm.2m.test = with(MIZZ_HE[MIZZ_HE$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSratio) + log(Flow)
                                                              +log(NPA1) +log(NPA2)
                                                              +log(CSR1) +log(CSR2)
                                                              + as.factor(Month) + as.factor(Year) ))
summary(MIZZ_HE.lm.2m)
summary(MIZZ_HE.lm.2m.test)


MIZZ_HE.lm.3m = with(MIZZ_HE[MIZZ_HE$NPA3 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MIZZ_HE.lm.3m)


MIZZ_HE.lm.4m = with(MIZZ_HE[MIZZ_HE$NPA4 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MIZZ_HE.lm.4m)


MIZZ_HE.lm.5m = with(MIZZ_HE[MIZZ_HE$NPA5 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MIZZ_HE.lm.5m)


MIZZ_HE.lm.6m = with(MIZZ_HE[MIZZ_HE$NPA6 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MIZZ_HE.lm.6m)


MIZZ_HE.lm.7m = with(MIZZ_HE[MIZZ_HE$NPA7 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                     +log(NPA7)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                     +log(CSRA7)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MIZZ_HE.lm.7m)


MIZZ_HE.lm.8m = with(MIZZ_HE[MIZZ_HE$NPA8 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                     +log(NPA7) +log(NPA8)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                     +log(CSRA7) +log(CSRA8)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MIZZ_HE.lm.8m)


MIZZ_HE.lm.9m = with(MIZZ_HE[MIZZ_HE$NPA9 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                     +log(NPA7) +log(NPA8) +log(NPA9)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                     +log(CSRA7) +log(CSRA8) +log(CSRA9)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MIZZ_HE.lm.9m)


MIZZ_HE.lm.10m = with(MIZZ_HE[MIZZ_HE$NPA10 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                       +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                       +log(NPA7) +log(NPA8) +log(NPA9) +log(NPA10)
                                                       +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                       +log(CSRA7) +log(CSRA8) +log(CSRA9) +log(CSRA10)
                                                       + as.factor(Month)
                                                       + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                       + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                       + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                       + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                       + X2013 + X2014 + X2015))
summary(MIZZ_HE.lm.10m)



# MSSP_CL
MSSP_CL = read.csv("MSSP_CL.csv")

MSSP_CL.lm = with(MSSP_CL[MSSP_CL$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow) + as.factor(Month)
                                                      + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                      + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                      + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                      + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                      + X2013 + X2014 + X2015))
summary(MSSP_CL.lm)


MSSP_CL.lm.1m = with(MSSP_CL[MSSP_CL$NPA1 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1)
                                                     +log(CSRA1)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_CL.lm.1m)


MSSP_CL.lm.2m = with(MSSP_CL[MSSP_CL$NPA2 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2)
                                                     +log(CSRA1) +log(CSRA2) 
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_CL.lm.2m)


MSSP_CL.lm.3m = with(MSSP_CL[MSSP_CL$NPA3 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_CL.lm.3m)


MSSP_CL.lm.4m = with(MSSP_CL[MSSP_CL$NPA4 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_CL.lm.4m)


MSSP_CL.lm.5m = with(MSSP_CL[MSSP_CL$NPA5 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_CL.lm.5m)


MSSP_CL.lm.6m = with(MSSP_CL[MSSP_CL$NPA6 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_CL.lm.6m)


MSSP_CL.lm.7m = with(MSSP_CL[MSSP_CL$NPA7 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                     +log(NPA7)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                     +log(CSRA7)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_CL.lm.7m)


MSSP_CL.lm.8m = with(MSSP_CL[MSSP_CL$NPA8 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                     +log(NPA7) +log(NPA8)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                     +log(CSRA7) +log(CSRA8)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_CL.lm.8m)


MSSP_CL.lm.9m = with(MSSP_CL[MSSP_CL$NPA9 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                     +log(NPA7) +log(NPA8) +log(NPA9)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                     +log(CSRA7) +log(CSRA8) +log(CSRA9)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_CL.lm.9m)


MSSP_CL.lm.10m = with(MSSP_CL[MSSP_CL$NPA10 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                       +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                       +log(NPA7) +log(NPA8) +log(NPA9) +log(NPA10)
                                                       +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                       +log(CSRA7) +log(CSRA8) +log(CSRA9) +log(CSRA10)
                                                       + as.factor(Month)
                                                       + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                       + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                       + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                       + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                       + X2013 + X2014 + X2015))
summary(MSSP_CL.lm.10m)




# MSSP_GR
MSSP_GR = read.csv("MSSP_GR.csv")

MSSP_GR.lm = with(MSSP_GR[MSSP_GR$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow) + as.factor(Month)
                                                      + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                      + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                      + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                      + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                      + X2013 + X2014 + X2015))
summary(MSSP_GR.lm)


MSSP_GR.lm.1m = with(MSSP_GR[MSSP_GR$NPA1 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1)
                                                     +log(CSRA1)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_GR.lm.1m)


MSSP_GR.lm.2m = with(MSSP_GR[MSSP_GR$NPA2 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2)
                                                     +log(CSRA1) +log(CSRA2) 
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_GR.lm.2m)


MSSP_GR.lm.3m = with(MSSP_GR[MSSP_GR$NPA3 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_GR.lm.3m)


MSSP_GR.lm.4m = with(MSSP_GR[MSSP_GR$NPA4 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_GR.lm.4m)


MSSP_GR.lm.5m = with(MSSP_GR[MSSP_GR$NPA5 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_GR.lm.5m)


MSSP_GR.lm.6m = with(MSSP_GR[MSSP_GR$NPA6 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_GR.lm.6m)


MSSP_GR.lm.7m = with(MSSP_GR[MSSP_GR$NPA7 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                     +log(NPA7)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                     +log(CSRA7)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_GR.lm.7m)


MSSP_GR.lm.8m = with(MSSP_GR[MSSP_GR$NPA8 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                     +log(NPA7) +log(NPA8)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                     +log(CSRA7) +log(CSRA8)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_GR.lm.8m)


MSSP_GR.lm.9m = with(MSSP_GR[MSSP_GR$NPA9 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                     +log(NPA7) +log(NPA8) +log(NPA9)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                     +log(CSRA7) +log(CSRA8) +log(CSRA9)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_GR.lm.9m)


MSSP_GR.lm.10m = with(MSSP_GR[MSSP_GR$NPA10 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                       +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                       +log(NPA7) +log(NPA8) +log(NPA9) +log(NPA10)
                                                       +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                       +log(CSRA7) +log(CSRA8) +log(CSRA9) +log(CSRA10)
                                                       + as.factor(Month)
                                                       + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                       + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                       + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                       + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                       + X2013 + X2014 + X2015))
summary(MSSP_GR.lm.10m)



# MSSP_OUT
MSSP_OUT = read.csv("MSSP_OUT.csv")

MSSP_OUT.lm = with(MSSP_OUT[MSSP_OUT$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow) + as.factor(Month)
                                                         + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                         + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                         + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                         + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                         + X2013 + X2014 + X2015))
summary(MSSP_OUT.lm)


MSSP_OUT.lm.1m = with(MSSP_OUT[MSSP_OUT$NPA1 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                        +log(NPA1)
                                                        +log(CSRA1)
                                                        + as.factor(Month)
                                                        + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                        + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                        + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                        + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                        + X2013 + X2014 + X2015))
summary(MSSP_OUT.lm.1m)


MSSP_OUT.lm.2m = with(MSSP_OUT[MSSP_OUT$NPA2 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                        +log(NPA1) +log(NPA2)
                                                        +log(CSRA1) +log(CSRA2) 
                                                        + as.factor(Month)
                                                        + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                        + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                        + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                        + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                        + X2013 + X2014 + X2015))
summary(MSSP_OUT.lm.2m)


MSSP_OUT.lm.3m = with(MSSP_OUT[MSSP_OUT$NPA3 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                        +log(NPA1) +log(NPA2) +log(NPA3)
                                                        +log(CSRA1) +log(CSRA2) +log(CSRA3)
                                                        + as.factor(Month)
                                                        + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                        + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                        + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                        + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                        + X2013 + X2014 + X2015))
summary(MSSP_OUT.lm.3m)


MSSP_OUT.lm.4m = with(MSSP_OUT[MSSP_OUT$NPA4 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow) ## all for group 9 OK?
                                                        +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4)
                                                        +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4)
                                                        + as.factor(Month)
                                                        + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                        + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                        + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                        + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                        + X2013 + X2014 + X2015))
summary(MSSP_OUT.lm.4m)


MSSP_OUT.lm.5m = with(MSSP_OUT[MSSP_OUT$NPA5 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                        +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5)
                                                        +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5)
                                                        + as.factor(Month)
                                                        + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                        + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                        + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                        + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                        + X2013 + X2014 + X2015))
summary(MSSP_OUT.lm.5m)


MSSP_OUT.lm.6m = with(MSSP_OUT[MSSP_OUT$NPA6 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                        +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                        +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                        + as.factor(Month)
                                                        + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                        + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                        + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                        + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                        + X2013 + X2014 + X2015))
summary(MSSP_OUT.lm.6m)


MSSP_OUT.lm.7m = with(MSSP_OUT[MSSP_OUT$NPA7 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                        +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                        +log(NPA7)
                                                        +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                        +log(CSRA7)
                                                        + as.factor(Month)
                                                        + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                        + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                        + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                        + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                        + X2013 + X2014 + X2015))
summary(MSSP_OUT.lm.7m)


MSSP_OUT.lm.8m = with(MSSP_OUT[MSSP_OUT$NPA8 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                        +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                        +log(NPA7) +log(NPA8)
                                                        +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                        +log(CSRA7) +log(CSRA8)
                                                        + as.factor(Month)
                                                        + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                        + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                        + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                        + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                        + X2013 + X2014 + X2015))
summary(MSSP_OUT.lm.8m)


MSSP_OUT.lm.9m = with(MSSP_OUT[MSSP_OUT$NPA9 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                        +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                        +log(NPA7) +log(NPA8) +log(NPA9)
                                                        +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                        +log(CSRA7) +log(CSRA8) +log(CSRA9)
                                                        + as.factor(Month)
                                                        + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                        + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                        + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                        + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                        + X2013 + X2014 + X2015))
summary(MSSP_OUT.lm.9m)


MSSP_OUT.lm.10m = with(MSSP_OUT[MSSP_OUT$NPA10 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                          +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                          +log(NPA7) +log(NPA8) +log(NPA9) +log(NPA10)
                                                          +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                          +log(CSRA7) +log(CSRA8) +log(CSRA9) +log(CSRA10)
                                                          + as.factor(Month)
                                                          + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                          + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                          + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                          + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                          + X2013 + X2014 + X2015))
summary(MSSP_OUT.lm.10m)



# MSSP_TH
MSSP_TH = read.csv("MSSP_TH.csv")

MSSP_TH.lm = with(MSSP_TH[MSSP_TH$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow) + as.factor(Month)
                                                      + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                      + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                      + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                      + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                      + X2013 + X2014 + X2015))
summary(MSSP_TH.lm)


MSSP_TH.lm.1m = with(MSSP_TH[MSSP_TH$NPA2 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1)
                                                     +log(CSRA1)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_TH.lm.1m)


MSSP_TH.lm.2m = with(MSSP_TH[MSSP_TH$NPA2 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2)
                                                     +log(CSRA1) +log(CSRA2) 
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_TH.lm.2m)


MSSP_TH.lm.3m = with(MSSP_TH[MSSP_TH$NPA3 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_TH.lm.3m)


MSSP_TH.lm.4m = with(MSSP_TH[MSSP_TH$NPA4 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_TH.lm.4m)


MSSP_TH.lm.5m = with(MSSP_TH[MSSP_TH$NPA5 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_TH.lm.5m)


MSSP_TH.lm.6m = with(MSSP_TH[MSSP_TH$NPA6 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_TH.lm.6m)


MSSP_TH.lm.7m = with(MSSP_TH[MSSP_TH$NPA7 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                     +log(NPA7)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                     +log(CSRA7)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_TH.lm.7m)


MSSP_TH.lm.8m = with(MSSP_TH[MSSP_TH$NPA8 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                     +log(NPA7) +log(NPA8)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                     +log(CSRA7) +log(CSRA8)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_TH.lm.8m)


MSSP_TH.lm.9m = with(MSSP_TH[MSSP_TH$NPA9 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                     +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                     +log(NPA7) +log(NPA8) +log(NPA9)
                                                     +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                     +log(CSRA7) +log(CSRA8) +log(CSRA9)
                                                     + as.factor(Month)
                                                     + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                     + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                     + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                     + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                     + X2013 + X2014 + X2015))
summary(MSSP_TH.lm.9m)


MSSP_TH.lm.10m = with(MSSP_TH[MSSP_TH$NPA10 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                       +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                       +log(NPA7) +log(NPA8) +log(NPA9) +log(NPA10)
                                                       +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                       +log(CSRA7) +log(CSRA8) +log(CSRA9) +log(CSRA10)
                                                       + as.factor(Month)
                                                       + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                       + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                       + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                       + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                       + X2013 + X2014 + X2015))
summary(MSSP_TH.lm.10m)


# OHIO_GRCH
OHIO_GRCH = read.csv("OHIO_GRCH.csv")

OHIO_GRCH.lm = with(OHIO_GRCH[OHIO_GRCH$NP_3mAvg != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow) + as.factor(Month)
                                                            + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                            + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                            + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                            + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                            + X2013 + X2014 + X2015))
summary(OHIO_GRCH.lm)


OHIO_GRCH.lm.1m = with(OHIO_GRCH[OHIO_GRCH$NPA1 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                           +log(NPA1)
                                                           +log(CSRA1)
                                                           + as.factor(Month)
                                                           + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                           + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                           + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                           + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                           + X2013 + X2014 + X2015))
summary(OHIO_GRCH.lm.1m)


OHIO_GRCH.lm.2m = with(OHIO_GRCH[OHIO_GRCH$NPA2 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                           +log(NPA1) +log(NPA2)
                                                           +log(CSRA1) +log(CSRA2) 
                                                           + as.factor(Month)
                                                           + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                           + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                           + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                           + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                           + X2013 + X2014 + X2015))
summary(OHIO_GRCH.lm.2m)


OHIO_GRCH.lm.3m = with(OHIO_GRCH[OHIO_GRCH$NPA3 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                           +log(NPA1) +log(NPA2) +log(NPA3)
                                                           +log(CSRA1) +log(CSRA2) +log(CSRA3)
                                                           + as.factor(Month)
                                                           + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                           + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                           + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                           + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                           + X2013 + X2014 + X2015))
summary(OHIO_GRCH.lm.3m)


OHIO_GRCH.lm.4m = with(OHIO_GRCH[OHIO_GRCH$NPA4 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                           +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4)
                                                           +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4)
                                                           + as.factor(Month)
                                                           + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                           + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                           + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                           + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                           + X2013 + X2014 + X2015))
summary(OHIO_GRCH.lm.4m)


OHIO_GRCH.lm.5m = with(OHIO_GRCH[OHIO_GRCH$NPA5 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                           +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5)
                                                           +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5)
                                                           + as.factor(Month)
                                                           + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                           + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                           + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                           + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                           + X2013 + X2014 + X2015))
summary(OHIO_GRCH.lm.5m)


OHIO_GRCH.lm.6m = with(OHIO_GRCH[OHIO_GRCH$NPA6 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                           +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                           +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                           + as.factor(Month)
                                                           + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                           + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                           + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                           + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                           + X2013 + X2014 + X2015))
summary(OHIO_GRCH.lm.6m)


OHIO_GRCH.lm.7m = with(OHIO_GRCH[OHIO_GRCH$NPA7 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                           +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                           +log(NPA7)
                                                           +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                           +log(CSRA7)
                                                           + as.factor(Month)
                                                           + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                           + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                           + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                           + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                           + X2013 + X2014 + X2015))
summary(OHIO_GRCH.lm.7m)


OHIO_GRCH.lm.8m = with(OHIO_GRCH[OHIO_GRCH$NPA8 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                           +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                           +log(NPA7) +log(NPA8)
                                                           +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                           +log(CSRA7) +log(CSRA8)
                                                           + as.factor(Month)
                                                           + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                           + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                           + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                           + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                           + X2013 + X2014 + X2015))
summary(OHIO_GRCH.lm.8m)


OHIO_GRCH.lm.9m = with(OHIO_GRCH[OHIO_GRCH$NPA9 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                           +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                           +log(NPA7) +log(NPA8) +log(NPA9)
                                                           +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                           +log(CSRA7) +log(CSRA8) +log(CSRA9)
                                                           + as.factor(Month)
                                                           + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                           + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                           + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                           + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                           + X2013 + X2014 + X2015))
summary(OHIO_GRCH.lm.9m)


OHIO_GRCH.lm.10m = with(OHIO_GRCH[OHIO_GRCH$NPA10 != 0,], lm(log(N_Conc) ~ log(NP_3mAvg) + log(CSRA) + log(Flow)
                                                             +log(NPA1) +log(NPA2) +log(NPA3) +log(NPA4) +log(NPA5) +log(NPA6)
                                                             +log(NPA7) +log(NPA8) +log(NPA9) +log(NPA10)
                                                             +log(CSRA1) +log(CSRA2) +log(CSRA3) +log(CSRA4) +log(CSRA5) +log(CSRA6)
                                                             +log(CSRA7) +log(CSRA8) +log(CSRA9) +log(CSRA10)
                                                             + as.factor(Month)
                                                             + X1977 + X1978 + X1979 + X1980 + X1981 + X1982 + X1983 + X1984 + X1985
                                                             + X1986 + X1987 + X1988 + X1989 + X1990 + X1991 + X1992 + X1993 + X1994
                                                             + X1995 + X1996 + X1997 + X1998 + X1999 + X2000 + X2001 + X2002 + X2003
                                                             + X2004 + X2005 + X2006 + X2007 + X2008 + X2009 + X2010 + X2011 + X2012
                                                             + X2013 + X2014 + X2015))
summary(OHIO_GRCH.lm.10m)


