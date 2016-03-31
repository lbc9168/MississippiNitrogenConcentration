TR = read.csv("1_Tennessee River.csv")
with(TR, plot(time,CONCENTRATION))
with(TR, matlines(time,CONCENTRATION))

WR = read.csv("2_Wabash River.csv")
with(WR, plot(time,CONCENTRATION))
with(WR, matlines(time,CONCENTRATION))

ORS = read.csv("5n8_Ohio River Source.csv")
with(ORS, plot(time,CONCENTRATION))
with(ORS, matlines(time,CONCENTRATION))

OHCD = read.csv("Ohio River at Cannelton Dam_IN.csv")
with(OHCD, plot(time,CONCENTRATION))
with(OHCD, matlines(time,CONCENTRATION))
