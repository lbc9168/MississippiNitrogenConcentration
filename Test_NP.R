setwd("C:/Users/Bingcai/OSU/AED Research/Mississipi/2015 Summer_All sites/Nitrogen Price data")

NP = read.csv("NP_csv.csv")
dc_date_sq = with(NP, dc_date^2)
dc_date_qt = with(NP, dc_date^3)
NP.1 = with(NP, lm(Price_Modi~dc_date))
NP.2 = with(NP, lm(Price_Modi~ dc_date_sq + dc_date))
NP.3 = with(NP, lm(Price_Modi~ dc_date_qt + dc_date_sq + dc_date))
summary(NP.1)
summary(NP.2)
summary(NP.3)

with(NP, plot(Period, Price_Modi))
