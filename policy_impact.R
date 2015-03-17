setwd("J:/AED Research/Mississipi/allsites_post11_16/12_8 MSSP_CL")

total_4m = read.csv("Total_4months.csv")

attach(total_4m)

Price_m = data.matrix(price)
Flow_m = data.matrix(flow)
Precp_m = data.matrix(precipitation)
CP_m = data.matrix(corn_price)

a0 <- matrix(-7.1321, ncol = 1, nrow = 87)
a1 <- matrix(-0.5324, ncol = 1, nrow = 87)
a2 <- matrix(1.09, ncol = 1, nrow = 87)
a3 <- matrix(-0.3263, ncol = 1, nrow = 87)
a4 <- matrix(0.2863, ncol = 1, nrow = 87)

as.matrix(Y)

Y = a0 + a1 %*% Price_m + a2 %*% Flow_m + a3 %*% Precp_m + a4 %*% CP_m
Y = a0 + (-0.5324) * log(Price_m) + 1.09 * log(Flow_m) + (-0.3263) * log(Precp_m) + 0.2863 * log(CP_m)
conc = exp(Y)

Y2 = a0 + (-0.5324) * log(Price_m*1.25) + 1.09 * log(Flow_m) + (-0.3263) * log(Precp_m) + 0.2863 * log(CP_m)
conc2 = exp(Y2)
diff = conc2-conc

percent = diff/N
plot(percent)
mean(percent)


Y3 = a0 + (-0.5324) * log(Price_m*1.15) + 1.09 * log(Flow_m) + (-0.3263) * log(Precp_m) + 0.2863 * log(CP_m)
conc3 = exp(Y3)
diff2 = conc3-conc

percent2 = diff2/N
plot(percent2)
mean(percent2)
