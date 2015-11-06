#empirical code
fileNames = c("alcoa.csv", "bayer.csv", "boeing.csv", "f.csv", "gm.csv", 
              "hilton.csv", "hyatt.csv", "ibm.csv", "intc.csv", "kellog.csv", 
              "mariott.csv", "proctergamble.csv", "southwest.csv", "united.csv")
fileURLs = paste("http://www.stat.berkeley.edu/~nolan/data/stocks/", 
                 fileNames, sep = "")

stockList = lapply(fileURLs, readData)[c(4, 5, 8, 9)]
names(stockList) = c("Ford", "GM", "IBM", "Intel")

stockList1 = lapply(fileURLs, readData, dateFormat = "%Y-%m-%d")[c(1, 2, 3, 6, 7, 10, 11, 12, 13, 14)]
names(stockList1) = c("Alcoa", "Bayer", "Boeing", "Hilton", "Hyatt",
                      "Kellog", "Mariott", "ProcterGamble", 
                      "Southwest", "United")

#1 - IBM and Intel - Computer Hardware
IBMIntel = combine2stocks(stockList$IBM, stockList$Intel)
train.period1 = seq(min(IBMIntel$Date), by = "5 years", length =2)
trainingData1 = data.frame(Date = IBMIntel$Date, Adj.Close.A = IBMIntel$Adj.Close.A, 
                           Adj.Close.B = IBMIntel$Adj.Close.B, Ratio = IBMIntel$Ratio)[
                             as.numeric(IBMIntel$Date) %in% train.period1[1]:
                               train.period1[2], ]
k.1 = trainingK.max = max(abs(trainingData1$Ratio - mean(trainingData1$Ratio)) / sd(trainingData1$Ratio))
k.star1 = getBest.K(x = trainingData1$Adj.Close.A, y = trainingData1$Adj.Close.B, k.min = 1, 
                    k.max = k.1, numK = 100)
computerProfit = getProfit.K(x = IBMIntel$Adj.Close.A, y = IBMIntel$Adj.Close.B, k = k.star1)

plotRatio(IBMIntel$Ratio, k.star1)
title(main = "IBM and Intel - Computer Hardware")
#get rid of integer() in postions
positions1 = getPositions(IBMIntel$Ratio, startDay = 1, k.star1)[-3]
for (i in 1:length(positions1)){
  showPosition(pos = positions1[[i]], ratios = IBMIntel$Ratio[positions1[[i]]], radius = 40)
}

#2 - Southwest and United - Airlines
Airline = combine2stocks(stockList1$Southwest, stockList1$United)
train.period2 = seq(min(Airline$Date), by = "5 years", length =2)
trainingData2 = data.frame(Date = Airline$Date, Adj.Close.A = Airline$Adj.Close.A, 
                           Adj.Close.B = Airline$Adj.Close.B, Ratio = Airline$Ratio)[
                             as.numeric(Airline$Date) %in% train.period2[1]:
                               train.period2[2], ]

k.2 = trainingK.max = max(abs(trainingData2$Ratio - mean(trainingData2$Ratio)) / sd(trainingData2$Ratio))
k.star2 = getBest.K(x = trainingData2$Adj.Close.A, y = trainingData2$Adj.Close.B, k.min = 1, 
                    k.max = k.2, numK = 100)
airlineProfit = getProfit.K(x = Airline$Adj.Close.A, y = Airline$Adj.Close.B, k = k.star2)

plotRatio(Airline$Ratio, k = k.star2)
title(main = "Southwest and United - Airlines")
#get rid of integer() in postions
positions2 = getPositions(Airline$Ratio, startDay = 1, k = k.star2)[-3]
for (i in 1:length(positions2)){
  showPosition(pos = positions2[[i]], ratios = Airline$Ratio[positions2[[i]]], radius = 30)
}

#3 - Hilton and Hyatt - Hotels
HotelHH = combine2stocks(stockList1$Hilton, stockList1$Hyatt)
train.period3 = seq(min(HotelHH$Date), by = "5 years", length =2)
trainingData3 = data.frame(Date = HotelHH$Date, Adj.Close.A = HotelHH$Adj.Close.A, 
                           Adj.Close.B = HotelHH$Adj.Close.B, Ratio = HotelHH$Ratio)[
                             as.numeric(HotelHH$Date) %in% train.period3[1]:
                               train.period3[2], ]

k.3 = trainingK.max = max(abs(trainingData3$Ratio - mean(trainingData3$Ratio)) / sd(trainingData3$Ratio))
k.star3 = getBest.K(x = trainingData3$Adj.Close.A, y = trainingData3$Adj.Close.B, k.min = 1, 
                    k.max = k.3, numK = 100)
hotelHHProfit = getProfit.K(x = HotelHH$Adj.Close.A, y = HotelHH$Adj.Close.B, k = k.star3)

plotRatio(HotelHH$Ratio, k = k.star3)
title(main = "Hilton and Hyatt - Hotels")
positions3 = getPositions(HotelHH$Ratio, startDay = 1, k = k.star3)
for (i in 1:length(positions3)){
  showPosition(pos = positions3[[i]], ratios = HotelHH$Ratio[positions3[[i]]], radius = 4)
}

#4 - Ford and GM - Cars
Car = combine2stocks(stockList$Ford, stockList$GM)
train.period4 = seq(min(Car$Date), by = "5 years", length =2)
trainingData4 = data.frame(Date = Car$Date, Adj.Close.A = Car$Adj.Close.A, 
                           Adj.Close.B = Car$Adj.Close.B, Ratio = Car$Ratio)[
                             as.numeric(Car$Date) %in% train.period4[1]:
                               train.period4[2], ]

k.4 = trainingK.max = max(abs(trainingData4$Ratio - mean(trainingData4$Ratio)) / sd(trainingData4$Ratio))
k.star4 = getBest.K(x = trainingData4$Adj.Close.A, y = trainingData4$Adj.Close.B, k.min = 1, 
                    k.max = k.4, numK = 100)
carProfit = getProfit.K(x = Car$Adj.Close.A, y = Car$Adj.Close.B, k = k.star4)

plotRatio(Car$Ratio, k = k.star4)
title(main = "Ford and GM - Cars")
#get rid of integer() in postions
positions4 = getPositions(Car$Ratio, startDay = 1, k = k.star4)[-19]
for (i in 1:length(positions4)){
  showPosition(pos = positions4[[i]], ratios = Car$Ratio[positions4[[i]]], radius = 10)
}

#5 - Mariott and Hyatt - Hotels2
HotelMHy = combine2stocks(stockList1$Mariott, stockList1$Hyatt)
train.period5 = seq(min(HotelMHy$Date), by = "5 years", length = 2)
trainingData5 = data.frame(Date = HotelMHy$Date, Adj.Close.A = HotelMHy$Adj.Close.A, 
                           Adj.Close.B = HotelMHy$Adj.Close.B, Ratio = HotelMHy$Ratio)[
                             as.numeric(HotelMHy$Date) %in% train.period5[1]:
                               train.period5[2], ]

k.5 = trainingK.max = max(abs(trainingData5$Ratio - mean(trainingData5$Ratio)) / sd(trainingData5$Ratio))
k.star5 = getBest.K(x = trainingData5$Adj.Close.A, y = trainingData5$Adj.Close.B, k.min = 1, 
                    k.max = k.5, numK = 100)
hotelMHyProfit = getProfit.K(x = HotelMHy$Adj.Close.A, y = HotelMHy$Adj.Close.B, k = k.star5)

plotRatio(HotelMHy$Ratio, k = k.star5)
title(main = "Mariott and Hyatt - Hotels2")
positions5 = getPositions(HotelMHy$Ratio, startDay = 1, k = k.star5)
for (i in 1:length(positions5)){
  showPosition(pos = positions5[[i]], ratios = HotelMHy$Ratio[positions5[[i]]], radius = 15)
}

#6 - Alcoa and Boeing - Industry/Metals
Industry = combine2stocks(stockList1$Alcoa, stockList1$Boeing)
train.period6 = seq(min(Industry$Date), by = "5 years", length = 2)
trainingData6 = data.frame(Date = Industry$Date, Adj.Close.A = Industry$Adj.Close.A, 
                           Adj.Close.B = Industry$Adj.Close.B, Ratio = Industry$Ratio)[
                             as.numeric(Industry$Date) %in% train.period6[1]:
                               train.period6[2], ]

k.6 = trainingK.max = max(abs(trainingData6$Ratio - mean(trainingData6$Ratio)) / sd(trainingData6$Ratio))
k.star6 = getBest.K(x = trainingData6$Adj.Close.A, y = trainingData6$Adj.Close.B, k.min = 1, 
                    k.max = k.6, numK = 100)
industryProfit = getProfit.K(x = Industry$Adj.Close.A, y = Industry$Adj.Close.B, k = k.star6)

plotRatio(Industry$Ratio, k = k.star6)
title(main = "Alcoa and Boeing - Industry/Metals")
positions6 = getPositions(Industry$Ratio, startDay = 1, k = k.star6)[-3]
for (i in 1:length(positions6)){
  showPosition(pos = positions6[[i]], ratios = Industry$Ratio[positions6[[i]]], radius = 125)
}




#simulation code
rhos = seq(.95, .99, by = .01)
psis = seq(0, .8, by = .2)
beta1.1 = c(0, .05)
beta1.2 = c(0, .05)
sigmas = c(0.5, 1, 1.5)

simResultsRef = expand.grid(sigmas, beta1.2, beta1.1, psis, rhos)
simResultsRef$distribution = 1:300

distributionList = list()
for (i in 1:length(rhos)) {
  for (j in 1:length(psis)) {
    for (k in 1:length(beta1.1)) {
      for (l in 1:length(beta1.2)) {
        for (m in 1:length(sigmas)) {
          dist = simProfitDist(rho = rhos[i], psi = psis[j],beta0 = c(100, 100), 
                               beta1 = c(beta1.1[k], beta1.2[l]), sigma = c(sigmas[m], sigmas[m]), 
                               n = 4000)
          distributionList = c(distributionList, list(dist))
        }
      }
    }
  }
}

Rho95 = distributionList[1:60]
mean95 = sapply(Rho95, mean)
mean_100_95 = mean(mean95)

Rho96 = distributionList[61:120]
mean96 = sapply(Rho96, mean)
mean_100_96 = mean(mean96)

Rho97 = distributionList[121:180]
mean97 = sapply(Rho97, mean)
mean_100_97 = mean(mean97)

Rho98 = distributionList[181:240]
mean98 = sapply(Rho98, mean)
mean_100_98 = mean(mean98)

Rho99 = distributionList[241:300]
mean99 = sapply(Rho99, mean)
mean_100_99 = mean(mean99)

save(simResultsRef, distributionList, 
     Rho95, Rho96, Rho97, Rho98, Rho99,
     mean95, mean96, mean97, mean98, mean99,
     mean_100_95, mean_100_96, mean_100_97, mean_100_98, mean_100_99,
     plotRatio, showPosition,
     Airline, HotelHH, Car, Industry, 
     positions2, positions3, positions4, positions6,
     k.star2, k.star3, k.star4, k.star6,
     airlineProfit, hotelHHProfit, carProfit, industryProfit,
     file = "simResultsAll.rda")
