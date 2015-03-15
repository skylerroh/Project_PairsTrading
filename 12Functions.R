readData = function(fileName, dateFormat = "%m/%d/%y"){
  stockData = read.csv(fileName, stringsAsFactors = FALSE)
  stockData$Date = as.Date(stockData$Date, dateFormat)
  stockData = stockData[order(stockData$Date), ]
  return(stockData[ , c("Date", "Adj.Close") ])
}

combine2stocks = function(stockA, stockB){
  stockA = stockA[stockA$Date %in% stockB$Date, ]
  stockB = stockB[stockB$Date %in% stockA$Date, ]
  stockAB = data.frame(Date = stockA$Date, 
                       Adj.Close.A = stockA$Adj.Close, 
                       Adj.Close.B = stockB$Adj.Close, 
                       Ratio = stockA$Adj.Close / stockB$Adj.Close)
  return (stockAB)
}

plotRatio = function(ratio, k = 1, date = seq(along = ratio), ...){
  plot(ratio ~ date, type = "l", xlab = "Date", ylab = "Ratio")
  abline(h = mean(ratio), lty = "dashed", col = "green")
  abline(h = mean(ratio) - (c(-k, k) * sd(ratio)), lty = "dashed", col = "red")
}

showPosition = function(pos, ratios, col = c("green", "red"), radius = 100){
  symbols(x = pos, y = ratios, circles = rep(radius, times = length(pos)), 
          fg = col, inches = FALSE, add = TRUE)
}

findNextPosition = function(ratio, startDay, k = 1, m = mean(ratio), s = sd(ratio)){
  openPosition = c()
  closePosition = c()
  possibleOpen = which(abs(ratio - m) >= k * s)
  if (length(possibleOpen[possibleOpen >= startDay]) == 0) {
    return (integer())
  } else {
    openPosition = possibleOpen[possibleOpen >= startDay][1]
  }
  if (ratio[openPosition] >= m + k * s) {
    possibleClose = which(ratio <= m)
    if (length(possibleClose[possibleClose > openPosition]) == 0) {
      closePosition = length(ratio)
    } else {
      closePosition = possibleClose[possibleClose > openPosition][1]
    }
  }
  if (ratio[openPosition] <= m - k * s) {
    possibleClose = which(ratio >= m)
    if (length(possibleClose[possibleClose > openPosition]) == 0) {
      closePosition = length(ratio)
    } else {
      closePosition = possibleClose[possibleClose > openPosition][1]
    }
  }
  return (nextPosition = c(openPosition, closePosition))
}

getPositions = function(ratio, startDay, k = 1, m = mean(ratio), s = sd(ratio)){
  x = findNextPosition(ratio, startDay, k, m, s)
  positions = list(x)
  while (x[2] != length(ratio) & !is.na(x[2])){
    x = findNextPosition(ratio, startDay = x[2], k, m, s)
    positions = c(positions, list(x))
  }
  return (positions)
}

positionProfit = function(pos, stockPriceA, stockPriceB, 
                          m = mean(stockPriceA / stockPriceB)){
  unitsA = 1 / stockPriceA[pos[1]]
  unitsB = 1 / stockPriceB[pos[1]]
  if (stockPriceA[pos[1]] / stockPriceB[pos[1]] > m){
    profit = (1 - stockPriceA[pos[2]] * unitsA) + 
      (stockPriceB[pos[2]] * unitsB - 1)
  } else {
    profit = (1 - stockPriceB[pos[2]] * unitsB) + 
      (stockPriceA[pos[2]] * unitsA - 1)
  }
  return (profit)
}

getProfit.K = function(x, y, k, m = mean(x/y), s = sd(x/y)){
  positions = getPositions(ratio = x / y, startDay = 1, k, m, s)
  profit.K = 0
  for (i in 1:length(positions)){
    if (!is.na(positions[[i]][2])){
      profit.i = positionProfit(pos = positions[[i]], stockPriceA = x, 
                                stockPriceB = y, m = m)
      profit.K = profit.K + profit.i
    }
  }
  return (profit.K)
}

getBest.K = function(x, y, k.min, k.max, numK, m = mean(x/y), s = sd(x/y)){
  k.sequence = seq(from = k.min, to = k.max, length = numK)
  k.profits = sapply(k.sequence, FUN = getProfit.K, x = x, y = y)
  k.star = k.sequence[order(k.profits, decreasing = TRUE)[1]]
  return(k.star)
}

set.seed(100)
stockSim = function(n = 4000, rho = 0.99, psi = 0, sigma = rep(1, 2),
                    beta0 = rep(100, 2), beta1 = rep(0, 2),
                    epsilon = matrix(rnorm(2*n, sd = sigma),
                                     nrow = n, byrow = TRUE)){
  x1 = numeric(n)
  x2 = numeric(n)
  x1[1] = 0 + epsilon[1,1]
  x2[1] = 0 + epsilon[1,2]
  for (i in 2:n) {
    x1[i] = rho * x1[i - 1] + psi * (1 - rho) * x2[i - 1] + epsilon[i, 1]
    x2[i] = rho * x2[i - 1] + psi * (1 - rho) * x1[i - 1] + epsilon[i, 2]
  }
  
  y1 = numeric(n)
  y1 = beta0 + beta1 * (1:n) + x1
  
  y2 = numeric(n)
  y2 = beta0 + beta1 * (1:n) + x2
  
  return (data.frame(y1, y2))
}

runSim = function(rho, psi, beta0 = c(100, 100), beta1 = c(0, 0),
                  sigma = c(1, 1), n = 4000){
  AB = stockSim(n = n, rho = rho, psi = psi, sigma = sigma, 
                beta0 = beta0, beta1 = beta1, 
                epsilon = matrix(rnorm(2*n, sd = sigma), 
                                 nrow = n, byrow = TRUE))
  stockA = data.frame(Date = 1:n, Adj.Close = AB[[1]])
  stockB = data.frame(Date = 1:n, Adj.Close = AB[[2]])
  
  stockAB = combine2stocks(stockA, stockB)
  train.period = c(1, floor(n/2))
  trainingData = data.frame(Date = stockAB$Date, Adj.Close.A = stockAB$Adj.Close.A, 
                            Adj.Close.B = stockAB$Adj.Close.B, Ratio = stockAB$Ratio)[
                              train.period[1]:train.period[2], ]
  trainingK.max = max(abs(trainingData$Ratio - mean(trainingData$Ratio)) / sd(trainingData$Ratio))
  
  k.star = getBest.K(x = trainingData$Adj.Close.A, y = trainingData$Adj.Close.B, k.min = 0.1, 
                     k.max = trainingK.max, numK = 100)
  
  test.period = c(train.period[2] + 1, n)
  testData = data.frame(Date = stockAB$Date, Adj.Close.A = stockAB$Adj.Close.A, 
                        Adj.Close.B = stockAB$Adj.Close.B, Ratio = stockAB$Ratio)[
                          test.period[1]:test.period[2], ]
  profit = getProfit.K(x = testData$Adj.Close.A, y = testData$Adj.Close.B, k = k.star)
  return(profit)
}

simProfitDist = function(..., B = 100)
  sapply(1:B, function(i, ...) runSim(...), ...)


