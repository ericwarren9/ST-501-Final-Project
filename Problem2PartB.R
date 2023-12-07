set.seed(500)
means1 <- double(1000)

for (i in 1:1000) {
  data1 <- rpois(n = 5, lambda = 1)
  means1[i] <- mean(data1)
}

means2 <- double(1000)

for (i in 1:1000) {
  data2 <- rpois(n = 10, lambda = 1)
  means2[i] <- mean(data2)
}

means3 <- double(1000)

for (i in 1:1000) {
  data3 <- rpois(n = 100, lambda = 1)
  means3[i] <- mean(data3)
}

means4 <- double(1000)

for (i in 1:1000) {
  data4 <- rpois(n = 5, lambda = 5)
  means4[i] <- mean(data4)
}

means5 <- double(1000)

for (i in 1:1000) {
  data5 <- rpois(n = 10, lambda = 5)
  means5[i] <- mean(data5)
}

means6 <- double(1000)

for (i in 1:1000) {
  data6 <- rpois(n = 100, lambda = 5)
  means6[i] <- mean(data6)
}

means7 <- double(1000)

for (i in 1:1000) {
  data7 <- rpois(n = 5, lambda = 25)
  means7[i] <- mean(data7)
}

means8 <- double(1000)

for (i in 1:1000) {
  data8 <- rpois(n = 10, lambda = 25)
  means8[i] <- mean(data8)
}

means9 <- double(1000)

for (i in 1:1000) {
  data9 <- rpois(n = 100, lambda = 25)
  means9[i] <- mean(data9)
}

par(mfrow = c(3, 3))
Hist1 <- hist(means1, col = "Blue", main = "Lambda = 1 n = 5", xlab = "", xlim = c(min(means1),max(means1)))
breaks1 <- Hist1$breaks
curve(dnorm(x, mean = 1, sd = sqrt(1/5)) * length(means1)*diff(breaks1), add = TRUE, col = "red", lwd = 2)

Hist2 <- hist(means2, col = "Blue", main = "Lambda = 1 n = 10", xlab = "", ylab = "", xlim = c(min(means1),max(means1)))
breaks2 <- Hist2$breaks
curve(dnorm(x, mean = 1, sd = sqrt(1/10)) * length(means2)*diff(breaks2), add = TRUE, col = "red", lwd = 2)

Hist3 <- hist(means3, col = "Blue", main = "Lambda = 1 n = 100", xlab = "", ylab = "", xlim = c(min(means1),max(means1)))
breaks3 <- Hist3$breaks
curve(dnorm(x, mean = 1, sd = sqrt(1/100)) * length(means3)*diff(breaks3), add = TRUE, col = "red", lwd = 2)

Hist4 <- hist(means4, col = "Blue", main = "Lambda = 5 n = 5", xlab = "", xlim = c(min(means4),max(means4)))
breaks4 <- Hist4$breaks
curve(dnorm(x, mean = 5, sd = sqrt(5/5)) * length(means4)*diff(breaks4), add = TRUE, col = "red", lwd = 2)


Hist5 <- hist(means5, col = "Blue", main = "Lambda = 5 n = 10", xlab = "", ylab = "", xlim = c(min(means4),max(means4)))
breaks5 <- Hist5$breaks
curve(dnorm(x, mean = 5, sd = sqrt(5/10)) * length(means5)*diff(breaks5), add = TRUE, col = "red", lwd = 2)

Hist6 <- hist(means6, col = "Blue", main = "Lambda = 5 n = 100", xlab = "", ylab = "", xlim = c(min(means4),max(means4)))
breaks6 <- Hist6$breaks
curve(dnorm(x, mean = 5, sd = sqrt(5/100)) * length(means6)*diff(breaks6), add = TRUE, col = "red", lwd = 2)

Hist7 <- hist(means7, col = "Blue", main = "Lambda = 25 n = 5", xlim = c(min(means7),max(means7)))
breaks7 <- Hist7$breaks
curve(dnorm(x, mean = 25, sd = sqrt(25/5)) * length(means7) * diff(breaks7), add = TRUE, col = "red", lwd = 2)

Hist8 <- hist(means8, col = "Blue", main = "Lambda = 25 n = 10", ylab = "", xlim = c(min(means7),max(means7)))
breaks8 <- Hist8$breaks
curve(dnorm(x, mean = 25, sd = sqrt(25/10)) * length(means8) * diff(breaks8), add = TRUE, col = "red", lwd = 2)


Hist9 <- hist(means9, col = "Blue", main = "Lambda = 25 n = 100", ylab = "", xlim = c(min(means7),max(means7)))
breaks9 <- Hist9$breaks
curve(dnorm(x, mean = 25, sd = sqrt(25/100)) * length(means9) * .5, add = TRUE, col = "red", lwd = 2)

Prob1 <- sum(means1 >= 1 + 2*sqrt(1/5)) / length(means1)

Prob2 <- sum(means2 >= 1 + 2*sqrt(1/10)) / length(means2)

Prob3 <- sum(means3 >= 1 + 2*sqrt(1/100)) / length(means3)

Prob4 <- sum(means4 >= 5 + 2*sqrt(5/5)) / length(means4)

Prob5 <- sum(means5 >= 5 + 2*sqrt(5/10)) / length(means5)

Prob6 <- sum(means6 >= 5 + 2*sqrt(5/100)) / length(means6)

Prob7 <- sum(means7 >= 25 + 2*sqrt(25/5)) / length(means7)

Prob8 <- sum(means8 >= 25 + 2*sqrt(25/10)) / length(means8)

Prob9 <- sum(means9 >= 25 + 2*sqrt(25/100)) / length(means9)

## Normal approximation probability
normapprox <- 1 - pnorm(2,0,1)