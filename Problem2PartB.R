set.seed(500)
means1 <- double(1000)

for (i in 1:1000) {
  data1 <- rpois(n = 10, lambda = 1)
  means1[i] <- mean(data1)
}

means2 <- double(1000)

for (i in 1:1000) {
  data2 <- rpois(n = 100, lambda = 1)
  means2[i] <- mean(data2)
}

means3 <- double(1000)

for (i in 1:1000) {
  data3 <- rpois(n = 5, lambda = 5)
  means3[i] <- mean(data3)
}

means4 <- double(1000)

for (i in 1:1000) {
  data4 <- rpois(n = 10, lambda = 5)
  means4[i] <- mean(data4)
}

means5 <- double(1000)

for (i in 1:1000) {
  data5 <- rpois(n = 100, lambda = 5)
  means5[i] <- mean(data5)
}

means6 <- double(1000)

for (i in 1:1000) {
  data6 <- rpois(n = 5, lambda = 25)
  means6[i] <- mean(data6)
}

means7 <- double(1000)

for (i in 1:1000) {
  data7 <- rpois(n = 10, lambda = 25)
  means7[i] <- mean(data7)
}

means8 <- double(1000)

for (i in 1:1000) {
  data8 <- rpois(n = 100, lambda = 25)
  means8[i] <- mean(data8)
}

means9 <- double(1000)

for (i in 1:1000) {
  data9 <- rpois(n = 1000, lambda = 25)
  means9[i] <- mean(data9)
}

Hist1 <- hist(means1, breaks = seq(0,2.2, by = .2), col = "Blue")
curve(rnorm(data1, mean = means1, sd = 1))

Hist2 <- hist(means2, breaks = seq(0,2.2, by = .2), col = "Blue")

Hist3 <- hist(means3, breaks = seq(0,2.2, by = .2), col = "Blue")

Hist4 <- hist(means4, breaks = seq(0, 10, by = 1), col = "Blue")

Hist5 <- hist(means5, breaks = seq(0, 10, by = 1), col = "Blue")

Hist6 <- hist(means6, breaks = seq(0, 10, by = 1), col = "Blue")

Hist7 <- hist(means7, breaks = seq(0, 50, by = 5), col = "Blue")

Hist8 <- hist(means8, breaks = seq(0, 50, by = 5), col = "Blue")

Hist9 <- hist(means9, breaks = seq(0, 50, by = 5), col = "Blue")
