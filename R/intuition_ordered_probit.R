# a) probability density function ####
x <- seq(from = -1, to = 5, by = 0.001)

mu_P <- 2.25
sd_P <- 0.5

P_dens <- dnorm(x, mean = mu_P, sd = sd_P)


pink_transp <- rgb(255, 20, 147,
                   max = 255,
                   alpha = 25,
                   names = "deeppink1")

pink_transp_completely <- rgb(255, 20, 147,
                              max = 255,
                              alpha = 0,
                              names = "deeppink1")

plot(x = x,
     y = P_dens,
     type="l",
     col = pink_transp_completely,
     main = "",
     xlab = "",
     ylab = "")

grid()
par(new = TRUE)

plot(x = x,
     y = P_dens,
     type="l",
     col = pink_transp_completely,
     main = paste("a) Normal probability density function with mu =",
                  mu_P,
                  "and",
                  "sigma =",
                  sd_P),
     xlab = "performance",
     ylab = "density")

polygon(x = x,
        y = P_dens,
        col = pink_transp,
        border = FALSE)

abline(v = mu_P, col = "deeppink1")

text(x = mu_P-0.1,
     y = 0.15,
     labels = "mu",
     srt = 90)

for (j in 1:5) {
  
  abline(v = j-1, col = "deeppink4") 
  
  text(x = j-1.1,
       y = 0.15,
       labels = paste("cut point", j),
       srt = 90)
  
}

for (j in 1:6) {
  
  text(x = 5 + 0.5 - j,
       y = 0.5,
       labels = paste("rank", j))
  
}

box()



# b) cumulative distribution function ####
x_2 <- seq(from = -3, to = 3, by = 0.001)

P_prob <- pnorm(x_2, mean = 0, sd = 1)

plot(x = x_2,
     y = P_prob,
     type="l",
     col = "deeppink1",
     main = "",
     xlab = "",
     ylab = "")

grid()
par(new = TRUE)

plot(x = x_2,
     y = P_prob,
     type="l",
     col = "deeppink1",
     main = "b) cumulative distribution function of the standard Normal",
     xlab = "performance",
     ylab = "probability")

abline(v = 0, col = "deeppink1")

text(x = -0.1,
     y = 0.55,
     labels = "mu",
     srt = 90)

for (j in 1:5) {
  
  abline(v = j - 1 - mu_P, col = "deeppink4") 
  
  text(x = j - 1.1 - mu_P,
       y = 0.565,
       labels = paste("cut point", j),
       srt = 90)
  
}

for (j in 1:6) {
  
  text(x = 5 + 0.5 - j - mu_P,
       y = 0.1,
       labels = paste("rank", j))
  
}

box()


