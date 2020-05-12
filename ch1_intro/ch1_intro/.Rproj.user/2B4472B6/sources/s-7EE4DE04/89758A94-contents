set.seed(3)
N   <- 108
n   <- 18
X <- runif(N, 0, 2)
Y <- runif(N)

these <- sample(N, n)

aas <- ggplot() + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) + 
  geom_point(aes(x = X, y = Y), color = 'skyblue3') + 
  geom_point(aes(x = X[these], y = Y[these]), color = 'red', shape = 21, size = 3) + 
  geom_point(aes(x = X[these], y = Y[these]), color = 'red') + 
  labs(x = NULL, y = NULL) + 
  theme(axis.text = element_blank(), axis.ticks = element_blank())

#---------------------------------- estratificado ----------------------------------#

set.seed(28)
samp <- matrix(NA, ncol = 12, nrow = 25)
minus <- c(1, 1, 8, 15, 8, 17, # x
           3, 17, 3, 3, 17, 17) # y

max <- c(3, 3, 12, 20, 12, 20, # x
         6, 20, 6, 6, 20, 20) # y


for(i in 1:12){
  samp[,i] <- runif(25, minus[i], max[i])
}

x_samp <- c(samp[,1:6])
y_samp <- c(samp[,7:12])

these2 <- c(sample(1:25, 8), sample(26:50, 8), sample(51:75, 8),
            sample(76:100, 8), sample(101:125, 8), sample(126:150, 8))

strat <- ggplot() + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) + 
  labs(x = NULL, y = NULL) + 
  theme(axis.text = element_blank(), axis.ticks = element_blank()) + 
  xlim(0,22) + ylim(0,25) + 
  geom_point(aes(x = x_samp, y = y_samp), color = 'skyblue3') + 
  geom_point(aes(x=x_samp[these2], y=y_samp[these2]), color='red', shape=21, size=3) +
  geom_point(aes(x = x_samp[these2], y = y_samp[these2]), color = 'red')


require(gridExtra)

grid.arrange(aas, strat, ncol = 1)