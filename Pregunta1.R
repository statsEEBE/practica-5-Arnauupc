# distribucio poblacional 

mu <- 95.3
sigma <- 5.7



#N distibuieix amb N(mu,sigma^2)
#pintem la fincio

curve(dnorm(x, mean=mu, sd=sigma), xlim=c(80,120),col='red')


#mirar on trenca un cable random
rnorm(1,mean=mu,sd=sigma) #aixo es simulacio, pq tots en dongui el mateix valor hem de fer set.seed
rnorm(4, mean=mu, sd=sigma)


#em pregunten per E(Y)

#convertir en una variable aleatoria

X<- function(i){rnorm(4, mean=mu, sd=sigma)}
Y<- function(i){sum(rnorm(4, mean=mu, sd=sigma))}

Y(1)

mean(sapply(1:5,Y))

#a)
Y100000 <- sapply(1:100000, Y)
mean(Y100000)

hist(Y100000)

hist(Y100000, freq=FALSE)
curve(dnorm(x, 4*mu,2*sigma), col="red", add=TRUE)
4*mu #aixo es la pregunta 1

# la b seria 
Y<- function(i){sum(rnorm(100, mean=mu, sd=sigma))}
Y100000 <- sapply(1:100000, Y)
var(Y100000)
#resposta
100*sigma^2

#si es selecciona de forma random quina es la pobabilitat de que la resistencia de calbe no sigui menor a 103
curve(dnorm(x, mean=mu, sd=sigma), xlim=c(80,120),col='red')

1-pnorm(103, mu, sigma)

#d)
Xbar <- function(i){mean(rnorm(4, mean=mu, sd=sigma))}
Xbar100000 <- sapply(1:100000, Xbar)
hist(Xbar100000)
mean(Xbar100000<98)#quqn repeteixo l'experimen dels 4 cables infinites vegadez
hist(Xbar100000, freq=FALSE)
curve(dnorm(x, mu,sigma/sqrt(4)), add=TRUE, col="red")
pnorm(98,mu,sigma/sqrt(4))





#e
Ssq <- function(i){var(rnorm(100, mean=mu, sd=sigma))}
Ssq100000 <- sapply(1:100000,Ssq)
hist(Ssq100000, freq=FALSE)
mean(Ssq100000>32)
hist(Ssq100000*(100-1)/sigma^2, freq=FALSE)
curve(dchisq(x,100-1),add=TRUE, col="red")

w<- 32*(100-1)/sigma^2
w
1-pchisq(w, 100-1) #aixo seria el valor teoric
mean(Ssq100000>32)#aixo el valor amb l'estimacio numerica
