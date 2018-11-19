library(ggplot2)
library(lattice)
library(rgl)
library(bbmle)

# Modelo autoregressivo de primeira ordem ---------------------------------

# Descrição do modelo:

# Os processos autorregressivos podem ser utilizados como modelos se for razoável
# assumir que o valor atual de uma série temporal depende do seu passado imediato 
# mais um erro aleatório.

# Observações correlacionadas, ou seja, existe dependencia linear entre as 
# variaveis, no caso os dados.

# Assuminto que o processo possui média zero:

# y_y+1 = p * y_t + e_t+1 onde e_t é chamado processo e tem distribuição normal 
# com média e variância sigma^2. 0 < p < 1.


# Simulação de dados para o modelo ----------------------------------------


# Fixando a primeira observação como zero e os parametros serão: 
# rho = 0.7, sigma^2 = 1


rho <- 0.7 
n <- 100
dados <- numeric(n) 
for(i in 2:n){
  dados[i] <- rho * dados[i - 1] + rnorm(1)}


plot(dados, type = "l")
abline(h = mean(dados))

plot(dados)


# Para expressão fechada retirando a primeira observação ------------------



## Um parâmetro -----------------------------------------------------------



### Função ----------------------------------------------------------------



loglikeli1 <- function(par){
  n <- length(dados)
  out <- prod(dnorm(dados[2:n], mean = par*dados[1:(n-1)], sd = 1))
  return(-out)
}

### Vetorizando a Função --------------------------------------------------

verll1 <- Vectorize(loglikeli1)


### Gráfico ---------------------------------------------------------------


curve(from = 0, to = 1, expr = verll1)
abline(v = 0.7)


### EMV -------------------------------------------------------------------

ll1 <- optimize(loglikeli1, c(0,1), maximum = TRUE)
emv1 <- ll1$maximum


## Para dois parâmetros ---------------------------------------------------


### Função ----------------------------------------------------------------

loglikeli2 <- function(par){
  n <- length(dados)
  out <- sum(dnorm(dados[2:n], mean = par[1]*dados[1:(n-1)], sd = par[2], log=TRUE))
  return(out)
}

loglikeli3 <- function(mu, tau, dados){
  n <- length(dados)
  out <- sum(dnorm(dados[2:n], mean = mu*dados[1:(n-1)], sd = tau, log=TRUE))
  return(-out)
}



### Vetorizando a função --------------------------------------------------

verll2 <- Vectorize(loglikeli3, vectorize.args = c("mu", "tau"))


### Gráfico ---------------------------------------------------------------

### EMV -------------------------------------------------------------------

ll2 <- optim(c(0.2, 1.5), fn = loglikeli3, hessian = TRUE) 
emv2 <- ll2$par
emv2

ll2$hessian

# Expressão com a primeira observação -------------------------------------



## Função -----------------------------------------------------------------

loglikeli4 <- function(rho, sigma, data){
  out <- dnorm(data[1], mean = 0, sd = (1/sqrt(1-(rho^2))), log = TRUE) +
    sum(dnorm(data[2:n], mean = rho*data[1:(n-1)], sd = sigma, log = TRUE))
  return(-out)
}


## Vetorizando a função ---------------------------------------------------

verll3 <- Vectorize(loglikeli4)


## Gráfico ----------------------------------------------------------------

persp3d()

## EMV --------------------------------------------------------------------

ll3 <- optim(c(0.3, 1.5), fn = loglikeli4, hessian = TRUE)
emv3 <- ll3$par
ll3  

plot(dados, main)

fe <- mle2(loglikeli3, list(mu = 0.3, tau = 1.5), data = list(dados = dados),
     lower = list(mu = -1, tau = 0),
     upper = list(mu = 1, tau = Inf),
     method="L-BFGS-B")

confint(fe, methot = "quad")


# Google stocks -----------------------------------------------------------

dados <- read.table("google.csv", header = TRUE,
                    sep = ",", dec = ".")
dados

plot(dados, type = "l")

loglikeli3 <- function(rho, sigma, data){
  n <- length(data)
  out <- sum(dnorm(data[2:n], mean = rho*data[1:(n-1)], sd = sigma, log=TRUE))
  return(-out)
}

library(bbmle)
library(ggplot2)


fe <- mle2(loglikeli3, list(rho = -8.7, sigma = 3.5), data = list(data = dados$price),
           lower = list(rho = -1, sigma = 0),
           upper = list(rho = 2, sigma = Inf),
           method="L-BFGS-B")
fe

ggplot(data = dados, aes(y = price, x = date, group = 1)) + geom_point(colour = "darkblue")+ 
  geom_line(colour = "blue") + theme_bw() 

confint(fe, method = "quad")

#Tentativa de simulação do caso anterior


rho <- 1 
n <- 100005
dados1 <- numeric(n) 
for(i in 2:n){
  dados1[i] <- rho * dados1[i - 1] + rnorm(1, 0, sd = 4.41)}

plot(dados1, type = "l")

fe1 <- mle2(loglikeli3, list(rho = 0.3, sigma = 1.5), data = list(data = dados1),
           lower = list(rho = -1, sigma = 0),
           upper = list(rho = 1, sigma = Inf),
           method="L-BFGS-B")
fe1



# Rmarkdown ---------------------------------------------------------------

# Dados simulados 

Vamos simular 500 observações que possuem um comportamento explicado pelo modelo AR(1). Os dados foram simulados no software R e foi utilizada a função set.seed para que se possa gerar uma amostra identica a essa em outras maquinas. Primeiramente atribuimos o valor de 0.4 ao parametro \(\rho\) e determinamos o ruído branco com uma distribuição normal com média zero e variância um. Após isso, utilizamos o loop for para condicionar os dados e assim gerar a amostra para o modelo.




```{r}
# Simulação dos dados

set.seed(209)
rho <- 0.4 
n <- 500
dados <- numeric(n) 
for(i in 2:n){
  dados[i] <- rho * dados[i - 1] + rnorm(1)}

```

## Descrição dos dados

```{r}
# Estrutura
str(dados)

# Estatísticas
summary(dados)
```

```{r}
# Gráfico

plot(dados ~ c(1:length(dados)), type = "l", ylab = "Observações", xlab = "Tempo", main  = "Observações simuladas", col = "dark blue")

```

Podemos agora visualizar o gráfico de correlação entre as observações, para podermos verificar se de fato os dados podem ser modelados por um modelo autorregressivo de primeira ordem. 

Para construir o gráfico abaixo foram utilizadas informações da função de autocorrelação apresentada na seção 2.

```{r}

```

loglikeli3

dados

emv <- mle2(loglikeli3, )

library(bbmle)

fe1 <- mle2(loglikeli3, list(rho = 0.3, sigma = 1.5), data = list(data = dados$trasf),
            lower = list(rho = -1, sigma = 0),
            upper = list(rho = 1, sigma = Inf),
            method="Nelder-Mead")


ggplot(data = dados, aes(y = trasf, x = date, group = 1)) + geom_point(colour = "darkblue")+ 
  geom_line(colour = "blue") + theme_bw() 

ggplot(data = dados, aes(y = price, x = date, group = 1)) + geom_point(colour = "darkblue")+ 
  geom_line(colour = "blue") + theme_bw() 
