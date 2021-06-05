###################################################
###      Introdução à Estatística com o R       ###
###              Filipe J. Zabala               ###
###        www.estatisticaclassica.com          ###
###           filipe.zabala@pucrs.br            ###
###          2019-04-27 e 2019-06-15            ###
###  Attribution 4.0 International (CC BY 4.0)  ###
### https://creativecommons.org/licenses/by/4.0 ###
###################################################

### Referências
# http://r4ds.had.co.nz/
# http://www.estatisticaclassica.com
# http://www.burns-stat.com/pages/Tutor/R_inferno.pdf
# https://www.rstudio.com/wp-content/uploads/2016/01/rstudio-IDE-cheatsheet.pdf
# https://www.rstudio.com/wp-content/uploads/2016/10/r-cheat-sheet-3.pdf
# https://cran.r-project.org/web/packages/markovchain/vignettes/an_introduction_to_markovchain_package.pdf
# https://cran.r-project.org/web/packages/markovchain/vignettes/markovchainCrashIntro.pdf


### Links para a instalação do R e do RStudio
# https://cloud.r-project.org/
# https://www.rstudio.com/products/rstudio/download/preview/


### Tópicos
# 0 Pacotes
# 1 Funções básicas do R
# 2 Criando e manipulando funções
# 3 Manipulando dados com dplyr and tidyr
# 4 Estatística	descritiva, visualização e séries temporais
# 5 Probabilidade
# 6 Cadeia de Markov  <-- 
# 7 Simulação e inferência
# 8 Tópicos em Modelos Lineares Generalizados
# 9 Aprendizado de máquina


###########################
### 6 Cadeias de Markov ###
###########################


# Exemplo 1. Número de clientes
(v0 <- c(324,233,210))
(P <- matrix(c(.7,.2,.1, .1,.6,.3, 0,.1,.9),
             nrow = 3, byrow = TRUE))
rownames(P) <- colnames(P) <- LETTERS[1:nrow(P)]
P
(v1 <- t(v0)%*%P)
(v2 <- v1%*%P)
(P2 <- P%*%P)
t(v0)%*%P2

# Exemplo 2. A short example

# cadeia irredutível, i.e., todos os estados se comunicam
(P <- matrix(c(.5,.2,.3, .15,.45,.4, .25,.35,.4),
             nrow = 3, byrow = TRUE))

# iniciando no estado 2
x0 <- c(0,1,0)

# distribuição de probabilidade dos estados após 1 passo
(x1 <- x0 %*% P)        # simbolicamente, x0*P

# distribuição de probabilidade dos estados após 2 passos
(x2 <- x1 %*% P)        # simbolicamente, x1*P
(x2 <- x0 %*% P %*% P)  # simbolicamente, x0*P²

# a probabilidade de atingir o estado 3 em 2 passos é
# Pr(X_2 = s_3 | X_0 = s_2)
x2[3]

# Exercício 1
# Crie uma função que encontre a distribuição estacionária
# em função de x0, P e do erro, apresentando o # iterações e 
# o vetor de erros.
# Aplique para o exemplo acima com e=0.01.

estac <- function(x0,P,e){
  z <- x0
  Pn <- P
  i <- 1
  erro <- min(abs(z - x0%*%Pn))
  
  while(erro[i] > e){
    z <- x0 %*% Pn
    Pn <- Pn %*% Pn
    i <- i + 1
    erro[i] <- min(abs(z - x0%*%Pn))
  }
  return(list(i=i,erro=erro,z=z,Pn=Pn))
}


x0 <- c(0,1,0)
(P <- matrix(c(.5,.2,.3, .15,.45,.4, .25,.35,.4),
             nrow = 3, byrow = TRUE))
rownames(P) <- colnames(P) <- LETTERS[1:nrow(P)]
e <- 0.01

estac(x0,P,e)


# Pacote markovchain
# sudo apt-get update
# sudo apt-get install libxml2-dev
# sudo apt-get install r-cran-igraph
# install.packages('markovchain', dep = T)
library(markovchain)


# transformando P em 'markovchain'
Pmc <- new('markovchain', transitionMatrix = P,
           states = LETTERS[1:ncol(P)],
           name='MarkovChain P')
Pmc

# gráfico
plot(Pmc)
plot(P)


# Exemplo 1. Shared device (6.9 Stewart)
P <- matrix(c(.64,.32,.04, .4,.5,.1, .25,.5,.25),
            nrow = 3, byrow = TRUE)
rownames(P) <- colnames(P) <- 0:2
P
(P2 <- P %*% P)
rowSums(P)

# Exemplo 2. Shared devicecontinued (6.10 Stewart)
P0 <- c(0,0,1)
P0 %*% P2

P0 <- c(1/3,1/3,1/3)
P0 %*% P2

h <- 60
P60 <- diag(3)
for(i in 1:h){
  P60 <- P60 %*% P
}
P60

# Exemplo 3. Wheather (6.11 Stewart)
P <- matrix(c(.7,.3, .4,.6), byrow = T, nrow = 2)
colnames(P) <- rownames(P) <- c('sunny','rainy')
ss <- diag(2)
m <- 400
for(i in 1:m){
  ss <- ss %*% P
}
ss
3/7
4/7

# Exemplo 4. Número de clientes
(v0 <- c(324,233,210))
(P <- matrix(c(.7,.2,.1, .1,.6,.3, 0,.1,.9),
            nrow = 3, byrow = TRUE))
rownames(P) <- colnames(P) <- LETTERS[1:nrow(P)]
P
(v1 <- t(v0) %*% P)
(v2 <- v1 %*% P)
(P2 <- P%*%P)
t(v0)%*%P2

# Exemplo 4. A short example

# cadeia irredutível, i.e., todos os estados se comunicam
(P <- matrix(c(.5,.2,.3, .15,.45,.4, .25,.35,.4),
             nrow = 3, byrow = TRUE))
plot(P)

# iniciando no estado 2
x0 <- c(0,1,0)

# distribuição de probabilidade dos estados após 1 passo
(x1 <- x0 %*% P)        # simbolicamente, x0*P

# distribuição de probabilidade dos estados após 2 passos
(x2 <- x1 %*% P)        # simbolicamente, x1*P
(x2 <- x0 %*% P %*% P)  # simbolicamente, x0*P²

# a probabilidade de atingir o estado 3 em 2 passos é
# Pr(X_2 = s_3 | X_0 = s_2)
x2[3]

# Exercício 1
# Crie uma função que encontre a distribuição estacionária
# em função de x0, P e do erro, apresentando o # iterações e 
# o vetor de erros.
# Aplique para o exemplo acima com e=0.01.

estac <- function(x0,P,e){
  z <- x0
  Pn <- P
  i <- 1
  erro <- min(abs(z - x0%*%Pn))
  
  while(erro[i] > e){
    z <- x0 %*% Pn
    Pn <- Pn %*% Pn
    i <- i + 1
    erro[i] <- min(abs(z - x0%*%Pn))
  }
  return(list(i=i,erro=erro,z=z,Pn=Pn))
}


x0 <- c(0,1,0)
(P <- matrix(c(.5,.2,.3, .15,.45,.4, .25,.35,.4),
             nrow = 3, byrow = TRUE))
rownames(P) <- colnames(P) <- LETTERS[1:nrow(P)]
e <- 0.01

estac(x0,P,e)
estac(c(1,0,0),P,e)


# Pacote markovchain
# sudo apt-get update
# sudo apt-get install libxml2-dev
# sudo apt-get install r-cran-igraph
# install.packages('markovchain', dep = T)
library(markovchain)


# transformando P em 'markovchain'
Pmc <- new('markovchain', transitionMatrix = P,
          states = LETTERS[1:ncol(P)],
          name='MarkovChain P')
Pmc

# gráfico
plot(Pmc)
plot(P)

# estados após n-passos
initialState <- c(0,1,0)
steps <- 4
finalState <- initialState*Pmc^steps # using power operator
finalState
estac(x0,P,e=0.01)$z
estac(x0,P,e=0.01)$i

steadyStates(Pmc) # S4 method
estac(x0,P,e=0.0001)



# advanced example
E <- matrix(0, nrow = 4, ncol = 4)
E[1, 2] <- 1;E[2, 1] <- 1/3; E[2, 3] <- 2/3
E[3,2] <- 1/4; E[3, 4] <- 3/4; E[4, 3] <- 1
mcMathematica <- new("markovchain", states = c("a", "b", "c", "d"), 
transitionMatrix = E, name = 'Mathematic')
summary(mcMathematica)

# using Alofi rainfall dataset
data(rain)
mysequence <- rain$rain
createSequenceMatrix(mysequence)

myFit <- markovchainFit(data=mysequence, confidencelevel = .9,
                        method = "mle")
myFit

alofiMc <- myFit$estimate
alofiMc
