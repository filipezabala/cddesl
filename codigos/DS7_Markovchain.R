################################################
###    Ciência de dados em software livre    ###
###         http://filipezabala.com          ###
###  https://github.com/filipezabala/cddesl  ###
###            Início: 2020-10-11            ###
###      Última atualização: 2021-06-23      ###
################################################

# Playlist
# https://www.youtube.com/playlist?list=PLgnUrXr7_7coSfm067nFXPvShO18o6GQ_

### Textos
# https://cran.r-project.org/doc/manuals/r-release/R-intro.pdf
# http://r4ds.had.co.nz/
# https://www.curso-r.com/
# https://www.ufrgs.br/wiki-r
# http://www.burns-stat.com/pages/Tutor/R_inferno.pdf
# https://www.amazon.com/ggplot2-Elegant-Graphics-Data-Analysis/dp/331924275X/

### Formulários e resumos
# https://www.rstudio.com/resources/cheatsheets/
# https://www.causascientia.org/math_stat/Dists/Compendium.pdf

### Gráficos
# https://plot.ly/r/
# https://www.r-graph-gallery.com/
# https://github.com/d3/d3/wiki/Gallery
# http://kateto.net/network-visualization
# https://www.shinyapps.org/apps/RGraphCompendium/index.php
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html

### Links para a instalação do R e RStudio
# https://cloud.r-project.org/
# https://www.rstudio.com/products/rstudio/download/preview/

### (R) tools
# https://cran.r-project.org/bin/windows/Rtools/
# https://cran.r-project.org/bin/macosx/tools/

### Tópicos
#  0 A primeira seção de R e RStudio
#  1 Funções básicas do R e RStudio
#  2 Objetos e funções úteis
#  3 Criando e manipulando funções
#  4 Manipulando dados com dplyr and tidyr
#  5 Estatística	descritiva, visualização e séries temporais
#  6 Probabilidade
#  7 Cadeias de Markov  <-- 
#  8 Simulação
#  9 Inferência
# 10 Tópicos em Modelos Lineares Generalizados
# 11 Aprendizado de máquina




###########################
### 7 Cadeias de Markov ###
###########################


# Exemplo 1. Número de clientes
(v0 <- c(324,233,210))
(P <- matrix(c(.7,.2,.1, .1,.6,.3, 0,.1,.9),
             nrow = 3, byrow = TRUE))
rownames(P) <- colnames(P) <- LETTERS[1:nrow(P)]
P
(v1 <- t(v0) %*% P)
(v2 <- v1 %*% P)
(P2 <- P %*% P)
t(v0) %*% P2


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
# Crie uma função que encontre a distribuição estacionária (steady state)
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
P
e <- 0.01

estac(x0,P,e)



# Exemplo 3. Shared device (6.9 Stewart)
P <- matrix(c(.64,.32,.04, .4,.5,.1, .25,.5,.25),
            nrow = 3, byrow = TRUE)
rownames(P) <- colnames(P) <- 0:2
P
(P2 <- P %*% P)
rowSums(P)

# Exemplo 4. Shared device continued (6.10 Stewart)
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

# Exemplo 5. Wheather (6.11 Stewart)
P <- matrix(c(.7,.3, .4,.6), byrow = T, nrow = 2)
colnames(P) <- rownames(P) <- c('sunny','rainy')
P
ss <- diag(2)
m <- 400
for(i in 1:m){
  ss <- ss %*% P
}
ss
3/7
4/7



# Pacote markovchain
# https://cran.r-project.org/web/packages/markovchain/vignettes/an_introduction_to_markovchain_package.pdf
# sudo apt-get update
# sudo apt-get install libxml2-dev
# sudo apt-get install r-cran-igraph
# install.packages('markovchain', dep = T)
library(markovchain)


(P <- matrix(c(.5,.2,.3, .15,.45,.4, .25,.35,.4),
             nrow = 3, byrow = TRUE))

# transformando P em 'markovchain'
Pmc <- new('markovchain', transitionMatrix = P,
           states = LETTERS[1:ncol(P)],
           name='MarkovChain P')
class(Pmc)
Pmc

# gráfico
plot(Pmc)
plot(P)


# estados após n-passos
initialState <- c(0,1,0)
steps <- 4
finalState <- initialState*Pmc^steps # using power operator
finalState
estac(initialState,P,e=0.01)$z
estac(initialState,P,e=0.01)$i

steadyStates(Pmc) # S4 method
stac(initialState,P,e=0.0001)


# advanced example
E <- matrix(0, nrow = 4, ncol = 4)
E[1, 2] <- 1;E[2, 1] <- 1/3; E[2, 3] <- 2/3
E[3,2] <- 1/4; E[3, 4] <- 3/4; E[4, 3] <- 1
mcMathematica <- new('markovchain', states = c('a', 'b', 'c', 'd'), 
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
