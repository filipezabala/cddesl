################################################
###    Ciência de dados em software livre    ###
###         http://filipezabala.com          ###
###  https://github.com/filipezabala/cddesl  ###
###            Início: 2020-10-11            ###
###      Última atualização: 2020-10-20      ###
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
# https://www.rstudio.com/wp-content/uploads/2016/10/r-cheat-sheet-3.pdf
# https://www.rstudio.com/wp-content/uploads/2016/01/rstudio-IDE-cheatsheet.pdf
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


### Tópicos
# 0 A primeira seção de R e RStudio
# 1 Funções básicas do R e RStudio 
# 2 Objetos e funções úteis
# 3 Criando e manipulando funções
# 4 Manipulando dados com dplyr and tidyr
# 5 Estatística	descritiva, visualização e séries temporais
# 6 Probabilidade  <-- 
# 7 Inferência
# 8 Tópicos em Modelos Lineares Generalizados
# 9 Aprendizagem de máquina



#######################
### 6 Probabilidade ###
#######################

# Normal ou gaussiana
par(mfrow=c(2,1))
curve(dnorm(x),-4,4)
pnorm(0)
qnorm(0.025)

# Gráficos
library(ggplot2)
rn <- rnorm(1000)
hn <- hist(rn, breaks = 'FD', freq = F)
curve(dnorm(x), add = T, col = 'blue')
lines(density(rn), col = 'red')

# dataset:
data <- data.frame(value=rnorm(10000))

# Usando ggplot2
p1 <- ggplot(data, aes(x=value)) + geom_histogram() + geom_density(aes(y=..density..*10))
p2 <- ggplot(data, aes(x=value)) + geom_histogram(binwidth = 0.05)
gridExtra::grid.arrange(p1,p2)

## TCL
# O Teorema Central do Limite afirma que
# a soma de n variáveis aleatórias (v.a.) indepentendes
# pode ser aproximada pela distribuição normal,
# qualquer que seja a distribuição destas n v.a.
# com média e variância finitas.

n <- 420
p <- 0.5
S <- 200
mS <-n*p
sS <- sqrt(n*p*(1-p))

# Aproximaçãao da binomial pela normal SEM correçãao de continuidade 
(z <- (S-mS)/sS)
pnorm(z)

# Aproximaçãao da binomial pela normal COM correçãao de continuidade
(zc <- (S+0.5-mS)/sS)
pnorm(zc)

# Probabilidade exata
pbinom(S,n,p)


# normal e t de Student/Lüroth
par(mfrow=c(1,1))
curve(dnorm(x), -10,10, col = 'blue')
curve(dt(x, df = 1), -10,10, add = T, col = 'red')
legend(-10,.35, c('N(0,1)','t(1)'), lty = 1, col=c('blue','red'))


#############################
### 5.1 Cadeias de Markov ###
#############################


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


# Exercício: ler as seguintes documentações:
?curve
?pnorm
?dt
?pexp