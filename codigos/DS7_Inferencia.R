################################################
###  Uma Introdução à Ciência de dados no R  ###
###              Filipe J. Zabala            ###
###        www.estatisticaclassica.com       ###
###           filipe.zabala@pucrs.br         ###
###           2018-11-12 a 2018-11-21        ###
################################################

### Referências
# https://cran.r-project.org/doc/manuals/r-release/R-intro.pdf
# http://r4ds.had.co.nz/
# http://www.estatisticaclassica.com
# http://www.burns-stat.com/pages/Tutor/R_inferno.pdf
# https://www.rstudio.com/wp-content/uploads/2016/10/r-cheat-sheet-3.pdf
# https://www.rstudio.com/wp-content/uploads/2016/01/rstudio-IDE-cheatsheet.pdf
# https://www.causascientia.org/math_stat/Dists/Compendium.pdf
# https://www.amazon.com/ggplot2-Elegant-Graphics-Data-Analysis/dp/331924275X/
# https://www.curso-r.com/
# https://www.ufrgs.br/wiki-r


### Links para a instalação do R e RStudio
# https://cloud.r-project.org/
# https://www.rstudio.com/products/rstudio/download/preview/


### Tópicos
# 1 Funções básicas do R
# 2 Criando e manipulando funções
# 3 Manipulando dados com dplyr and tidyr
# 4 Estatística	descritiva, visualização e séries temporais
# 5 Probabilidade
# 6 Inferência  <--
# 7 Tópicos em Modelos Lineares Generalizados
# 8 Aprendizagem de máquina


####################
### 6 Inferência ###
####################

##
# Intervalos de confiança (frequentistas) assintóticos

# Exemplo 4.27 (IC para \pi)
n <- 117
p <- 25/n
z <- abs(qnorm(0.025))
(e <- z*sqrt(p*(1-p)/n))
(LIpi <- p - e)
(LSpi <- p + e)


##
# Intervalos de confiança (frequentistas) via simulação
# https://projecteuclid.org/download/pdf_1/euclid.aos/1176344552

dr <- read.table('http://www.estatisticaclassica.com/data/drinks.txt',
                 head=T)
head(dr)
dim(dr)

x <- dr$temp
n <- nrow(dr)
nboot <- 10
nSims <- 1000 # numero de simulaçoes

# média teoria
(z <- qnorm(.025))
cat(mean(x)+z*sd(x)/sqrt(n), mean(x)-z*sd(x)/sqrt(n))

(t <- qt(.025,n-1))
cat(mean(x)+t*sd(x)/sqrt(n), mean(x)-t*sd(x)/sqrt(n))

# média via simulação (bootstrap)
mu <- rep(NA, nSims) # empty vector to store means
for(i in 1:nSims){
  mu[i] <- mean(sample(x, size = nboot, replace=TRUE))
}
quantile(mu,c(0.025,0.975))
mean(x)

# variância teoria
(n-1)*var(x)/qchisq(0.975, n-1)
(n-1)*var(x)/qchisq(0.025, n-1)

# variância simulação
v <- rep(NA, nSims) # empty vector to store medians
for (i in 1:nSims){  
  v[i] <- var(sample(x, size = nboot, replace=TRUE))
}
quantile(v,c(0.025,0.975))
var(x)


# mediana via simulação
me <- rep(NA, nSims) # empty vector to store medians
for (i in 1:nSims){  
  me[i] <- median(sample(x, size = nboot, replace=TRUE))
}
quantile(me,c(0.025,0.975))
median(x)


# Abordagem bayesiana

# https://en.wikipedia.org/wiki/Beta_distribution

par(mfrow=c(2,1))
curve(dbeta(x,2,5))
curve(dbeta(x,5,2))

# priori
a0 <- 1
b0 <- 1
par(mfrow=c(1,1))
curve(dbeta(x,a0,b0))

# Intervalo de Credibilidade a priori
qbeta(.025,a0,b0)
qbeta(.975,a0,b0)

# verossimilhanca binomial
n <- 117 # total
s <- 25  # sucessos
f <- n-s # fracassos
p <- s/n # proporção amostral

# posteriori
a1 <- a0+s
b1 <- b0+f
curve(dbeta(x,a1,b1))

# Intervalo de Credibilidade a posteriori
qbeta(.025,a1,b1)
qbeta(.975,a1,b1)

medVar <- function(a,b){
  # média
  m <- a/(a+b)
  
  # variância
  v <- (a*b)/((a+b)^2 * (a+b+1))
  
  return(list(media=m,variancia=v))
}
medVar(a0,b0)
medVar(a1,b1)


# média, variancia e int. de cred. via simulação (Gibbs sampler)
nSims <- 1000
simul <- rbeta(nSims,a1,b1)
(m <- mean(simul))
(v <- var(simul))
quantile(simul,c(0.025,0.975))

# 1. Programar um simulador da função beta que utilize a fdp de beta e runif.

##
# Testes de hipóteses

# Equivalência entre Testes de Hipóteses e Intervalos de Confiança

ic <- function(n){
  cat('[', 0.5-.98/sqrt(n), ',', 0.5+.98/sqrt(n), ']')
}

# 2. Considere a função 'ic' descrita acima. Generalize-a em função de
# 2a. \pi_0 e
# 2b. \alpha

# Exemplos 4.36 e 4.40 - Lançando a moeda e medindo seu equilíbrio
theta <- 0.5
x <- 40
n <- 100
p <- x/n
(zt <- sqrt(n)*(p-theta)/sqrt(theta*(1-theta)))
(p_value <- 2*pnorm(-abs(zt)))  # Multiplica-se por 2 pelo teste ser bilateral

# 3. Escreva uma função que generalize os Exemplos 4.36 e 4.40.


# Exemplo 4.41 - Princípio da verossimilhança 2
1-pbinom(8,12,0.5)
1-pnbinom(8,3,0.5)


# Exemplo 4.43 - Teste qui-quadrado de aderência
o <- c(20,14,18,17,22,29)     # Observados
p <- rep(1/6,6)               # Distribuição uniforme (dado equilibriado)
e <- 120*p                    # Valores esperados se o dado for equilibrado
k <- length(o)                # Número de categorias
qui <- sum((o-e)^2/e)         # Equação (60)
1-pchisq(qui,k-1)             # p-value
chisq.test(o)                 # Pela função 'chisq.test', facilita bastante


# Teste qui-quadrado (de Pearson) e exato (de Fisher) de homogeneidade para tabelas r x c
tab.bi <- matrix(c(90,10,70,30), nrow=2, byrow=T)       # Tabela 2.8
chisq.test(tab.bi)                                      # Teste qui-quadrado de Pearson
fisher.test(tab.bi)                                     # Teste exato de Fisher

# Exemplo 4.44 - Duas moedas
x1 <- 7
n1 <- 35
x2 <- 7
n2 <- 100
p1 <- x1/n1
p2 <- x2/n2
ph <- (x1+x2)/(n1+n2)
(zt <- (p1-p2)/sqrt(ph*(1-ph)*(1/n1+1/n2)))
(p_value <- 2*pnorm(-abs(zt)))
