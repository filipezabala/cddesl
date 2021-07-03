################################################
###    Ciência de dados em software livre    ###
###         http://filipezabala.com          ###
###  https://github.com/filipezabala/cddesl  ###
###            Início: 2020-10-11            ###
###      Última atualização: 2021-07-03      ###
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
#  7 Cadeias de Markov
#  8 Simulação
#  9 Inferência  <-- 
# 10 Tópicos em Modelos Lineares Generalizados
# 11 Aprendizado de máquina


####################
### 9 Inferência ###
####################

##
# Estimação pontual - não viés
# Exemplo 5.3 e 4.19 (http://filipezabala.com/enrs/amostragem-1.html#exm:aasc)
X <- c(24,32,49)
mean(X)
mxc <- c(24.0,28.0,36.5,28.0,32.0,40.5,36.5,40.5,49.0)
mean(mxc)


##
# Intervalos de confiança (frequentistas) assintóticos

# Exemplo 4.27 (IC para \pi)
n <- 125
p <- 25/n
z <- abs(qnorm(0.025))
(e <- z*sqrt(p*(1-p)/n))
(LIpi <- p - e)
(LSpi <- p + e)


##
# Intervalos de confiança (frequentistas) via simulação
# https://www.jstor.org/stable/2958830

dr <- read.table('http://www.filipezabala.com/data/drinks.txt',
                 header = T)
head(dr)
dim(dr)

x <- dr$temp
n <- nrow(dr)
B <- 1000 # numero de simulaçoes

# média teoria
(z <- qnorm(.025))
cat(mean(x)+z*sd(x)/sqrt(n), mean(x)-z*sd(x)/sqrt(n))

(t <- qt(.025,n-1))
cat(mean(x)+t*sd(x)/sqrt(n), mean(x)-t*sd(x)/sqrt(n))

# média via simulação (bootstrap)
mu <- rep(NA, B) # empty vector to store sample means
for(i in 1:B){
  mu[i] <- mean(sample(x, size = n, replace=TRUE))
}
quantile(mu, c(0.025,0.975))
mean(x)

# mediana via simulação
me <- rep(NA, B) # empty vector to store sample medians
for (i in 1:B){  
  me[i] <- median(sample(x, size = n, replace=TRUE))
}
quantile(me,c(0.025,0.975))
median(x)

# variância teoria
cat((n-1)*var(x)/qchisq(0.975, n-1), 
    (n-1)*var(x)/qchisq(0.025, n-1))

# variância simulação - NÃO FUNCIONA
v <- rep(NA, B) # empty vector to store sample variances
for (i in 1:B){  
  v[i] <- var(sample(x, size = n, replace=TRUE))
}
quantile(v,c(0.025,0.975))
var(x)

# Exercício: verificar como seria possível criar ICs para variância e dp via bootstrap.
# https://math.stackexchange.com/questions/2384639/bootstrap-confidence-interval-in-r-using-replicate-and-quantile?newreg=46ec8b2e55b84b25a3c1c78802c1903c


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
