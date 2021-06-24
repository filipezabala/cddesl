################################################
###    Ciência de dados em software livre    ###
###         http://filipezabala.com          ###
###  https://github.com/filipezabala/cddesl  ###
###            Início: 2020-10-11            ###
###      Última atualização: 2021-06-24      ###
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
#  8 Simulação  <-- 
#  9 Inferência
# 10 Tópicos em Modelos Lineares Generalizados
# 11 Aprendizado de máquina


###################
### 8 Simulação ###
###################

## Método da transformação inversa

# 1. Gere n números aleatórios de uma U(0,1)
# 2. Aplique os n números em F^{-1}, a inversa da função F desejada
# 3. Calcule F^{-1}(u)

# Teorema da Transformação Integral de Probabilidade (Kotz et al (2005), p. 6476)
# Se uma variável aleatória X tem uma função de distribuição contínua 
# F(x), então a variável aleatória U = F(X) tem uma distribuição uniforme 
# no intervalo (0,1), ou seja, é uma variável aleatória U(0,1).

# Exponencial
# (adaptado de http://www.leg.ufpr.br/~walmes/ensino/ce089-2015-02/aula03_tip-2015-02.html)
Fx <- function(x, lambda){
  1-exp(-lambda*x)
}

iFx <- function(x, lambda){
  -log(1-x)/lambda
}

# gráficos das funções
l <- 2
curve(Fx(x, l), 0, 3)
curve(iFx(x, l), 0, 1)

# simulando
n <- 1000
u <- runif(n)
x <- iFx(u, l)

# histograma
hist(x, probability = T)
curve(l*exp(-l*x), add = T, col = 'red')
rug(x)
hist(rexp(n, l), probability = T) # função stats::rexp

# densidade
plot(density(x, from = 0))
curve(l*exp(-l*x), add = T, col = 'red')
rug(x)

# FDA empírica
plot(ecdf(x))
curve(Fx(x, lambda = l), add = T, col = 'red', from = 0)

# PP-plot
Pobs <- (1:length(x))/length(x) # Freq. rel. acum. observadas.
Pteo <- Fx(sort(x), lambda = l) # Prob acum. teóricas.

plot(Pobs ~ Pteo, pch = '.',
     xlab = "Frequências relativas acumuladas teóricas",
     ylab = "Probabilidades acumuladas observadas")
abline(a = 0, b = 1, col = 'red')

# Exercício. Considere a função f(x) = 2x/9, 0 < x < 3.
# a. Verifique se f(x) é uma função densidade de probabilidade.
# b. Encontre F(x).
# c. Simule F^{-1}(x) pelo método da da transformação inversa.


## Método da rejeição

