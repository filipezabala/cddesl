################################################
###    Ciência de dados em software livre    ###
###         http://filipezabala.com          ###
###  https://github.com/filipezabala/cddesl  ###
###            Início: 2020-10-11            ###
###      Última atualização: 2021-06-19      ###
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
#  6 Probabilidade  <-- 
#  7 Cadeias de Markov
#  8 Inferência
#  9 Tópicos em Modelos Lineares Generalizados
# 10 Aprendizado de máquina



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


# Exercício: ler as seguintes documentações:
?curve
?pnorm
?dt
?pexp

