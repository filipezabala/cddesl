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
#  3 Criando e manipulando funções  <-- 
#  4 Manipulando dados com dplyr and tidyr
#  5 Estatística	descritiva, visualização e séries temporais
#  6 Probabilidade
#  7 Cadeias de Markov
#  8 Simulação
#  9 Inferência
# 10 Tópicos em Modelos Lineares Generalizados
# 11 Aprendizado de máquina



#######################################
### 3 Criando e manipulando funções ###
#######################################

# elevar ao quadrado
quad = function(x){
  return(x**2)
} 

quad(3)
quad(1:10)   # permite vetores


# fórmula de bhaskara
bhaskara <- function(a,b,c)
{
  delta <- b^2 - 4*a*c
  x1 <- (-b-sqrt(delta))/(2*a)
  x2 <- (-b+sqrt(delta))/(2*a)
  cat('Primeira raiz = ', x1, '\n')
  cat('Segunda raiz = ', x2, '\n')
  cat('Delta = ', delta, '\n')
  cat('Soma das raízes (-b/a) = ', -b/a, '\n')
  cat('Produto das raízes (c/a) = ', c/a, '\n')
}
args(bhaskara)
bhaskara(1,-1,-2)
# Exercício: generalize a função para admir raízes complexas.


# switch()
central <- function(x, type = 'mean') {
  switch(type,
         mean = mean(x),
         median = median(x),
         trimmed = mean(x, trim = .1))
}
set.seed(1)
x <- rnorm(10)
x

central(x)
central(x, 'mean')
central(x, 'median')
central(x, 'trimmed')


# exemplos de assign
assign('nome',3)
nome
nome = 3 # equivalente

# paste0('nome', 1)
# nome1 = 1^2

for(i in 1:4){
  assign(paste0('nome',i),i^2)
}

(a <- 4:1)
assign('a[1]', 2)
a[1] == 2          # FALSE
get('a[1]') == 2   # TRUE

# colocar lista de nomes especiais do R!
# https://cran.r-project.org/doc/manuals/R-lang.pdf, p. 48 (53 pdf)


# variáveis globais, <<-
# exemplo de 'The R Inferno', pg. 35
x <- 1
y <- 2
fun <- function(){
  x <- 101
  y <<- 102
}
fun()
x
y

# usando assign
x <- 1
y <- 2
fun <- function() {
  x <- 101
  assign('y',102)
}
fun()
x
y


# .Last.value retorna o último valor obtido
.Last.value


# lapply retorna uma lista do mesmo tamanho de X, cada elemento é o resultado 
# da aplicação de FUN ao elemento correspondente de X.
# https://www.datacamp.com/community/tutorials/r-tutorial-apply-family

(X = matrix(1:20, nrow = 4, byrow = T))
colnames(X) = paste0('V', 1:ncol(X))
X

# for
for(i in 1:ncol(X)){
  print(sum(X[,i]))
}

# colSums
colSums(X)

# apply
apply(X, 2, sum)

# lapply
class(X)
class(as.data.frame(X))
lapply(as.data.frame(X), sum)
sapply(as.data.frame(X), sum)

# Exercício: rodar e entender
args(lapply)   # retorna apenas os argumentos da função. Tente apenas 'lapply'
head(iris)
class(iris)
length(iris)
dim(iris)
lapply(iris, class)  # é aplicada a função as.list() a X, no caso iris
str(iris)

# do.call constrói e executa uma chamada de função de um nome ou função e uma lista de argumentos.
# é amplamente usada, por exemplo, para transformar listas em estruturas mais simples 
# (muitas vezes com rbind ou cbind).
do.call('complex', list(imag = 1:3))
complex(imaginary = 1:3)

# transformando lista em vetor com do.call
(x <- lapply(iris, class))
class(x)
(y <- do.call(c, x))
class(y)


# ellipsis (três pontos) permite:
# - um número arbitrário e variado de argumentos
# - passar argumentos para outras funções
# http://www.burns-stat.com/the-three-dots-construct-in-r/

# exemplo 1 - argumentos arbitrários: pode-se listar quantos argumentos desejar,
# nomeando-os ou não
c
c(a=7, 33.1, c=NA)
names(.Last.value)


# exemplo 2 - 8.1.57 (The R Inferno)
xlis <- list(A=1:4, B=c('a', 'x'))
xlis
c(xlis, C=6:5)
c(xlis, list(C=6:5))

# NA
# Not Available
# Not Applicable
# Not Annouced


# passando argumentos
args(apply)
(myMat <- matrix(1:15, c(5,3)))
myMat[5,1] <- NA
myMat
sum(is.na(myMat))
apply(myMat, 1, mean)  # 1 indica aplicação da função mean() por linha
apply(myMat, 2, mean)  # 2 indica aplicação da função mean() por coluna

apply(myMat, 2, mean, na.rm=T)  # na.rm é argumento de mean(), não de apply()
apply(myMat, 2, mean, na=T)     # admite abreviatura

colMeans(myMat, na.rm = T)      # alternativa mais eficiente
?rowMeans

# exemplo 3 - personalizando uma função existente
x <- rnorm(100)
y <- x + rnorm(100)
plot(x,y)

# utilizando ellipsis (...)
my.plot <- function(..., pch.new = 15) {
  plot(..., pch = pch.new)
}

my.plot(x,y)
my.plot(x,y, col = 'red', pch.new = 12)


# declarando x e y nos argumentos
my.plot <- function(x,y,..., pch.new = 15) {
  plot(x,y,..., pch = pch.new)
}

my.plot(x,y)
my.plot(x,y, col = 'red', pch.new = 11)



# os três componentes da função
formals(quad)
body(quad)
environment(quad)

# .Internal(inspect(x)) chama o código interno construído pelo interpretador R, porém
.Internal(inspect(quad)) # ... 'Only true R wizards should even consider using this function'



# Exercício

# 1. Crie uma função que converta entre as escalas Celsius e Farenheit. Sugestão: ?switch
# https://en.wikipedia.org/wiki/Scale_of_temperature

conv <- function(grau, grau_in = 'celsius'){
  switch(grau_in,
         celsius = data.frame(celsius = grau, faherenheit = grau*1.8+32),
         fahrenheit = data.frame(celsius = (grau-32)/1.8, fahrenheit = grau)
  )
}

conv(0)
conv(0:32, 'fahrenheit')

# 2. Faça uma função que converta entre as escalas Farenheit (ºF), Celsius (ºC), Kelvin (K), Rankine (ºRa), 
# Réaumur (ºRé), Rømer (ºRø), Newton (ºN) e Delisle (ºD).


# 3. O UBER cobra um preço base de R$ 2.30, R$ 1.20 por km rodado e R$ 0.20 por minuto 
# decorrido durante a viagem. a) Crie uma função chamada 'uber' que calcule o total da tarifa em
# função da quilometrgem e do tempo. b) Crie um gráfico 3D com as três informações. Use
# as funções persp e rgl::plot3d.

uber <- function(km,tempo){
  print(2.3 + 1.2*km + .2*tempo)
  # print(data.frame(km = km, tempo = tempo, preco = preco))
}

uber(7,15)
uber(0:10,0:10)
ueg <- uber(expand.grid(0:10, 0:10), expand.grid(0:10, 0:10))
rgl::plot3d(ueg)

# 4. Ler:
# http://adv-r.had.co.nz/Style.html


# =)