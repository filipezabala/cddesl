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
# https://www.r-graph-gallery.com/
# https://www.shinyapps.org/apps/RGraphCompendium/index.php
# https://www.curso-r.com/
# https://www.ufrgs.br/wiki-r


### Links para a instalação do R e RStudio
# https://cloud.r-project.org/
# https://www.rstudio.com/products/rstudio/download/preview/


### Tópicos
# 1 Funções básicas do R
# 2 Criando e manipulando funções  <-- 
# 3 Manipulando dados com dplyr and tidyr
# 4 Estatística	descritiva, visualização e séries temporais
# 5 Probabilidade
# 6 Inferência
# 7 Tópicos em Modelos Lineares Generalizados
# 8 Aprendizagem de máquina


#######################################
### 2 Criando e manipulando funções ###
#######################################

# elevar ao quadrado
quad <- function(x){
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
bhaskara(1,-1,-2)
# Exercício: generalize a função para admir raízes complexas.


# switch()
central <- function(x, type = 'mean') {
  switch(type,
         mean = mean(x),
         median = median(x),
         trimmed = mean(x, trim = .1))
}
x <- rnorm(10)

central(x)
central(x, 'mean')
central(x, 'median')
central(x, 'trimmed')


# exemplos de assign
assign('nome',3)
nome

for(i in 1:4){
  assign(paste0('nome',i),i^2)
}

(a <- 1:4)
assign('a[1]', 2)
a[1] == 2          # FALSE
get('a[1]') == 2   # TRUE


# variáveis globais, <<-
# exemplo de 'The R Inferno', pg. 35
x <- 1
y <- 2
fun <- function() {
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


# passando argumentos
args(apply)
(myMat <- matrix(1:15, c(5,3)))
myMat[5,1] <- NA
myMat
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


# =)