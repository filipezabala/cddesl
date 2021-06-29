################################################
###    Ciência de dados em software livre    ###
###         http://filipezabala.com          ###
###  https://github.com/filipezabala/cddesl  ###
###            Início: 2020-10-11            ###
###      Última atualização: 2021-06-28      ###
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
#  2 Objetos e funções úteis  <-- 
#  3 Criando e manipulando funções
#  4 Manipulando dados com dplyr and tidyr
#  5 Estatística	descritiva, visualização e séries temporais
#  6 Probabilidade
#  7 Cadeias de Markov
#  8 Simulação
#  9 Inferência
# 10 Tópicos em Modelos Lineares Generalizados
# 11 Aprendizado de máquina




#################################
### 2 Objetos e funções úteis ###
#################################

# Fatores
xx <- factor(letters[1:10])                     # fatores são categorias
as.numeric(factor(101:103))                     # atenção! ('The R Inferno', pg 82)
as.numeric(as.character(factor(101:103)))       # solução 1, sad but true
f <- factor(101:103); as.numeric(levels(f))[f]  # solução 2, mais difícil de lembrar

ff <- factor(c('AA', 'BA', 'CA'))   # ff, fator de 3 níveis
ff
ff[1:2]             # mesmo com o filtro o nível 'CA' ainda está lá
ff[1:2, drop=TRUE]  # solução 1, usando drop
factor(ff[1:2])     # solução 2, transformando f[1:2] em um novo fator

factor(letters[1:4], ordered = FALSE) # fatores não ordenados (padrão)
factor(letters[1:4], ordered = TRUE)  # fatores ordenados
factor(letters[1:4], ordered = TRUE, levels = c('c','a','d','b'))  # fatores reordenados

# Mapeia de 4 diferentes valores para somente dois níveis
(x <- c('Homem', 'Masculino', 'Homem', 'Mulher', 'Feminino'))
(xf <- factor(x, levels = c('Masculino', 'Homem' , 'Mulher', 'Feminino'),
              labels = c('Masculino', 'Masculino', 'Feminino', 'Feminino')))

# Manipulando objetos
x <- 2
class(x)   # Apresenta a classe de 'x'
rm(x)      # Remove o objeto
objects()  # Listando objetos
save.image(file = 'aula1.RData') # Salvando área de trabalho


# Vetores
(v <- c(2,0,1,2,4,2))   # Atribui o vetor (2,0,1,2,4,2) a 'v' e apresenta 'v'
2*v
v^3
v[3]
v[-3]
v[c(2,5)]
length(v)

(x <- 1:10)           # gera uma sequência regular de 1 a 10
class(x)              # 'integer'
1/x                   # novo vetor, não guardado na memória
class(1/x)            # ponto flutuante
(y <- x^2)            # gerando outras variáveis de exemplo
(z <- 2*x+y+1)

(v1 <- c(x,y,z))      # 'c' concatena os vetores 'x', 'y' e 'z'
length(v1)            # utiliza-se 'length' para vetores e listas

e1 <- vector()        # atribuindo um objeto 'vazio'
e2 <- numeric()       # equivalente a 'vector()'
e1[3] <- 17; e1       # o objeto assume a dimensão da posição fornecida
e2[10] <- 10; e2
length(e1)
length(e2)

# Matrizes
(m1 <- cbind(x,y,z))    # Cola os vetores como colunas, transformando em uma matriz 10x3
dim(m1)                 # Utiliza-se 'dim' para matrizes e data frames
(m2 <- rbind(x,y,z))    # Cola os vetores como linhas, transformando em uma matriz 3x10
dim(m2)

t(m2)                 # transposta
all.equal(m1,t(m2))   # compara os objetos
identical(m1,t(m2))   # mais restritiva que 'all.equal'

class(m1)               # 'cbind' e 'rbind' geram objetos da classe 'matrix'
class(m2)

rownames(m1) <- paste0('Linha ', 1:10)    # ?paste
m1
colnames(m2) <- LETTERS[1:10]     # ?letters
m2

(m3 <- matrix(1:16, nrow=4, ncol=4))            # distribui os valores por coluna
is.matrix(m3)
(m4 <- matrix(1:20, nrow=4, ncol=5, byrow=T))   # distribui os valores por linha
(m5 <- matrix(0, nrow=5, ncol=7))               # repete o valor fornecido em todas as celulas

m1 %*% m2             # o produto de matrizes é feito com %*%
crossprod(t(m1),m2)   # t(x) %*% y, mais r?pido que %*%
m2 %*% m1
crossprod(m1,t(m2))


# Array
(a1 <- array(data = 1:30, dim = c(2,5,3)))
dim(a1)
a1[1,3,2]
a1[,,1]
a1[,1,]
a1[1,,]


# Listas
l1 <- list(matriz1=m1, matriz2=m2, char=c('aa', 'bb'), array1=a1)
l1
is.list(l1)
l1$matriz2    # $ para chamar pelo nome
l1[[2]]       # [[ ]] para chamar pela posicao
class(l1)
class(l1$matriz2)
diag(l1$matriz2)
class(l1$char)
lapply(l1, class)
l1$char[1]
names(l1)

# listas como retorno de funções
x <- rnorm(100)
y <- rpois(100,4)
plot(x,y)

fit <- lm(y ~ x)
names(fit)

fit$coefficients
fit$residuals
model.matrix(fit)
fit$effects

# Data frame
# Lendo o arquivo 'hospital.txt' direto do link
h <- read.table('http://www.filipezabala.com/data/hospital.txt', header=T)
class(h)     # classe
dim(h)       # dimensão
head(h)      # cabeçalho, teste 'tail(hosp)'
summary(h)   # função genérica de resumo
str(h)       # estrutura (structure)

h.matrix <- as.matrix(h)
class(h.matrix)
str(h.matrix)
head(h.matrix)
lapply(h.matrix, class)

# =)
