################################################
###    Ciência de dados em software livre    ###
###         http://filipezabala.com          ###
###  https://github.com/filipezabala/cddesl  ###
###            Início: 2020-10-11            ###
###      Última atualização: 2020-10-19      ###
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
# 1 Funções básicas do R  <-- 
# 2 Criando e manipulando funções
# 3 Manipulando dados com dplyr and tidyr
# 4 Estatística	descritiva, visualização e séries temporais
# 5 Probabilidade
# 6 Inferência
# 7 Tópicos em Modelos Lineares Generalizados
# 8 Aprendizagem de máquina



##############################
### 1 Funções básicas do R ###
##############################

# Executar comandos
# Opção 1: clicar no botão 'Run'
# Opção 2: Ctrl+Enter (Windows)
#          command+return (Mac) 

# Comentar/descomentar:
# Ctrl+Shift+C (Windows)
# command+shift+C (Mac)


# Instalando os pacotes necessários (rodar uma vez)
# No terminal, sudo rstudio
# packs <- c('pracma','rgl','car','tidyverse')
# install.packages(packs, dep = T)
# update.packages(ask = F)


# Instalando e atualizando pacotes (rodar uma vez)
library(MASS)         # Para a função fractions()
library(pracma)       # nthroot(x,n)
library(car)          # recode()
library(rgl)          # demo(rgl)


# Demonstração de algumas funcionalidades
demo()              # Lista todos as demonstrações disponíveis, mesmo em pacotes não carregados
demo(persp)         # Gráficos 3D
demo(rgl)           # Gráficos 3D avançados
demo(colors)        # Cores por nome
demo(Hershey)       # Tabelas de caracteres
demo(plotmath)      # Notação matemática


# Detalhes da distribuição
licence()
RShowDoc('COPYING') # GNU Version 2, June 1991
RShowDoc('GPL-3')   # GNU Version 3, June 2007


# Contribuidores
contributors()


# Citação
citation()        # R
citation('car')   # pacotes


# Ajuda e documentação
help()              # Ajuda
help.start()        # Ajuda em html
help(mean)          # Chama a ajuda para a função 'mean'
?mean               # Equivalente a 'help(mean)'
help.search('mean') # Busca por tópicos contendo 'mean'
??mean              # Equivalente a 'help.search('mean')'
example(mean)       # Roda os exemplos da documentação de 'mean'
apropos('mean')     # Encontra funções que contenham 'mean'


## Comandos básicos ##

# Operações
2+5             # Adição
sum(2,5)        # Adição através da função 'sum'
2-5             # Subtração
2*5             # Multiplicação
2/5             # Divisão
2^5             # Potenciação
2**5            # Equivale a 2^5, utilizada em Python
sqrt(16)        # Raiz quadrada
sqrt(-17+0i)    # Raiz quadrada para os Complexos
pracma::nthroot(16,2)   # {pracma} Raiz n-ésima (n=2)
pracma::nthroot(16,4)   # {pracma} Raiz n-ésima (n=4)

# Prioridade das operações (calcule mentalmente antes de rodar)
1+2*5 
(1+2)*5
1/2*5^3
1/(2*5)^3

# Sequências regulares
1:100
seq(1, 100, by = 1)
seq(1, 100, by = 2)
100:1
-100:1
-(100:1)

rep(1:4, times = 2)         # repete a sequência 1:4 duas vezes
rep(1:4, length.out = 10)   # limita o numero de observacoes
rep(1:4, each = 2)          # repete cada elemento duas vezes


# Atribuições
x <- 2
(3 -> z)
y = 4
4 = y
x*y+z

# Funções matemáticas
pi
cos(c(0,30,45,60)*pi/180)
MASS::fractions(cos(c(0,30,45,60)*pi/180))


# Looping

# for
for(i in 1:10) print(i)
for(i in seq(1,10,2)) print(i)

# celsius para farenheit
for(celsius in 20:30){
  print( c(celsius, round(1.8*celsius+32, 0)) )
}

# while
x <- 0
while(x < 5) {x <- x+1; print(x)}

x <- 0
while(x < 5) {x <- x+1; if (x == 3) break; print(x)}  # pára quando x == 3

x <- 0
while(x < 5) {x <- x+1; if (x == 3) next; print(x)}   #  omite x == 3


# Arredondamento
round(153.456789,3) # Arredonda para 3 decimais
for(i in 6:-2){print(round(153.456789,i))}
options(digits = 9) # Ajusta apresentação para 9 dígitos (padrão é 7)
for(i in 6:-2){print(round(153.456789,i))}


# Valores lógicos
2 < 5            # TRUE ou T
2 > 5            # FALSE ou F
1 == 1           # Igualdade simbolizada por ==
1 != 1           # Diferença simbolizada por !=
(1==1) & (1==2)  # Intersecção/E simbolizado por & ou &&
(1==1) | (1==2)  # União/OU simbolizado por |
!(1==1)          # Negação simbolizada por !
as.numeric(c(T,F,F,T,T,T,T,F,T))  # conversão para numérico
sum(c(T,F,F,T,T,T,T,F,T))
?Logic  # ajuda de 'Logical Operators'


# Fatores
factor(letters[1:10])                           # os fatores são categorias

as.numeric(factor(101:103))                     # atenção! ('The R Inferno', pg 82)
as.numeric(as.character(factor(101:103)))       # solução 1, sad but true
f <- factor(101:103); as.numeric(levels(f))[f]  # solução 2, mais difícil de lembrar

ff <- factor(c('AA', 'BA', 'CA'))   # ff, fator de 3 níveis
ff
ff[1:2]             # mesmo com o filtro o nível 'CA' ainda está lá
ff[1:2, drop=TRUE]  # solução 1, usando drop
factor(ff[1:2])     # solução 2, transformando f[1:2] em um novo fator


# Valores faltantes (NA/missing values)
(z1 <- c(1:3, NA))    # NA é um símbolo especial
(z2 <- c(1:3, NB))    # erro, pois 'NB' não é símbolo especial
is.na(z1)
mean(z1)
mean(z1, na.rm = T)


# Diretório de trabalho (working directory)
getwd()        # Apresenta o diretório corrente
setwd('H:/')   # Ajusta o diretório corrente
dir()          # Apresenta o conteúdo do diretório corrente

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
class(l1$matriz2)
diag(l1$matriz2)
class(l1$char)
l1$char[1]
names(l1)


# Data frame
# Lendo o arquivo 'hospital.txt' direto do link
h <- read.table('http://www.estatisticaclassica.com/data/hospital.txt', 
                head=T)
dim(h)       # dimensão
head(h)      # cabeçalho, teste 'tail(hosp)'
summary(h)   # função genérica de resumo
str(h)       # estrutura (structure)

class(h)     # classe
h.matrix <- as.matrix(h)
class(h.matrix)


# Funções úteis

(x <- 1:10)
rev(x)          # reverte o vetor
cumsum(x)       # soma acumulada
cumsum(rev(x))  # soma acumulada inversa

(x <- c(3:5, 11:8, 8 + 0:5))
(ux <- unique(x))           # apresenta uma lista de valores distintos
length(x)                   # 13 elementos, contando as repeticoes
length(unique(x))           # 9 elementos únicos
duplicated(x)               # indica as posicoes onde há duplicações

(tab <- table(duplicated(x)))     # cria uma tabela
prop.table(tab)                   # percentuais

match(1:10, c(1,9,8,4))     # retorna um vetor das posições do primeiro objeto que corresponde com o segundo
1:10 %in% c(1,9,8,4)        # retorna TRUE onde há correspondência e FALSE caso contrário

(r1 <- rank(x1 <- c(3,1,4,15,92)))    # indica a ordem de cada elemento do vetor
x2 <- c(3,1,4,1,5,9,2,6,5,3,5)
names(x2) <- letters[1:length(x2)]
(r2 <- rank(x2))                          # ties.method = c('average', 'first', 'last', 'random', 'max', 'min')

sample(1:60, size=6)                      # amostra pseudo-aleatoria para jogar na mega-sena
set.seed(20); sample(1:60, size=6)        # ajustando a semente pseudo-aleatoria
set.seed(20); sort(sample(1:60, size=6),decreasing=T)  # ordenando, para facilitar a leitura

letters                       # letras minusculas
LETTERS                       # letras maiusculas
LETTERS[1]                    # primeira posição de LETTERS
noquote(letters)              # sem aspas

substr('abcdef', 2, 4)        # apresenta da segunda ate a quarta posição

x <- c('Tche', 'Churrasco', 'Chimarrao', 'Farrapo', 'Gaita', 'Bah')
strsplit(x, 'a')              # retira a letra 'a'

tolower(x)    # minusculas
toupper(x)    # maiusculas

texto <- 'Rio Grande do Sul'
nchar(texto)    # conta o número de caracteres, contando os espaços
stringi::stri_stats_latex(texto)  # relatório mais completo

x <- c(2,4,-2,6,7,8)
diff(x)           # tira a diferenca, note que restam n-1 elementos
range(x)          # apresenta o mínimo e o máximo
diff(range(x))    # tira a diferenca entre o mínimo e o máximo (amplitude)

as.Date('11/3/1981', format = '%d/%m/%Y')   # coloca no formato de data...
as.Date('3:11:1981', format = "%m:%d:%Y")   # ... não importa o separador
(data <- as.Date('11/3/1981', format='%d/%m/%Y'))
class(data)       # classe Date
months(data)      # mês
weekdays(data)    # dia da semana
quarters(data)    # trimestre
Sys.Date() - data # número de dias entre hoje e data

(m <- cbind(x = 1:10, y = (-4:5)^2))    
split(m, col(m))      # divide uma matriz em uma lista pelas colunas, gerando uma lista
split(1:20, 1:2)      # divide a sequencia de 1 a 10 de dois em dois valores (pares/ímpares)

(x <- rep(1:3,3))
car::recode(x, " c(1,2) = 'A'; else = 'B' ")  # recodifica

# =)
