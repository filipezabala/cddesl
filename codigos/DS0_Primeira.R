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
# 0 A primeira seção de R e RStudio  <-- 
# 1 Funções básicas do R e RStudio
# 2 Objetos e funções úteis
# 3 Criando e manipulando funções
# 4 Manipulando dados com dplyr and tidyr
# 5 Estatística	descritiva, visualização e séries temporais
# 6 Probabilidade
# 7 Inferência
# 8 Tópicos em Modelos Lineares Generalizados
# 9 Aprendizagem de máquina



#########################################
### 0 A primeira seção de R e RStudio ###
#########################################


# Executar comandos
# Opção 1: clicar no botão 'Run'
# Opção 2: Ctrl+Enter (Windows)
#          command+return (Mac) 

# Comentar/descomentar:
# Ctrl+Shift+C (Windows)
# command+shift+C (Mac)


2+4*3
(2+4)*3
2^3
2**3
3^2

1:10
10:1
-(10:1)
-10:1

x <- 2
5*x
x <- x+1
x

(y = 4)
(3 -> z)

x+y*z

(v <- c(2,0,1,2,4,2))
2*v
v^2
v^3

v[3]
v[-3]
v[c(3,5)]
length(v)
?length

MASS::fractions(0.5)
library(MASS)
fractions(0.5)
fractions(0.25)

letters[1:20]
LETTERS
noquote(LETTERS)

substr('abcdef', 2, 4)

x <- c('Chimarrao', 'Gaita', 'Bah')
strsplit(x, 'a') # retira a letra 'a'

tolower(x)
toupper(x)
