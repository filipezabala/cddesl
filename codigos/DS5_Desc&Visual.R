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
# 5 Estatística	descritiva, visualização e séries temporais <-- 
# 6 Probabilidade
# 7 Inferência
# 8 Tópicos em Modelos Lineares Generalizados
# 9 Aprendizagem de máquina



#################################################################
### 5 Estatística	descritiva, visualização e séries temporais ###
#################################################################

# Lendo o arquivo 'hospital.txt' direto do link, vide Zabala (2018a)
h <- read.table('http://www.estatisticaclassica.com/data/hospital.txt', head = T)
dim(h)
head(h)   # teste tail(h, 10)
attach(h) # cuidado ao usar attach! Veja ?detach

# Variável discreta, Exemplos 2.13 e 2.14
(tab <- table(filhos))                    # Frequência simples/absoluta
prop.table(tab)                           # Frequência relativa
cumsum(tab)                               # Frequência acumulada
round(cumsum(tab)/length(filhos),2)       # Frequência acumulada relativa
cumsum(rev(tab))                          # Frequência acumulada inversa
round(cumsum(rev(tab))/length(filhos),2)  # Frequência acumulada relativa inversa


# Comparando Sturges, Scott e Freedman-Diaconis, Exemplo 2.19
NC <- function(x) c(i = i, n = 10^i,                 # Quantidades simuladas
                    Sturges = nclass.Sturges(x),     # Sturges (1926)
                    Scott = nclass.scott(x),         # Scott (1979)
                    FD = nclass.FD(x))               # Freedman-Diaconis (1981)
for(i in 1:6){set.seed(i); print(NC(rnorm(10^i)))}   # Pode ser demorado para i>6


# Variável contínua - Exemplo 2.21
pretty(nclass.Sturges(altura))            # Valores 'bonitos' para o número de classes
hist(altura)$breaks                       # Quebras de valores gerados com a função 'hist'
(f <- hist(altura)$counts)                # Frequências das classes
cumsum(f)                                 # Frequência acumulada
round(cumsum(f)/length(altura),2)         # Frequência acumulada relativa
cumsum(rev(f))                            # Frequência acumulada inversa
round(cumsum(rev(f))/length(altura),2)    # Frequência acumulada relativa inversa


# Medidas de localização
summary(h)

# Separatrizes
options(digits = 3)                                 # Para melhorar a apresentação
quantile(altura, probs = seq(0, 1, 1/2))            # Mediana
quantile(altura, probs = seq(0, 1, 1/3))            # Tercis
quantile(altura, probs = seq(0, 1, 1/4))            # Quartis
quantile(altura, probs = seq(0, 1, 1/10))           # Decis

# Medidas de dispersão
diff(sapply(h, range))
sapply(h, var)
sapply(h, sd)


# Instalando os pacotes necessários (rodar uma vez)
# No terminal, sudo R
# packs <- c('tidyverse','maps','nycflights13')
# install.packages(packs, dep = T)
# update.packages(ask = F)
library(ggplot2)
library(maps)
library(dplyr)


# Gráficos ggplot2 são baseados em 'The Grammar of Graphics' 
# de Wilkinson (2005), e possuem 3 componentes:
# 1. dados
# 2. atributos estéticos (aesthetic) como cor, forma e tamanho e
# 3. pelo menos uma camada de objetos geométricos (geom) como pontos,
# linhas e barras.


# gráfico básico, primeiras duas posições de aes são x e y
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()

# base R
plot(mpg$displ, mpg$hwy)


# colorindo por classe
ggplot(mpg, aes(displ, hwy, colour = class)) +
  geom_point()

# base R
plot(mpg$displ, mpg$hwy, 
     col = c('red','blue','green','yellow2','seagreen2','tomato1','tan1')[as.factor(mpg$class)])


# quebrando por classe com facet_wrap()
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~class)

# adicionando linha de tendência via (ainda não documentada) predictdf
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth()

# adicionando linha de tendência linear
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(method = 'lm')



##
# Séries Temporais

# base R
plot(economics$date, economics$unemploy/economics$pop, type = 'l')
plot(economics$date, economics$uempmed, type = 'l')

# séries temporais com gráficos de linha
ggplot(economics, aes(date, unemploy / pop)) + geom_line()
ggplot(economics, aes(date, uempmed)) + geom_line()


# colorindo uma série temporal
presidential <- subset(presidential, start > economics$date[1])
ggplot(economics) +
  geom_rect(
    aes(xmin = start, xmax = end, fill = party),
    ymin = -Inf, ymax = Inf, alpha = 0.2,
    data = presidential
  ) + geom_vline(
    aes(xintercept = as.numeric(start)),
    data = presidential,
    colour = 'grey50', alpha = 0.5
  ) + geom_text(
    aes(x = start, y = 2500, label = name),
    data = presidential,
    size = 3, vjust = 0, hjust = 0, nudge_x = 50
  ) +
  geom_line(aes(date, unemploy)) + scale_fill_manual(values = c('blue', 'red'))


# função de previsão
source('~/../Downloads/fits.R')
fits(economics$pce, graf = T)
fits(economics$pop, graf = T)
fits(economics$psavert, graf = T)
fits(economics$uempmed, graf = T)
fits(economics$unemploy, graf = T)
fits(economics$unemploy/economics$pop, graf = T)




# boxplot e afins
boxplot(mpg$hwy ~ mpg$drv)                  # boxplot base R
ggplot(mpg, aes(drv, hwy)) + geom_boxplot() # boxplot clássico
ggplot(mpg, aes(drv, hwy)) + geom_point()   # exato
ggplot(mpg, aes(drv, hwy)) + geom_jitter()  # adiciona um ruído para evitar sobreposição
ggplot(mpg, aes(drv, hwy)) + geom_violin()  # representação compacta da 'densidade'


# histogramas e polígonos de frequência
hist(mpg$hwy)
ggplot(mpg, aes(hwy)) + geom_histogram()
ggplot(mpg, aes(hwy)) + geom_freqpoly()

# colorindo por tração (drv)
ggplot(mpg, aes(displ, colour = drv)) +
  geom_freqpoly(binwidth = 0.5)

ggplot(mpg, aes(displ, fill = drv)) +
  geom_histogram(binwidth = .2) +
  facet_wrap(~drv, ncol = 1)



# histograma somente de um vetor
# https://stackoverflow.com/questions/31173889/one-vector-to-dataframe-for-use-in-ggplot-histogram-in-r
xn <- rnorm(1000)
data <- as.data.frame(xn)
main <- 'Título'
ggplot(data, aes(x=xn)) +
  geom_histogram(binwidth = .1) +
  ggtitle(main)


# ggplot code, boxplot de vetor
set.seed(1); x <- rchisq(100,1)
mydata <- data.frame(x)
names(mydata) <- c('x')
qplot(y=mydata$x, x= 1, geom = "boxplot") + 
  geom_hline(yintercept=mean(mydata$x))


# gráfico de barras
ggplot(mpg, aes(manufacturer)) +
  geom_bar()

# alterando o ângulo dos rótulos de x com theme()
ggplot(mpg, aes(manufacturer)) +
  geom_bar() + 
  theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0))

# colorindo por grupo (colunas)
df <- data.frame(dose=c("D0.5", "D1", "D2"),
                 len=c(4.2, 10, 29.5))
p <- ggplot(df, aes(x=dose, y=len, fill=dose)) +
  geom_bar(stat="identity")+theme_minimal()
p

# colorindo por classe (colunas empilhadas)
ggplot(mpg, aes(manufacturer, fill = drv)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0))

# colorindo com variável contínua (degradê)
ggplot(mpg, aes(manufacturer, fill = hwy, group = hwy)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0))

# modificando os eixos
ggplot(mpg, aes(cty, hwy)) +
  geom_point() +
  xlab('cidade') +
  ylab('estrada')

# remova os rótulos dos eixos com NULL
ggplot(mpg, aes(cty, hwy)) +
  geom_point() +
  xlab(NULL) +
  ylab(NULL)

# xlim() and ylim() modificam os limites dos eixos
ggplot(mpg, aes(drv, hwy)) +
  geom_jitter(width = 0.25) +
  xlim('f', 'r') +
  ylim(20, 30)

# para escalas contínuas, use NA para ajustar somente um limite
ggplot(mpg, aes(drv, hwy)) +
  geom_jitter(width = 0.25, na.rm = TRUE) +
  ylim(NA, 30)

# dois eixos
# https://rpubs.com/MarkusLoew/226759

# pode-se atribuir um gráfico a um objeto para manipulação posterior
p <- ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) + 
  geom_point()  + 
  labs(colour = 'texto')
print(p)
p + facet_wrap(~drv)

# salva png no disco
getwd()
ggsave('plot.png', width = 5, height = 5)
dir()


# descreva a estrutura brevemente com summary()
summary(p)

# salve o objeto completo com saveRDS()
saveRDS(p, 'plot.rds')
q <- readRDS('plot.rds')


# gráficos rápidos: qplot()
qplot(displ, hwy, data = mpg)
qplot(displ, data = mpg)


# gráficos básicos
df <- data.frame(
  x = c(3, 1, 5),
  y = c(2, 4, 6),
  label = c("a","b","c")
)

p <- ggplot(df, aes(x, y, label = label)) +
  labs(x = NULL, y = NULL) + # Oculta os eixos
  theme(plot.title = element_text(size = 12)) # Comprime o título do gráfico

# geom_point() produz um gráfico de dispersão
p + geom_point() + ggtitle('point')

# geom_text() adiciona texto diretamente ao gráfico
p + geom_text() + ggtitle('texto')

# geom_bar() produz um gráfico de barra
p + geom_bar(stat = 'identity') + ggtitle('bar') 

# geom_tile() funciona como geom_rect() e geom_raster(), desenhando retângulos mas parametrizadas de forma diferente
p + geom_tile() + ggtitle('raster')

# geom_line() produz um gráfico de linha, conectando os pontos da esquerda para a direta
p + geom_line() + ggtitle('line')

# geom_area() produz um gráfico de área, que é um gráfico de linha preenchido no eixo y
p + geom_area() + ggtitle('area')

# geom_path() produz um gráfico de linha, conectando os pontos na ordem que aparacem nos dados (caminho/path)
p + geom_path() + ggtitle('path')

# geom_polygon() desenha polígonos, que são caminhos (path) preenchidos
p + geom_polygon() + ggtitle('polygon')


# rotulagem

# tipos de fontes (family)
df <- data.frame(x = 1, y = 3:1, family = c('sans', 'serif', 'mono')) 
ggplot(df, aes(x, y)) +
  geom_text(aes(label = family, family = family))

# fontface
df <- data.frame(x = 1, y = 3:1, face = c('plain', 'bold', 'italic'))
ggplot(df, aes(x, y)) +
  geom_text(aes(label = face, fontface = face))


# alinhamento
df <- data.frame(
  x = c(1, 1, 2, 2, 1.5),
  y = c(1, 2, 1, 2, 1.5),
  text = c(
    'bottom-left', 'bottom-right',
    'top-left', 'top-right', 'center'
  )
)
ggplot(df, aes(x, y)) +
  geom_text(aes(label = text))
ggplot(df, aes(x, y)) +
  geom_text(aes(label = text), vjust = 'inward', hjust = 'inward')


# empurrãozinho (nudge)
df <- data.frame(trt = c("a", "b", "c"), resp = c(1.2, 3.4, 2.5)) 
ggplot(df, aes(resp, trt)) +
  geom_point() +
  geom_text(aes(label = paste0("(", resp, ")")), nudge_y = -0.25) + xlim(1, 3.6)


# verificar sobreposição
ggplot(mpg, aes(displ, hwy)) +
  geom_text(aes(label = model)) +
  xlim(1, 8)
ggplot(mpg, aes(displ, hwy)) +
  geom_text(aes(label = model), check_overlap = TRUE) + xlim(1, 8)


# rótulos (labels)
# install.packages('directlabels',dep=T)
library(directlabels)
ggplot(mpg, aes(displ, hwy, colour = class)) + 
  geom_point(show.legend = FALSE) + 
  directlabels::geom_dl(aes(label = class), method = 'smart.grid')


# gráficos de superfície
ggplot(faithfuld, aes(eruptions, waiting)) + 
  geom_contour(aes(z = density, colour = ..level..))
ggplot(faithfuld, aes(eruptions, waiting)) + 
  geom_raster(aes(fill = density))


# mapas
mi_counties <- map_data('county', 'michigan') %>% 
  select(lon = long, lat, group, id = subregion)
head(mi_counties)

ggplot(mi_counties, aes(lon, lat)) +
  geom_polygon(aes(group = group)) +
  coord_quickmap()
ggplot(mi_counties, aes(lon, lat)) +
  geom_polygon(aes(group = group), fill = NA, colour = 'grey50') + 
  coord_quickmap()

# metadados em mapas
mi_census <- midwest %>%
  tbl_df() %>%
  filter(state == 'MI') %>%
  mutate(county = tolower(county)) %>%
  select(county, area, poptotal, percwhite, percblack)
mi_census

census_counties <- left_join(mi_census, mi_counties, by = c('county' = 'id')) 
census_counties

ggplot(census_counties, aes(lon, lat, group = county)) +
  geom_polygon(aes(fill = poptotal)) +
  coord_quickmap()

ggplot(census_counties, aes(lon, lat, group = county)) +
  geom_polygon(aes(fill = percwhite)) +
  coord_quickmap()


# intervalos verticais
y <- c(18, 11, 16)
df <- data.frame(x = 1:3, y = y, se = c(1.2, 0.5, 1.0))
base <- ggplot(df, aes(x, y, ymin = y - se, ymax = y + se)) 

base + geom_crossbar()
base + geom_errorbar()

base + geom_pointrange()
base + geom_linerange()

base + geom_smooth(stat = 'identity')
base + geom_ribbon()


# ponderando

# não ponderado
ggplot(midwest, aes(percwhite, percbelowpoverty)) + geom_point()

# ponderado pelo tamanho da população
ggplot(midwest, aes(percwhite, percbelowpoverty)) + 
  geom_point(aes(size = poptotal / 1e6)) + 
  scale_size_area('Population\n(millions)', breaks = c(0.5, 1, 2, 4))


# lidando com a sobreposição
df <- data.frame(x = rnorm(2000), y = rnorm(2000))
norm <- ggplot(df, aes(x, y)) + xlab(NULL) + ylab(NULL) 

norm + geom_point()
norm + geom_point(shape = 1)    # círculos vazados
norm + geom_point(shape = ".")  # 1 pixel

norm + geom_point(alpha = 1/3)  # alterando a transparência
norm + geom_point(alpha = 1/5)
norm + geom_point(alpha = 1/10)

norm + geom_bin2d()
norm + geom_bin2d(bins = 10)

# install.packages('hexbin', dep=T)
norm + geom_hex()
norm + geom_hex(bins = 10)


# exemplo usando group_by()
library(nycflights13)
by_tailnum <- group_by(flights, tailnum)
delay <- summarise(by_tailnum,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dist < 2000)

ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()


# Exercícios

# 1. Leia o artigo http://flowingdata.com/2015/09/23/years-you-have-left-to-live-probably/

# 2. O UBER cobra um preço base de R$ 2.30, R$ 1.20 por km rodado e R$ 0.20 por minuto 
# decorrido durante a viagem. a) Crie um gráfico de dispersão univariado com km por preço, 
# e depois outro cruzando tempo com preço. b) Crie um gráfico 3D com as três informações. Use
# as funções persp e rgl::plot3d.

# 2a
fkm <- function(km){
  print(preco <- 2.3 + 1.2*km)
}
fkm(10)

# base R
plot(fkm(0:10), type = 'l')

# ggplot2
ggplot(data.frame(x=c(0, 10)), aes(x)) + 
  stat_function(fun=fkm) +
  xlab('km') +
  ylab('preço/km')


fte <- function(tempo){
  print(preco <- 2.3 + .2*tempo)
}
fte(10)

# base R
plot(fte(0:10), type = 'l')

# ggplot2
ggplot(data.frame(x=c(0, 10)), aes(x)) + 
  stat_function(fun=fkm) +
  xlab('km') +
  ylab('preço/km')


# função completa
install.packages('rgl',dep=T)
library(rgl)
uber <- function(km,tempo){
  print(preco <- 2.3 + 1.2*km + .2*tempo)
  # print(data.frame(km = km, tempo = tempo, preco = preco))
}

uber(0:10,0:10)
ueg <- uber(expand.grid(0:10, 0:10), expand.grid(0:10, 0:10))
rgl::plot3d(ueg)


# gerando superfície
x <- 0:10
y <- 0:10
z <- uber(x,y)

fit <- lm(y ~ x + z)
grid.lines = 26
x.pred <- seq(0, 10, length.out = grid.lines)
y.pred <- seq(0, 10, length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(uber(x.pred,y.pred), 
                 nrow = grid.lines, ncol = grid.lines)


# persp()
persp(x.pred, y.pred, z.pred, theta = -30, phi = 30, 
      xlab = 'km', ylab = 'tempo', zlab = 'R$')


# adicionando superfície de regressão
rgl::rgl.surface(x.pred, y.pred, z.pred, color = "steelblue", 
                 alpha = 0.5, lit = FALSE)  
# adicionando linhas de grade
rgl::rgl.surface(x.pred, y.pred, z.pred, color = "black",
                 alpha = 0.5, lit = FALSE, front = "lines", back = "lines")


persp3d(uber, xlim = c(0,10), ylim = c(0,10), xlab = 'km', ylab = 'tempo', zlab = 'R$')


# 3. Acesse o link http://archive.ics.uci.edu/ml/datasets.html e escolha um conjunto de dados.
# a. Descreva brevemente o conjunto selecionado, utilizando algumas estatísticas descritivas vistas nesta lição.
# b. Crie visualizações para o conjunto escolhido, destacando pontos que considere relevante.
# c. Envie seu código para filipe.zabala@pucrs.br no formato T1_NOME_SOBRENOME.R


# =)