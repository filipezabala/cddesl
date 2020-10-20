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
# 2 Criando e manipulando funções
# 3 Manipulando dados com dplyr and tidyr  <-- 
# 4 Estatística	descritiva, visualização e séries temporais
# 5 Probabilidade
# 6 Inferência
# 7 Tópicos em Modelos Lineares Generalizados
# 8 Aprendizagem de máquina


###############################################
### 3 Manipulando dados com dplyr and tidyr ###
###############################################
packs <- c('tidyverse','nycflights13')
install.packages(packs,dep=T)

library(tidyverse)
library(nycflights13)


###
## dplyr
# https://dplyr.tidyverse.org/

# tibble, o data frame moderno
vignette('tibble')


# desempenho
l <- replicate(26, sample(100), simplify = FALSE)
names(l) <- letters
class(l)

# https://en.wikipedia.org/wiki/Benchmark_(computing)
# install.packages('microbenchmark',dep=T)
microbenchmark::microbenchmark(
  as_tibble(l),
  as.data.frame(l)
)


# imprimindo na tela (printing)
head(volcano)
class(volcano)
as_tibble(volcano)
tbl_df(volcano)    # equivale a as_tibble



# obtendo subgrupos (subsetting)
df1 <- data.frame(x = 1:3, y = 3:1)
length(df1)  # em data.frame a função length() retorna o número de colunas
ncol(df1)
dim(df1)
class(df1[, 1:2])
class(df1[, 1])   # classe 'integer'

# mantém a classe
df2 <- as_tibble(df1)
class(df2[, 1:2])
class(df2[, 1])

class(df2[[1]])
class(df2$x)


# tibbles não fazem captura parcial
df <- data.frame(abc = 1)
df$a

df2 <- tibble(abc = 1)
df2$a
df2$abc

# referência a variáveis recém criadas
data.frame(
  x1 = 1:5, 
  y1 = 1, 
  z1 = x1^2 + y1
)

tibble(
  x2 = 1:5, 
  y2 = 1, 
  z2 = x2^2 + y2
)


# tibbles permitem nomes não válidos no R
df <- data.frame(
  `:)` = 'smile', 
  ` ` = 'space',
  `2000` = 'number'
)
df

tb <- tibble(
  `:)` = 'smile', 
  ` ` = 'space',
  `2000` = 'number'
)
tb


# tribble: 'tr'ansposed tibble
tribble(
  ~x, ~y, ~z,
  #--|--|----
  'a', 2, 3.6,
  'b', 1, 8.5
)


###
## magrittr forward-pipe operator: %>%
#

# A %>% B indica o valor A como entrada da função B
head(iris)
iris %>% head

# pode-se ajustar apenas os parâmetros da função
iris %>% head(10)

# criando dois vetores e calculando a distância euclidiana entre eles
x1 <- 1:5; x2 <- 2:6
sqrt(sum((x1-x2)^2))

# piping
(x1-x2)^2 %>% sum() %>% sqrt()

# usando o 'dot place-holder'
'Encontrando o perdão e substituindo.' %>% gsub('perdão', 'padrão', .)

# quando o ponto está aninhado, o valor ainda vem primeiro
set.seed(1); sample(1:10) %>% paste0(LETTERS[.])
set.seed(1); sample(1:10) %>% {LETTERS[.]} %>% paste0
set.seed(1); aa <- sample(1:10); paste0(aa, LETTERS[aa])


# dplyr fornece uma função para cada verbo na manipulação de dados

# filter(), slice()
# arrange()
# select(), rename()
# distinct()
# mutate(), transmute()
# summarise()
# sample_n(), sample_frac()

library(nycflights13)
flights

# filter()
filter(flights, month == 1, day == 1)
flights %>%
  filter(month == 1, day == 1)

table(filter(flights, month == 1 | month == 2)$month)
table(flights$month)

# base R
flights[flights$month == 1 & flights$day == 1, ]


# slice()
slice(flights, 1:11)
flights %>% 
  print(n = 7)
flights %>% 
  print(n = 11, width = Inf)

# base R
flights[1:11,]


# arrange() é similar a filter(), mas reordena as colunas ao invés de filtrá-las
arrange(flights, year, month, day)
flights %>% arrange(year, month, day)
arrange(flights, desc(arr_delay))

# base R
flights[order(flights$year, flights$month, flights$day), ]
flights[order(flights$arr_delay, decreasing = TRUE), ]
flights[order(-flights$arr_delay), ]


# select() seleciona colunas
select(flights, year, month, day)
select(flights, year:day)         # note year:day
select(flights, -(year:day))
select(flights, starts_with('a'))
select(flights, ends_with('e'))
select(flights, contains('el'))
select(flights, contains('tail'))
flights %>% select_if(is.character) 

flights %>%
  select(matches('rr|ar')) %>%
  head(2)


# abordagem base R para selecionar as colunas carrier e dep_delay, e ordenar por dep_delay
flights[order(flights$dep_delay), c('carrier', 'dep_delay')]

# abordagem dplyr
flights %>%
  select(carrier, dep_delay) %>%
  arrange(dep_delay)

# use 'desc' para descendente
flights %>%
  select(carrier, dep_delay) %>%
  arrange(desc(dep_delay))

# método aninhado para selecionar as colunas carrier e dep_delay e filtrar por atrasos maiores que 60 minutos
filter(select(flights, carrier, dep_delay), dep_delay > 60)

flights %>%
  select(carrier,dep_delay) %>%
  filter(dep_delay > 60)

# rename() para renomear colunas
flights
rename(flights, tail_num = tailnum)
select(flights, contains('tail'))


# distinct() encontra valores únicos na tabela
dim(distinct(flights, tailnum))

# base::unique()
length(unique(flights$tailnum))


# mutate() para adicionar colunas, permitindo utilizar as colunas recém criadas, mas não guarda
mutate(flights,
       gain = arr_delay - dep_delay,
       gain_per_hour = gain / (air_time / 60),
       speed = distance / air_time * 60)

flights %>% 
  mutate(gain = arr_delay - dep_delay,
         gain_per_hour = gain / (air_time / 60),
         speed = distance / air_time * 60)

# base::transform() não permite utilizar as colunas recém criadas, e note o desempenho
transform(flights,
          gain = arr_delay - dep_delay,
          # gain_per_hour = gain / (air_time / 60),
          speed = distance / air_time * 60)

# transmute() mantém apenas as variáveis criadas
transmute(flights,
          gain = arr_delay - dep_delay,
          gain_per_hour = gain / (air_time / 60),
          speed = distance / air_time * 60
)


# sumarize()
summarise(flights,
          delay = mean(dep_delay, na.rm = TRUE))


# sample_n() retira uma amostra para um número fixo
sample_n(flights, 10) # sem reposição
flights %>% sample_n(10)
sample_n(flights, 10, replace = TRUE)  # com reposisção

# sample_frac() retira uma amostra para uma fração fixa
sample_frac(flights, 0.01)
flights %>% sample_frac(0.01)


# cria uma tabela
# agrupando por mean
dmnds <- aggregate(price ~ clarity + color, diamonds, mean)
xtabs(price ~ ., data = dmnds)
# agrupando por length
dmnds <- aggregate(price ~ clarity + color, diamonds, length)
xtabs(price ~ ., data = dmnds)
# agrupando por length apenas por 'color'
dmnds <- aggregate(price ~ color, diamonds, length)
xtabs(price ~ ., data = dmnds)


# group_by()
by_tailnum  <- group_by(flights, tailnum)
delay <- summarise(by_tailnum,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dist < 2000)
delay

# abordagem base R para calcular o atraso médio para cada destino
head(with(flights, tapply(arr_delay, dest, mean, na.rm=TRUE)))
head(aggregate(arr_delay ~ dest, flights, mean))

# abordagem dplyr: crie uma tabela agrupada por dest, e resuma cada grupo calculando a média por arr_delay
flights %>%
  group_by(dest) %>%
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE))


# para cada companhia, calculando os atrasos mínimos e máximos nas chegadas e partidas
flights %>%
  group_by(carrier) %>%
  summarise_each(funs(min(., na.rm=TRUE), max(., na.rm=TRUE)), matches('delay'))


# para cada dia do ano, contando o total de voos e ordenando de forma descrescente por flight_count
flights %>%
  group_by(month, day) %>%
  summarise(flight_count = n()) %>%
  arrange(desc(flight_count))

# mais simples com a função 'tally'
flights %>%
  group_by(month, day) %>%
  tally(sort = TRUE)


# para cada destino, contando o total de voos e o número de avioões diferentes
flights %>%
  group_by(dest) %>%
  summarise(flight_count = n(), plane_count = n_distinct(tailnum))

# para cada destino, mostrando o número de voos por mês
flights %>%
  group_by(dest) %>%
  select(month) %>%
  table() %>%
  head()


# para cada companhia, calculando qual dois dias do ano tiveram os maiores atrasos nas partidas
# note: smallest (not largest) value is ranked as 1, so you have to use `desc` to rank by largest value
flights %>%
  group_by(carrier) %>%
  select(month, day, dep_delay) %>%
  filter(min_rank(desc(dep_delay)) <= 2) %>%
  arrange(carrier, desc(dep_delay))

# mais simples com a função 'top_n'
flights %>%
  group_by(carrier) %>%
  select(month, day, dep_delay) %>%
  top_n(2) %>%
  arrange(carrier, desc(dep_delay))


# para cada mês, calculando o número de voos e a mudança do mês anterior
flights %>%
  group_by(month) %>%
  summarise(flight_count = n()) %>%
  mutate(change = flight_count - lag(flight_count))

# mais simples com a função 'tally'
flights %>%
  group_by(month) %>%
  tally() %>%
  mutate(change = n - lag(n))


###
## combinando conjuntos de dados
# https://cran.r-project.org/web/packages/dplyr/vignettes/two-table.html

a <- tibble(
  x1 = LETTERS[1:3], 
  x2 = 1:3
)

b <- tibble(
  x1 = LETTERS[c(1,2,4)], 
  x3 = c(T,F,T)
)

##
# mutating joins

# left_join(), une fazendo correspondência das linhas de b para a
left_join(a, b, by = 'x1')

# right_join(), une fazendo correspondência das linhas de a para b
right_join(a, b, by = 'x1')

# inner_join(), une os dados mantendo apenas as linhas comuns aos dois conjuntos
inner_join(a, b, by = 'x1')

# full_join(), une os dados mantendo todos os valores, todas as linhas
full_join(a, b, by = 'x1')


##
# filtering joins

# semi_join(), todas as linhas em a que tem correspondência com b
semi_join(a, b, by = 'x1')

# anti_join(), todas as linhas em a que não tem correspondência com b
anti_join(a, b, by = 'x1')


##
# operações com conjuntos

y <- tibble(
  x1 = LETTERS[1:3], 
  x2 = 1:3
)

z <- tibble(
  x1 = LETTERS[2:4], 
  x2 = 2:4
)

# intersect(), linhas que aparecem simultaneamente em y e z
intersect(y,z)

# union(), linhas que aparecem em pelo menos um dos conjuntos y e z
union(y, z)

# setdiff(y, z), linhas que aparecem em y mas não em z
setdiff(y, z)


##
# binding

# bind_rows(), acrescenta z a y como novas linhas
bind_rows(y, z)

# bind_cols(y, z), acrescenta z a y como novas colunas
bind_cols(y, z)


###
## tidyr
# http://tidyr.tidyverse.org/

# http://stackoverflow.com/questions/1181060
stocks <- data_frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)

# gather(), transforma colunas em linhas
# https://rstudio-pubs-static.s3.amazonaws.com/58498_dd3b603ba4fb4b469bb1c57b5a951c39.html#gather-function
gather(stocks, stock, price, -time)
stocks %>% gather(stock, price, -time) %>% print(n=30)

# separate(), separa uma coluna em múltiplas colunas
(sep_time <- separate(stocks, time, c('t1', 't2', 't3')))

# spread(), Spread rowns into columns
table2
spread(table2, country, count)
spread(table2, year, count)
spread(table2, type, count)

# unite(), une múltiplas colunas em uma
unite(sep_time, time2, t1:t3, sep = '-')


##
# para saber mais
browseVignettes(package = c('dplyr', 'tidyr'))


###
# Exercício

# 1) No prefácio de 'The R Inferno' (2011) Patrick Burns resume: 
# "If you are using R and you think you’re in hell, this is a map for you."
# http://www.burns-stat.com/pages/Tutor/R_inferno.pdf
# 
# Compare os códigos base R com as soluções do tidyverse


# pg. 9, a função unique retorna tamanhos estranhos, e distinct()?
(test <- c(.3, .4-.1, .5-.2, .6-.3, .7-.4) )
unique(test)
unique(tbl_df(test))
distinct(tbl_df(test), value)


# pg. 67, as classes são diferentes, e com tibble?
xmat <- array(1:4, c(2,2))
class(xmat)
class(xmat[1,])  # simple vector, not a matrix
class(xmat[1, , drop=FALSE]) # still a matrix

xmat2 <- as_tibble(xmat)
class(xmat2)                  # tbl
class(xmat2[1,])              # tbl
class(xmat2[1, , drop=FALSE]) # tbl


# pg. 95, 
(xdf <- data.frame(R=1:2, J=3:4, E=5:6, K=7:8))
subset(xdf, select=J:K)
subset(xdf, select=-E)

(xdf2 <- as_tibble(xdf))
subset(xdf2, select=J:K)
subset(xdf2, select=-E)
select(xdf2, J:K)
select(xdf2, -E)


# =)