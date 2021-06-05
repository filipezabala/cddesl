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
# 5 Estatística	descritiva, visualização e séries temporais
# 6 Probabilidade
# 7 Inferência
# 8 Tópicos em Modelos Lineares Generalizados
# 9 Aprendizagem de máquina  <-- 




#################################
### 9 Aprendizagem de máquina ###
#################################
# https://edu.kpfu.ru/pluginfile.php/278552/mod_resource/content/1/MachineLearningR__Brett_Lantz.pdf


###
## Identificação textual
# http://www.scientificamerican.com/article/how-a-computer-program-helped-show-jk-rowling-write-a-cuckoos-calling/
# https://www.youtube.com/watch?v=j1V2McKbkLo
# http://www.labnol.org/software/wget-command-examples/28750/

# wget -r -A.htm http://obamaspeeches.com
# wget -r -A.htm http://www.americanrhetoric.com/speechbanka-f.htm
# wget -r -A.htm http://www.americanrhetoric.com/speechbankg-l.htm


##
# Init

# Clean memory
rm(list=ls())

# Libraries
library(tidyverse)
library(tm)
library(class)

# Set options
options(stringsAsFactors=F)

# Set parameters
authors <- c('barackobama', 'gwbush')
pathname <- '~/Dropbox/PUC/Extensão/2018-11 - ICDR1/dados/speeches/'

# Clean text
cleanCorpus <- function(corpus)
{
  corpus.tmp <- tm_map(corpus, stripWhitespace) # retirando espaços em branco
  corpus.tmp <- tm_map(corpus.tmp, content_transformer(tolower)) # transformando em minúsculas
  corpus.tmp <- tm_map(corpus.tmp, removePunctuation) # retirando pontuação
  corpus.tmp <- tm_map(corpus.tmp, removeNumbers) # retirando números
  corpus.tmp <- tm_map(corpus.tmp, removeWords, stopwords('english'))
  # myStopwords <- c(stopwords('portuguese'), "nao", "pag")
  # corpus.tmp <- tm_map(corpus.tmp, removeWords, myStopwords)
  return(corpus.tmp)
}

# Build TDM (TermDocumentMatrix)
generateTDM <- function(auth, path)
{
  s.dir <- sprintf("%s/%s", path, auth) # Concatenates path/auth
  s.cor <- Corpus(DirSource(directory = s.dir, encoding = "UTF-8"))

  s.cor.cl <- cleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl)
  s.tdm <- removeSparseTerms(s.tdm, 0.7)
  result <- list(name = auth, tdm = s.tdm)
}

tdm <- lapply(authors, generateTDM, path = pathname)
str(tdm)


# Attach name
bindAuthor2TDM <- function(tdm)
{
  s.mat <- t(data.matrix(tdm[['tdm']]))
  s.df <- as.data.frame(s.mat, stringsAsFactors = F)

  s.df <- cbind(s.df, rep(tdm[['name']], nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- 'targetAuthor'
  return(s.df)
}

authTDM <- lapply(tdm, bindAuthor2TDM)
str(authTDM)

# Stack
tdm.stack <- do.call(plyr::rbind.fill, authTDM)
tdm.stack[is.na(tdm.stack)] <- 0

M <- 1000
accuracy <- vector(length=M)
ini <- Sys.time()
for(i in 1:M){
  # Hold-out sample
  train.idx <- sample(nrow(tdm.stack), ceiling(nrow(tdm.stack) * 0.7))
  test.idx <- (1:nrow(tdm.stack))[-train.idx]
  sort(train.idx)
  sort(test.idx)

  # Model - KNN
  tdm.auth <- tdm.stack[,'targetAuthor']
  tdm.stack.nl <- tdm.stack[, !colnames(tdm.stack) %in% 'targetAuthor']
  knn.pred <- knn(tdm.stack.nl[train.idx, ], tdm.stack.nl[test.idx, ], tdm.auth[train.idx])

  # Accuracy
  (conf.mat <- table(Actual = tdm.auth[test.idx], "Predictions" = knn.pred))
  (accuracy[i] <- sum(diag(conf.mat)/length(test.idx) * 100))
  print(i/M)
}
Sys.time()-ini
par(mfrow=c(1,1))
hist(accuracy)
