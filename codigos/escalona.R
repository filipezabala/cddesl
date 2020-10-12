################################################
###  Uma Introdução à Ciência de dados no R  ###
###              Filipe J. Zabala            ###
###        www.estatisticaclassica.com       ###
###           filipe.zabala@pucrs.br         ###
###           2018-11-12 a 2018-11-21        ###
################################################

# install.packages('matlib',dep=T)
library(matlib)
?echelon
?gaussianElimination

# exemplo 1 (solucao unica)
(A <- matrix(c(1,3,-2,5, 3,5,6,7, 2,4,3,8), nrow=3, byrow=T))
echelon(A)
echelon(A, reduced = F)
echelon(A, verbose = T)

# exemplo 2 (infinitas solucoes)
(B <- matrix(c(-2,1,1,4, -4,2,2,8, -6,3,3,12), nrow=3, byrow=T))
echelon(B)
echelon(B, verbose = T)

# exemplo 3 (sem solucao)
(C <- matrix(c(1,-2,3,7, 2,3,-1,4, 4,1,3,1), nrow=3, byrow=T))
echelon(C)
echelon(C, verbose = T)

# funcao que indica o tipo da solucao, se for sistema (matriz r x (r+1))
escalona <- function(A, B, ...){
  library(matlib)
  sol <- echelon(A, B, ...)

  if(!is_square_matrix(sol)){
    smc <- rowSums(abs(sol[,1:(ncol(sol)-1)]))  # soma modulo coeficientes
    ti <- sol[,ncol(sol)] # termo independente
    
    if(prod(smc)!=0) {msg <- 'Solucao unica'}
    else if(prod(smc)==0 & prod(smc+abs(ti))==0) {msg <- 'Infinitas solucoes'}
    else msg <- 'Sem solucao'
  }
  return(list(sol=sol,msg=msg))
}

escalona(A, reduced = F)
escalona(B, verbose = T, fractions = T)
escalona(C, verbose = T, latex = T)
