taxa_nov = read.csv2("Taxa_Nov.csv", fileEncoding="latin2")
##taxa_nov <- taxa_nov[,-c(1)]

row.names(taxa_nov)
row.names(taxa_nov) <- taxa_nov[ ,1]
library(plyr)

tanimoto <- function(this, other) {
  this <- this > 0
  other <- other > 0
  ## a - number of rows where both columns are 1
  ta <- sum(this & other)
  ## b - number of rows where this and not the other column is 1
  tb <- sum(this & (!other))
  ## c - number of rows where the other and not this column is 1
  tc <- sum((!this) & other)
  ## d - number of rows where both columns are 0
  td <- sum(!this & !other)
  (ta + td) / (ta+td+2*(tb+tc))
}

tanimoto(taxa_nov)
tanimoto(taxa_nov[,1],taxa_nov[,1])


tanimoto.dist <- function(x){
  col.num <- ncol(x)
  dist.mat <- matrix(rep(NA,col.num^2),nrow=col.num)
  colnames(dist.mat) <- names(x)
  rownames(dist.mat) <- names(x)
  for(sor in 1:col.num){
    for(oszlop in 1:col.num){
      dist.mat[sor, oszlop] <- tanimoto(x[,sor],x[,oszlop])
    }
  }
  dist.mat
}

tan.dist.nov<-tanimoto.dist(taxa_nov)