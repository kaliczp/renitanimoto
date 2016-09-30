taxa_nov = read.csv2("Taxa_Nov.csv", fileEncoding="latin2")
row.names(taxa_nov) <- taxa_nov[ ,1]
taxa_nov <- taxa_nov[,-1]
taxa_nov <- t(taxa_nov)

library(plyr)

tanimoto <- function(this, other) {
    ## Convert to presence absence
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

## Két különbözõ oszlopot kell megadni!!!
tanimoto(taxa_nov[,1],taxa_nov[,2])

tanimoto.index <- function(x){
  col.num <- ncol(x)
  index.mat <- matrix(rep(NA,col.num^2),nrow=col.num)
  colnames(index.mat) <- colnames(x)
  rownames(index.mat) <- colnames(x)
  for(sor in 1:col.num){
    for(oszlop in 1:col.num){
      index.mat[sor, oszlop] <- tanimoto(x[,sor],x[,oszlop])
    }
  }
  index.mat
}

tan.index.nov <- tanimoto.index(taxa_nov)
ossz.tan.dis.nov <- -log2(tan.index.nov)
ossz.tan.dis.nov <- as.dist(ossz.tan.dis.nov)

## Kluszter

hc.comp.nov <- hclust(ossz.tan.dis.nov) # complet
hc.ward.nov <- hclust(ossz.tan.dis.nov, "ward.D") # Ward
hc.single.nov <- hclust(ossz.tan.dis.nov, "single")

## távolság leírására
par(mar = c(5.1, 4.1, 4.1, 0.1))
plot(hc.comp.nov, cex = 0.5, ylab="", main="Complete")
identify(hc.comp.nov) #?identify.hclust
plot(hc.ward.nov, cex = 0.5, ylab="", main="Ward")

## Ha kell a nullára húzza a számokat
plot(hc.comp.nov, cex = 0.5, ylab="", main="Complete", hang=-0.1)
