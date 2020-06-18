# main
rm(list=ls())

# libraries
library(ggplot2)
library(voteABM) # Build if this comes up with an error
library(circular)
library(stringr)

#objects
loop = T
plot = F
n.iter = 1000
n.indiv = 10
hypotheses = c("no.vote", "sub.vote", "all.vote")
table =data.frame ( topo = rep(9,3), bias = c(0,3,1),hypothesis= hypotheses)



#loop
if ( loop){
  for ( l in 1:nrow(table)){
    t = Sys.time()
    for ( i in 1:n.iter){
      # l = 1
      mo = model ( n.indiv = n.indiv, hypothesis = table$hypothesis[l],plot =plot,
                   topo = table$topo[l], vote.var = table$bias[l],num.informed = 0,max.turn.rate = 0.75 )
      topo       = as.character(table$topo[l])
      bias       = as.character(table$bias[l])
      hypothesis = as.character(table$hypothesis[l])

      if( l==1 & i ==1){
        stats = c(topo,bias,hypothesis,mo$cormax,mo$sym,mo$cormax.sync,mo$sym.sync)
      }else {
        stats = rbind( stats, c(topo,bias,hypothesis,mo$cormax,mo$sym,mo$cormax.sync,mo$sym.sync))
      }
      print(i)
    }
    print(Sys.time()-t)
    print( paste ( l,"/",nrow(table)))
  }
  mat = as.data.frame(stats)
  for(  i in c(1,2,4,5,6,7)){
    mat[,i] = as.numeric(as.character(mat[,i]))
  }
  folderDS(c("Output", "stats"))
  save(mat,file = file.path ( PROJHOME , "Output","stats","stats.rda"))
} else {
  load(file.path ( PROJHOME , "Output","stats","stats.rda"))
}

par ( mfrow = c(2,3),mar = c(3,3,1,1))
for ( j in 1:3){
bp = boot.predict(mat,hypothesis=hypotheses[j],sync = F)
cormax= bp$cormax
sym   = bp$sym   
corCI = bp$corCI 
symCI = bp$symCI 


ma = max (c( corCI[2],symCI[2]))
mi = min (c( corCI[1],symCI[1]))
ymax = ifelse ( ma > 2, ma, 2)
plot( c( cormax,sym),xlim = c(0,3),ylim = c(-20,4), xaxt = "n",ylab="", xlab= "")
axis( 1, labels =  c("Cor", "Sym"), at = c(1,2),cex.axis=0.8)
abline ( h = 0, lty = 2)
segments( c(1,2), c(corCI[1],symCI[1]), y1 = c(corCI[2],symCI[2]))
}
