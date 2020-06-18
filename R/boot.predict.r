boot.predict = function ( mat, hypothesis = NULL, n.decisions = 21, boot.perms1 = 1000, perms2 = 1000,sync = F){
  
  # vars
  
  # boot.perms1 = 1000
  # perms2 = 100
  # n.decisions = 21
  # l=1
  # hypothesis = "all.vote"
  # sync = F
  cormax      = vector()
  sym       = vector()
  cormax.sd = vector()
  sym.sd    = vector()
  for ( i in 1:perms2){
    col.cormax = ifelse ( sync, 6,4)
    col.sym    = ifelse ( sync, 7,5)
    
    df = as.data.frame( mat[mat[,3]==hypothesis,])
    
    # convert matrix
    boot.mat = matrix(NA,boot.perms1,2)
    sam = sample (1:nrow(df),n.decisions)
    cormax.21 = df[sam,col.cormax]
    sym.21    = df[sam,col.sym]
    for ( j in 1:boot.perms1){
      
      boot.mat[j,1] = median(sample(cormax.21,replace = T)) # median as sample is far from normal - try( hist(mat[mat[,3] == "no.vote",6])), clearly zero, but mean is high
      boot.mat[j,2] = median(sample(sym.21,replace = T  ))
      
    }
    cormax    = c(cormax    , mean ( boot.mat[,1]))
    sym       = c(sym       , mean ( boot.mat[,2]))
    cormax.sd = c(cormax.sd , sd(boot.mat[,1]))
    sym.sd    = c(sym.sd    , sd(boot.mat[,2]))
  }
  cormax      = mean(cormax    )
  sym       = mean(sym       )
  cormax.sd = mean(cormax.sd )
  sym.sd    = mean(sym.sd    )
  
  corCI = cormax + ((1.96*cormax.sd ) * c(-1,1) )
  symCI = sym    + ((1.96*sym.sd    ) * c(-1,1) )
  
  return( list ( 
    cormax= cormax      ,
    sym   = sym       ,
    corCI = corCI   ,
    symCI = symCI     ))
  
}
