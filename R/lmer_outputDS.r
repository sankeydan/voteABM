lmer_outputDS = function( lmer.object, rounder){
  # lmer.object = m50
  #rounder = 2
  sum = summary(lmer.object)
  CIs = confint(lmer.object)

  cb = cbind( n = nobs( lmer.object),  cof, CIs[dimnames(cof)[[1]],])
  for ( i in 2:6){
    cb[,i] = round(cb[,i], rounder)
  }
  cb = rbind( dimnames(cb)[[2]], cb)
  dimnames(cb)[[2]]= NULL
  return(cb)
}