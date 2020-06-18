takestockDS  = function ( i, length,print){
  # length = 20001
  # print = 100
  sq = round( seq( 1, length, length.out = print))

  if ( i %in% sq){
    print ( paste (  which ( sq %in% i  ), "%"))
  }
}
