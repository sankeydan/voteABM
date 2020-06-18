
circ.seq = function ( angle1, angle2, length){

  # angle2 = 5.12
  # angle1 = 0.54
  # length = 20

  if( angle1 > pi){
    wi=which.min(c(abs( angle1 -       angle2) ,
                   abs( angle1 - (2*pi)-angle2)))
    if(wi==1){
      turn =   seq(angle1,angle2,       length.out = length )
    }
    if(wi==2){
      turn =   seq(angle1,(2*pi)+angle2,length.out = length )
    }
  }
  if(angle1 < pi){
    wi=which.min(c(abs( angle1 -       angle2) ,
                   abs( -angle1 - ((2*pi)-angle2)) ))
    if(wi==1){
      turn =   seq(angle1,angle2,       length.out = length )
    }
    if(wi==2){
      turn =   seq(angle1,angle2-2*pi,length.out = length )
    }
  }

  turn = ifelse ( turn > 2*pi, turn-2*pi, turn)
  turn = ifelse ( turn < 0   , turn+2*pi, turn)
  turn
  return( turn)
}
