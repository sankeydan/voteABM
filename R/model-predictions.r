model = function (
  n.indiv = 20,
  head.dept = pi*(1/4),
  time = 100,
  hypothesis = NULL ,#c("no.vote" , "sub.vote" , "all.vote")
  turning.time = 20,
  travel.time = 20,
  speed = 1.5,
  vote.var = 3000,
  topo = n.indiv-1,
  max.turn.rate = 0.1,
  plot = F){

  # model.predictions

  library(magick)
  library(circular)
  library(stringr)
  library(goats2)

  #objects


  # stopifnot
  if(n.indiv %% 2 != 0){
    stop("n.indiv must be even")
  }

  if( is.null(hypothesis)){
    stop( "specify hypothesis")
  }

  # function
  mov.av = function ( x, smooth, speed){
    #x = group.speed
    mv = rep(x[1],length(x))
    for ( i in (smooth+1):(length(x)-smooth)){
      mv[i] = mean(x[(i-smooth):(i+smooth)])
    }
    mv[(length(x)-smooth+1):length(x)] = speed
    return(mv)
  }

  #movement.initialise

  m.init = round( rnorm( n.indiv , 65, 10 ))
  m.init = ifelse ( m.init < 50 , 50,m.init)
  m.init = ifelse ( m.init > 79 , 79,m.init)

  # group speed
  speed.matix = matrix( NA , time, n.indiv)
  for ( i in 1:n.indiv){
    speed.matix[,i] = c(rep(0,m.init[i]),rep(speed,100-m.init[i]))
  }
  group.speed = rowMeans(speed.matix)
  smooth.speed = mov.av(group.speed,10,speed)

  # build dataframe
  dat = array( NA, c(time,4,n.indiv),
               dimnames = list(NULL,c("x","y","h","s"),NULL))

  #initialise positions/heading/speed
  dat[1,"x",] =  rnorm( n.indiv, 30,22)
  dat[1,"y",] =  rnorm( n.indiv, 10,22)
  heads = runif( n.indiv , 0 , 2*pi)
  dat[1,"h",] =  heads
  dat[ ,"s",] =  speed.matix

  # vote initialise
  if( hypothesis == "no.vote"){
    for(j in 1:n.indiv){
      dat[2:m.init[j],,j] = rep( dat[1,,j],each=m.init[j]-1)
    }
  } else {
    vote =  suppressWarnings (rvonmises( n.indiv , head.dept, vote.var ))
    # vote = ifelse ( vote > pi, (2*pi)-vote,vote)
    v.init = round( rnorm( n.indiv , 20, 7))
    v.init = ifelse ( v.init > 35 , 35 ,v.init)
    v.init = ifelse ( v.init < 10 , 10 ,v.init)
  }
  if( hypothesis == "sub.vote"){
    for ( j in 1:(n.indiv/2) ){
      #j=1
      dat[1:v.init[j],"h",j] = dat[1,"h",j]
      turn = circ.seq(dat[1,"h",j], vote[j],turning.time)
      dat[v.init[j]:(v.init[j]+turning.time-1),"h",j] = turn
      dat[(v.init[j]+turning.time-1):m.init[j],"h",j] = vote[j]
    }

    for(j in ((n.indiv/2)+1):n.indiv){
      dat[2:m.init[j],,j] = rep( dat[1,,j],each=m.init[j]-1)
    }
  }
  if( hypothesis == "all.vote"){
    for ( j in 1:n.indiv){
      #j =1
      dat[1:v.init[j],"h",j] = dat[1,"h",j]
      turn = circ.seq(dat[1,"h",j], vote[j],turning.time)
      dat[v.init[j]:(v.init[j]+turning.time-1),"h",j] = turn
      dat[(v.init[j]+turning.time-1):m.init[j],"h",j] = vote[j]
    }
  }

  for ( j in 1:n.indiv){
    dat[,"h",j] = ifelse( dat[,"h",j] > pi, -abs((2 * pi) - dat[,"h",j]),dat[,"h",j])
  }

  for ( i in 2:time){
    #i=63
    goat.pos = t(dat[(i-1),c("x","y"),])
    dis.mat = matrix(NA,nrow(goat.pos),nrow(goat.pos))
    for ( j in 1:nrow(goat.pos)){
      for ( k in 1:nrow(goat.pos)){
        if ( j != k){
          # j=4
          # k=6
          dist  = dis(goat.pos[j,"x"],
                      goat.pos[k,"x"],
                      goat.pos[j,"y"],
                      goat.pos[k,"y"])
          dis.mat[c(j,k),c(j,k)] = dist
        }
      }
    }
    diag(dis.mat) = NA
    for( j in 1:n.indiv){
      #j=12

      if ( is.na( dat[i,"h",j])){
        if ( dat[i,"s",j] != 0){
          or = order ( dis.mat[j,])[1:topo]
          neighbour.heads = circular(dat[(i-1),"h",or])
          neighbour.mean  = mean.circular(neighbour.heads)
          focal.head = dat[(i-1),"h",j]
          heading.diff = pi - abs(abs(neighbour.mean - focal.head) - pi)
          focal.head = ifelse ( heading.diff < max.turn.rate,neighbour.mean, focal.head)
          if (focal.head != neighbour.mean){
            focal.head.plus  = ifelse ( focal.head>0, focal.head + heading.diff,focal.head - heading.diff)
            focal.head.plus  = ifelse ( focal.head.plus > pi, -abs((2*pi) - focal.head.plus), focal.head.plus )
            focal.head.plus  = ifelse ( focal.head.plus < -pi, abs((2*pi)-abs(focal.head.plus)),focal.head.plus)
            focal.head.minus = ifelse ( focal.head<0, focal.head + heading.diff,focal.head - heading.diff)

            sign = c(1,-1)[which (circular(round(c(focal.head.plus,focal.head.minus),2))==round(neighbour.mean,2))]

            if( focal.head>0){
              focal.head = focal.head + (max.turn.rate*sign )
            } else {
              focal.head = focal.head - (max.turn.rate*sign)
            }
            focal.head = ifelse ( focal.head > pi, -abs((2*pi) - focal.head), focal.head )
            focal.head = ifelse ( focal.head < -pi, abs((2*pi)-abs(focal.head)),focal.head)

          }
        } else {
          focal.head = dat[(i-1),"h",j]
        }
        
        # turning with respect to previous timestep
        x = dat[i-1,"x",j]
        y = dat[i-1,"y",j]
        h = dat[i-1,"h",j]
        s = dat[i,"s",j]
        dat[i,"x",j] = x + s*sin(h)
        dat[i,"y",j] = y + s*cos(h)
        if ( length(dat[i,"h",j]) == length(focal.head)){
        dat[i,"h",j] = focal.head
        } else {
          dat[i,"h",j] = dat[(i-1),"h",j]
        }

      } else {
        x = dat[i-1,"x",j]
        y = dat[i-1,"y",j]
        h = dat[i-1,"h",j]
        s = dat[i,"s",j]
        dat[i,"x",j] = x + s*sin(h)
        dat[i,"y",j] = y + s*cos(h)
      }
    }
  }


  #decision par
  group.head = suppressWarnings( apply( dat[,"h",],1, function (x) {  mean.circular(x)}))
  decision.par = sqrt ( ((sin(group.head)+sin(group.head[time]))/2)^2 +
                          ((cos(group.head)+cos(group.head[time]))/2)^2)
  smooth.dec = mov.av(decision.par,5,1)



  # plot
  if ( plot){
    par(mfrow = c(1,1))
    plot( c(smooth.speed[2:i],rep(NA,time-i)), bty = "n" ,
          type = "l" , ylim = c(0,speed), yaxt = "n" , xaxt  = "n" ,
          ylab = "Group mean speed", xlab = "Time", lwd = 3)
    axis(1, at = c(0,time), labels = c("", ""),line = 1.5)
    axis(2, at = c(0,1), labels = c("", ""),line = 1.5)

    # plot decision par
    par(new=T)
    plot( c(smooth.dec,rep(NA,length(decision.par)-i) ),bty="n", type = "l"
          ,yaxt = "n" , ylim = c(decision.par[1],1), col = "red",
          ylab = "Decision parameter" , xaxt = "n" , xlab = "Time", lwd = 2)

    axis(4)
  }

  # Scaled between 0 and 1
  vec = smooth.dec
  vec2 = (vec - min(vec))
  scaled = vec2 / max(vec2)

  # return
  return ( list ( smooth.speed, smooth.dec, scaled ))
}










