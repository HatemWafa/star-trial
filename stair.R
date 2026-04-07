



stair <- function(values, step=NULL, direction="x"){
  if(direction=="x"){
    s = step/2
    values <- sapply(values, function(x) {x=c(x-s, x+s)}) %>% as.vector
  }
  
  if(direction=="y"){
    values <- sapply(values, function(y) replicate(2,y)) %>% as.vector
  }
  
  return(values)
}