CV = function(data,top = 30){
  sd = apply(data,1,sd)
  mean = apply(data,1,mean)
  res = sd/mean * 100
  xx <- rank(res)
  res <- res[xx<=(length(xx)-top)]
  return(res)
}

#CV for log transformed data
CVlog = function(data,top=30){
  sd=apply(data,1,sd)
  res = (exp(sd)-1)
  xx <- rank(res)
  res <- res[xx<=(length(xx)-top)]
  return(res)
}
