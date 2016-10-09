measures.calc <-
function(r1,r0){
  rd <- r1 - r0
  rr <- r1 / r0
  or <- (r1/(1-r1)) / (r0/(1-r0))
  standerdized <- c(rd,rr,or)
  return(c(rd, rr, or))  
}
