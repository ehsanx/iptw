standardization <-
function(form.table.object){
  mL1 <- form.table.object$condounder.exposed.data
  mL0 <- form.table.object$condounder.unexposed.data
  
  x1 <- mL1$L1xA1[1]
  y1 <- mL1$L1xA0[1]
  n1 <- sum(mL1$L1xA1)
  m1 <- sum(mL1$L1xA0)
  N1 <- n1+m1
  x2 <- mL0$L0xA1[1]
  y2 <- mL0$L0xA0[1]
  n2 <- sum(mL0$L0xA1)
  m2 <- sum(mL0$L0xA0)
  N2 <- n2+m2
  N <- N1+N2
  
  r1 <- (x1/n1*N1 + x2/n2*N2)/N
  r2 <- (y1/m1*N1 + y2/m2*N2)/N
  #  r1 <- mL1$L1xA1[1]/sum(mL1$L1xA1)*sum(mL1)/sum(mL0+mL1) + 
  #  mL0$L0xA1[1]/sum(mL0$L0xA1)*sum(mL0)/sum(mL0+mL1)
  #  r0 <- mL1$L1xA0[1]/sum(mL1$L1xA0)*sum(mL1)/sum(mL0+mL1) + 
  #  mL0$L0xA0[1]/sum(mL0$L0xA0)*sum(mL0)/sum(mL0+mL1)
  standerdized <- measures.calc(r1,r2)
  names(standerdized) <- c("sRD", "sRR", "sOR")
  
  r1 <- (x1+x2)/N
  r2 <- (n1*y1/m1 + n2*y2/m2)/N
  direct.standerdized <- measures.calc(r1,r2)
  names(direct.standerdized) <- c("d.sRD", "d.sRR", "d.sOR")
  
  r1 <- (m1*x1/n1 + m2*x2/n2)/N
  r2 <- (y1+y2)/N
  indirect.standerdized <- measures.calc(r1,r2)
  names(indirect.standerdized) <- c("id.sRD", "id.sRR", "id.sOR")
  
  
  return(list(standerdized.total = standerdized,
              direct.standerdized = direct.standerdized,
              indirect.standerdized = indirect.standerdized))
}
