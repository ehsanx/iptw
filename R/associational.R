associational <-
function(form.table.object){
  mL1 <- form.table.object$condounder.exposed.data
  mL0 <- form.table.object$condounder.unexposed.data
  m <- mL1 + mL0
  dimnames(m) <- list(c("Y1","Y0"),c("A1","A0"))
  Total <- colSums(m)
  m <- rbind(m,Total)
  m
  r1a <- m[1,1]/m[3,1]
  r0a <- m[1,2]/m[3,2]
  rd <- r1a - r0a
  rr <- r1a / r0a
  or <- (r1a/(1-r1a)) / (r0a/(1-r0a))
  associational <- c(rd,rr,or)
  names(associational) <- c("aRD", "aRR", "aOR")
  return(associational)
}
