form.table <-
function(Y1A1L1, Y1A0L1, Y1A1L0, Y1A0L0, 
                       Y0A1L1, Y0A0L1, Y0A1L0, Y0A0L0){
  dataL1 <- c(Y1A1L1, Y0A1L1, Y1A0L1, Y0A0L1)
  dataL0 <- c(Y1A1L0, Y0A1L0, Y1A0L0, Y0A0L0)
  L <- c(rep(1,4),rep(0,4))
  A <- rep(c(rep(1,2),rep(0,2)),2)
  Y <- rep(c(1,0),4)
  N <- c(dataL1,dataL0)
  data <- as.data.frame(cbind(L,A,Y,N))
  mL1 <- matrix(data$N[data$L==1],2,2)
  dimnames(mL1) <- list(c("Y1","Y0"),c("L1xA1","L1xA0"))
  mL0 <- matrix(data$N[data$L==0],2,2)
  dimnames(mL0) <- list(c("Y1","Y0"),c("L0xA1","L0xA0"))
  combm <- cbind(mL1, mL0)
  total <- colSums(combm)
  rbind(combm, total)
  m <- mL1 + mL0
  dimnames(m) <- list(c("Y1","Y0"),c("A1","A0"))
  mL1 <- as.data.frame(mL1)
  mL0 <- as.data.frame(mL0)
  
  #data[ nrow(data):1, ]
  return(list(grouped.data = data, crude.data=m, 
                            stratified.table = combm, 
                            condounder.exposed.data = mL1, 
                            condounder.unexposed.data = mL0))
}
