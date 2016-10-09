iptw <-
function(form.table.object, type = "w"){
  if (type %in% c("0", "w", "sw", "wn", "swn", "swn2") == FALSE) stop ("Weight type incorrectly defined")
  data <- form.table.object$grouped.data
  prA1L1 <- sum(data$N[data$A == 1 & data$L == 1]) / sum(data$N[data$L == 1])
  prA0L1 <- sum(data$N[data$A == 0 & data$L == 1]) / sum(data$N[data$L == 1])
  prA1L0 <- sum(data$N[data$A == 1 & data$L == 0]) / sum(data$N[data$L == 0])
  prA0L0 <- sum(data$N[data$A == 0 & data$L == 0]) / sum(data$N[data$L == 0])
  
  prA1L1a <- sum(data$N[data$A == 1]) / sum(data$N)
  prA0L1a <- sum(data$N[data$A == 1]) / sum(data$N)
  prA1L0a <- sum(data$N[data$A == 1]) / sum(data$N)
  prA0L0a <- sum(data$N[data$A == 1]) / sum(data$N)
  
  data$pr <- c(rep(prA1L1,2), rep(prA0L1,2), rep(prA1L0,2), rep(prA0L0,2))
  data$pr2 <- c(rep(prA1L1a,2), rep(prA0L1a,2), rep(prA1L0a,2), rep(prA0L0a,2))
  
  data$w <- 1/data$pr
  data$wn <- data$w*sum(data$N)/sum(data$w*data$N)
  data$sw <- data$pr2/data$pr
  data$swn <- data$sw*sum(data$N)/sum(data$sw*data$N)
  data$swn2 <- data$sw/sum(data$sw*data$N)
  
  data$Np.w <- data$N*data$w
  data$Np.wn <- data$N*data$wn
  data$Np.sw <- data$N*data$sw
  data$Np.swn <- data$N*data$swn
  data$Np.swn2 <- data$N*data$swn2
  
  if (type == "0") {
    data$Np <- data$N
    data$w <- rep(1, length(data$w))
    }
  if (type == "w") {
    data$Np <- data$Np.w
    data$w <- data$w
    }
  if (type == "sw") {
    data$Np <- data$Np.sw
    data$w <- data$sw
    }
  if (type == "wn") {
    data$Np <- data$Np.wn
    data$w <- data$wn
    }
  if (type == "swn") {
    data$Np <- data$Np.swn
    data$w <- data$swn
    }
  if (type == "swn2") {
    data$Np <- data$Np.swn2
    data$w <- data$swn2
    }
  data <- as.data.frame(data[c("L","A","Y","N", "w", "Np")])
  mL1 <- matrix(data$Np[data$L==1],2,2)
  dimnames(mL1) <- list(c("Y1","Y0"),c("L1xA1","L1xA0"))
  mL0 <- matrix(data$Np[data$L==0],2,2)
  dimnames(mL0) <- list(c("Y1","Y0"),c("L0xA1","L0xA0"))
  combm <- cbind(mL1, mL0)
  total <- colSums(combm)
  combm <- rbind(combm, total)
  m <- mL1 + mL0
  dimnames(m) <- list(c("Y1","Y0"),c("A1","A0"))
  Total <- colSums(m)
  m <- rbind(m,Total)
  r1 <- m[1,1]/m[3,1]
  r0 <- m[1,2]/m[3,2]
  rd <- r1 - r0
  rr <- r1 / r0
  or <- (r1/(1-r1)) / (r0/(1-r0))
  causal <- c(rd,rr,or)
  names(causal) <- c("IPTW-RD", "IPTW-RR", "IPTW-OR")
  return(list(IPTW.measures = causal, pseudo.data = data, crude.data=m, 
              stratified.table = combm))
}
