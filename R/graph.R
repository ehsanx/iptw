graph <-
function(form.table.object, type = "w"){
  if (type %in% c("0", "w", "sw", "wn", "swn", "swn2") == FALSE) stop ("Weight type incorrectly defined")
  data <- form.table.object$grouped.data
  #L
  prL0 <- sum(data$N[data$L==0])/sum(data$N)
  prL1 <- sum(data$N[data$L==1])/sum(data$N)
  prL <- c(prL0, prL1)
  data$prL <- rep(rev(prL),each=4)
  #LA
  L0A0 <- sum(data$N[data$A==0 & data$L==0])
  prL0A0 <- sum(data$N[data$A==0 & data$L==0])/sum(data$N[data$L==0])
  L0A1 <- sum(data$N[data$A==1 & data$L==0])
  prL0A1 <- sum(data$N[data$A==1 & data$L==0])/sum(data$N[data$L==0])
  L1A0 <- sum(data$N[data$A==0 & data$L==1])
  prL1A0 <- sum(data$N[data$A==0 & data$L==1])/sum(data$N[data$L==1])
  L1A1 <- sum(data$N[data$A==1 & data$L==1])
  prL1A1 <- sum(data$N[data$A==1 & data$L==1])/sum(data$N[data$L==1])
  LA <- c(L0A0, L0A1, L1A0, L1A1)
  LAt <- c(L0A0 + L0A1, L1A0 + L1A1)
  data$LA <- rep(rev(LA),each=2)
  data$LAt <- rep(rev(LAt),each=4)
  prLA <-c(prL0A0, prL0A1, prL1A0, prL1A1)
  prLAt <-c(prL0A0+prL0A1, prL1A0+prL1A1)
  data$prLA <- rep(rev(prLA),each=2)
  data$prLAt <- rep(rev(prLAt),each=4)
  prLA
  #LAY
  prL0A0Y0 <- sum(data$N[data$A==0 & data$L==0 & data$Y==0])/
    sum(data$N[data$L==0& data$A==0])
  prL0A0Y1 <- sum(data$N[data$A==0 & data$L==0 & data$Y==1])/
    sum(data$N[data$L==0& data$A==0])
  prL0A1Y0 <- sum(data$N[data$A==1 & data$L==0 & data$Y==0])/
    sum(data$N[data$L==0& data$A==1])
  prL0A1Y1 <- sum(data$N[data$A==1 & data$L==0 & data$Y==1])/
    sum(data$N[data$L==0& data$A==1])
  prL1A0Y0 <- sum(data$N[data$A==0 & data$L==1 & data$Y==0])/
    sum(data$N[data$L==1& data$A==0])
  prL1A0Y1 <- sum(data$N[data$A==0 & data$L==1 & data$Y==1])/
    sum(data$N[data$L==1& data$A==0])
  prL1A1Y0 <- sum(data$N[data$A==1 & data$L==1 & data$Y==0])/
    sum(data$N[data$L==1& data$A==1])
  prL1A1Y1 <- sum(data$N[data$A==1 & data$L==1 & data$Y==1])/
    sum(data$N[data$L==1& data$A==1])
  LAY <- c(prL0A0Y0, prL0A0Y1, prL0A1Y0, prL0A1Y1, prL1A0Y0, 
           prL1A0Y1, prL1A1Y0, prL1A1Y1)
  data$LAY <- rev(LAY)
  
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
  # yyy <- data[ nrow(data):1, ]
  data$Np.w <- data$N*data$w
  data$Np.wn <- data$N*data$wn
  data$Np.sw <- data$N*data$sw
  data$Np.swn <- data$N*data$swn
  data$Np.swn2 <- data$N*data$swn2
  data$Np <- data$Np.swn2
  
  if (type == "0") {data$Np <- data$N}
  if (type == "w") {data$Np <- data$Np.w}
  if (type == "sw") {data$Np <- data$Np.sw}
  if (type == "wn") {data$Np <- data$Np.wn}
  if (type == "swn") {data$Np <- data$Np.swn}
  if (type == "swn2") {data$Np <- data$Np.swn2}
  
  data <- round(data, 2)
  #x11()
  par(mfrow=c(1,1), mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(1:100,1:100, axes=F, xlab = "",ylab ="",type="n")
  x1 = 0; x2 = 20; y1 = 45; y2 = 55
  lines(c(x1,x1),c(y1,y2))
  lines(c(x2,x2),c(y1,y2))
  lines(c(x1,x2),c(y1,y1))
  lines(c(x1,x2),c(y2,y2))
  text(x1+9.5,y1+4.67, paste("L=1 (", data$prL[1], ")", sep = ""))
  x1 = x1; x2 = x2; y1 = y2; y2 = y2+10
  lines(c(x1,x1),c(y1,y2))
  lines(c(x2,x2),c(y1,y2))
  lines(c(x1,x2),c(y1,y1))
  lines(c(x1,x2),c(y2,y2))
  text(x1+9.5,y1+4.67, paste("L=0 (", data$prL[8], ")", sep = ""))
  text(75,98, paste("N"))
  text(81,98, paste("N*"))
  text(90,98, paste("f(A|L)"))
  text(98,98, paste("W"))
  
  x1 = 50; x2 = 70; y1 = 75; y2 = 85
  lines(c(x1,x1),c(y1,y2))
  lines(c(x2,x2),c(y1,y2))
  lines(c(x1,x2),c(y1,y1))
  lines(c(x1,x2),c(y2,y2))
  text(x1+9.5,y1+4.67, paste("Y=1 (", data$LAY[7], ")", sep = ""))
  x1 = x1; x2 = x2; y1 = y2; y2 = y2+10
  lines(c(x1,x1),c(y1,y2))
  lines(c(x2,x2),c(y1,y2))
  lines(c(x1,x2),c(y1,y1))
  lines(c(x1,x2),c(y2,y2))
  text(x1+9.5,y1+4.67, paste("Y=0 (", data$LAY[8], ")", sep = ""))
  lines(c(20,50),c(55,85))#x2,x1,y2,y1
  text(35,70, paste("Untreated (", data$prLAt[8], ")", sep = ""))
  text(35,66, paste(data$LA[5],"+",data$LA[7], "=",data$LAt[8]), col="red")
  text(75,90, paste(round(data$N[8])))
  text(75,80, paste(round(data$N[7])))
  text(81,90, paste(round(data$Np[8])), col="red")
  text(81,80, paste(round(data$Np[7])), col="red")
  text(90,90, paste(data$pr[8]))
  text(90,80, paste(data$pr[7]))
  text(98,90, paste(data$w[8]))
  text(98,80, paste(data$w[7]))
  
  x1 = 50; x2 = 70; y1 = 50; y2 = 60
  lines(c(x1,x1),c(y1,y2))
  lines(c(x2,x2),c(y1,y2))
  lines(c(x1,x2),c(y1,y1))
  lines(c(x1,x2),c(y2,y2))
  text(x1+9.5,y1+4.67, paste("Y=1 (", data$LAY[5], ")", sep = ""))
  x1 = x1; x2 = x2; y1 = y2; y2 = y2+10
  lines(c(x1,x1),c(y1,y2))
  lines(c(x2,x2),c(y1,y2))
  lines(c(x1,x2),c(y1,y1))
  lines(c(x1,x2),c(y2,y2))
  text(x1+9.5,y1+4.67, paste("Y=0 (", data$LAY[6], ")", sep = ""))
  lines(c(20,50),c(55,60))#x2,x1,y2,y1
  text(35,59, paste("Treated (", data$prLAt[6], ")", sep = ""))
  text(35,54, paste(data$LA[5],"+",data$LA[7], "=",data$LAt[6]), col="red")
  text(75,65, paste(round(data$N[6])))
  text(75,55, paste(round(data$N[5])))
  text(81,65, paste(round(data$Np[6])), col="red")
  text(81,55, paste(round(data$Np[5])), col="red")
  text(90,65, paste(data$pr[6]))
  text(90,55, paste(data$pr[5]))
  text(98,65, paste(data$w[6]))
  text(98,55, paste(data$w[5]))
  
  x1 = 50; x2 = 70; y1 = 25; y2 = 35
  lines(c(x1,x1),c(y1,y2))
  lines(c(x2,x2),c(y1,y2))
  lines(c(x1,x2),c(y1,y1))
  lines(c(x1,x2),c(y2,y2))
  text(x1+9.5,y1+4.67, paste("Y=1 (", data$LAY[3], ")", sep = ""))
  x1 = x1; x2 = x2; y1 = y2; y2 = y2+10
  lines(c(x1,x1),c(y1,y2))
  lines(c(x2,x2),c(y1,y2))
  lines(c(x1,x2),c(y1,y1))
  lines(c(x1,x2),c(y2,y2))
  text(x1+9.5,y1+4.67, paste("Y=0 (", data$LAY[4], ")", sep = ""))
  lines(c(20,50),c(55,35))#x2,x1,y2,y1
  text(35,45, paste("Untreated (", data$prLAt[4], ")", sep = ""))
  text(35,41, paste(data$LA[1],"+",data$LA[3], "=",data$LAt[4]), col="red")
  text(75,40, paste(round(data$N[4])))
  text(75,30, paste(round(data$N[3])))
  text(81,40, paste(round(data$Np[4])), col="red")
  text(81,30, paste(round(data$Np[3])), col="red")
  text(90,40, paste(data$pr[4]))
  text(90,30, paste(data$pr[3]))
  text(98,40, paste(data$w[4]))
  text(98,30, paste(data$w[3]))
  
  x1 = 50; x2 = 70; y1 = 0; y2 = 10
  lines(c(x1,x1),c(y1,y2))
  lines(c(x2,x2),c(y1,y2))
  lines(c(x1,x2),c(y1,y1))
  lines(c(x1,x2),c(y2,y2))
  text(x1+9.5,y1+4.67, paste("Y=1 (", data$LAY[1], ")", sep = ""))
  x1 = x1; x2 = x2; y1 = y2; y2 = y2+10
  lines(c(x1,x1),c(y1,y2))
  lines(c(x2,x2),c(y1,y2))
  lines(c(x1,x2),c(y1,y1))
  lines(c(x1,x2),c(y2,y2))
  text(x1+9.5,y1+4.67, paste("Y=0 (", data$LAY[2], ")", sep = ""))
  lines(c(20,50),c(55,10))#x2,x1,y2,y1
  text(35,30, paste("Treated (", data$prLAt[2], ")", sep = ""))
  text(35,26, paste(data$LA[3],"+",data$LA[1], "=",data$LAt[2]), col="red")
  text(75,15, paste(round(data$N[2])))
  text(75,5, paste(round(data$N[1])))
  text(81,15, paste(round(data$Np[2])), col="red")
  text(81,5, paste(round(data$Np[1])), col="red")
  text(90,15, paste(data$pr[2]))
  text(90,5, paste(data$pr[1]))
  text(98,15, paste(data$w[2]))
  text(98,5, paste(data$w[1]))
  
  # locator(n = 1, type = "n")
  return(extended.data=data)  
}
