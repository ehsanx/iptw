ungrouped.data <-
function(form.table.object){
  data <- form.table.object$grouped.data
  md <- NULL
  for (i in 1:8){
    md.add <- t(matrix(unlist(data[i,c("L","A","Y")]),3,as.vector(data[i,"N"])))
    md <- rbind(md,md.add)
  }
  colnames(md) <- c("L","A","Y")
  md <- as.data.frame(md)
  return(md)
}
