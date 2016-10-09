iptw.regression <-
function(form.table.object, type = "w"){
  options(warn = -1)
  require(survey)
  require(boot)
  if (type %in% c("0", "w", "sw", "wn", "swn", "swn2") == FALSE) stop ("Weight type incorrectly defined")
  md <- ungrouped.data(form.table.object)
  tr.base.model <- glm(A~1, family = binomial(logit), data = md)
  md$base[md$A == 1] <- predict.glm(tr.base.model, type = "response")[md$A == 1]
  md$base[md$A == 0] <- 1 - predict.glm(tr.base.model, type = "response")[md$A == 0]
  
  tr.full.model <- glm(A~L, family = binomial(logit), data = md)
  md$full[md$A == 1] <- predict.glm(tr.full.model, type = "response")[md$A == 1]
  md$full[md$A == 0] <- 1 - predict.glm(tr.full.model, type = "response")[md$A == 0]
  md$w <- 1/md$full
  md$sw <- md$base/md$full
  md$wn <- md$w/mean(md$w)
  md$swn <- md$sw/mean(md$sw)
  md$swn2 <- md$sw/sum(md$sw)
  
  if (type == "0") {md$weight <- rep(1,length(md$w))}
  if (type == "w") {md$weight <- md$w}
  if (type == "sw") {md$weight <- md$sw}
  if (type == "wn") {md$weight <- md$wn}
  if (type == "swn") {md$weight <- md$swn}
  if (type == "swn2") {md$weight <- md$swn2}
  
  rd.model <- svyglm(Y ~ A, design = svydesign(~ 1, weights = ~ weight, data = md))
  rd <- summary(rd.model)$coefficients[2,1]
  
  or.model <- glm(Y ~ A, data = md, family = binomial(), weights = weight)
  or <- exp(summary(or.model)$coefficients[2,1])
  
  rr.model <- glm(Y ~ A, data = md, family = binomial(log), weights = weight)
  rr <- exp(summary(rr.model)$coefficients[2,1])

  model.measure <- as.numeric(cbind(rd, rr, or))
  names(model.measure) <- c("IPTW-RD", "IPTW-RR", "IPTW-OR")
  return(model.measure)
}
