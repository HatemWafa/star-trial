


LinReg_back <- function(depvar, indpvar, data, threshold, numvar=NA) {
  repeat {
    if(!is.na(numvar)[1] & any((indpvar %in% numvar)==TRUE)) {
      facvar <- indpvar[-which(indpvar %in% numvar)]
      } else {facvar <- indpvar}
    lev <- apply(data[,facvar],2,table)  # identify levels of factor variables
    lev <- lapply(lev, length) %>% cbind
    vvv <- rownames(lev)[which(lev>2)]
    for(i in vvv){
      data[,i] <- as.factor(data[,i])
    }
    
    mdl <- lm(
      reformulate(indpvar, depvar), 
      data = data
    )
    tbl <- mdl %>% summary %>% .["coefficients"] %>% as.data.frame %>% .[-1,-3]
    names(tbl) <- c("coeff", "se", "pval")
    
    for(i in vvv){
      fac <- tbl[grep(paste0("^",i),rownames(tbl),value = TRUE),]
      fac <- fac[which(fac$pval==(fac$pval %>% min)),]
      
      rr <- grep(paste0("^",i),rownames(tbl))
      
      if(length(rr)>1) {
        tbl <- tbl[-rr[-1],]
        tbl[rr[1],] <- fac
        rownames(tbl)[rr[1]] <- i
      }
    }    
    
    
    ex <- which(tbl$pval==(tbl$pval %>% max))
    vardrop <- rownames(tbl)[ex]
    
    indpvar <- indpvar[-ex]
    
    if ((tbl$pval %>% max) <= threshold) {break}
  }
  
  
  # p <- c("", tbl$pval %>% sapply(pval))          # p-values
  p <- mdl %>% summary %>% .["coefficients"] %>% 
    as.data.frame %>% .[,4] %>% sapply(pval)     # p-values
  rc <- round(mdl %>% coef, digits = 2)          # regression coefficients
  ci <- round(mdl %>% confint, digits = 2)       # confidence intervals
  
  out <- cbind(
    paste0(rc, " (", ci[,1]," to ",ci[,2],")"),
    p
  )
  rownames(out) <- names(rc)
  colnames(out) <- c("Coeff (95% CI)", "P-value")
  
  return(out)
}










LinReg <- function(depvar, indpvar, data, round=2) {
  
  mdl <- lm(
    reformulate(indpvar, depvar), 
    data = data
  )
  
  
  # p <- c("", tbl$pval %>% sapply(pval))          # p-values
  p <- mdl %>% summary %>% .["coefficients"] %>% 
    as.data.frame %>% .[,4] %>% sapply(pval)     # p-values
  rc <- round(mdl %>% coef, digits = round)          # regression coefficients
  ci <- round(mdl %>% confint, digits = round)       # confidence intervals
  
  out <- cbind(
    paste0(rc, " (", ci[,1]," to ",ci[,2],")"),
    p
  )
  rownames(out) <- names(rc)
  colnames(out) <- c("Coeff (95% CI)", "P-value")
  
  return(out)
}
