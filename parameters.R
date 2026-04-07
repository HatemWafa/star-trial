is.even <- function(x) x %% 2 == 0
is.odd <- function(x) x %% 2 != 0


# Round P-values and point significance
## UPDATED April 2026
pval <- function(x) {
  if(is.nan(x)) {x="N/A"} else if (is.na(x)) {x=""} else if(is.character(x)) {
    if (x == "<0.001") {
      return("<0.001*")
    } else if (x == "<0.01") {
      return("<0.01*")
    } else {
      x <- suppressWarnings(as.numeric(x))
    }
  } else {
    options(scipen = 999)
    if (x < 0.0001) {x <- "<0.0001*"}
    else if (x >= 0.0001 & x < 0.001) {x <- paste0(round(x,4),"*")}
    else if (x >=0.001 & x < 0.05) {x <- paste0(round(x, 3),"*")}
    else (x <- round(x, 2))
    x <- x %>% as.character(x)
  }
  if(is.na(x)) {x=""}
  return(x)
}
## Old version
# pval <- function(x) {
#   if (is.character(x)) {
#     if (x == "<0.001") {
#       return("<0.001*")
#     } else if (x == "<0.01") {
#       return("<0.01*")
#     } else {
#       x <- as.numeric(x)
#     }
#   }
#   
#   if(is.nan(x)) {x="N/A"} else if (is.na(x)) {x=""} else {
#     options(scipen = 999)
#     if (x < 0.0001) {x <- "<0.0001*"}
#     else if (x >= 0.0001 & x < 0.001) {x <- paste0(round(x,4),"*")}
#     else if (x >=0.001 & x < 0.05) {x <- paste0(round(x, 3),"*")}
#     else (x <- round(x, 2))
#     x <- x %>% as.character(x)
#   }
#   return(x)
# }

# paste 95% confidence interval between parenthesis
paste95 <- function(Lo95, Hi95) paste0("(", Lo95, "-", Hi95, ")")
orci <- function(x) paste0(x[1], " (", x[2], "-", x[3], ")")



pp <- function(x) {
  if(x=="<0.0001*") {x <- "p<0.0001*"}
  else (x <- paste0("p=",x))
  
  return(x)
}



midline.decimal <- function(x) {gsub(".", "·", x, fixed=TRUE)}



aOR <- function(outcome, vars, data){
  pred <- vars[-match(outcome,vars)]
  
  #Univariate-------------------------------------------------------------------------
  pval02 <- function(x) {
    options(scipen = 999)
    if (x < 0.0001) {x <- "<0.0001*"}
    else if (x >= 0.0001 & x < 0.001) {x <- paste0(round(x,4),"*")}
    else if (x >=0.001 & x <= 0.2) {x <- paste0(round(x, 3),"*")}
    else (x <- round(x, 2))
    
    return(x)
  }
  
  fmla <- lapply(pred, function(x){ x <- as.formula(paste(outcome, x, sep = "~"))})
  m <- lapply(fmla, function(mdl) {mdl <- glm(mdl, data = data, family = "binomial")})
  
  m.uni <- lapply(m, function(uni) {s <- summary(uni)
  p <- s$coefficients[,4]
  p <- unlist(lapply(p, pval02))[-1]
  or <- round(exp(coef(uni)), digits = 2)[-1]
  ci <- round(exp(confint(uni)), digits = 2)[-1,]
  if(length(or)==1) {est <- paste0(or, " (",ci[1],"-", ci[2], ")")}
  if(length(or)!=1) {est <- paste0(or, " (",ci[,1],"-", ci[,2], ")")}
  tbl <- cbind(est,p)
  })
  m.uni <- abind(m.uni, along = 1)
  
  
  #Multivariate-----------------------------------------------------------------------
  sig <- lapply(m, function(sig) {sig <- summary(sig)
  sig <- sig$coefficients[-1,4]
  sig <- any(sig<=0.2)})
  sig <- unlist(sig)
  sig <- grep(TRUE, sig)
  
  pred <- pred[sig]
  pred <- paste(pred, collapse = "+")
  fmla <- as.formula(paste(outcome, pred ,sep = "~"))
  
  m <- glm(fmla, data = data, family = "binomial")
  s <- summary(m)
  p <- s$coefficients[,4]
  p <- unlist(lapply(p, pval))
  or <- round(exp(coef(m)), digits = 2)
  ci <- round(exp(confint(m)), digits = 2)
  est <- paste0(or, " (",ci[,1],"-", ci[,2], ")")
  m.multi <- cbind(est,p)[-1,]
  
  #Combined----------------------------------------------------------------------------
  m.uni <- cbind(m.uni, rep("",length(row.names(m.uni))), rep("",length(row.names(m.uni))))
  i <- match(row.names(m.multi),row.names(m.uni))
  m.uni[i,3:4] <- m.multi
  
  tbl <- m.uni
  colnames(tbl) <- c("OR (95%CI)", "Univariate", "OR (95%CI)", "Multivariate")
  
  return(tbl)
}



OR <- function(outcome, predictor, data){
  pval02 <- function(x) {
    options(scipen = 999)
    if (x < 0.0001) {x <- "<0.0001*"}
    else if (x >= 0.0001 & x < 0.001) {x <- paste0(round(x,4),"*")}
    else if (x >=0.001 & x <= 0.2) {x <- paste0(round(x, 3),"*")}
    else (x <- round(x, 2))
    
    return(x)
  }
  
  fmla <- lapply(predictor, function(x){ x <- as.formula(paste(outcome, x, sep = "~"))})
  m <- lapply(fmla, function(mdl) {mdl <- glm(mdl, data = data, family = "binomial")})
  
  m.uni <- lapply(m, function(uni) {s <- summary(uni)
  p <- s$coefficients[,4]
  p <- unlist(lapply(p, pval02))[-1]
  or <- round(exp(coef(uni)), digits = 2)[-1]
  ci <- round(exp(confint(uni)), digits = 2)[-1,]
  if(length(or)==1) {est <- paste0(or, " (",ci[1],"-", ci[2], ")")}
  if(length(or)!=1) {est <- paste0(or, " (",ci[,1],"-", ci[,2], ")")}
  tbl <- cbind(est,p)
  })
  m.uni <- abind(m.uni, along = 1)
  return(m.uni)
}





makeTransparent = function(..., alpha=0.5) {
  
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  
  alpha = floor(255*alpha)  
  newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)
  
  .makeTransparent = function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  
  newColor = apply(newColor, 2, .makeTransparent, alpha=alpha)
  
  return(newColor)
  
}








create_logical_dummies <- function(var, prefix = NULL) {
  # Convert input to factor if not already
  var <- as.factor(var)
  
  # Get unique levels
  levels_x <- levels(var)
  
  # Set prefix if not provided
  if (is.null(prefix)) {
    prefix <- deparse(substitute(var))
  }
  
  # Create logical dummy variables
  dummy_df <- setNames(
    as.data.frame(sapply(levels_x, function(lvl) var == lvl)),
    paste0(prefix, "_", make.names(levels_x))
  )
  
  return(dummy_df)
}







rescale <- function(old_values, old_min, old_max, new_min, new_max) {
  new_values <- (new_max - new_min) / (old_max - old_min) * (old_values - old_min) + new_min
  return(new_values)
}



insert_blank_above <- function(where, which, what,
                               on_not_found = c("skip", "append_end", "error"),
                               ignore_case = FALSE) {
  stopifnot(is.data.frame(where))
  on_not_found <- match.arg(on_not_found)
  
  # resolve column
  col_idx <- if (is.numeric(which)) {
    if (which < 1 || which > ncol(where)) stop("`which` index out of bounds.")
    which
  } else {
    match(which, names(where), nomatch = 0)
  }
  if (col_idx == 0) stop("Column `which` not found in `where`.")
  
  col <- where[[col_idx]]
  # prepare comparison vectors
  if (is.factor(col) || is.character(col)) {
    col_cmp <- as.character(col)
    what_cmp <- as.character(what)
    if (ignore_case) {
      col_cmp  <- toupper(col_cmp)
      what_cmp <- toupper(what_cmp)
    }
    # NA matching handled separately
    pos <- vapply(what_cmp, function(w) {
      if (is.na(w)) which(is.na(col))[1] else match(w, col_cmp, nomatch = NA_integer_)
    }, integer(1))
  } else {
    # numeric/dates/etc.: direct match
    pos <- vapply(what, function(w) {
      if (is.na(w)) which(is.na(col))[1] else {
        hit <- which(col == w)
        if (length(hit)) hit[1] else NA_integer_
      }
    }, integer(1))
  }
  
  # handle not-found according to policy
  if (anyNA(pos)) {
    if (on_not_found == "error") {
      missing_vals <- what[is.na(pos)]
      stop("Values not found in column: ", paste(unique(missing_vals), collapse = ", "))
    } else if (on_not_found == "append_end") {
      pos[is.na(pos)] <- nrow(where) + 1L  # append at end for each missing value
    } else { # "skip"
      pos <- pos[!is.na(pos)]
    }
  }
  
  if (!length(pos)) return(where)
  
  # ensure unique positions and insert bottom-up to avoid index shifts
  pos <- sort(unique(pos))
  blank <- where[0, , drop = FALSE]; blank[1, ] <- NA
  
  out <- where
  for (p in rev(pos)) {
    top <- if (p > 1) out[1:(p - 1), , drop = FALSE] else out[0, , drop = FALSE]
    bot <- out[p:nrow(out), , drop = FALSE]
    out <- rbind(top, blank, bot)
  }
  rownames(out) <- NULL
  out
}
