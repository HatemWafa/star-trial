library(stringdist)

findvar <- function(X, Y) {
  sapply(X, function(x){
    # x <- gsub("[^[:alpha:]]", "", x)
    # y <- gsub("[^[:alpha:]]", "", rownames(t))
    
    x <- sub(" .*", "", x)
    y <- sub(" .*", "", Y)
    which(y %in% x) %>% return
  }) %>% return
}




tab1 <- function(vars, strata=NA, data, 
                 test = TRUE, nonnormal=NULL, overall=FALSE, 
                 catDigits = 1, contDigits = 2, percentonly=FALSE, showAllLevels=FALSE,
                 mtable=FALSE, mcodes=c(NA,""), decichar=".", lancet=FALSE) {
  # if(catDigits==0) {cats=0; catDigits=1} else {cats=catDigits}
  if(lancet==TRUE) {catDigits=2; contDigits=1; test=FALSE}
  
  if(mtable==TRUE) { # produce missingness table
    recodedvars <- vars[!vars%in%strata]
    data[,recodedvars] <- apply(data[,recodedvars], 2, function(x){
      x <- as.character(x)
      x <- ifelse(x %in% mcodes, 1, 0)
    })
    data[recodedvars] <- lapply(data[recodedvars], factor, levels= c(0,1), labels = c("Complete","Missing"))
  }
  
  
  if(is.na(strata)) {
    t <- tableone::CreateTableOne(vars = vars, data = data)
  } else {
    t <- tableone::CreateTableOne(vars = vars, data = data, strata = strata)
  }
  
  t <- print(t, 
             noSpaces = TRUE, 
             explain=FALSE, 
             nonnormal = nonnormal, 
             catDigits = catDigits, 
             contDigits = contDigits,
             test=test,
             showAllLevels=showAllLevels
  )
  if(showAllLevels==TRUE) {rownames(t)=paste(rownames(t),t[,1]); t=t[,-1]}
  
  
  if(overall==TRUE) {
    o <- tableone::CreateTableOne(vars = vars, data = data)
    o <- print(o, 
               noSpaces = TRUE, 
               explain=FALSE, 
               nonnormal = nonnormal, 
               catDigits = catDigits, 
               contDigits = contDigits,
               test=test,
               showAllLevels=showAllLevels
               )
    if(showAllLevels==TRUE) {rownames(o)=paste(rownames(o),o[,1]); o=o[,-1]}
    
    t <- cbind(t,o)
  }
  
  # FORMATTING CHANGES
  # if(cats==0) {catDigits=0}
  
  t[1,] <- paste0("(N=",t[1,],")")
  
  if(length(vars)==1) {tempvars=c(vars,vars)} else {tempvars=vars}
  catvars <- sapply(data[,tempvars], function(x) is.factor(x) | is.character(x) | is.logical(x))
  if(length(vars)==1) {catvars=catvars[1]}
  if(any(catvars==TRUE)) {
    catvars <- catvars[which(catvars)] %>% names
    catlev <- sapply(data[,catvars], function(x) length(unique(x) %>% na.omit))
    
    # variables with levels to which we need to add %s
    lvls <- c(catvars[which(catlev<=2)] %>% findvar(rownames(t)),
              (1:nrow(t))[!(1:nrow(t)) %in% findvar(c("n",vars), rownames(t))]
    ) %>% unlist %>% as.vector()
    
    t[lvls,] <- apply(t[lvls,], c(1,2), function(x) {
      if(lancet==TRUE) {
        x <- ifelse(x==""|x=="-", "", x %>% replace_less_than_one)
        x <- gsub(")", "%)", x) %>% round_numbers(uto=0,lto=0)
        x <- gsub("^0 \\(<1%\\)", "-", x)
      } else {
        x <- gsub("(\\d+\\.\\d+)", "\\1%", x)
      }
      return(x)
    })
    if(percentonly==TRUE) {
      t[lvls,] <- apply(t[lvls,], c(1,2), function(x) {gsub(".*?\\(([^)]*)\\).*?", "\\1", x)})
    }
  }
  
  if(lancet==TRUE) {
    if(is.null(dim(t[rownames(t)%in%nonnormal,]))) {
      t[rownames(t)%in%nonnormal,] <- sapply(t[rownames(t)%in%nonnormal,], round_numbers, uto=0, lto=0)
    } else {
      t[rownames(t)%in%nonnormal,] <- apply(t[rownames(t)%in%nonnormal,] ,c(1,2), round_numbers, uto=0, lto=0)
    }
  }
  
  if(!is.null(nonnormal)) {
    t[findvar(nonnormal, rownames(t)),] <- sapply(t[findvar(nonnormal, rownames(t)),], function(x) {
      # x <- gsub("\\[", "(", x)
      # x <- gsub("\\]", ")", x)
      x <- gsub("\\, ", "-", x)
    })
  }
  
  
  t <- t %>% apply(c(1,2), function(x) {
    gsub("\\.", decichar, x)
  })
  
  
  return(t)
}


tab1 <- function(vars, strata=NA, data, 
                 test = TRUE, nonnormal=NULL, overall=FALSE, 
                 catDigits = 1, contDigits = 2, percentonly=FALSE, showAllLevels=FALSE,
                 mtable=FALSE, mcodes=c(NA,""), decichar=".", lancet=FALSE,
                 cat95 = FALSE) {     # <── NEW ARGUMENT
  
  if(lancet==TRUE) {catDigits=2; contDigits=1; test=FALSE}
  
  if(mtable==TRUE) { # produce missingness table
    recodedvars <- vars[!vars%in%strata]
    data[,recodedvars] <- apply(data[,recodedvars], 2, function(x){
      x <- as.character(x)
      x <- ifelse(x %in% mcodes, 1, 0)
    })
    data[recodedvars] <- lapply(data[recodedvars], factor, levels= c(0,1), labels = c("Complete","Missing"))
  }
  
  if(is.na(strata)) {
    t <- tableone::CreateTableOne(vars = vars, data = data)
  } else {
    t <- tableone::CreateTableOne(vars = vars, data = data, strata = strata)
  }
  
  t <- print(t, 
             noSpaces = TRUE, 
             explain=FALSE, 
             nonnormal = nonnormal, 
             catDigits = catDigits, 
             contDigits = contDigits,
             test=test,
             showAllLevels=showAllLevels
  )
  if(showAllLevels==TRUE) {rownames(t)=paste(rownames(t),t[,1]); t=t[,-1]}
  
  if(overall==TRUE) {
    o <- tableone::CreateTableOne(vars = vars, data = data)
    o <- print(o, 
               noSpaces = TRUE, 
               explain=FALSE, 
               nonnormal = nonnormal, 
               catDigits = catDigits, 
               contDigits = contDigits,
               test=test,
               showAllLevels=showAllLevels
    )
    if(showAllLevels==TRUE) {rownames(o)=paste(rownames(o),o[,1]); o=o[,-1]}
    t <- cbind(t,o)
  }
  
  t[1,] <- paste0("(N=",t[1,],")")
  
  if(length(vars)==1) {tempvars=c(vars,vars)} else {tempvars=vars}
  catvars <- sapply(data[,tempvars], function(x) is.factor(x) | is.character(x) | is.logical(x))
  if(length(vars)==1) {catvars=catvars[1]}
  
  if(any(catvars==TRUE)) {
    catvars <- catvars[which(catvars)] %>% names
    catlev <- sapply(data[,catvars], function(x) length(unique(x) %>% na.omit))
    
    # variables with levels to which we need to add %s
    lvls <- c(catvars[which(catlev<=2)] %>% findvar(rownames(t)),
              (1:nrow(t))[!(1:nrow(t)) %in% findvar(c("n",vars), rownames(t))]
    ) %>% unlist %>% as.vector()
    
    t[lvls,] <- apply(t[lvls,], c(1,2), function(x) {
      if(lancet==TRUE) {
        x <- ifelse(x==""|x=="-", "", x %>% replace_less_than_one)
        x <- gsub(")", "%)", x) %>% round_numbers(uto=0,lto=0)
        x <- gsub("^0 \\(<1%\\)", "-", x)
      } else {
        x <- gsub("(\\d+\\.\\d+)", "\\1%", x)
      }
      return(x)
    })
  }
  
  # --- NEW: Add exact binomial 95% CI ---
  if(cat95 == TRUE) {
    for (v in catvars) {
      if(is.null(strata)) {
        tab <- table(data[[v]]) %>% as.vector %>% data.frame
        rownames(tab) <- names(table(data[[v]]))
        n <- sum(tab)
      } else {
        tab <- cbind(table(data[[v]], data[[strata]]), table(data[[v]])) %>% as.data.frame
        n <- tab %>% apply(2,sum)
      }
      
      BT <- lapply(rownames(tab), function(lvl) {
        x <- tab[lvl,]
        VV <- lapply(seq_along(x), function(xxxx) {
          if (x[xxxx] > 0) {
            bt <- binom.test(x[xxxx]%>%as.numeric, n[xxxx]%>%as.numeric, conf.level = 0.95)
            out <- paste0(
              "%); ",
              (100*bt$conf.int[1]) %>% round_numbers(uto=0,lto=0), "% to ",
              (100*bt$conf.int[2]) %>% round_numbers(uto=0,lto=0),"%)"
            )
          } else {out=""}
          out <- out %>% matrix
          return(out)
        }) %>% do.call("cbind",.)
        if(overall==FALSE) {VV <- VV[,-ncol(VV)]}
        return(VV)
      }) %>% do.call("rbind",.) 
      ndex <- grep(v, rownames(t)) : (grep(v, rownames(t))+catlev[[v]])
      if(catlev[[v]]>2) {ndex <- ndex[-1]} else {ndex <- ndex[1]; BT <- BT[nrow(BT),]}
      t[ndex,] <-  paste0(t[ndex,], BT) %>% 
        matrix(ncol=ncol(t)) %>% 
        apply(c(1,2), function(ww) gsub("%)%)","%",ww))
      
      
    }
  }
  
  
  
  if(percentonly==TRUE) {
    t[lvls,] <- apply(t[lvls,], c(1,2), function(x) {gsub(".*?\\(([^)]*)\\).*?", "\\1", x)})
  }
  
  
  if(lancet==TRUE) {
    if(is.null(dim(t[rownames(t)%in%nonnormal,]))) {
      t[rownames(t)%in%nonnormal,] <- sapply(t[rownames(t)%in%nonnormal,], round_numbers, uto=0, lto=0)
    } else {
      t[rownames(t)%in%nonnormal,] <- apply(t[rownames(t)%in%nonnormal,] ,c(1,2), round_numbers, uto=0, lto=0)
    }
  }
  
  if(!is.null(nonnormal)) {
    t[findvar(nonnormal, rownames(t)),] <- sapply(t[findvar(nonnormal, rownames(t)),], function(x) {
      x <- gsub("\\, ", "-", x)
    })
  }
  
  t <- t %>% apply(c(1,2), function(x) {
    gsub("\\.", decichar, x)
  })
  
  return(t)
}




### OLD VERSION
round_numbers <- function(str, commas=TRUE, cutoff=10, uto=0, lto=1) {
  # Find all numbers in the string
  number_positions <- gregexpr("\\d+(\\.\\d+)?", str)
  num_matches <- regmatches(str, number_positions) %>% unlist
  replacement <- num_matches
  
  # # Figure out whether numbers are equals but different format
  # num_matches %>% as.numeric %>% anyDuplicated
  # sapply(num_matches, nchar)
  
  # Loop through the matches and round the numbers
  for (i in seq_along(num_matches)) {
    num <- as.numeric(num_matches[i])
    if (num >= cutoff) {
      rounded <- round(num, uto)
      # Convert to characters with the same length
      rounded <- rounded %>% formatC(format="f", digits=uto, width=nchar(max(rounded)))
      if(commas==TRUE) {rounded <- rounded %>% formatC(format="d", big.mark=",")}
    } else {
      rounded <- round(num, lto)
      rounded <- rounded %>% formatC(format="f", digits=lto, width=nchar(max(rounded)))
      # if(commas==TRUE) {rounded <- rounded %>% formatC(format="d", big.mark=",")}
    }
    
    replacement[i] <- rounded
    # str <- gsub(num_matches[i], as.character(rounded), str)
  }
  
  
  for (i in seq_along(num_matches)) {
    str <- sub(num_matches[i], replacement[i], str)
  }
  
  return(str)
}




### NEW VERSION (20260120)
round_numbers <- function(str,
                          cutoff = 10,
                          lto = 2,
                          uto = 1,
                          bigno = Inf,
                          bigto = 0,
                          commas = TRUE) {
  str <- str %>% as.character
  stopifnot(is.character(str))
  
  pat <- "-?\\d{1,3}(?:,\\d{3})+(?:\\.\\d+)?|-?\\d+(?:\\.\\d+)?"
  
  fmt <- function(num, digits, use_commas) {
    formatC(round(num, digits), format = "f", digits = digits,
            big.mark = if (use_commas) "," else "",
            big.interval = 3)
  }
  
  vapply(str, function(s) {
    pos  <- gregexpr(pat, s, perl = TRUE)
    hits <- regmatches(s, pos)[[1]]
    if (length(hits) == 0) return(s)
    
    repl <- vapply(hits, function(h) {
      num <- suppressWarnings(as.numeric(gsub(",", "", h)))
      if (!is.finite(num)) return(h)
      
      absn <- abs(num)
      d <- if (absn >= bigno) bigto else if (absn >= cutoff) uto else lto
      
      fmt(num, d, commas)
    }, character(1))
    
    regmatches(s, pos) <- list(repl)
    s
  }, character(1))
}
# c(0.1,0.0235,0.423,0.26,1,4,7.4365,5.46673,10,54.6432,96.4245,100,100.4, 100.5634,1000, NA,"", "Hatem 1.2 0.3456234 1000000") %>% sapply(round_numbers,lto=2,uto=1,bigno=200) %>% as.vector()






commas <- function(number) {
  if(is.numeric(number)) {
    number %>% formatC(format="d", big.mark=",") %>% return
  } else {
    # Use regular expression to identify numeric substrings in the string
    matches <- gregexpr("\\b\\d+\\b", number)
    matches <- regmatches(number, matches)[[1]]
    
    # Format each identified number with commas
    formatted_numbers <- lapply(matches, function(x) {
      format(as.numeric(x), big.mark = ",")
    })
    
    # Replace the original numbers in the string with the formatted numbers
    for (i in seq_along(matches)) {
      number <- gsub(paste0("\\b", matches[i], "\\b"), formatted_numbers[[i]], number)
    }
    
    return(number)
  }
  
}





# Function to replace numbers between brackets with "<1" if they are less than one
replace_less_than_one <- function(x) {
  # Pattern to match numbers within brackets
  pattern <- "\\(([^)]+)\\)"
  
  # Extract matches
  matches <- regmatches(x, gregexpr(pattern, x))
  
  # Process each match
  modified_matches <- lapply(matches[[1]], function(m) {
    # Extract the number from the match
    num <- as.numeric(gsub("[()]", "", m))
    # Check if the number is less than one and replace if true
    if (!is.na(num) && num < 1) {
      return("(<1)")
    }
    return(m)
  })
  
  # Replace the original matches with the modified ones
  regmatches(x, gregexpr(pattern, x)) <- modified_matches
  return(x)
}

