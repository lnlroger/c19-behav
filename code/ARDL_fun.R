
ARDL.flex <- function(data.input = df.now,
                      depvar = "diff.Movement",
                      indepvar = "diff.StringencyIndex",
                      impulsevar = indepvar[1],
                      max.p = 12, max.q = 12, 
                      write.output = FALSE,
                      path.output = "ARDL-output.RData",
                      by.var = "Country",
                      vars.to.deseason = "diff.Movement",
                      seasonality = NULL,
                      t.min = 30
){
  
  library("tidyverse")
  library("lubridate")
  library("vars")
  library("zoo")
  library("dLagM")
  library("lmtest")
  
  # Prepare dataset  ----
  df.ARDL <- data.input
  
  df.ARDL["ObsUnit"] <- df.ARDL[by.var]
  
  df.ARDL <- df.ARDL %>%
    filter(n() >= t.min)
  
  n.units <- length(unique(df.ARDL$ObsUnit))
  
  
  # Prepare empty data.frame to store key results in
  ardl.summary <- data.frame(ObsUnit = unique(df.ARDL$ObsUnit),
                             LongRunCoefficient = rep(NA,n.units),
                             p.best = rep(NA,n.units),
                             q.best = rep(NA,n.units),
                             bg.pval.best = rep(NA,n.units),
                             bic.best = rep(NA,n.units),
                             r2.best = rep(NA,n.units),
                             r2.adj.best = rep(NA,n.units))
  
  # Prepare empty matrices to store key test statistics in for all considered models
  bg.matrix <- matrix(data = rep(NA,max.q*max.p), 
                      nrow = max.p, ncol = max.q,
                      dimnames = list(paste0("p=",1:max.p),
                                      paste0("q=",1:max.q)))
  
  bic.matrix = bg.matrix 
  lr.matrix = bg.matrix 
  
  # Combine y and x (or several x's) into formula for ARDL
  formula.ardl.char <- paste(depvar, " ~ ", paste(indepvar,collapse = "+"))
  formula.ardl <- as.formula(formula.ardl.char)
  rm(formula.ardl.char)
  
  # Create list to store all output resulting from function (object to return)
  output.ardl <- list()
  
  # Loop running over each Country (or other observational unit)
  i <- 1
  
  # Restrict dataset to current country
  for (ctry in unique(df.ARDL$ObsUnit)) {
    df.now <- df.ARDL %>%
      filter(ObsUnit == ctry)
    
    # If desired, remove seasonality by regressing weekday dummies on depvar and
    # preserving residuals (~Frisch-Waugh-Lovell Theorem)
    df.now$weekday <- weekdays(df.now$Date)
    for (v in vars.to.deseason) {
      
      df.now[v] <- remove.seasonality(data = df.now,
                                      var.to.clean = v, 
                                      seasonal.var = seasonality)
    }
    
    # Run ARDL for all possible lag combinations and store selection criteria
    for (p.ardl in 1:max.p) { # Lags of independent variable
      for (q.ardl in 1:max.q) { # Autoregressive lags
        
        # Run estimation
        
        model.ardl <- ardlDlm(formula = formula.ardl, 
                              data = df.now,
                              p = p.ardl , q = q.ardl)
        
        # Store specification criteria in respective matrices
        bgtest.now <- bgtest(model.ardl$model, order = 2, type = "F") 
        bg.matrix[p.ardl,q.ardl] <- bgtest.now$p.value
        bic.matrix[p.ardl,q.ardl] <- BIC(model.ardl$model)
        
        # As well as lr-coefficient resulting from the model
        lr.numerator.this <- 
          sum(model.ardl$model$coefficients[c(paste0(impulsevar,".t"),
                                              paste0(impulsevar,".",1:p.ardl))])
        lr.denominator.this <-   
          (1-sum(model.ardl$model$coefficients[paste0(depvar,".",1:q.ardl)]))
        
        lr.matrix[p.ardl,q.ardl] <- lr.numerator.this / lr.denominator.this
      }
    }
    
    # Store specification criteria & LR matrices in output list for later reference
    output.ardl[["bgtest"]][[ctry]] <- bg.matrix
    output.ardl[["bic"]][[ctry]] <- bic.matrix
    output.ardl[["longrun"]][[ctry]] <- lr.matrix
    
    # Select best model in two steps:
    # 1. Identify subset of models that do not exhibit autocorrelation according to BG test
    # 2. Select the model with lowest BIC from that subset.
    
    bg.matrix.pass <- bg.matrix >= 0.1
    
    if(sum(bg.matrix.pass > 0)){
      bic.matrix.smallest <- min(bic.matrix[which(bg.matrix.pass == TRUE)])
      best.model <- which(bic.matrix == bic.matrix.smallest & bg.matrix.pass,
                          arr.ind = TRUE)
      
      p.best <- best.model[1,1]
      q.best <- best.model[1,2]
    }
    
    # However, if no model passes BG test, just pick the one that has lowest BIC regardless
    if(is_empty(best.model)){
      best.model <- which(bic.matrix == min(bic.matrix), 
                          arr.ind = TRUE)
      p.best <- best.model[1,1]
      q.best <- best.model[1,2]
    }
    
    # Now run the model that was selected as best choice in the previous step
    model.ardl <- ardlDlm(formula = formula.ardl, 
                          data = df.now,
                          p = p.best , q = q.best)
    
    # Compute long run coefficient
    lr.numerator <- 
      sum(model.ardl$model$coefficients[c(paste0(impulsevar,".t"),
                                          paste0(impulsevar,".",1:p.best))])
    lr.denominator <-   
      (1-sum(model.ardl$model$coefficients[paste0(depvar,".",1:q.best)]))
    
    lr.coefficient.best <- lr.numerator / lr.denominator
    
    
    # Store model in output list just for future reference (make optional?)
    output.ardl[["model"]][[ctry]] <- model.ardl
    
    # Create entry in summary table of final output
    sum.model.ardl <- summary(model.ardl)
    
    this.row <- which(ardl.summary$ObsUnit == ctry)
    ardl.summary[["LongRunCoefficient"]][this.row] <- lr.coefficient.best
    ardl.summary[["p.best"]][this.row] <- p.best
    ardl.summary[["q.best"]][this.row] <- q.best
    ardl.summary[["bg.pval.best"]][this.row] <- bg.matrix[best.model]
    ardl.summary[["bic.best"]][this.row] <- bic.matrix[best.model]
    ardl.summary[["r2.best"]][this.row] <- sum.model.ardl$r.squared
    ardl.summary[["r2.adj.best"]][this.row] <- sum.model.ardl$adj.r.squared
    
    # Just to keep track of progress when running code:
    print(paste0("Unit ",i," out of ", n.units,
                 " (", ctry, ")"))
    i <- i+1
  }
  
  # Store complete summary table in final output list
  output.ardl[["summary"]] <- ardl.summary
  
  # If desired, store output in an external file
  if (write.output == TRUE) {
    save(output.ardl, file = path.output)  
  }
  
  return(output.ardl)
  
}

remove.seasonality <- function(data = df.now, 
                               var.to.clean = "diff.Movement", 
                               seasonal.var = "weekday"){
  formula.clean <- as.formula(paste(var.to.clean, "~", seasonal.var, "-1"))
  lm.clean <- lm(formula = formula.clean, data = data)
  return(lm.clean$residuals)
}
