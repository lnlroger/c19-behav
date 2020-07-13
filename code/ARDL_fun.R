
ARDL.flex <- function(data.input = df.now,
                      y = "Movement",
                      max.p = 3, max.q = 3, 
                      write.output = FALSE,
                      path.output = "ARDL-output.Rds",
                      by.var = "Country"
){
  
  library("tidyverse")
  library("lubridate")
  library("vars")
  library("zoo")
  library("dLagM")
  library("lmtest")
  
  # ARDL ----
  
  
  
  df.ARDL <- data.input
  
  df.ARDL["ObsUnit"] <- df.ARDL[by.var]
  
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
  
  
  # Create list to store all output resulting from function (object to return)
  output.ardl <- list()
  
  # Loop running over each Country (or other observational unit)
  i <- 1
  for (ctry in unique(df.ARDL$ObsUnit)) {
    df.now <- df.ARDL %>%
      filter(ObsUnit == ctry)
    
    # Run ARDL for all possible lag combinations and store selection criteria
    for (p.ardl in 1:max.p) { # Lags of independent variable
      for (q.ardl in 1:max.q) { # Autoregressive lags
        
        # Run estimation
        
        model.ardl <- ardlDlm(formula = diff.Movement ~ diff.StringencyIndex, 
                              data = df.now,
                              p = p.ardl , q = q.ardl)
        
        # Store specification criteria in respective matrices
        bgtest.now <- bgtest(model.ardl$model, order = 2, type = "F") 
        bg.matrix[p.ardl,q.ardl] <- bgtest.now$p.value
        bic.matrix[p.ardl,q.ardl] <- BIC(model.ardl$model)
        
      }
    }
    
    # Store specification criteria matrices in output list for later reference
    output.ardl[["bgtest"]][[ctry]] <- bg.matrix
    output.ardl[["bic"]][[ctry]] <- bic.matrix
    bg.matrix.pass <- bg.matrix >= 0.1
    
    # Select best model in two steps:
    # 1. Identify subset of models that do not exhibit autocorrelation according to BG test
    # 2. Select the model with lowest BIC from that subset.
    
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
    model.ardl <- ardlDlm(formula = diff.Movement ~ diff.StringencyIndex, 
                          data = df.now,
                          p = p.best , q = q.best)
    
    # Compute long run coefficient
    lr.coefficient.best <- sum(model.ardl$model$coefficients[c("diff.StringencyIndex.t",paste0("diff.StringencyIndex.",1:p.best))]) /
      (1-sum(model.ardl$model$coefficients[paste0("diff.Movement.",1:q.best)]))
    
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
  
  # If desired, store summary table in an external file
  if (write.output == TRUE) {
    write_rds(ardl.summary, path.output)  
  }
  
  return(output.ardl)
  
}

