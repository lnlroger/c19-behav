rm(list = ls())
source("ARDL_import-data.R")

library("dLagM")
library("lmtest")

max.p <- 12
max.q <- 12

# ARDL ----
df.ARDL <- df.use %>%
  filter(sub_region_1 == "") %>%
  group_by(Country) %>%
  mutate(diff.Movement = c(NA, diff(Movement)),
         diff.StringencyIndex = c(NA, diff(StringencyIndex))) %>%
  dplyr::select(Country, Date, diff.Movement, diff.StringencyIndex) %>%
  drop_na()

rm(df.use)

ardl.summary <- data.frame(Country = unique(df.ARDL$Country),
                           LongRunCoefficient = rep(NA,length(unique(df.ARDL$Country))),
                           p.best = rep(NA,length(unique(df.ARDL$Country))),
                           q.best = rep(NA,length(unique(df.ARDL$Country))),
                           bg.pval.best = rep(NA,length(unique(df.ARDL$Country))),
                           bic.best = rep(NA,length(unique(df.ARDL$Country))))
                          
bg.matrix <- matrix(data = rep(NA,max.q*max.p), 
                    nrow = max.p, ncol = max.q,
                    dimnames = list(paste0("p=",1:max.p),
                                    paste0("q=",1:max.q)))

bic.matrix = bg.matrix 

models.ardl <- list()

i <- 1
for (ctry in unique(df.ARDL$Country)) {
# for (ctry in c("United States")) { # DEVELOPMENT 
  df.now <- df.ARDL %>%
    filter(Country == ctry)
  
  for (p.ardl in 1:max.p) { # Lags of independent variable
    for (q.ardl in 1:max.q) { # Autoregressive lags
      
      # Run estimation
      
      model.ardl <- ardlDlm(formula = diff.Movement ~ diff.StringencyIndex, 
                            data = df.now,
                            p = p.ardl , q = q.ardl)
      # Misspecification test
      bgtest.now <- bgtest(model.ardl$model, order = 2, type = "F") 
      bg.matrix[p.ardl,q.ardl] <- bgtest.now$p.value
      bic.matrix[p.ardl,q.ardl] <- BIC(model.ardl$model)
      
    }
  }
  
  bg.matrix.pass <- bg.matrix >= 0.1
  
  if(sum(bg.matrix.pass > 0)){
  bic.matrix.smallest <- min(bic.matrix[which(bg.matrix.pass == TRUE)])
  best.model <- which(bic.matrix == bic.matrix.smallest & bg.matrix.pass,
                      arr.ind = TRUE)
  
    p.best <- best.model[1,1]
    q.best <- best.model[1,2]
  }
  
  if(is_empty(best.model)){
    best.model <- which(bic.matrix == min(bic.matrix), 
                        arr.ind = TRUE)
    p.best <- best.model[1,1]
    q.best <- best.model[1,2]
  }
  
  model.ardl <- ardlDlm(formula = diff.Movement ~ diff.StringencyIndex, 
                        data = df.now,
                        p = p.best , q = q.best)
  # Long run parameter
  
  lr.coefficient.best <- sum(model.ardl$model$coefficients[c("diff.StringencyIndex.t",paste0("diff.StringencyIndex.",1:p.best))]) /
    (1-sum(model.ardl$model$coefficients[paste0("diff.Movement.",1:q.best)]))
  
  
  this.row <- which(ardl.summary$Country == ctry)
  ardl.summary[["LongRunCoefficient"]][this.row] <- lr.coefficient.best
  ardl.summary[["p.best"]][this.row] <- p.best
  ardl.summary[["q.best"]][this.row] <- q.best
  ardl.summary[["bg.pval.best"]][this.row] <- bg.matrix[best.model]
  ardl.summary[["bic.best"]][this.row] <- bic.matrix[best.model]
  
  print(paste0("Country ",i," out of ", length(unique(df.ARDL$Country)),
               " (", ctry, ")"))
  i <- i+1
}




#rm(df.now, df.ARDL, model.ardl, ctry, lr.coefficient.ardl,p.ardl,q.ardl)

#write_rds(lr.coeffs, "../compliance/LongRunCoefficients_ARDL_test.rds")
