rm(list = ls())
source("ARDL_import-data.R")

library("dLagM")

# ARDL ----
df.ARDL <- df.use %>%
  group_by(Country) %>%
  mutate(diff.Movement = c(NA, diff(Movement)),
         diff.StringencyIndex = c(NA, diff(StringencyIndex))) %>%
  dplyr::select(Country, Date, diff.Movement, diff.StringencyIndex) %>%
  drop_na()

lr.coeffs <- data.frame(Country = unique(df.ARDL$Country))
models.ardl <- list()

# Prepare data
for (pq in 1:3) {
  variable.name <- paste0("LongRunCoeff_",pq)
  for (ctry in unique(df.ARDL$Country)) {
  
    df.now <- df.ARDL %>%
      filter(Country == ctry)
    
    # Run estimation
    p.ardl <- pq # Lags of independent variable
    q.ardl <- pq # Autoregressive lags
    model.ardl <- ardlDlm(formula = diff.Movement ~ diff.StringencyIndex, 
                          data = df.now,
                          p = p.ardl , q = q.ardl)
    # Long run parameter
    
    lr.coefficient.ardl <- sum(model.ardl$model$coefficients[c("diff.StringencyIndex.t",paste0("diff.StringencyIndex.",1:p.ardl))]) /
      (1-sum(model.ardl$model$coefficients[paste0("diff.Movement.",1:q.ardl)]))
    
    lr.coeffs[[variable.name]][which(lr.coeffs$Country == ctry)] <- lr.coefficient.ardl
  }
}

rm(df.now, df.ARDL, model.ardl, ctry, lr.coefficient.ardl,p.ardl,q.ardl)

write_rds(lr.coeffs, "../compliance/LongRunCoefficients_ARDL.rds")
