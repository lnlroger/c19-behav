dta <- readRDS("ARDL_cov_03092020.Rds")

shortrun <- data.frame(ObsUnit = rep(NA,length(dta$model)),
                       ShortRunCoefficient = rep(NA,length(dta$model)))
rownames(shortrun) <- names(dta$model)

for (i in names(dta$model)) {
  shortrun[i,1] <- i
  shortrun[i,2] <- dta$model[[i]]$model$coefficients[["diff.StringencyIndex.t"]]
  print(i)
}

summary.combine <- left_join(dta$summary,shortrun)

write_rds(summary.combine, "summaryLongAndShort.rds")
