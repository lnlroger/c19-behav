rm(list = ls())
source("LR_import-data.R")

library("ggpubr")
library("dLagM")

# Compare CoronaNet and Oxford Index ----

# Scatter
ggplot(
  na.omit(df.use[,c("Date","IndexCoronaNet","StringencyIndex")]), 
  aes(x = IndexCoronaNet, 
      y = StringencyIndex)
  ) +
  geom_point(size = 0.1) +
  geom_smooth(method=lm, color="darkred", fill="blue") + 
  stat_cor(method = "pearson") +
  theme_bw()
  

# Time Series Comparison
countries.to.plot <- c(unique(df.use$Country)[seq(1,163,35)],
                       "United States",
                       "Germany",
                       "Nicaragua") # Random bunch of countries

df.plot.ts.compare <-
  df.use %>%
  filter(Country %in% countries.to.plot) %>%
  dplyr::select(Country,Date,StringencyIndex,IndexCoronaNet) %>%
  pivot_longer(
    -c(
      Country,
      Date
    ),
    names_to = "series",
    values_to = "value"
  ) %>%
  arrange(Country, series, Date) %>%
  na.omit()

ggplot(df.plot.ts.compare, aes(x = Date, y = value, colour = series)) +
  geom_line() +
  facet_grid(cols = vars(Country), rows = vars(series),
             scales = "free_y") +
  theme_bw() +
  theme(legend.position = 'none',
        axis.text.x=element_blank(),
        axis.ticks.x = element_blank()) 

rm(df.plot.ts.compare)

# 'Gantt chart', illustrating measures underlying Oxford Stringency Index
df.plot.ts.gantt <-
  df.use %>%
  filter(Country %in% countries.to.plot) %>%
  dplyr::select(Country, Date, StringencyIndex,
                C1_Flag, 
                C2_Flag,
                C3_Flag,
                C4_Flag,
                C5_Flag,
                C6_Flag,
                C7_Flag,) %>%
  replace_na(list(C1_Flag = 0, 
                  C2_Flag = 0,
                  C3_Flag = 0, 
                  C4_Flag = 0,
                  C5_Flag = 0, 
                  C6_Flag = 0,
                  C7_Flag = 0))

ggplot(df.plot.ts.gantt, aes(x = Date, y = StringencyIndex)) +
  scale_alpha_continuous(range = c(0, 0.3)) +
  geom_segment(aes(y = 0, yend = 10,
      x = Date, xend = Date,
      alpha = as.numeric(C1_Flag)),
    inherit.aes = F,
    colour = "red", size = 2) +
  geom_segment(aes(y = 10, yend = 20,
                   x = Date, xend = Date,
                   alpha = as.numeric(C2_Flag)),
               inherit.aes = F,
               colour = "orange", size = 2) +
  geom_segment(aes(y = 20, yend = 30,
                   x = Date, xend = Date,
                   alpha = as.numeric(C3_Flag)),
               inherit.aes = F,
               colour = "yellow", size = 2) +
  geom_segment(aes(y = 30, yend = 40,
                   x = Date, xend = Date,
                   alpha = as.numeric(C4_Flag)),
               inherit.aes = F,
               colour = "greenyellow", size = 2) +
  geom_segment(aes(y = 40, yend = 50,
                   x = Date, xend = Date,
                   alpha = as.numeric(C5_Flag)),
               inherit.aes = F,
               colour = "green", size = 2) +
  geom_segment(aes(y = 50, yend = 60,
                   x = Date, xend = Date,
                   alpha = as.numeric(C6_Flag)),
               inherit.aes = F,
               colour = "turquoise", size = 2) +
  geom_segment(aes(y = 60, yend = 70,
                   x = Date, xend = Date,
                   alpha = as.numeric(C7_Flag)),
               inherit.aes = F,
               colour = "blue", size = 2, show.legend = TRUE) +
  geom_line() +
  ylim(0, 100) +
  facet_wrap(~Country, ncol = 4) +
  theme_bw() +
  theme(legend.position = "none")

rm(df.plot.ts.gantt,countries.to.plot)


# ARDL ----
df.ARDL <- df.use %>%
  group_by(Country) %>%
  mutate(diff.Movement = c(NA, diff(Movement)),
         diff.StringencyIndex = c(NA, diff(StringencyIndex))) %>%
  dplyr::select(Country, Date, diff.Movement, diff.StringencyIndex) %>%
  drop_na()

lr.coeffs <- data.frame(Country = unique(df.ARDL$Country),
                        LongRunCoeff = NA)
models.ardl <- list()

# Prepare data
for (ctry in unique(df.ARDL$Country)) {

  df.now <- df.ARDL %>%
    filter(Country == ctry)
  
  # Run estimation
  p.ardl <- 1 # Lags of independent variable
  q.ardl <- 1 # Autoregressive lags
  model.ardl <- ardlDlm(formula = diff.Movement ~ diff.StringencyIndex, 
                        data = df.now,
                        p = p.ardl , q = q.ardl)
  # Long run parameter
  lr.coefficient.ardl <- sum(model.ardl$model$coefficients[2:5]) /
    (1-sum(model.ardl$model$coefficients[6:8]))
  
  lr.coeffs$LongRunCoeff[which(lr.coeffs$Country == ctry)] <- lr.coefficient.ardl
  models.ardl[[ctry]] <- model.ardl
}

rm(df.now, df.ARDL, model.ardl, ctry, lr.coefficient.ardl,p.ardl,q.ardl)

#write_rds(lr.coeffs, "LongRunCoefficients_ARDL.rds")

# See if there's something going on with the coefficients
lr.coeffs.covariates <- lr.coeffs %>%
  left_join(df.use) %>%
  dplyr::select(Country, LongRunCoeff, polity2, risktaking, 
                patience, ROL, GDP.capita, CaseLog,
                Continent, Region) %>%
  group_by(Country) %>%
  fill(polity2, risktaking, patience, ROL, GDP.capita, CaseLog, 
       Continent, Region, .direction = "updown") %>%
  distinct(Country, LongRunCoeff, polity2, risktaking, patience, ROL, GDP.capita, CaseLog,
           Continent, Region) %>%
  filter(row_number()==n())

df.coeffs.reorder <- lr.coeffs.covariates %>%
  ungroup() %>%
  mutate(Country = forcats::fct_reorder(Country, LongRunCoeff))

ggplot(arrange(df.coeffs.reorder,LongRunCoeff),aes(y=Country, x=LongRunCoeff)) + 
  geom_point(colour="blue",alpha=0.25)+
  theme_bw()+
  theme(#axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size=7))

coeffs.by.continent <- lr.coeffs.covariates %>%
  group_by(Continent) %>%
  summarise(LongRunCoeff = mean(LongRunCoeff, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Continent = forcats::fct_reorder(Continent, LongRunCoeff))

ggplot(arrange(coeffs.by.continent,LongRunCoeff),aes(y=Continent, x=LongRunCoeff)) + 
  geom_point(colour="blue")+
  theme_bw()+
  theme(#axis.text.x = element_text(angle = 90, hjust = 1),
    text = element_text(size=10))

coeffs.by.region <- lr.coeffs.covariates %>%
  group_by(Region) %>%
  summarise(LongRunCoeff = mean(LongRunCoeff, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Region = forcats::fct_reorder(Region, LongRunCoeff))

ggplot(arrange(coeffs.by.region,LongRunCoeff),aes(y=Region, x=LongRunCoeff)) + 
  geom_point(colour="blue")+
  theme_bw()+
  theme(#axis.text.x = element_text(angle = 90, hjust = 1),
    text = element_text(size=10))

ggplot(lr.coeffs.covariates, aes(LongRunCoeff,polity2)) +
  geom_point() +
  geom_smooth(method=lm, color="darkred", fill="blue") + 
  stat_cor(method = "pearson") +
  theme_bw()

ggplot(lr.coeffs.covariates, aes(LongRunCoeff,risktaking)) +
  geom_point() +
  geom_smooth(method=lm, color="darkred", fill="blue") + 
  stat_cor(method = "pearson") +
  theme_bw()

ggplot(lr.coeffs.covariates, aes(LongRunCoeff,patience)) +
  geom_point() +
  geom_smooth(method=lm, color="darkred", fill="blue") + 
  stat_cor(method = "pearson") +
  theme_bw()

ggplot(lr.coeffs.covariates, aes(LongRunCoeff,ROL)) +
  geom_point() +
  geom_smooth(method=lm, color="darkred", fill="blue") + 
  stat_cor(method = "pearson") +
  theme_bw()

#

ggplot(lr.coeffs.covariates, aes(LongRunCoeff,GDP.capita)) +
  geom_point() +
  geom_smooth(method=lm, color="darkred", fill="blue") + 
  stat_cor(method = "pearson") +
  theme_bw()
ggplot(lr.coeffs.covariates, aes(LongRunCoeff,CaseLog)) +
  geom_point() +
  geom_smooth(method=lm, color="darkred", fill="blue") + 
  stat_cor(method = "pearson") +
  theme_bw()

