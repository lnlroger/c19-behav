library(tidyverse)
library(ggpubr)
library(ggpmisc)

M<-c(0,-0.95,-0.97,-0.93,-0.93,-0.93,-0.93,-0.93,-0.93,-0.95,0,-0.3,-0.4,-0.5,-0.5,-0.51,-0.52,-0.53,-0.54,-0.55)
PI<-rep(seq(0,0.95,0.1),2)
C<-c(rep("Country1",10),rep("Country2",10))

df<-data.frame(M,PI,C)

my.formula <- y ~ x

df%>%
  group_by(C)%>%
  ggplot(aes(x=PI,y=M))+
  geom_point(size=4,alpha=0.5)+
  stat_smooth(method="lm")+
  stat_cor(method = "pearson",label.y=c(-1.25), size=4)+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +    
  facet_wrap(~C)+
  theme_bw()
  


ggsave("compliance_eg.png")
