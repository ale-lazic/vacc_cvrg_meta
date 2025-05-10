# Code author: Aleksandra Lazic (https://orcid.org/0000-0002-0433-0483)

# importing dataset

library(readr)

dat = read_csv("https://raw.githubusercontent.com/ale-lazic/vacc_cvrg_meta/refs/heads/main/vacc_cvrg_meta.csv")

# calculating Cohen's D

library(dplyr)

dat = mutate(dat, D = (M1-M2)/sqrt((SD1^2+SD2^2)/2))

# converting Cohen's D to Hedges' g

library(esc)

dat$HedgesG = hedges_g(dat$D, dat$N)

# calculating sampling variance

library(metafor)

# "SMD" for the standardized mean difference

dat =
  escalc(
    measure = "SMD",
    m1i = M1,
    sd1i = SD1,
    n1i = N1,
    m2i = M2,
    sd2i = SD2,
    n2i = N2,
    data = dat, 
    include = Design == "Between"
  ) 

# "SMCRP" for the standardized mean change using raw score standardization with pooled standard deviations (Cousineau, 2020)

dat =
  escalc(
    measure = "SMCRP",
    m1i = M1,
    sd1i = SD1,
    m2i = M2,
    sd2i = SD2,
    ni = N,
    ri = Correlation,
    data = dat, 
    include = Design == "Within"
  ) 

# fitting a random effects model

es <- rma(yi = HedgesG , vi = vi, data=dat)
es

# forest plot 

png(file='forestplot.png', res = 600, bg="white", w = 6, h = 5, units = "in")

forest(es, order = "obs",
       slab = paste(Study, N, sep=", N = "), 
       mlab="Random Effects Model",
       xlim=c(-3.7,1.5), 
       alim=c(-0.3, 0.5),
       ilab=cbind(Coverage, Design),
       ilab.xpos=c(-1.5,-0.8), 
       cex=0.75, 
       header="Study", 
       xlab = "Hedges' g")
text(c(-1.5,-0.8), es$k+2, c("Vaccination\nCoverages", "Design"), 
     cex=0.75, 
     font=2)

dev.off() 
