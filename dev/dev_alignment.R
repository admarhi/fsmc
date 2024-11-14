load("dev/dev_data/mar15.rda")
load("dev/dev_data/dat1.rda")

c1 <- MiCo(
  species = mar15$Co1$MO,
  metabolites = mar15$Co1$met,
  fluxes = mar15$Co1$val,
  name = "mar15_Co1")

c2 <- MiCo(
  species = mar15$Co2$MO,
  metabolites = mar15$Co2$met,
  fluxes = mar15$Co2$val,
  name = "mar15_Co2")

c3 <- MiCo(
  species = dat1$Co1$MO,
  metabolites = dat1$Co1$met,
  fluxes = dat1$Co1$val,
  name = "dat1_Co1")

c4 <- MiCo(
  species = dat1$Co2$MO,
  metabolites = dat1$Co2$met,
  fluxes = dat1$Co2$val,
  name = "dat1_Co2")

alig <- MiCoAl(c1, c2, c3, c4); alig@alignment


plotAlignment(alig)

a <- MicrobiomeFunctionAlignment(as.list(c(
  f6p[c(1,20,50,94,120)], 
  ac[c(1,34,42,53,69)], 
  cit[c(1,12,23,43,54)]
)))
a
