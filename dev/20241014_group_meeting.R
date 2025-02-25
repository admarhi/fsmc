test0 <-
  tibble::tribble(
    ~uptake, ~secretion, ~flux, ~species,
    "m1", "m2", 1, "s1",
    "m1", "m3", 1, "s1",
    "m1", "m3", 1, "s2",
    "m1", "m4", 1, "s2",
    "m2", "m3", 1, "s2",
    "m2", "m4", 1, "s2",
    "m3", "m2", 1, "s3",
    "m3", "m4", 1, "s3"
  ) %>%
  pivotMiCo(
    species = "species",
    from = "uptake",
    to = "secretion",
    flux = "flux"
  )

test0 <- MicrobiomeFunction(test0, "test microbiome")

getMet(test0)
getCo(test0)
plot(test0)


ac <- MicrobiomeFunction(
  misosoup$ac_A1R12_1, "ac", metabolite_col = "metabolites", flux_col = "fluxes"
)
cit <- MicrobiomeFunction(
  misosoup$cit_A1R12_1, "cit", metabolite_col = "metabolites", flux_col = "fluxes"
)
f6p <- MicrobiomeFunction(
  misosoup$f6p_B3M02_1, "f6p", metabolite_col = "metabolites", flux_col = "fluxes"
)

ac
cit
f6p

plot(ac)
plot(cit)
plot(f6p)

alignment1 <- MicrobiomeFunctionAlignment(ac, cit, f6p, name = "Alignment Test")
alignment1

plotAlignmentHeatmap(alignment1, 0.8)
plotAlignmentNetwork(alignment1, 0.95)
