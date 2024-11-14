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


a <- MicrobiomeFunction(sbs_list$ac_A1R12_1, "ac")
c <- MicrobiomeFunction(sbs_list$cit_A1R12_1, "cit")
f <- MicrobiomeFunction(sbs_list$f6p_A3R04_3, "f6p")

a
c
f

alignment1 <- MicrobiomeFunctionAlignment(a, c, f, name = "Alignment Test")
alignment1

plotAlignmentHeatmap(alignment1, 0.8)
plotAlignmentNetwork(alignment1, 0.95)
