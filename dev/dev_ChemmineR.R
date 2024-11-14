# BiocManager::install("ChemmineR")
library(ChemmineR)

# Make a list of BiGG metabolites and the metanetx
raw_bigg <- readr::read_delim("dev/BiGG_metabolites.tsv")
readr::problems(raw_bigg)
raw_bigg[3190:3191,]

clean_bigg <- raw_bigg %>%
  dplyr::filter(!is.na(.data$database_links)) %>% 
  dplyr::mutate(
    metanetx = dplyr::if_else(
      stringr::str_detect(.data$database_links, "MetaNetX"), 
      stringr::str_extract(.data$database_links, "MNXM\\d+"), NA),
    inchikey = dplyr::if_else(
      stringr::str_detect(.data$database_links, "InChI Key"), 
      stringr::str_extract(.data$database_links, "inchikey/[A-Z0-9\\-]+"), NA),
    inchikey = stringr::str_remove(.data$inchikey, "inchikey/")
  ) %>% 
  dplyr::select(-"model_list", -"database_links") %>% 
  dplyr::filter(!is.na(inchikey))

ikq <- ChemmineR::pubchemInchikey2sdf(unique(clean_bigg$inchikey))
ChemmineR::pubchemInchikey2sdf("VHRGRCVQAFMJIZ-UHFFFAOYSA-P")
# readr::write_rds(ikq, "dev/bigg_metabolites.rds")
# ikq <- readr::read_rds("dev/bigg_metabolites.rds")
sdfset <- ikq$sdf_set
ChemmineR::validSDF(sdfset)

apset <- ChemmineR::sdf2ap(sdfset)
fpset <- ChemmineR::desc2fp(apset, descnames=1024, type="FPset")
# Find highest Tanimoto score 
ChemmineR::fpSim(fpset[1], fpset, method="Tanimoto") 

as.matrix(fpset)
ChemmineR::cmp.duplicated(apset, type=2)
ChemmineR::cmp.cluster(db=apset, cutoff = c(0.7, 0.8, 0.9), quiet = TRUE)

cid(sdfset) <- ChemmineR::sdfid(sdfset)
sdfset

simMA <- sapply(cid(fpset), function(x) fpSim(fpset[x], fpset, sorted=FALSE))
hc <- hclust(as.dist(1-simMA), method="single") 
plot(as.dendrogram(hc), edgePar=list(col=4, lwd=2), horiz=TRUE) 
