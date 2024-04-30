


sol_to_exchange_network = function(sol_tmp,met_medium,filterLow=1e-03){
  # This function takes a solution from a yaml file and extracts exchanged reactions
  # for metabolites not present in the base medium. Therefore, it also requires
  # a yaml file with the metabolites present in the medium. It returns a dataframe
  # representing a directed network, in which negative (positive) values represent a
  # metabolite consumed (excreted) by a species.
  # author: apascualgarcia.github.io
  # date: April 25th, 2022 (Berlin)

  #browser()
  sol_tmp = unlist(sol_tmp) # convert solution in a named vector
  idx_ex = grep("_e_", names(sol_tmp)) # identify exchanged reactions
  if (length(idx_ex) == 0) {
    return()
  }
  sol_tmp = sol_tmp[idx_ex] # select only those
  idx_ex = c() # among reactions
  for (met in met_medium) {
    # exclude those involving metab present in the base medium
    idx_ex_tmp = grep(met, names(sol_tmp))
    idx_ex = c(idx_ex, idx_ex_tmp)
  }
  matched = match(seq(1, length(sol_tmp)), idx_ex) # take a list of indexes of sol_tmp and check which are not in idx_ex
  sol_tmp = sol_tmp[is.na(matched)] # we are looking for those not found
  # ... finally, exclude reactions that are too small
  idx_high = which(abs(sol_tmp) > filterLow)
  sol_tmp = sol_tmp[idx_high]
  #names(sol_tmp) # double check
  sol.names = names(sol_tmp)
  net.names = t(sapply(
    sol.names,
    FUN = function(x) { stringi::stri_split(x, fixed = "_e_")[[1]] }
  ))
  net.names = as.data.frame(net.names)
  colnames(net.names) = c("nodeA", "nodeB")
  net.names$nodeA <- stringi::stri_replace_first(
    net.names$nodeA, fixed = "R_EX_", replacement = "")
  net.names$nodeB = stringi::stri_replace_first(
    net.names$nodeB, fixed = "_i", replacement = "")
  net.df = net.names
  net.df$weight = sol_tmp
  rownames(net.df) = seq(1, dim(net.df)[1], 1)
  return(net.df)
}
