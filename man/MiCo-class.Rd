% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllClasses.R
\docType{class}
\name{MiCo-class}
\alias{MiCo-class}
\title{Microbial Community Class}
\description{
A class to represent communities of microorganisms.
}
\section{Slots}{

\describe{
\item{\code{species}}{A character vector representing the microorganisms present in
the community.}

\item{\code{metabolites}}{A character vector representing the metabolites consumed
and produced within the community.}

\item{\code{fluxes}}{A numeric vector representing the fluxes of each metabolite in
the community.}

\item{\code{edges}}{A list containing a hash of neighbouring metabolites and
neighbouring species. For neighbouring metabolites, both the ingoing and
outgoing flux is specified.}

\item{\code{bin_matrix}}{A binary matrix representing the presence of fluxes.}

\item{\code{name}}{A string representing the name of the community.}

\item{\code{n_edges_matrix}}{A matrix representing the number of species in the
edge between two metabolites.}

\item{\code{flux_prod_j_matrix}}{A matrix representing the total flux of production
of j in the community.}

\item{\code{eff_flux_prod_j_matrix}}{A matrix giving the effective fluxes of
production of j in the community.}

\item{\code{flux_cons_i_matrix}}{A matrix giving the total flux of consumption of i
in the community.}

\item{\code{eff_flux_cons_i_matrix}}{A matrix giving the effective fluxes of
consumption of i in the community.}
}}

