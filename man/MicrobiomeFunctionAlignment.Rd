% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MicrobiomeFunctionAlignment.R
\name{MicrobiomeFunctionAlignment}
\alias{MicrobiomeFunctionAlignment}
\title{Alignment of \code{MicrobiomeFunction} objects}
\usage{
MicrobiomeFunctionAlignment(
  ...,
  name = NA_character_,
  min_rxn = 1,
  min_mb = 2,
  pairwise = FALSE
)
}
\arguments{
\item{...}{A minimum of two \code{Microbiome Function} objects.}

\item{name}{Character scaler specifying a name or ID for the Alignment.}

\item{min_rxn}{Numeric scalar giving the minimum fraction of reactions.}

\item{min_mb}{Numeric scalar giving the minimum fraction of microbiomes.}

\item{pairwise}{Boolean scalar to toggle pairwise / multiple alignment.}
}
\description{
Alignment of \code{MicrobiomeFunction} objects
}
