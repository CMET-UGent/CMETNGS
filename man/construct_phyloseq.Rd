% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/construct_phyloseq.R
\name{construct_phyloseq}
\alias{construct_phyloseq}
\title{Function to automatically generate a phyloseq object from mothur output}
\usage{
construct_phyloseq(
  shared,
  taxonomy,
  otureps = NULL,
  metadata = NULL,
  remove_singletons = TRUE,
  dataname = "phydata"
)
}
\arguments{
\item{shared}{file location string for the shared file
(preferably absolute path)}

\item{taxonomy}{file location string for the taxonomy file (preferably
absolute path)}

\item{otureps}{optional file location for the respresentative fasta file (
preferably absolute path)}

\item{metadata}{optional file location for the metadata.xlsx file (preferably
absolute path)}

\item{remove_singletons}{boolean (defaults to TRUE) indicating wether or not
absolute singletonds should be removed}

\item{dataname}{string indicating the name of data, will be used if the sample
names are completely numerical as a prefix (default: "phydata")}
}
\value{
A phyloseq object
}
\description{
This function uses a shared file (i.e. a mothur-formatted OTU contingency
table),a taxonomy file (i.e. an RDP naive bayesian fixed-rank
asignment detail) and an optional fasta file with representative sequences
for every OTU as well as an optional metadata file to be merged into a
phyloseq object.
}
\examples{
## Example without otureps
# make sure library(CMETNGS) is loaded

## Example with otureps

}
\keyword{mothur}
\keyword{phyloseq,}
