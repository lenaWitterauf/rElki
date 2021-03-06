% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outlier_anglebased.R
\name{fast_abod}
\alias{fast_abod}
\title{Fast-ABOD Outlier score calculation}
\usage{
fast_abod(data_matrix, k)
}
\arguments{
\item{data_matrix}{numeric Matrix containing data the outlier score is
calculated for. Rows are treated as observations, columns as features.}

\item{k}{Number. Neighbourhood-size used to calculate outlier scores.}
}
\value{
List of outlier scores. The score at position x belongs to the
  observation given in row x of the original data_matrix.
}
\description{
\code{fast_abod} returns the Fast-ABOD (approximateABOF) Outlier score for
every observation in the given data_matrix. ABOD is short for Angle-Based
Outlier Detection.
}
\examples{
data_matrix <- matrix(c(1:30), nrow=10, ncol=3)
result      <- fast_abod(data_matrix, 3)
for(index in c(1:10)) {
    print(paste('Observation:', paste(data_matrix[index,], collapse=',')))
    print(paste('Score:',       result[index]))
}


}
\seealso{
\url{https://elki-project.github.io/releases/release0.7.5/javadoc/de/lmu/ifi/dbs/elki/algorithm/outlier/anglebased/FastABOD}
for ELKI documentation.
}
