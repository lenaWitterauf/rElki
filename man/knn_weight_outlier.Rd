% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outlier_distance.R
\name{knn_weight_outlier}
\alias{knn_weight_outlier}
\title{KNN Weight Outlier score calculation}
\usage{
knn_weight_outlier(data_matrix, k)
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
\code{knn_weight_outlier} returns the KNN Weight Outlier score for every observation in the
given data_matrix. The accumulated distances of an observation to its k-nearest observations is used as outlier score.
}
\examples{
data_matrix <- matrix(c(1:30), nrow=10, ncol=3)
result      <- knn_weight_outlier(data_matrix, 3)
for(index in c(1:10)) {
    print(paste('Observation:', paste(data_matrix[index,], collapse=',')))
    print(paste('Score:',       result[index]))
}


}
\seealso{
\url{https://elki-project.github.io/releases/release0.7.5/javadoc/de/lmu/ifi/dbs/elki/algorithm/outlier/distance/KNNWeightOutlier.html}
  for ELKI documentation.
}
