#' KNN Outlier score calculation.
#'
#' \code{knn_outlier} returns the KNN Outlier score for every observation in the
#' given data_matrix. The distance of an observation to its k-nearest
#' observation is used as outlier score.
#'
#' @seealso
#' \url{https://elki-project.github.io/releases/release0.7.5/javadoc/de/lmu/ifi/dbs/elki/algorithm/outlier/distance/KNNOutlier.html}
#' for ELKI documentation.
#'
#' @param data_matrix numeric Matrix containing data the outlier score is
#'   calculated for. Rows are treated as observations, columns as features.
#' @param k Number. Neighbourhood-size used to calculate outlier scores.
#' @return List of outlier scores. The score at position x belongs to the
#'   observation given in row x of the original data_matrix.
#' @examples
#' data_matrix <- matrix(c(1:30), nrow=10, ncol=3)
#' result      <- knn_outlier(data_matrix, 3)
#' for(index in c(1:10)) {
#'     print(paste('Observation:', paste(data_matrix[index,], collapse=',')))
#'     print(paste('Score:',       result[index]))
#' }
#'
#'
#' @export
knn_outlier <- function(data_matrix, k) {
  database         <- create_and_initialize_database(data_matrix)
  
  parameterization <- create_list_parameterization()  
  k_option         <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/outlier/distance/KNNOutlier$Parameterizer',
                                     'K_ID')
  parameterization <- set_list_parameterization_option(parameterization, 
                                                       k_option, as_java_integer(k))
  algorithm        <- parameterize_or_abort('de/lmu/ifi/dbs/elki/algorithm/outlier/distance/KNNOutlier', parameterization)
  
  result           <- run_outlier_algorithm(algorithm, database)
  read_outlier_result_scores(result, database)
}

#' KNN Weight Outlier score calculation.
#'
#' \code{knn_weight_outlier} returns the KNN Weight Outlier score for every observation in the
#' given data_matrix. The accumulated distances of an observation to its k-nearest observations is used as outlier score.
#'
#' @seealso
#'   \url{https://elki-project.github.io/releases/release0.7.5/javadoc/de/lmu/ifi/dbs/elki/algorithm/outlier/distance/KNNWeightOutlier.html}
#'   for ELKI documentation.
#'
#' @param data_matrix numeric Matrix containing data the outlier score is
#'   calculated for. Rows are treated as observations, columns as features.
#' @param k Number. Neighbourhood-size used to calculate outlier scores.
#' @return List of outlier scores. The score at position x belongs to the
#'   observation given in row x of the original data_matrix.
#' @examples
#' data_matrix <- matrix(c(1:30), nrow=10, ncol=3)
#' result      <- knn_weight_outlier(data_matrix, 3)
#' for(index in c(1:10)) {
#'     print(paste('Observation:', paste(data_matrix[index,], collapse=',')))
#'     print(paste('Score:',       result[index]))
#' }
#'
#'
#' @export
knn_weight_outlier <- function(data_matrix, k) {
  database         <- create_and_initialize_database(data_matrix)
  
  parameterization <- create_list_parameterization()  
  k_option         <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/outlier/distance/KNNWeightOutlier$Parameterizer',
                                     'K_ID')
  parameterization <- set_list_parameterization_option(parameterization, 
                                                       k_option, as_java_integer(k))
  algorithm        <- parameterize_or_abort('de/lmu/ifi/dbs/elki/algorithm/outlier/distance/KNNWeightOutlier', parameterization)
  
  result           <- run_outlier_algorithm(algorithm, database)
  read_outlier_result_scores(result, database)
}

#' ODIN Outlier score calculation.
#'
#' \code{odin} returns the ODIN Outlier score for every observation in the given
#' data_matrix. The in-degree of the KNN graph of an observation is used as
#' outlier score.
#'
#' @seealso
#' \url{https://elki-project.github.io/releases/release0.7.5/javadoc/de/lmu/ifi/dbs/elki/algorithm/outlier/distance/ODIN.html}
#' for ELKI documentation.
#'
#' @param data_matrix numeric Matrix containing data the outlier score is
#'   calculated for. Rows are treated as observations, columns as features.
#' @param k Number. Neighbourhood-size used to calculate outlier scores.
#' @return List of outlier scores. The score at position x belongs to the
#'   observation given in row x of the original data_matrix.
#' @examples
#' data_matrix <- matrix(c(1:30), nrow=10, ncol=3)
#' result      <- odin(data_matrix, 3)
#' for(index in c(1:10)) {
#'     print(paste('Observation:', paste(data_matrix[index,], collapse=',')))
#'     print(paste('Score:',       result[index]))
#' }
#'
#'
#' @export
odin <- function(data_matrix, k) {
  database         <- create_and_initialize_database(data_matrix)
  
  parameterization <- create_list_parameterization()  
  k_option         <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/outlier/distance/ODIN$Parameterizer',
                                     'K_ID')
  parameterization <- set_list_parameterization_option(parameterization, 
                                                       k_option, as_java_integer(k))
  algorithm        <- parameterize_or_abort('de/lmu/ifi/dbs/elki/algorithm/outlier/distance/ODIN', parameterization)
  
  result           <- run_outlier_algorithm(algorithm, database)
  read_outlier_result_scores(result, database)
}