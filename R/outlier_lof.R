#' LOF Outlier score calculation.
#'
#' \code{lof} returns the LOF Outlier score for every observation in the
#' given data_matrix. 
#'
#' @seealso
#'   \url{https://elki-project.github.io/releases/release0.7.5/javadoc/de/lmu/ifi/dbs/elki/algorithm/outlier/lof/LOF.html}
#'   for ELKI documentation.
#'
#' @param data_matrix numeric Matrix containing data the outlier score is
#'   calculated for. Rows are treated as observations, columns as features.
#' @param k Number. Neighbourhood-size used to calculate outlier scores.
#' @return List of outlier scores. The score at position x belongs to the
#'   observation given in row x of the original data_matrix.
#' @examples
#' data_matrix <- matrix(c(1:30), nrow=10, ncol=3)
#' result      <- lof(data_matrix, 3)
#' for(index in c(1:10)) {
#'     print(paste('Observation:', paste(data_matrix[index,], collapse=',')))
#'     print(paste('Score:',       result[index]))
#' }
#'
#'
#' @export
lof <- function(data_matrix, k) {
  database         <- create_and_initialize_database(data_matrix)
  
  parameterization <- create_list_parameterization()  
  k_option         <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/LOF$Parameterizer',
                                     'K_ID')
  parameterization <- set_list_parameterization_option(parameterization, 
                                                       k_option, as_java_integer(k))
  algorithm        <- parameterize_or_abort('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/LOF', parameterization)
  
  result           <- run_outlier_algorithm(algorithm, database)
  read_outlier_result_scores(result, database)
}

simplifiedLof <- function(data_matrix, k) {
  #TODO
  }

cof <- function(data_matrix, k) {
  #TODO
}

inflo <- function(data_matrix, k) {
  #TODO
}

loop <- function(data_matrix, k) {
  #TODO
}

ldof <- function(data_matrix, k) {
  #TODO
}

ldf <- function(data_matrix, k) {
  #TODO
}

kdeos <- function(data_matrix, k) {
  #TODO
}