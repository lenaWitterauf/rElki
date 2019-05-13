#' LOF Outlier score calculation.
#'
#' \code{lof} returns the LOF Outlier score for every observation in the
#' given data_matrix. LOF is short for Local Outlier Factor
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

#' Simplified LOF Outlier score calculation.
#'
#' \code{simplified_lof} returns the Simplified LOF Outlier score for every observation in the
#' given data_matrix. LOF is short for Local Outlier Factor.
#'
#' @seealso
#'   \url{https://elki-project.github.io/releases/release0.7.5/javadoc/de/lmu/ifi/dbs/elki/algorithm/outlier/lof/SimplifiedLOF.html}
#'   for ELKI documentation.
#'
#' @param data_matrix numeric Matrix containing data the outlier score is
#'   calculated for. Rows are treated as observations, columns as features.
#' @param k Number. Neighbourhood-size used to calculate outlier scores.
#' @return List of outlier scores. The score at position x belongs to the
#'   observation given in row x of the original data_matrix.
#' @examples
#' data_matrix <- matrix(c(1:30), nrow=10, ncol=3)
#' result      <- simplified_lof(data_matrix, 3)
#' for(index in c(1:10)) {
#'     print(paste('Observation:', paste(data_matrix[index,], collapse=',')))
#'     print(paste('Score:',       result[index]))
#' }
#'
#'
#' @export
simplified_lof <- function(data_matrix, k) {
  database         <- create_and_initialize_database(data_matrix)
  
  parameterization <- create_list_parameterization()  
  k_option         <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/SimplifiedLOF$Parameterizer',
                                     'K_ID')
  parameterization <- set_list_parameterization_option(parameterization, 
                                                       k_option, as_java_integer(k))
  algorithm        <- parameterize_or_abort('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/SimplifiedLOF', parameterization)
  
  result           <- run_outlier_algorithm(algorithm, database)
  read_outlier_result_scores(result, database)
}

#' COF Outlier score calculation.
#'
#' \code{cof} returns the COF Outlier score for every observation in
#' the given data_matrix. COF is short for Connectivity-based Outlier Factor.
#'
#' @seealso
#' \url{https://elki-project.github.io/releases/release0.7.5/javadoc/de/lmu/ifi/dbs/elki/algorithm/outlier/lof/COF.html}
#' for ELKI documentation.
#'
#' @param data_matrix numeric Matrix containing data the outlier score is
#'   calculated for. Rows are treated as observations, columns as features.
#' @param k Number. Neighbourhood-size used to calculate outlier scores.
#' @return List of outlier scores. The score at position x belongs to the
#'   observation given in row x of the original data_matrix.
#' @examples
#' data_matrix <- matrix(c(1:30), nrow=10, ncol=3)
#' result      <- cof(data_matrix, 3)
#' for(index in c(1:10)) {
#'     print(paste('Observation:', paste(data_matrix[index,], collapse=',')))
#'     print(paste('Score:',       result[index]))
#' }
#'
#'
#' @export
cof <- function(data_matrix, k) {
  database         <- create_and_initialize_database(data_matrix)
  
  parameterization <- create_list_parameterization()  
  k_option         <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/COF$Parameterizer',
                                     'K_ID')
  parameterization <- set_list_parameterization_option(parameterization, 
                                                       k_option, as_java_integer(k))
  algorithm        <- parameterize_or_abort('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/COF', parameterization)
  
  result           <- run_outlier_algorithm(algorithm, database)
  read_outlier_result_scores(result, database)
}

#' INFLO Outlier score calculation.
#'
#' \code{inflo} returns the INFLO Outlier score for every observation in the
#' given data_matrix. INFLO is short for Influence Outlierness using Symetric
#' Relationship. LOF in combination with a reverse kNN is used to calculate
#' outlier score.
#'
#' @seealso
#' \url{https://elki-project.github.io/releases/release0.7.5/javadoc/de/lmu/ifi/dbs/elki/algorithm/outlier/lof/INFLO.html}
#' for ELKI documentation.
#'
#' @param data_matrix numeric Matrix containing data the outlier score is
#'   calculated for. Rows are treated as observations, columns as features.
#' @param k Number. Neighbourhood-size used to calculate outlier scores.
#' @param m Floating point number. Is used as pruning threshold and has to be
#'   greater than 0.0. If NA, ELKI's default is used (1.0).
#' @return List of outlier scores. The score at position x belongs to the
#'   observation given in row x of the original data_matrix.
#' @examples
#' data_matrix <- matrix(c(1:30), nrow=10, ncol=3)
#' result      <- inflo(data_matrix, 3)
#' for(index in c(1:10)) {
#'     print(paste('Observation:', paste(data_matrix[index,], collapse=',')))
#'     print(paste('Score:',       result[index]))
#' }
#'
#'
#' @export
inflo <- function(data_matrix, k, m = NA) {
  database         <- create_and_initialize_database(data_matrix)
  
  parameterization <- create_list_parameterization()  
  k_option         <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/INFLO$Parameterizer',
                                     'K_ID')
  parameterization <- set_list_parameterization_option(parameterization, 
                                                       k_option, as_java_integer(k))
  if(!is.na(m)) {
    m_option         <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/INFLO$Parameterizer',
                                       'M_ID')
    parameterization <- set_list_parameterization_option(parameterization, 
                                                         m_option, as_java_double(m))
  }
  algorithm        <- parameterize_or_abort('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/COF', parameterization)
  
  result           <- run_outlier_algorithm(algorithm, database)
  read_outlier_result_scores(result, database)
}

#' LoOP Outlier score calculation.
#'
#' \code{loop} returns the LoOP Outlier score for every observation in the given
#' data_matrix. LoOP is short for Local Outlier Probabilities. LOF in
#' combination with statistical methods is used to calculate outlier score.
#'
#' @seealso
#' \url{https://elki-project.github.io/releases/release0.7.5/javadoc/de/lmu/ifi/dbs/elki/algorithm/outlier/lof/LoOP.html}
#' for ELKI documentation.
#'
#' @param data_matrix numeric Matrix containing data the outlier score is
#'   calculated for. Rows are treated as observations, columns as features.
#' @param k_reach Number. Neighbourhood-size used to calculate outlier scores.
#' @param k_comp Number. Neighbourhood-size used to calculate outlier scores. If
#'   NA, k_reach is used.
#' @param lambda Floating point number. If NA, ELKI's default is used (2.0).
#' @return List of outlier scores. The score at position x belongs to the
#'   observation given in row x of the original data_matrix.
#' @examples
#' data_matrix <- matrix(c(1:30), nrow=10, ncol=3)
#' result      <- loop(data_matrix, 3)
#' for(index in c(1:10)) {
#'     print(paste('Observation:', paste(data_matrix[index,], collapse=',')))
#'     print(paste('Score:',       result[index]))
#' }
#'
#'
#' @export
loop <- function(data_matrix, k_reach, k_comp = NA, lambda = NA) {
  database         <- create_and_initialize_database(data_matrix)
  
  parameterization <- create_list_parameterization()  
  k_reach_option   <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/LoOP$Parameterizer',
                                     'KREACH_ID')
  parameterization <- set_list_parameterization_option(parameterization, 
                                                       k_reach_option, as_java_integer(k_reach))
  k_comp_option    <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/LoOP$Parameterizer',
                                     'KCOMP_ID')
  if(!is.na(k_comp)) {
    parameterization <- set_list_parameterization_option(parameterization, 
                                                         k_comp_option, as_java_integer(k_comp))
  } else {
    parameterization <- set_list_parameterization_option(parameterization, 
                                                         k_comp_option, as_java_integer(k_reach))
  }
  if(!is.na(lambda)) {
    lambda_option    <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/LoOP$Parameterizer',
                                       'LAMBDA_ID')
    parameterization <- set_list_parameterization_option(parameterization, 
                                                         lambda_option, as_java_double(lambda))
  }
  algorithm        <- parameterize_or_abort('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/LoOP', parameterization)
  
  result           <- run_outlier_algorithm(algorithm, database)
  read_outlier_result_scores(result, database)
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