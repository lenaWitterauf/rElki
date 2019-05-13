#' LOF Outlier score calculation
#'
#' \code{lof} returns the LOF Outlier score for every observation in the
#' given data_matrix. LOF is short for Local Outlier Factor.
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

#' Simplified LOF Outlier score calculation
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
  k_option         <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/LOF$Parameterizer',
                                     'K_ID')
  parameterization <- set_list_parameterization_option(parameterization, 
                                                       k_option, as_java_integer(k))
  algorithm        <- parameterize_or_abort('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/SimplifiedLOF', parameterization)
  
  result           <- run_outlier_algorithm(algorithm, database)
  read_outlier_result_scores(result, database)
}

#' COF Outlier score calculation
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

#' INFLO Outlier score calculation
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
#' @param m Number. Is used as pruning threshold and has to be
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
  algorithm        <- parameterize_or_abort('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/INFLO', parameterization)
  
  result           <- run_outlier_algorithm(algorithm, database)
  read_outlier_result_scores(result, database)
}

#' LoOP Outlier score calculation
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
#' @param k_comp Number. Neighbourhood-size used to calculate outlier scores.
#'   Defaults to k_reach
#' @param lambda Number. If NA, ELKI's default is used (2.0).
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
loop <- function(data_matrix, k_reach, k_comp = k_reach, lambda = NA) {
  database         <- create_and_initialize_database(data_matrix)
  
  parameterization <- create_list_parameterization()  
  k_reach_option   <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/LoOP$Parameterizer',
                                     'KREACH_ID')
  parameterization <- set_list_parameterization_option(parameterization, 
                                                       k_reach_option, as_java_integer(k_reach))
  k_comp_option    <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/LoOP$Parameterizer',
                                     'KCOMP_ID')
  parameterization <- set_list_parameterization_option(parameterization, 
                                                       k_comp_option, as_java_integer(k_comp))
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

#' LDOF Outlier score calculation
#'
#' \code{ldof} returns the LDOF Outlier score for every observation in the
#' given data_matrix. LDOF is short for Local Distance-Based Outlier Factor.
#'
#' @seealso
#'   \url{https://elki-project.github.io/releases/release0.7.5/javadoc/de/lmu/ifi/dbs/elki/algorithm/outlier/lof/LDOF.html}
#'   for ELKI documentation.
#'
#' @param data_matrix numeric Matrix containing data the outlier score is
#'   calculated for. Rows are treated as observations, columns as features.
#' @param k Number. Neighbourhood-size used to calculate outlier scores.
#' @return List of outlier scores. The score at position x belongs to the
#'   observation given in row x of the original data_matrix.
#' @examples
#' data_matrix <- matrix(c(1:30), nrow=10, ncol=3)
#' result      <- ldof(data_matrix, 3)
#' for(index in c(1:10)) {
#'     print(paste('Observation:', paste(data_matrix[index,], collapse=',')))
#'     print(paste('Score:',       result[index]))
#' }
#'
#'
#' @export
ldof <- function(data_matrix, k) {
  database         <- create_and_initialize_database(data_matrix)
  
  parameterization <- create_list_parameterization()  
  k_option         <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/LDOF$Parameterizer',
                                     'K_ID')
  parameterization <- set_list_parameterization_option(parameterization, 
                                                       k_option, as_java_integer(k))
  algorithm        <- parameterize_or_abort('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/LDOF', parameterization)
  
  result           <- run_outlier_algorithm(algorithm, database)
  read_outlier_result_scores(result, database)
}

#' LDF Outlier score calculation
#'
#' \code{ldf} returns the LDF Outlier score for every observation in the given
#' data_matrix. Kernel density estimation in
#' combination with the reachability concept of LOF is used to calculate outlier score.
#'
#' @seealso
#' \url{https://elki-project.github.io/releases/release0.7.5/javadoc/de/lmu/ifi/dbs/elki/algorithm/outlier/lof/LDF.html}
#' for ELKI documentation.
#'
#' @param data_matrix numeric Matrix containing data the outlier score is
#'   calculated for. Rows are treated as observations, columns as features.
#' @param k Number. Neighbourhood-size used to calculate outlier scores.
#' @param h Number. Bandwidth scaling factor. Defaults to ELKI's default (1).
#' @param c Number. Scaling constant to limit value range to 1/c. Defaults to ELKI's default (0.1).
#' @return List of outlier scores. The score at position x belongs to the
#'   observation given in row x of the original data_matrix.
#' @examples
#' data_matrix <- matrix(c(1:30), nrow=10, ncol=3)
#' result      <- ldf(data_matrix, 3)
#' for(index in c(1:10)) {
#'     print(paste('Observation:', paste(data_matrix[index,], collapse=',')))
#'     print(paste('Score:',       result[index]))
#' }
#'
#'
#' @export
ldf <- function(data_matrix, k, h = 1, c = 0.1) {
  database         <- create_and_initialize_database(data_matrix)
  
  parameterization <- create_list_parameterization()  
  k_option         <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/LDF$Parameterizer',
                                     'K_ID')
  parameterization <- set_list_parameterization_option(parameterization, 
                                                       k_option, as_java_integer(k))
  h_option         <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/LDF$Parameterizer',
                                     'H_ID')
  parameterization <- set_list_parameterization_option(parameterization, 
                                                       h_option, as_java_double(h))
  c_option         <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/LDF$Parameterizer',
                                     'C_ID')
  parameterization <- set_list_parameterization_option(parameterization, 
                                                       c_option, as_java_double(c))
  algorithm        <- parameterize_or_abort('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/LDF', parameterization)
  
  result           <- run_outlier_algorithm(algorithm, database)
  read_outlier_result_scores(result, database)
}

#' KDEOS  Outlier score calculation
#'
#' \code{kdeos} returns the KDEOF Outlier score for every observation in the given
#' data_matrix. Kernel density estimation in
#' combination with LOF is used to calculate outlier score.
#'
#' @seealso
#' \url{https://elki-project.github.io/releases/release0.7.5/javadoc/de/lmu/ifi/dbs/elki/algorithm/outlier/lof/KDEOS.html}
#' for ELKI documentation.
#'
#' @param data_matrix numeric Matrix containing data the outlier score is
#'   calculated for. Rows are treated as observations, columns as features.
#' @param k_min Number. Minimum Neighbourhood-size used to calculate outlier scores.
#' @param k_max Number. Maximum Neighbourhood-size used to calculate outlier scores. Defaults to k_min
#' @param kernel_scale Number. Kernel scaling parameter. If NA, ELKI's default is used (0.25).
#' @param min_bandwidth Number. Minimum bandwidth for kernel density estimation. If NA, ELKI's default is used (0).
#' @param idim Number. Intrinsic dimensionality of this data set. If NA, ELKI's default is used (-1, implies using true data dimensionality).
#' @return List of outlier scores. The score at position x belongs to the
#'   observation given in row x of the original data_matrix.
#' @examples
#' data_matrix <- matrix(c(1:30), nrow=10, ncol=3)
#' result      <- kdeos(data_matrix, 3)
#' for(index in c(1:10)) {
#'     print(paste('Observation:', paste(data_matrix[index,], collapse=',')))
#'     print(paste('Score:',       result[index]))
#' }
#'
#'
#' @export
kdeos <- function(data_matrix, k_min, k_max = k_min, kernel_scale = NA, min_bandwidth = NA, idim = NA) {
  database         <- create_and_initialize_database(data_matrix)
  
  parameterization <- create_list_parameterization()  
  k_min_option     <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/KDEOS$Parameterizer',
                                     'KMIN_ID')
  parameterization <- set_list_parameterization_option(parameterization, 
                                                       k_min_option, as_java_integer(k_min))
  k_max_option     <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/KDEOS$Parameterizer',
                                     'KMAX_ID')
  parameterization <- set_list_parameterization_option(parameterization, 
                                                       k_max_option, as_java_integer(k_max))
  if(!is.na(kernel_scale)) {
    scale_option      <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/KDEOS$Parameterizer',
                                       'KERNEL_SCALE_ID')
    parameterization  <- set_list_parameterization_option(parameterization, 
                                                          scale_option, as_java_double(kernel_scale))
  }
  if(!is.na(min_bandwidth)) {
    bandwidth_option  <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/KDEOS$Parameterizer',
                                        'KERNEL_MIN_ID')
    parameterization  <- set_list_parameterization_option(parameterization, 
                                                          bandwidth_option, as_java_double(min_bandwidth))
  }
  if(!is.na(idim)) {
    idim_option       <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/KDEOS$Parameterizer',
                                        'IDIM_ID')
    parameterization  <- set_list_parameterization_option(parameterization, 
                                                          idim_option, as_java_integer(idim))
  }
  algorithm        <- parameterize_or_abort('de/lmu/ifi/dbs/elki/algorithm/outlier/lof/KDEOS', parameterization)
  
  result           <- run_outlier_algorithm(algorithm, database)
  read_outlier_result_scores(result, database)
}