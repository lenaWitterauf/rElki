#' CASH clustering algorithm
#'
#' \code{cash} returns a list of clusters for the given data_matrix, formed according to CASH.
#' 
#' @seealso
#'   \url{https://elki-project.github.io/releases/release0.7.5/javadoc/de/lmu/ifi/dbs/elki/algorithm/clustering/correlation/CASH.html}
#'   for ELKI documentation.
#'
#' @param data_matrix numeric Matrix containing data the clusters are
#'   calculated for. Rows are treated as observations, columns as features.
#' @param minpts Number. Threshold for minimum number of points in a cluster.
#' @param maxlevel Number. Maximum level for splitting the hypercube.
#' @param jitter Number. Maximum jitter for distance values.
#' @param adjust Boolean. Apply adjustment heuristic for interval choosing.
#' @param mindim Number. Minimum dimensionality of the subspaces to be found.
#' @return List of clusters. The indices of each cluster belong to the indices
#'            in the given original data_matrix
#' @examples
#' data_matrix <- matrix(c(1:30), nrow=10, ncol=3)
#' result      <- cash(data_matrix, 5, 10, 1, adjust = TRUE)
#' for(cluster_index in c(1:length(result))) {
#'     print(paste('Cluster: ', cluster_index))
#'     print(paste('Indices: ', paste(result[[cluster_index]], collapse=',')))
#' }
#'
#'
#' @export
cash <- function(data_matrix, minpts, maxlevel, jitter, adjust = NA, mindim = NA) {
  database         <- create_and_initialize_database(data_matrix)
  
  parameterization <- create_list_parameterization()  
  
  minpts_option    <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/clustering/correlation/CASH$Parameterizer',
                                     'MINPTS_ID')
  parameterization <- set_list_parameterization_option(parameterization, 
                                                       minpts_option, as_java_integer(minpts))

  maxlevel_option  <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/clustering/correlation/CASH$Parameterizer',
                                     'MAXLEVEL_ID') 
  parameterization <- set_list_parameterization_option(parameterization, 
                                                       maxlevel_option, as_java_integer(maxlevel))

  jitter_option    <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/clustering/correlation/CASH$Parameterizer',
                                     'JITTER_ID') 
  parameterization <- set_list_parameterization_option(parameterization, 
                                                       jitter_option, as_java_integer(jitter))
  
  if(!is.na(mindim)) {
    mindim_option    <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/clustering/correlation/CASH$Parameterizer',
                                       'MINDIM_ID') 
    parameterization <- set_list_parameterization_option(parameterization, 
                                                         mindim_option, as_java_integer(mindim))
  }
  if(!is.na(adjust)) {
    adjust_option    <- read_option_id('de/lmu/ifi/dbs/elki/algorithm/clustering/correlation/CASH$Parameterizer',
                                       'ADJUST_ID') 
    parameterization <- set_list_parameterization_option(parameterization, 
                                                         adjust_option, as_java_boolean(adjust))
  }
  
  algorithm        <- parameterize_or_abort('de/lmu/ifi/dbs/elki/algorithm/clustering/correlation/CASH', parameterization)
  
  result           <- run_cluster_algorithm(algorithm, database)
  read_cluster_result(result, database)
}