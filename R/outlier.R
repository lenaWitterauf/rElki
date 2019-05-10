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

#' @export
knnWeightOutlier <- function(data_matrix, k) {
  rJava::.jcall("RElki", "[D", "knnWeightOutlier", convert_to_java_double_array(data_matrix), as.integer(k))
}

#' @export
odin <- function(data_matrix, k) {
  rJava::.jcall("RElki", "[D", "odin", convert_to_java_double_array(data_matrix), as.integer(k))
}

#' @export
lof <- function(data_matrix, k) {
  rJava::.jcall("RElki", "[D", "lof", convert_to_java_double_array(data_matrix), as.integer(k))
}

#' @export
simplifiedLof <- function(data_matrix, k) {
  rJava::.jcall("RElki", "[D", "simplifiedLof", convert_to_java_double_array(data_matrix), as.integer(k))
}

#' @export
cof <- function(data_matrix, k) {
  rJava::.jcall("RElki", "[D", "cof", convert_to_java_double_array(data_matrix), as.integer(k))
}

#' @export
inflo <- function(data_matrix, k) {
  rJava::.jcall("RElki", "[D", "inflo", convert_to_java_double_array(data_matrix), as.integer(k))
}

#' @export
loop <- function(data_matrix, k) {
  rJava::.jcall("RElki", "[D", "loop", convert_to_java_double_array(data_matrix), as.integer(k))
}

#' @export
ldof <- function(data_matrix, k) {
  rJava::.jcall("RElki", "[D", "ldof", convert_to_java_double_array(data_matrix), as.integer(k))
}

#' @export
ldf <- function(data_matrix, k) {
  rJava::.jcall("RElki", "[D", "ldf", convert_to_java_double_array(data_matrix), as.integer(k))
}

#' @export
kdeos <- function(data_matrix, k) {
  rJava::.jcall("RElki", "[D", "kdeos", convert_to_java_double_array(data_matrix), as.integer(k))
}

#' @export
fastAbod <- function(data_matrix, k) {
  rJava::.jcall("RElki", "[D", "fastAbod", convert_to_java_double_array(data_matrix), as.integer(k))
}

cast_to_outlier_result <- function(algorithm_result) {
  rJava::.jcast(algorithm_result,
                'de/lmu/ifi/dbs/elki/result/outlier/OutlierResult')
}

run_outlier_algorithm <- function(algorithm, database) {
  result <- rJava::.jcall(algorithm,
                          'Lde/lmu/ifi/dbs/elki/result/Result;',
                          'run',
                          rJava::.jcast(database, 'de/lmu/ifi/dbs/elki/database/Database'))
  cast_to_outlier_result(result)
}

read_outlier_result_scores <- function(result, database) {
  scores <- rJava::.jcall(result,
                          'Lde/lmu/ifi/dbs/elki/database/relation/DoubleRelation;',
                          'getScores')
  read_result_scores(database, scores)
}
