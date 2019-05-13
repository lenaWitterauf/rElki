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
#' result      <- odin(data_matrix, 3)
#' for(index in c(1:10)) {
#'     print(paste('Observation:', paste(data_matrix[index,], collapse=',')))
#'     print(paste('Score:',       result[index]))
#' }
#'
#'
#' @exportMethod
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
