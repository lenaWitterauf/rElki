read_outlier_result_scores <- function(result, database) {
  scores <- rJava::.jcall(result,
                          'Lde/lmu/ifi/dbs/elki/database/relation/DoubleRelation;',
                          'getScores')
  read_result_scores(database, scores)
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