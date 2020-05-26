read_cluster_result <- function(result, database) {
  clusters <- rJava::.jcall(result,
                          'Ljava/util/List;',
                          'getAllClusters')
  convert_clusters(clusters, database)
}

convert_clusters <- function(clusters, database) {
  cluster_list    <- as.list(clusters)
  cluster_results <- list()
  cluster_index   <- 1
  for(cluster in cluster_list) {
    cluster                          <- rJava::.jcast(cluster, 'de/lmu/ifi/dbs/elki/data/Cluster')
    cluster_results[[cluster_index]] <- read_result_cluster(database, cluster)
    cluster_index                    <- cluster_index + 1
  }
  
  cluster_results
}

cast_to_cluster_result <- function(algorithm_result) {
  rJava::.jcast(algorithm_result,
                'de/lmu/ifi/dbs/elki/data/Clustering')
}

run_cluster_algorithm <- function(algorithm, database) {
  result <- rJava::.jcall(algorithm,
                          'Lde/lmu/ifi/dbs/elki/result/Result;',
                          'run',
                          rJava::.jcast(database, 'de/lmu/ifi/dbs/elki/database/Database'))
  cast_to_cluster_result(result)
}