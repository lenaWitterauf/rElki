convert_to_java_double_array <- function(data_matrix) {
  stopifnot(is.matrix(data_matrix))
  
  mode(data_matrix) <- 'double'
  java_array <- rJava::.jarray(data_matrix, dispatch=TRUE)
  rJava::.jcast(java_array, '[[D')
}

create_and_initialize_database <- function(data_matrix) {
  java_data_array     <- convert_to_java_double_array(data_matrix)
  database_connection <- rJava::.jnew('de/lmu/ifi/dbs/elki/datasource/ArrayAdapterDatabaseConnection', 
                                      java_data_array)
  database            <- create_database_with_connection(database_connection)
  rJava::.jcall(database, 'V', 'initialize')
  database
}

create_database_with_connection <- function(database_connection) {
  parameterization           <- create_list_parameterization()
  database_connection_option <- read_option_id('de/lmu/ifi/dbs/elki/database/AbstractDatabase$Parameterizer',
                                               'DATABASE_CONNECTION_ID')
  parameterization           <- set_list_parameterization_option(parameterization, 
                                                                 database_connection_option,
                                                                 database_connection)
  parameterize_or_abort('de/lmu/ifi/dbs/elki/database/StaticArrayDatabase', parameterization)
}

read_result_scores <- function(database, result_scores) {
  number_vector_field <- rJava::.jfield('de/lmu/ifi/dbs/elki/data/type/TypeUtil', 
                                        'Lde/lmu/ifi/dbs/elki/data/type/VectorFieldTypeInformation;',
                                        'NUMBER_VECTOR_FIELD')
  database_relation <- rJava::.jcall(database,
                                     'Lde/lmu/ifi/dbs/elki/database/relation/Relation;',
                                     'getRelation',
                                     rJava::.jcast(number_vector_field, 'de/lmu/ifi/dbs/elki/data/type/TypeInformation'),
                                     rJava::.jarray(rJava::.jnew(class = 'java/lang/Object')))
  
  raw_data_db_ids <- rJava::.jcall(database_relation,
                                   'Lde/lmu/ifi/dbs/elki/database/ids/DBIDs;',
                                   'getDBIDs')
  raw_data_db_ids <- rJava::.jcast(raw_data_db_ids, 
                                   'de/lmu/ifi/dbs/elki/database/ids/DBIDRange')
  result_score_iterator <- rJava::.jcall(result_scores,
                                         'Lde/lmu/ifi/dbs/elki/database/ids/DBIDIter;',
                                         'iterDBIDs')

  result <- c()
  while(rJava::.jcall(result_score_iterator, 'Z', 'valid')) {
    result_score_iterator_ref <- rJava::.jcast(result_score_iterator,
                                               'de/lmu/ifi/dbs/elki/database/ids/DBIDRef')
    result_offset             <- rJava::.jcall(raw_data_db_ids, 'I', 'getOffset', result_score_iterator_ref)
    result_value              <- rJava::.jcall(result_scores, 'D', 'doubleValue', result_score_iterator_ref)
    result[result_offset+1]   <- result_value
    result_score_iterator     <- rJava::.jcall(result_score_iterator, 
                                              'Lde/lmu/ifi/dbs/elki/database/ids/DBIDIter;',
                                              'advance')
  }
  
  result
}