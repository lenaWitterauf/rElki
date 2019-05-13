create_list_parameterization <- function() {
  rJava::.jnew('de/lmu/ifi/dbs/elki/utilities/optionhandling/parameterization/ListParameterization')
}

read_option_id <- function(class_name, option_name) {
  rJava::.jfield(class_name,
                 'Lde/lmu/ifi/dbs/elki/utilities/optionhandling/OptionID;',
                 option_name)
}

set_list_parameterization_option <- function(parameterization, option, option_value) {
  rJava::.jcall(parameterization, 
                'Lde/lmu/ifi/dbs/elki/utilities/optionhandling/parameterization/ListParameterization;',
                'addParameter',
                option, rJava::.jcast(option_value))
}

parameterize_or_abort <- function(class_name, parameterization) {
  rJava::.jcall('de/lmu/ifi/dbs/elki/utilities/ClassGenericsUtil',
                'Ljava/lang/Object;',
                'parameterizeOrAbort',
                get_class_object(class_name),
                rJava::.jcast(parameterization, 'de/lmu/ifi/dbs/elki/utilities/optionhandling/parameterization/Parameterization'))
}

get_class_object <- function(class_name) {
  class_name_string <- rJava::.jnew('java/lang/String', gsub('/', '.', class_name))
  rJava::J(class = 'java/lang/Class', method = 'forName', class_name_string)
}

as_java_integer <- function(integer) {
  integer <- as.integer(integer)
  rJava::.jnew('java/lang/Integer', integer)
}

as_java_double <- function(double) {
  double <- as.double(double)
  rJava::.jnew('java/lang/Double', double)
}