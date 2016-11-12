#Português
#Descrição: Esta função gera gera o campo classe de diametro na base de dados baseando no campo DAP e uma determinada amplitude
#Parametros: base de dados (database), nome do campo que contém o diametro DAP (diameter_names), amplite desejada para criação das classes (amplitude)
# e por fim o nome que deeja para o campo classe de diametro (name_of_diameter_class)
#Exemplo de chamada: generate_diameter_classes(database,"dap",2,"classedediametro")

#' @title generate_diameter_classes
#' @description This function identifies non-existent column names in the database and informs the user
#' @param database data.frame, data.table or any database
#' @param diameter_names string with name of the field that contains the diameters of database
#' @param amplitude desired amplitude for class creation, example: 1,2,4,6,7
#' @param name_of_diameter_class string with name you want for the field class of diameter
#' @import data.table
#' @export
generate_diameter_classes = function(database,diameter_names,amplitude,name_of_diameter_class) {
  DAP_MIN = -1
  DAP_MAX = -1
  if(check_variables(database,diameter_names)){
    aux = 0
    data = as.data.table(sort_columns_crescent(database,diameter_names))
    DAP_MIN = as.integer(get_min(database,diameter_names))
    DAP_MAX = as.integer(get_max(database,diameter_names))

    data[,eval(parse(text=name_of_diameter_class)):= 0]
    for (i in seq(DAP_MIN,DAP_MAX,amplitude)) {
      data[eval(parse(text=diameter_names))>= aux & eval(parse(text=diameter_names)) <i, eval(parse(text=name_of_diameter_class)) := i]
      aux = i
    }
    return (as.data.table(data))
  }
  else{
    find_missing_variable(database,diameter_names)
  }
}
