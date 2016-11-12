#Português
#Descrição: Esta função gera uma nova cor randômica para cada classe de diametro existente na base
#Parametros: base de dados(database), nome da coluna que contém as classes de diametro (diameter_classe_name)
#Exemplo de chamada: generate_colors_dap_class(database,"classedediametro")

#' @title getColors
#' @description This function generates a new random color for each diameter class in the base
#' @param database data.frame, data.table or any database
#' @param diameter_classe_name string with the name of field (column) containing the diameter classes
#' @import data.table
#' @import randomcoloR
#' @export
getColors <- function(database,diameter_classe_name){
  if(check_variables(database,diameter_classe_name)){
    data = as.data.table(database)
    diameter_classes = unique(database[,diameter_classe_name])
    number_colors  = length(diameter_classes)
    colors = distinctColorPalette(number_colors)
    data[,eval(parse(text="color")):= ""]
    for (i in 1:number_colors) {
      data[eval(parse(text=diameter_classe_name)) == diameter_classes[i],eval(parse(text="color")) := colors[i]]
    }

    return (as.data.frame(data))

  }
  else{
    find_missing_variable(database,diameter_classe_name)
  }
}
