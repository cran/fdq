#Português
#Descrição: Esta função identifica valores de sítios ou locais inexistentes na base de dados e notifica os mesmos ao usuário
#Parametros: base de dados (database), nome do campo que representa sítio ou local (place_name), conjuntos de locais ou sítios a serem checados (places_to_check)
#Exemplo de chamada:  find_missing_place(database,"s",c(21,33,32)

#' @title find_missing_place
#' @description This function identifies values of sites or locations in the database and notifies them to the user
#' @param database data.frame, data.table or any database
#' @param place_name string that contains the field name representing site or place in database
#' @param places_to_check vector containing the values of places/sites to be checked like c(21,33,48)
#' @import data.table
#' @export
find_missing_place = function(database, place_name, places_to_check){
  if(check_variables(database,place_name)){
    missing_place = NULL
    data = as.data.table(database)
    for (place in places_to_check) {
      if(!(place %in% data[,eval(parse(text=place_name))])){
        missing_place = paste(missing_place,place,sep=" ")
      }
    }
    return(paste("Place(s):",missing_place," not found in ",deparse(substitute(database)), "!",sep=""))

  }
  else{
    find_missing_variable(database,place_name)
  }
}
