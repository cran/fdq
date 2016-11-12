#Português
#Descrição: Esta função retorna uma base de dados de um determinado sítio ou local presente na base original
#Parâmetros: base de dados (database), nome da coluna que representa o local ou sítio (place_name), valor do local ou sítio (place_value)
#Exemplo de chamada: get_place(database,"places",12)

#' @title get_place
#' @description This function returns a database from a particular site or location present in the original database
#' @param database data.frame, data.table or any database
#' @param place_name string with the name of the column that represents the place
#' @param place_value vector with values of that you want to filter the sites/places of the database
#' @import data.table
#' @import sqldf
#' @export
#English
#Description:
#Parameters: database, n(place_name), place value (place_value)

get_place = function(database,place_name,place_value){
	query_one = paste("SELECT * FROM",deparse(substitute(database)),"WHERE",place_name,"==",place_value)
	return(as.data.table(sqldf(query_one)))
}
