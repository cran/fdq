#Português
#Descrição: Esta função verifica se determinado conjunto de sítios ou locais existe em uma coluna da base de dados
#Parametros: base de dados (database), nome da coluna que representa sítio ou local (place_name) e valores a serem checados (places_to_check)
#Exemplo de chamada:  check_existing_place(database,"s",c(12,21,33))

#' @title check_existing_place
#' @description This function checks whether a particular set of sites or locations exists in a database column
#' @param database data.frame, data.table or any database
#' @param place_name string name of the column representing site or place
#' @param places_to_check value(s) to be checked, example: c(12,21,33)
#' @import data.table
#' @export
check_existing_place = function(database,place_name,places_to_check){
	flag = FALSE
	if(check_variables(database,place_name)){
		database = as.data.table(database)
		for(place in places_to_check){
			if(place %in% database[,eval(parse(text=place_name))]) {
				flag = TRUE
			}
			else{
				flag = FALSE
				break
			}
		}
		return (flag)
	}
	else{
		find_missing_variable(database,place_name)
	}
}
