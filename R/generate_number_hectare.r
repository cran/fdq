#Português
#Descrição: Esta função gera gera o campo NHa que representa o número de árvores sobreviventes por hectare
#Parametros: base de dados (database), nome do campo que contém área (area_name), nome do campo que contém o número de árvores (n_name)
#nome para o campo NHa (nha_name)
#Exemplo de chamada: generate_number_hectare(database,"area","n","NHa")

#' @title generate_number_hectare
#' @description This function generates the NHa, field that represents the number of surviving trees per hectare
#' @param database data.frame, data.table or any database
#' @param area_name string with the name of field containing area in database
#' @param n_name string with the name of field containing numbers of trees in database
#' @param nha_name string with name you want for the field number of trees per hectare
#' @import data.table
#' @export
generate_number_hectare = function(database,area_name,n_name,nha_name = "NHa"){
	if(check_variables(database,area_name)){
		if(check_variables(database,n_name)){
			database = as.data.table(database)
			database[,eval(parse(text=nha_name)):=0]
			database[,eval(parse(text=nha_name)) := (eval(parse(text=n_name)))*10000/(eval(parse(text=area_name)))]

			return (as.data.frame(database))
		}
		else{
			find_missing_variable(database,n_name)
		}
	}
	else{
		find_missing_variable(database,area_name)
	}
}
