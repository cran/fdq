#Portugês
#Descrição: Esta função concatena valores de idades em uma string para uma query e retorna a mesma
#Parâmetros: base de dados (database), nome da coluna que representa idade (age_name), idades que você deseja montar string (age_values)
#Exemplo de chamada: get_ages(database,"idadearred",c(12,24,36))

#' @title get_ages
#' @description This function concatenates age values in a string for a query and returns the same
#' @param database data.frame, data.table or any database
#' @param age_name string with the name of field (column) containing the ages
#' @param age_values vector with the age values you want to assemble string to made query, example: c(12,24,36)
#' @import sqldf
#' @import data.table
#' @export

get_ages = function(database,age_name, age_values){
	auxiliar = NULL
	for (value in age_values) {
		if(value == age_values[length(age_values)]){
			auxiliar = paste(auxiliar,value)
		}
		else{
			auxiliar = paste(auxiliar,value,",")
		}
	}
	query_two = paste("SELECT * FROM",deparse(substitute(database)),"WHERE",age_name,"IN (",auxiliar,")")
	return(as.data.table(sqldf(query_two)))
}
