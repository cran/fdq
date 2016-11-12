#Português
#Descrição: Esta função verifica se determinado conjunto de idades existe em uma coluna da base de dados
#Parametros: base de dados (database), nome da coluna que representa idade (ages_name) e idades a serem checadas
#Exemplo de chamada:  check_existing_ages(database,"idadearred",c("12","24","48"))

#' @title check_existing_ages
#' @description This function checks if a given set of ages exists in a database column
#' @param database data.frame data.table or any database
#' @param ages_name string name of the column representing ages 
#' @param ages_to_check string name/vector of the column (s) representing ages to be checked
#' @import data.table
#' @export
check_existing_ages = function(database,ages_name,ages_to_check){
	flag = FALSE
	if(check_variables(database,ages_name)){
		for(age in ages_to_check){
			database = as.data.table(database)
			if(age %in% database[,eval(parse(text=ages_name))]) {
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
		find_missing_variable(database,ages_name)
	}
}
