#Português
#Descrição: Esta função verifica se a coluna informada existe dentro da base
#Parametros: base de dados (database), conjunto de coluna(s) a serem analidas (variables)
#Exemplo de chamada:  check_variables(database,c("idadearred","dap"))
#' @title check_variables
#' @description This function checks if the entered column exists within the base
#' @param database data.frame, data.table or any database
#' @param variables vector of strings with names of columns
#' @examples
#' test <- data.frame("tree","diametrer","N")
#' check_variables(test,c("tree","diameter"))
#' @return TRUE for all variables in database, or FALSE for variables not present in columns
#' @export
check_variables = function(database,variables){
	flag = FALSE
	for (variable in variables) {
		if(variable %in% names(database)){
				flag = TRUE
		}
		else{
			flag = FALSE
			break
		}
	}
	return(flag)
}
