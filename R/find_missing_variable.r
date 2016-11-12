#Português
#Descrição: Esta função identifica nomes de coluna (s) que são inexistentes na base de dados e infroma ao usuário
#Parametros: base de dados (database), conjunto (s) de nomes de colunas a serem checadas (variables)
#Exemplo de chamada: find_missing_variable(database,"dap")

#' @title find_missing_variable
#' @description This function identifies non-existent column names in the database and informs the user
#' @param data_base data.frame, data.table or any database
#' @param variables vector string that contains the name(s) of columns to be checked in database
#' @export
find_missing_variable = function(data_base,variables){
	variable_missing = NULL
	for(variable in variables){
		if(!(variable %in% names(data_base))){
			variable_missing  = paste(variable_missing,variable,sep=" ")
		}
	}
	return(paste("Variable(s):",variable_missing," not found in database!",sep=""))
}
