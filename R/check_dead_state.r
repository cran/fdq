#Português
#Descrição: Esta função verifica se o campo estado da base é igual a morto (M) e existe algum tipo de medição
#Parametros: base de dados (data_base), nome do campo que representa estado (state), conjunto de variáveis de medição para serem analisadas (measurement_variables)
#Exemplo de chamada: check_dead_state(database,"estado",c("dap","ht"))

#' @title check_dead_state
#' @description This function checks if the base state field is equal to dead (M) and there is some kind of measurement
#' @param data_base data.frame data.table or any database
#' @param state string field name representing state column in database
#' @param measurement_variables string vector that contains a set of measurement variables to be analyzed, this variables are names of columns in database
#' @import data.table
#' @export
check_dead_state = function(data_base,state,measurement_variables){
	result = NULL
	if(state %in% names(data_base)){
		paired_database = as.data.table(data_base)
		state_m = paired_database[eval(parse(text=state))=='m' | eval(parse(text=state))=='M', ]
		for (variable in measurement_variables) {
			if(variable %in% names(state_m)){
				result = rbind(result,state_m[eval(parse(text=variable))!=0 | is.na(eval(parse(text=variable)))==FALSE, ])
			}
			else{
				paste(variable,"not found in database!")
			}
		}
		return(as.data.frame(result))
	}
	else{
		paste(state,"not found in the database")
	}
}
