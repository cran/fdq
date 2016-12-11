#Português
#Descrição: Esta função verifica se o campo estado da base é igual a morto (M) e existe algum tipo de medição
#Parametros: base de dados (data_base), nome do campo que representa estado (state), conjunto de variáveis de medição para serem analisadas (measurement_variables)
#Exemplo de chamada: check_dead_state(database,"estado",c("dap","ht"))

#' @title check_dead_state
#' @description This function checks if the base state field is equal to dead (M) and there is some kind of measurement
#' @param data_base data.frame data.table or any database
#' @param state string field name representing state column in database
#' @param measurement_variables string vector that contains a set of measurement variables to be analyzed, this variables are names of columns in database
#' @import sqldf
#' @export
check_dead_state = function(data_base,state,measurement_variables){
	if(check_variables(data_base,state)){
		if(check_variables(data_base,measurement_variables)){
				state_dead = sqldf(paste("SELECT * FROM",deparse(substitute(data_base)),"WHERE",state,"= 'm' OR",state,"= 'M' "))
				query = "SELECT * FROM state_dead WHERE"
				for (i in measurement_variables) {
					if(i == measurement_variables[length(measurement_variables)]){
      					query = paste(query,i)
   					}
    				else{
      					query = paste(query,i,"OR")
    				}
				}
				query = paste(query,"NOT IN (0)")
				return(as.data.frame(sqldf(query)))
			}
		else{
			find_missing_variable(data_base,measurement_variables)
		}
	}
	else{
		find_missing_variable(data_base,state)
	}
}
