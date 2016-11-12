#Português
#Descrição: Esta análise verifica quais variáveis de medições possuem valores iguais a 0 e depois checa se existem variáveis nos estados que
#o usuário informou
#Parametros: base de dados (data_base) , nome do campo que contém a(s) variável(s) de medição a ser analisada (measurement_variables),
# nome do campo que contém os estados a serem checado (state_name), conjunto de estado(s) a serem verificados (states_to_check)
#Exemplo de chamada: check_zero_measurement(database,c("dap","ht"),"estados",c("N"))

#' @title check_zero_measurement
#' @description This analysis verifies which measurement variables have values equal to 0 and then checks if there are variables in the states that the user reported
#' @param data_base data.frame, data.table or any database
#' @param measurement_variables string vector containing name of the field(s) it represents measurement variable(s) to be analyzed
#' @param state_name string vector containing the name of the variable than represents state in database
#' @param states_to_check string vector containing the name of the the states to be checked, the user can inform this names in a string vector like ("F","N")
#' @import data.table
#' @export
check_zero_measurement = function(data_base, measurement_variables,state_name,states_to_check){
	aux_zero = NULL
	aux_state = NULL
	if(check_variables(data_base,measurement_variables)){
		if(check_variables(data_base,state_name)){
			paired_database = as.data.table(data_base)
			for (variable in measurement_variables) {
				aux_zero = rbind(aux_zero,paired_database[eval(parse(text=variable))==0, ])
			}
			aux_state = aux_zero[eval(parse(text=state_name)) %in% states_to_check, ]
			return(as.data.frame(aux_state))
		}
		else{
			find_missing_variable(data_base,state_name)
		}
	}
	else{
		find_missing_variable(data_base,measurement_variables)
	}
}
