#Português
#Descrição: Esta função verifica se variaveis de medição possuem registros do tipo DAP2<DAP1, HT2<HT1 em idades consecutivas i+1 e i
#é necessário que a base já esteja pareada para realizar tal análise, para saber mais sobre pareamento consultar o pacote Fgmutils
#Parametros: base dados pareada (data_base), campo que contém as variáveis de medição na idade 1 (measurement_variable1),
#campo que contém as variáveis de medição na idade 2 (measurement_variable2)
#Exemplo de chamada: check_measurement_ages(database,"dap1","dap2")

#' @title check_measurement_ages
#' @description This function verifies if measurement variables have records of type DAP2 <DAP1, HT2 <HT1 in consecutive ages i + 1 and i tt is necessary that the base is already paired to perform such analysis, to know more about pairing consult the Fgmutils package
#' @param data_base data.frame, data.table or any database
#' @param measurement_variable1 string field containing the measurement variables at age 1 
#' @param measurement_variable2 string field containing the measurement variables at age 2 
#' @import data.table
#' @export
check_measurement_ages = function(data_base,measurement_variable1,measurement_variable2){
	if(measurement_variable1 %in% names(data_base)){
		if(measurement_variable2 %in% names(data_base)){
			paired_database = as.data.table(data_base)
			return(as.data.frame(as.data.table(data_base)
				[eval(parse(text=measurement_variable2))<eval(parse(text=measurement_variable1)), ]))
		}
		else{
			paste("Variable",measurement_variable2,"not found in the database!")
		}
	}
	else{
		paste("Variable",measurement_variable1,"not found in the database!")
	}
}
