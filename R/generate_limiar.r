#Português
#Descrição: Esta função gera um limiar para determinados valores basendo-se no campo informado e um valor máximo para ajuste
#Parametros: base de dados (database), campo que deseja-se trabalhar (field), valor máximo para limiar(max)
#Exemplo de chamada: generate_limiar(dfBrutos,"nha",1000)

#' @title generate_limiar
#' @description This function generates a threshold for certain values based on the field entered and a maximum value for setting
#' @param database, data.table, data.frame or any database
#' @param field, string name with column you want to work on
#' @param max, number with the maximum value you want to establish the threshold
#' @export
generate_limiar <- function(database, field, max) {
 	if(check_variables(database,field)){
 		if (! is.nan(max)){
		    database = data.frame(database)
		    for (i in 1:nrow(database))
		      if (database[i, field] > max)
		        database[i, field] = max
		  	}
		return(database)
 	}
 	else{
 		find_missing_variable(database,field)
 	}
}
