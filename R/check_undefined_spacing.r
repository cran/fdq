#Português
#Descrição: Esta função verifica se existe algum registro com espaçamento indefinido (0 ou NA)
#Parametros: base de dados (data_base), nome da(s) variavel(s) que contém os espaçamentos (spacings)
#Exemplo de chamada: check_undefined_spacing(database,"espacamento")

#' @title check_undefined_spacing
#' @description This function checks if there is any record with undefined spacing (0 or NA)
#' @param data_base data.frame, data.table or any database
#' @param spacings string vector containing the name of the variable (s) than represent spacings in database
#' @import data.table
#' @export
check_undefined_spacing = function(data_base, spacings){
	aux_spacing = NULL
	if(check_variables(data_base,spacings)){
		paired_database = as.data.table(data_base)
		for (spacing in spacings) {
			aux_spacing = rbind(aux_spacing,paired_database[is.na(eval(parse(text=spacing)))==TRUE | eval(parse(text=spacing))==0, ])
		}
		return(as.data.frame(aux_spacing))
	}
	else{
		find_missing_variable(data_base,spacings)
	}
}
