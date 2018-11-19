#Português
#Descrição: Esta função verifica se o campo de idade possui mais de uma idade, retornando TRUE para para sim e FALSE para não
#Parametros: base de dados (database), nome da coluna que representa idade (age_name)
#Exemplo de chamada:  check_size_age_parcel(database,"idadearred",)
#' @title check_size_age_parcel
#' @description This function checks if the age field is more than one age, returning TRUE to for yes and FALSE for no
#' @param database data.frame, data.table or any database
#' @param age_name string containing the name of the column that represents age
#' @import data.table
#' @import stats
#' @export
check_size_age_parcel  = function(database, age_name){
	if(check_variables(database,age_name)){
		database = as.data.table(database)
		ages = unique(na.omit(database[,eval(parse(text=age_name))]))
		if(length(ages) == 1){
			return (FALSE)
		}
		else{
			return (TRUE)
		}
	}
	else{
		find_missing_variable(database,age_name)
	}
}
