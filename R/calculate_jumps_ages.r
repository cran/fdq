#Português
#Descrição: Esta função gera os saltos por classe de diâmetro dentre as idades existentes na base de dados
#Parametros:base de dados (database), nome do campo que representa o identificador único de cada árvore (id_field)
#nome do campo que representa as idades arredondadas dentro da base(rounded_ages_field) e por fim o nome do campo
#que representa as classes de diâmetro (diameter_class_field)
#Exemplo de chamada: ccalculate_jumps_ages(parcela_356,"cod_id","idadearred","classedediametro")

#' @title calculate_jumps_ages
#' @description This function generates the jumps per diameter class among the ages in the database
#' @param database data.frame data.table
#' @param id_field string name of column with unique identifier of each tree
#' @param rounded_ages_field string name of column with rounded ages
#' @param diameter_class_field string name of column with diametric classes
#' @import sqldf
#' @import Fgmutils
#' @import data.table
#' @export
calculate_jumps_ages = function(database,id_field,rounded_ages_field,diameter_class_field){
	if(check_variables(database,id_field)){
		if(check_variables(database,rounded_ages_field)){
			if(check_variables(database,diameter_class_field)){
				idades = NULL
				fn = NULL
				campo = diameter_class_field
				idadearred = rounded_ages_field
				base = data.frame(database)
				eval(parse(text=(paste("dfSaltos = data.table(cod_id = unique(base$",id_field,"))",sep=""))))
				setkey(dfSaltos,"cod_id")
				eval(parse(text=(paste("idades = sort(unique(base$",rounded_ages_field,"))",sep=""))))
				for (idade in 1:length(idades)) {
  					eval(parse(text = paste0("dfSaltos$age_", idades[[idade]], " = 0")))
				}

				for (i in 1:length(idades)) {
  					idade = idades[[i]]
  					print(paste0("Collecting age ", idade, " in ", i, " of ", length(idades)))
  					b2 = fn$sqldf("select distinct cod_id, $campo from base where $idadearred == $idade")
  					dfSaltos[b2$cod_id, paste0("age_", idade)] = eval(parse(text = paste0("b2$", campo)))
				}

				if (length(idades) > 1){
					for (i in (length(idades)):2) {
  						idade = idades[[i]]
  						print(paste0("Calculating jumps in age ", idade, " in ", i, " of ", length(idades)))
  						eval(parse(text = paste0("dfSaltos$age_", idade, " = dfSaltos$age_", idade, " - dfSaltos$age_", idades[[i - 1]])))
					}
				}
				return (dfSaltos)
			}
			else{
				find_missing_variable(database,diameter_class_field)
			}
		}
		else{
			find_missing_variable(database,rounded_ages_field)
		}
	}
	else{
		find_missing_variable(database,id_field)
	}
}