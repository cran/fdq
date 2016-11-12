#Português
#Descrição: Esta análise verifica diferenças de idades em um base pareada, caso as idades arredondadas estejam em meses a verificação
#é se a diferença são 12 meses, caso esteja em ano as idades consecutivas devem apresentar apenas diferença de 1 ano, dúvidas sobre como
#parear sua base consultar o pacote Fgmutils
#Parametros:base de dados pareada (data_base), nome do campo que representa a idade arredondada na idade1 (rounded_age1),
#nome do campo que representa a idade arredondada na idade2 (rounde_age2)
#Exemplo de chamada: check_ages(database,"rounded_age1","rounded_age2", TRUE)

#' @title check_ages
#' @description This analysis verifies age differences on a paired basis, if the rounded ages are in months the check is if the difference is 12 months, if it is in year the consecutive ages should only present difference of 1 year, doubts about how to pair your base consult The Fgmutils package
#' @param data_base data.frame data.table
#' @param rounded_age1 string name of column rounde age one
#' @param rounded_age2 string name of column rounde age two
#' @param months TRUE for age in months or FALSE for age in years
#' @import data.table
#' @export
check_ages = function(data_base,rounded_age1,rounded_age2,months = FALSE){
	if((rounded_age1 %in% names(data_base))){
		if(rounded_age2 %in% names(data_base)){
			if(months == TRUE){
				paired_database = as.data.table(data_base)
				result = paired_database[((eval(parse(text=rounded_age2))-eval(parse(text=rounded_age1)))!=12), ]
				return (as.data.frame(result))
			}
			else{
				paired_database = as.data.table(data_base)
				result = paired_database[((eval(parse(text=rounded_age2))-eval(parse(text=rounded_age1)))!=1), ]
				return (as.data.frame(result))
			}

		}
		else{
			paste("Variable",rounded_age2,"not found in the database")
		}
	}
	else{
		paste("Variable",rounded_age1,"not found in the database")
	}
}
