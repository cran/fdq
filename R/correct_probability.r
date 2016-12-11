#Português
#Descrição: Esta função ajusta a probilidade após a geração das classes de árvores sobreviventes por hectare
#Parametros: base de dados (data_base) , nome do campo que contém as parcelas (plot_field),nome do campo que contém as idades arredondadas
#(rounded_age_field), nome do campo que contem a classe de árvores sobreviventes por hectare (nha_class_field)
#Exemplo de chamada: correct_probability(dfBrutos,"parcela","idadearred","probabilidade","nhaclasse")

#' @title correct_probability
#' @description This function will adjust the probability after generation of surviving tree classes per hectare
#' @param database data.frame, data.table or any database
#' @param plot_field string name of column with plots of database
#' @param rounded_age_field string name of column with rounded ages, examples: 12,24,36,48
#' @param probability_field string name of column with probabilitys
#' @param nha_class_field string name of column with NHa (surviving tree classes per hectare) classes, 
#' @import data.table
#' @import sqldf
#' @export
correct_probability = function(database,plot_field,rounded_age_field, probability_field,nha_class_field){
	if(check_variables(database,plot_field)){
		if(check_variables(database,rounded_age_field)){
			if(check_variables(database,probability_field)){
				if(check_variables(database,nha_class_field)){
					dfMDD = data.frame(database)
					query = paste("SELECT distinct ",plot_field,",",rounded_age_field," FROM dfMDD")
					df = sqldf(query)
					parcela = NULL
					prob2 = NULL
					nha = NULL
					for (i in 1:nrow(df)) {
						eval(parse(text=(paste("parcela = df$",plot_field,"[i]",sep=""))))
						eval(parse(text=(paste("idadearred = df$",rounded_age_field,"[i]",sep=""))))

						cat("\nPlot: ", parcela, "\nprob2:")
						
						eval(parse(text=(paste("prob = dfMDD[dfMDD$",plot_field,"==parcela & dfMDD$",rounded_age_field,"==idadearred,]$",probability_field))))	
						cat(sum(prob))
						fc = 1/sum(prob)	
						prob = fc * prob
						cat("\nprob2 corr:", sum(prob2))
						eval(parse(text=(paste("dfMDD[dfMDD$",plot_field,"==parcela & dfMDD$",rounded_age_field,"==idadearred,]$",probability_field," = prob"))))
						eval(parse(text=(paste("nha = dfMDD[dfMDD$",plot_field,"==parcela & dfMDD$",rounded_age_field,"==idadearred,]$",nha_class_field))))
						eval(parse(text=(paste("dfMDD[dfMDD$",plot_field,"==parcela & dfMDD$",rounded_age_field,"==idadearred,]$",nha_class_field," = prob * nha"))))
						remove(prob, nha)
					}

				remove(df)
				return (dfMDD)

				}
				else{
					find_missing_variable(database,nha_class_field)
				}
			}
			else{
				find_missing_variable(database,probability_field)
			}
		}
		else{
			find_missing_variable(database,rounded_age_field)
		}
	}
	else{
		find_missing_variable(database,plot_field)
	}
}