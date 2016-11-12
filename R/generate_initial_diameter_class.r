#Português
#Descrição: Esta função gera o campo classe dap inicial para auxiliar no processo de incremento diamétrico
#Parametros: base de dados (database), campo que representa parcela (plot_name), campo que representa idade arredondada (age_name)
#Exemplo de chamada: generate_initial_diameter_class(database,"parcela","idadearred")

#' @title generate_initial_diameter_class
#' @description This function generates the initial class field to aid in the process of diametric increasing
#' @param database data.frame, data.table or any database
#' @param plot_name string with the name of field representing plots in database
#' @param age_name string with the name of field representing rounded age
#' @import data.table
#' @import sqldf
#' @import plyr
#' @import Fgmutils
#' @import utils
#' @export
generate_initial_diameter_class = function(database,plot_name,age_name){
	if(check_variables(database,plot_name)){
		if(check_variables(database,age_name)){
			database = as.data.table(database)

			query = paste("SELECT * FROM ",deparse(substitute(database))," ORDER BY ",plot_name,",",age_name,sep="")
			dados = as.data.table(sqldf(query))

			query = paste("SELECT ",plot_name,",",age_name," FROM dados GROUP BY ",plot_name,",",age_name," ORDER BY ",plot_name,",",
				age_name,sep="")

			dados_parcela = data.table(sqldf(query))
			eval( paste( 'setkey(dados_parcela,' , plot_name , ')' ) )
			dados_primeira_idade = ddply(dados_parcela,.(eval(parse(text=plot_name))),head,n=1)
			dados_primeira_idade = data.frame(dados_primeira_idade)

			query = paste("SELECT ",plot_name,",",age_name," FROM dados_primeira_idade",sep="")
			dados_primeira_idade = sqldf(query)
			names(dados_primeira_idade)[2] = "classeDAPIdadeInicial"

			database[,eval(parse(text="classeDAPIdadeInicial")) := -999]
			database = data.frame(database)
			database = atualizaCampoBase(camposAtualizar = "classeDAPIdadeInicial",
				baseAgrupada = dados_primeira_idade,
				baseAtualizar = database,
				keys = plot_name)
			return (database)
		}
		else{
			find_missing_variable(database,age_name)
		}
	}
	else{
		find_missing_variable(database,plot_name)
	}

}
