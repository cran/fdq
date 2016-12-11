#Português
#Descrição: Esta função gera o campo classes de NHa que representa as classes com o número de árvores sobreviventes por hectare
#Parametros: base de dados (database)
#nome do campo que contém nha, que é o número de árvores sobreviventes por hectare (nha_field)
#nome do campo que contém o número de árvores em deteminada classe ou parcela  (n_field)
#nome do campo que contém as áreas (area_field)
#nome do campo que contém as parcelas (plot_field)
#nome do campo que contém as idades arredondadas (rounded_age_field)
#nome do campo que contém as idades (age_field)
#nome do campo que contém as classes de diâmetro (diameter_class_field)
#nome do campo que contém as estados (state_field)
#nome do campo que contém os diâmetros(dap) (dap_field)
#amplitude das classes de diêmatro, ex: 2
#Exemplo de chamada: generate_number_hectare_class(parcela_356,"nha","n","area","parcela","idadearred","idade","classedediametro","estado","dap",2)

#' @title generate_number_hectare_class
#' @description This function generates the NHa classes column which represents the classes with the number of surviving trees per hectare
#' @param database data.frame, data.table or any database
#' @param nha_field, name of the column containing the number of trees surviving per hectare
#' @param n_field, name of the column containing the number of trees in a plot, or diameter class
#' @param area_field, name of the column containing the area
#' @param plot_field, name of the column containing the plots
#' @param rounded_age_field, name of the column containing the rounded ages, example: 12,24,36
#' @param age_field, name of the column containing the  ages
#' @param diameter_class_field, name of the column containing the diameter classes
#' @param state_field, name of the column containing the states of trees
#' @param dap_field, name of the column containing the diameter (DAP) of trees
#' @param amplitude, integer integer number with the amplitude of diameter classes, example: 1, 2, 3
#' @import data.table
#' @import Fgmutils
#' @import sqldf
#' @export
generate_number_hectare_class = function(database,nha_field,n_field,area_field,plot_field,rounded_age_field,age_field,diameter_class_field,state_field,dap_field,amplitude = 2){
	if(check_variables(database,nha_field)){
		if(check_variables(database,n_field)){
			if(check_variables(database,area_field)){
				if(check_variables(database,plot_field)){
					if(check_variables(database,rounded_age_field)){
						if(check_variables(database,age_field)){
							if(check_variables(database,diameter_class_field)){
								if(check_variables(database,state_field)){
									if(check_variables(database,dap_field)){
										dfBrutos = data.frame(database)
										minimum_dap = 0
										maximum_dap = 0
										parcela = NULL
										idadearred = NULL
										classeDAP =  NULL

										eval(parse(text=paste("dfBrutos$",nha_field,"= 10000*dfBrutos$",n_field,"/dfBrutos$",area_field,sep="")))
										query = paste("SELECT ",plot_field,",",rounded_age_field,",",age_field,",",n_field,",",nha_field," FROM
										dfBrutos GROUP BY ",plot_field,",",rounded_age_field,",",age_field,sep="")
										dfMDDParcelasIdades = sqldf(query)	
										eval(parse(text=(paste("minimum_dap = min(dfBrutos$",diameter_class_field,")",sep=""))))
										eval(parse(text=(paste("maximum_dap = max(dfBrutos$",diameter_class_field,")",sep=""))))
										maximum_dap = maximum_dap + amplitude													  
										nParcelas = nrow(dfMDDParcelasIdades)										  
										eval(parse(text=paste("(classeDAP = defineClasses2(dados=c(",minimum_dap,",",maximum_dap,"), amplitude=",amplitude,"))",sep=""))) 
										linha = dfMDDParcelasIdades

										dfMDD2 = data.frame()
										for (i in 1:nParcelas) {
				
											eval(parse(text=paste("parcela = dfMDDParcelasIdades$",plot_field,"[i]",sep="")))
											eval(parse(text=paste("idadearred = dfMDDParcelasIdades$",rounded_age_field,"[i]",sep="")))
											
											
											cat("\nPlot:",parcela, ", age:", idadearred, ", class center diameter: ")
										
											for (j in 1:length(classeDAP$centro)) {
												linha = dfMDDParcelasIdades[dfMDDParcelasIdades$parcela==parcela& dfMDDParcelasIdades$idadearred==idadearred,]
											
												linha$classedediametro = classeDAP$centro[j]
												linha$classeDAPInf = classeDAP$classe[j,1]
												linha$classeDAPSup = classeDAP$classe[j,2]
												
												cat(linha$classedediametro, " ")
												
												eval(parse(text=paste("linha$nclasse = nrow(dfBrutos[dfBrutos$",diameter_class_field,
												" == linha$classedediametro & dfBrutos$",plot_field,"== parcela & dfBrutos$",
												rounded_age_field,"==idadearred & dfBrutos$",state_field,"!= 'M'  & dfBrutos$",
												state_field,"!='' & dfBrutos$",dap_field," > 0,])",sep="")))
													
											
												eval(parse(text=paste("linha$probabilidade = linha$nclasse/linha$",n_field,sep="")))
												eval(parse(text=paste("linha$nclasseha = linha$probabilidade * linha$",nha_field,sep="")))
												

												dfMDD2 = rbind(dfMDD2, linha)	
											}
										}
											dfMDD= dfMDD2
											dfMDD = data.frame(dfMDD)
											return(dfMDD)
									}
									else{
										find_missing_variable(database,dap_field)
									}		
								}
								else{
									find_missing_variable(database,state_field)
								}

							}
							else{
								find_missing_variable(database,diameter_class_field)
							}
						}
						else{
							find_missing_variable(database,age_field)
						}
					}
					else{
						find_missing_variable(database,rounded_age_field)
					}
				}
			}
			else{
				find_missing_variable(database,area_field)
			}
		}
		else{
			find_missing_variable(database,n_field)
		}
	}
	else{
		find_missing_variable(database,nha_field)
	}
}