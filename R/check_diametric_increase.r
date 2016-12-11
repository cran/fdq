#Português
#Descrição: Esta função é a implementação do processo proposto por Demolinari (2006) em sua dissertação de mestrado que
#é o incremento diamétrico, dada uma parcela é possível acompanhar todo crescimento das árvores contidas na mesma em todas as
#idades presentes na parcela, isso é realizado através de gráficos de barras coloridas onde cada cor representa uma classe de diametro
#e a medidade que as idades vão avançando para cada idade é gerado um novo gráfico que contém as mudanças representadas pelas cores, ou seja
#,a quantidade de árvores que migraram de classe, isso possbilita a identificação de comportamentos inconsistentes dentro da parcela.
#Exemplo de chamada: check_diametric_increase(dfBrutos,"cod_id","parcela",356,"idade","idadearred","classedediametro",dfMDD,"parcela","classedediametro","idadearred","nclasseha")
#' @title check_diametric_increase
#' @description This function is an implementation of the process proposed by Demolinari (2006) in his master's thesis that is the diametric increase, given a plot it is possible to follow all the growth of the trees contained in the same at all ages present in the plot, this is done through Of colored color graphics where each color represents a class of diameter and a meditation that as an advance for each age is generated a new graphic that contains as changes represented by cores or a quantity of trees that migrate of class, this can have an identification Of inconsistent behaviors within the plot.
#' @param database, data.table, data.frame or any database
#' @param id_field, string with the name of column containing the unique identifier of each tree
#' @param plot_field, string with the name of column containing the plots of database
#' @param plot_analyzed, the number of plot to be analyzed, example: 356
#' @param ages_field, string with the name of column containing the ages of each tree
#' @param rounded_ages_field, string with the name of column containing the rounded ages of each tree, example: 12, 36
#' @param diameter_classes_field, string with the name of column containing the diameter class of each tree
#' @param database_nhaclasses, data.table, data.frame, or any database containg the NHa Class of database used in this analysis. To obtain this database you simply have to submit the original base the generate_number_hectare_class function contained in this package
#' @param plot_field_database_nhaclassses, string with the name of column containing the plots of database resulting from the generate_number_hectare_class function
#' @param diameter_classes_field_database_nhaclasses, string with the name of column containing the diameter class of each tree of database resulting from the generate_number_hectare_class function
#' @param rounded_ages_field_database_nhaclasses, string with the name of column containing the rounded ages of each tree of database resulting from the generate_number_hectare_class function
#' @param nha_classes_database_nhaclasses, string with the name of column containing the NHa Class of database resulting from the generate_number_hectare_class function
#' @import data.table
#' @import ggplot2
#' @import Fgmutils
#' @export
check_diametric_increase = function(database,id_field,plot_field,plot_analyzed,ages_field,rounded_ages_field,
									diameter_classes_field,database_nhaclasses,plot_field_database_nhaclassses
									,diameter_classes_field_database_nhaclasses,rounded_ages_field_database_nhaclasses,
									nha_classes_database_nhaclasses){
	if(check_variables(database,id_field)){
		if(check_variables(database,plot_field)){
			if(check_variables(database,ages_field)){
				if(check_variables(database,rounded_ages_field)){
					if(check_variables(database,diameter_classes_field)){
						if(check_variables(database_nhaclasses,plot_field_database_nhaclassses)){
							if(check_variables(database_nhaclasses,rounded_ages_field_database_nhaclasses)){
								if(check_variables(database_nhaclasses,nha_classes_database_nhaclasses)){
									idades = NULL
									paleta = NULL
									df2 = NULL
									todasclasses = NULL
									idades2 = NULL
									classes = NULL
									mini = 0
									maxi = 0
									lst = NULL
									base = data.frame(database)[, c(id_field, plot_field, ages_field, diameter_classes_field)]
									base$nclasseha = -999
									initial_age = 12
									print("Rounding Ages...")
									eval(parse(text=(paste("base$",rounded_ages_field,"= roundAge(plots = base$"
									,plot_field,",ages = base$",ages_field,", firstAge = ",initial_age,")",sep=""))))

									eval(parse(text=(paste("parcelas = sample(x = unique(base$",plot_field,"), size = 60)",sep=""))))
									parcelas = c(plot_analyzed)

									for (p in 1:length(parcelas)) {
										print(paste("ploting parcel", p, " of ", length(parcelas)))

										  b2 = base[base[,plot_field] == parcelas[[p]],]
										  dfMDD2 = database_nhaclasses[database_nhaclasses[, plot_field_database_nhaclassses]== parcelas[[p]],]

										  eval(parse(text=(paste("idades = unique(b2$",rounded_ages_field,")",sep=""))))

										  eval(parse(text=(paste("dfMDD2[dfMDD2$",nha_classes_database_nhaclasses,"<= 0,'",nha_classes_database_nhaclasses,"'] = 1",sep=""))))

										  for (i in 1:nrow(dfMDD2)) {
										    eval(parse(text=(paste("b2[b2$",diameter_classes_field," == dfMDD2[i,'",diameter_classes_field_database_nhaclasses,"']
										    	& b2$",rounded_ages_field," == dfMDD2[i,'",rounded_ages_field_database_nhaclasses,"'],
										        '",nha_classes_database_nhaclasses,"'] = max(1 , dfMDD2[i,'",nha_classes_database_nhaclasses,"'])",sep=""))))
										  }

										  eval(parse(text=(paste("idades2 = intersect(idades, unique(b2$",rounded_ages_field,"))",sep=""))))
										  eval(parse(text=(paste("todasclasses = unique(dfMDD2$",diameter_classes_field_database_nhaclasses,")",sep=""))))
										  eval(parse(text=(paste("mini = min(b2$",diameter_classes_field,")",sep=""))))
										  eval(parse(text=(paste("maxi = max(b2$",diameter_classes_field,")",sep=""))))
										  ini = min(todasclasses)
										  fim = max(todasclasses)

										  for (j in 1:length(idades2)) {
											  eval(parse(text=(paste("classes = unique(b2[b2$",rounded_ages_field," == idades2[[j]],'",diameter_classes_field,"'])",sep=""))))

											  for(i in todasclasses){
											    if (!(i %in% classes) & (i >= mini & i <= maxi))
											    {
											        b2 = rbind(b2, data.frame(
											          cod_id = "1",
											          parcela = parcelas[[p]],
											          idade = idades[[j]],
											          classedediametro = i,
											          nclasseha = 1,
											          idadearred = idades[[j]]))

											        }
											    }
										    }
										      eval(parse(text=(paste("b2 <- b2[order(b2$",diameter_classes_field,"),]",sep=""))))

											  eval(parse(text=(paste("paleta = generate_colors_diameter_class(database = data.frame(
											  classe = unique( b2[b2$",rounded_ages_field," == idades[[1]],'",diameter_classes_field,"']))
											  , diameter_classe_name = 'classe')",sep=""))))

											  b2$color = "\"#000000\""

											  eval(parse(text=(paste("a <- b2[b2$",rounded_ages_field," == idades[[1]], ]",sep=""))))

											  for(i in 1:nrow(paleta)){
											  	a[eval(parse(text=(paste("a$",diameter_classes_field,sep="")))) == paleta[i, "classe"], "color"] = paste0("\"",  paleta[i, "color"] , "\"")
											  }


											  for (i in 1:nrow(a))
											    eval(parse(text=(paste("b2[b2$",id_field," == a[i,'",id_field,"'],'","color","'] = a[i, '","color","']",sep=""))))


											  dfsaida = data.frame(idadearred = double(), classedediametro = double(), nclasseha = double(), color = character())

											  for(i in unique(eval(parse(text=(paste("b2$",rounded_ages_field,sep="")))))){

											    eval(parse(text=(paste("df  = b2[b2$",rounded_ages_field," == i, ]",sep=""))))

											    for(c in unique(eval(parse(text=(paste("df$",diameter_classes_field,sep="")))))) {
											      eval(parse(text=(paste("df2 = df[df$",diameter_classes_field,"== c,]",sep=""))))
											      colors = unique(df2$color)

											      for (cor in colors)
											      dfsaida = rbind(dfsaida, data.frame(

											        idadearred = i,
											        classedediametro = c,
											        nclasseha = max( unique(df2$nclasseha) * ( nrow(df2[df2$color == cor,]) / nrow(df2)  ) )  ,
											        color = cor
											      ))

											    }
											  }
											    b2 = dfsaida

												levels(b2$color) = union(levels(b2$color),  "\"#EBEBEB\"")
												b2[b2$nclasseha <= 1, "color"] = "\"#EBEBEB\""

												maximo = 0
												  for (i in 1:length(idades)) {
												    eval(parse(text=(paste("lst = mount_list(database = b2[b2$",rounded_ages_field,"== idades[[i]], ],
												    	fieldQuantityName = 'nclasseha', diameter_classes_name = '",diameter_classes_field,"')",sep=""))))
												    if(lst$max > maximo)
												      maximo = lst$max
												  }

												for (i in idades) {
													plot_cols(
													list = mount_list(eval(parse(text=(paste("database = b2[b2$",rounded_ages_field," == i, ]",sep=""))))
													,fieldQuantityName = "nclasseha",
													diameter_classes_name = diameter_classes_field)$lista,
													name = paste0( "p", parcelas[[p]],"i", i),
													my_title = paste("Plot: ", parcelas[[p]], ", age: ",i, " months" ),
													ceil = maximo)
												}

									}

								}
								else{
									find_missing_variable(database_nhaclasses,nha_classes_database_nhaclasses)
								}
							}
							else{
								find_missing_variable(database_nhaclasses,rounded_ages_field_database_nhaclasses)
							}
						}
						else{
							find_missing_variable(database_nhaclasses,plot_field_database_nhaclassses)
						}
					}
					else{
						find_missing_variable(database,diameter_classes_field)
					}
				}
				else{
					find_missing_variable(database,rounded_ages_field)
				}
			}
			else{
				find_missing_variable(database,ages_field)
			}
		}
		else{
			find_missing_variable(database,plot_field)
		}
	}
	else{
		find_missing_variable(database,id_field)
	}

}
