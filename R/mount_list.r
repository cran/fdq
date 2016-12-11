#Português
#Descrição: Esta função serve de auxilio para a função check_diametric_increase, a mesma gera listas de valores a serem plotados como colunas
#Parametros: base de dados (database), um nome para o campo cor que será criado(fieldColorName), 
#nome do campo que representa o número de árvores (fieldQuantityName), nome do campo que contém as classes de diâmetro(diameter_diameter_classes_name_name)
#Exemplo de chamada: mount_list(dfBrutos,"color","n","classedediametro")
#' @title mount_list
#' @description This function serves as a help for the check_diametric_increase function, it generates lists of values to be plotted as columns
#' @param database, data.table, data.frame or any database
#' @param fieldColorName, string with the name you want for the color column
#' @param fieldQuantityName, string name of column with number of trees
#' @param diameter_classes_name, string name of column with the diameter classes
#' @import data.table
#' @export
mount_list = function(database,fieldColorName = "color",fieldQuantityName = "n",diameter_classes_name = "classedediametro"){
	if(check_variables(database,fieldQuantityName)){
		if(check_variables(database,diameter_classes_name)){
			 k = list()
			 tamMax = 0
			 for (i in 1:nrow(database)){  
			    classe =  database[i, diameter_classes_name]
			    
			    tmp =  database[database[, diameter_classes_name] == classe, c(fieldColorName, fieldQuantityName)]
			    
			    tmp = na.omit(tmp)
			    
			    str = ""
			    tam = 0
			    for(j in 1:nrow(tmp))
			    {
			      
			      if (j >1 ) str = paste0(str, ", ")
			      
			      str = paste0(str, tmp[j, fieldColorName], " = ", tmp[j, fieldQuantityName])
			     tam = tam +  tmp[j, fieldQuantityName]
			    }
			    
			    
			   if(length(tam) > 0)
			    if(tam > tamMax)
			      tamMax = tam

			   if(length(classe) > 0)
			   if (str != "")
			   if(!grepl(pattern = "nan", x =  str, ignore.case = T)){
			    if(!is.nan(classe) & !is.na(classe))
			    eval(parse(text = paste( "k$`", classe, "` = c(", str, ")")))
			    
			   }
			    else 
			      warning(paste("There was an error with the string ", str))
			  }
			  return(list(lista =  k, max = tamMax))
		}
		else{
			find_missing_variable(database,find_missing_variable)
		}
	}
	else{
		find_missing_variable(database,fieldQuantityName)
	}

}