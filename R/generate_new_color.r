#Português
#Descrição: Esta função gera uma nova cor randômica sem repetir as que foram informadas no campo passado como parâmetro
#Parametros: cojuntos de cores existentes (colors)
#Exemplo de chamada: generate_new_color(database,c("#6140bc" "#e75bf7" "#d15102" "#6a0b9e" "#e8ad4e"))

#' @title generate_new_color
#' @description This function generates a new random color without repeating the ones that were entered in the last field as parameter
#' @param colors vector of strings containing existing colors, exemple: c("#6140bc" "#e75bf7" "#d15102" "#6a0b9e" "#e8ad4e")
#' @import randomcoloR
#' @export
generate_new_color = function(colors){
	flag = FALSE
	iterator = 1

	while(flag == FALSE){
		color = distinctColorPalette(iterator)
		if(color %in% colors){
			flag = FALSE
			iterator = iterator+1
		}
		else{
			flag = TRUE
		}
	}
	return (color)
}
