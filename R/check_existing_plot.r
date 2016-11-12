#Português
#Descrição: Esta função verifica se determinado conjunto de parcelas existe em uma coluna da base de dados
#Parametros: base de dados (database), nome da coluna que representa parcela (plots_name) e valores a serem checados (plots_to_check)
#Exemplo de chamada:  check_existing_plots(database,"parcela",c(356,122,560))

#' @title check_existing_plots
#' @description This function checks if a particular set of parcels exists in a database column
#' @param database data.frame, data.table or any database
#' @param plots_name string column name representing parcels in the base 
#' @param plots_to_check value(s) to be checked, example: c(356,122)
#' @import data.table
#' @export
check_existing_plots = function(database,plots_name,plots_to_check){
  flag = FALSE
  if(check_variables(database,plots_name)){
    for(plot in plots_to_check){
      database = as.data.table(database)
      if(plot %in% database[,eval(parse(text=plots_name))]) {
        flag = TRUE
      }
      else{
        flag = FALSE
        print(paste("Plot",plot,"not found in database!"))
        break
      }
    }
    return (flag)
  }
  else{
    find_missing_variable(database,plots_name)
  }
}
