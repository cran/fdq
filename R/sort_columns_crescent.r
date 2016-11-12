#Português
#Descrição: Ordena a base de dados de forma crescente basendo-se na coluna selecionada
#Parametros: base de dados (database), coluna(colums)
#Exemplo de chamada:  sort_columns_crescent(database,"dap")

#' @title sort_columns_crescent
#' @description Sorts the database incrementally based on the selected column
#' @param database data.frame, data.table or any database
#' @param column string with the name of the column you want sort the database
#' @import data.table
#' @export
sort_columns_crescent = function(database,column){
  if(check_variables(database,column)){

      data = as.data.table(database)
      return(as.data.frame(data[order(eval(parse(text=column))),]))
    }
  else{
    find_missing_variable(database,column)
  }
}
