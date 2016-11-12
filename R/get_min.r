#Português
#Descrição: Esta função retorna o valor mínimo de um ou mais campos de variáveis de medição
#Parâmetros: base de dados (database), nome da coluna ou colunas que deseja-se saber o valor mínimo(variables)
#Exemplo de chamada: get_min(database,c("dap1","dap2"))

#' @title get_min
#' @description This function returns the minimum value of one or more fields of measurement variables
#' @param database data.frame, data.table or any database
#' @param variables string vector with name(s) of the column (s) you want to know the minimum value
#' @import data.table
#' @export
get_min = function(database,variables){
  if(check_variables(database,variables)){
    database = as.data.table(database)
    MIN = 999999
    for (variable in variables) {
      if(min(database[,eval(parse(text=variable))]) < MIN){
        MIN = min(database[,eval(parse(text=variable))])
      }
    }
    return (MIN)
  }
  else{
    find_missing_variable(database,variables)
  }

}
