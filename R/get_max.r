#Português
#Descrição: Esta função retorna o valor máximo de um ou mais campos de variáveis de medição
#Parâmetros: base de dados (database), nome da coluna ou colunas que deseja-se saber o valor máximo(variables)
#Exemplo de chamada: get_max(database,c("dap1","dap2"))

#' @title get_max
#' @description This function returns the maximum value of one or more fields of measurement variables
#' @param database data.frame, data.table or any database
#' @param variables string vector with name(s) of the column (s) you want to know the maximum value
#' @import data.table
#' @export
get_max = function(database,variables){
  if(check_variables(database,variables)){
    database = as.data.table(database)
    MAX = -1
    for (variable in variables) {
      if(max(database[,eval(parse(text=variable))]) > MAX){
        MAX = max(database[,eval(parse(text=variable))])
      }
    }
    return (MAX)
  }
  else{
    find_missing_variable(database,variables)
  }

}
