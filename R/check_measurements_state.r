#Português
#Descrição: Esta função verifica se existe variavel de medição com valor igual a 0 e se os respectivos estados são diferentes de M,F,A
#Parametros: base de dados (data_base), conjunto de variaveis a serem analisadas (measurement_variables), nome do campo que representa o estado (state)
#Exemplo de chamada: check_measurement_ages(database,c("dap","ht"),"estado")

#' @title check_measurements_state
#' @description This function checks if there is a measurement variable with value equal to 0 and if the respective states are different from M, F, A
#' @param data_base data.frame, data.table or any database
#' @param measurement_variables set of variables to be analyzed, this set can be a vector of string with names of colunms  
#' @param state string name of the field that represents the state in database
#' @import data.table
#' @export
check_measurements_state = function(data_base,measurement_variables,state){
  aux_measurement = NULL
  if(check_variables(data_base,measurement_variables)){
    if(check_variables(data_base,state)){
      paired_database = as.data.table(data_base)
      for (variable in measurement_variables) {
        aux_measurement = rbind(aux_measurement,paired_database[eval(parse(text=variable))==0, ])
      }
      aux_state = aux_measurement[eval(parse(text=state))!="",]
      aux_state = aux_state[eval(parse(text=state))!= 'M' & eval(parse(text=state))!='F' & eval(parse(text=state))!='A', ]
      return(as.data.frame(aux_state))
    }
    else{
      find_missing_variable(data_base,state)
    }
  }
  else{
    find_missing_variable(data_base,measurement_variables)
  }
}
