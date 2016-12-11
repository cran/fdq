#Português
#Descrição: Esta função verifica se os clones de uma árvore possuem parcelas diferentes
#Parametros: base de dados(database), nome do campo que contém as parcelas (parcel_name), nome do campo que contém os clones (clone_name)
#variável (s) que deseja agrupar o resultado da análise (variables_to_group)
#Exemplo de chamada: check_clones_different_plot(database,"parcela","clone",c("parcela","clone"))

#' @title check_clones_different_plot
#' @description This function checks if the clones of a tree have different plots
#' @param database data.frame, data.table or any database
#' @param parcel_name string name of the field containing the parcels
#' @param clone_name string name of the field containing the clones
#' @param variables_to_group string(s) variable (s) that you want to group the result of the analysis
#' @import sqldf
#' @export
check_clones_different_plot = function(database,parcel_name,clone_name, variables_to_group){
    result = NULL
    if(check_variables(database,parcel_name)){
      if(check_variables(database,clone_name)){
        if(check_variables(database,variables_to_group)){
            query_one = mount_query(database,c(parcel_name,clone_name),variables_to_group,1)
            result_query = sqldf(query_one)
            query_two = mount_query(result_query,parcel_name,clone_name,2)
            return(as.data.frame(sqldf(query_two)))
        }
        else{
          find_missing_variable(database,variables_to_group)
        }
      }
      else{
        find_missing_variable(database,clone_name)
      }
    }
    else{
      find_missing_variable(database,parcel_name)
    }
}
