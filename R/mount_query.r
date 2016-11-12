#Português
#Descrição: Essa função auxilia checagens que necessitam agrupar campos de determinadas medições
#Parâmetros
#Exemplo de chamada: mount_query(database,c("dap","age"),c("dap"),1)

#' @title mount_query
#' @description This auxiliary function checks that need to group fields of certain measurements
#' @param database data.frame, data.table or any database
#' @param select_names string vector with the name(s) of the column(s) you want to include in the selection 
#' @param group_names string vector with the name(s) of the column(s) you want to group the results 
#' @param option options to make the query, can be 1,2,3 each one for one use in the analysis functions
#' @import sqldf
#' @export
mount_query = function(database,select_names,group_names,option){
  if(option == 1){
    query = "SELECT"
    for (selected_name in select_names) {
      if(selected_name == select_names[length(select_names)]){
        query = paste(query,selected_name)
      }
      else{
        query = paste(query,selected_name,",")
      }
    }
    query = paste(query,"FROM",deparse(substitute(database)),"GROUP BY")
    for (group_name in group_names) {
      if(group_name == group_names[length(group_names)]){
        query = paste(query,group_name)
      }
      else{
        query = paste(query,group_name,",")
      }
    }
    return(query)
  }
  else if(option == 2){
    query = "SELECT"
    for (selected_name in select_names) {
      if(selected_name == select_names[length(select_names)]){
        query = paste(query,selected_name)
      }
      else{
        query = paste(query,selected_name,",")
      }
    }

    query = paste(query, " ,COUNT(",group_names,") AS Clone_Numbers FROM ",deparse(substitute(database))," GROUP BY",sep="")
    for (selected_name in select_names) {
      if(selected_name == select_names[length(select_names)]){
        query = paste(query,selected_name)
      }
      else{
        query = paste(query,selected_name,",")
      }
    }
    query = paste(query," HAVING COUNT(",group_names,")>1",sep="")
    return(query)
  }
  else if(option == 3){
    query = "SELECT"
    for (selected_name in select_names) {
      if(selected_name == select_names[length(select_names)]){
        query = paste(query,selected_name)
      }
      else{
        query = paste(query,selected_name,",")
      }
    }
    query = paste(query, " ,COUNT(",group_names[1],") AS Spacing_Age1, COUNT(",group_names[2],") AS Spacing_Age2 FROM ",deparse(substitute(database))," GROUP BY",sep="")
    for (selected_name in select_names) {
      if(selected_name == select_names[length(select_names)]){
        query = paste(query,selected_name)
      }
      else{
        query = paste(query,selected_name,",")
      }
    }
    query = paste(query," HAVING COUNT(",group_names[1],")>1 & COUNT(",group_names[2],")>1",sep="")
    return(query)

  }
}
