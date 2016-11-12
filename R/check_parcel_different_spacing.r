#Português
#Descrição: Esta função verifica parcelas com espaçamento diferente em idades i e i+1, é necessário que base esteja pareada inclusive
#o campo que representa o espaçamento, dúvidas sobre como parear sua base consulte o pacote Fgmutils
#Parametros: base de dados(database), nome do campo que contém as parcelas (parcel_name), nome do campo que contém o espaçamento na primeira idade (spacing_age1),
#nome do campo que contém o espaçamento na segunda idade (spacing_age2), variável (s) que deseja agrupar o resultado da análise (variables_to_group)
#Exemplo de chamada: check_parcel_different_spacing(database,"parcela","espacamento1","espacamento2",c("parcela"))

#' @title check_parcel_different_spacing
#' @description This function checks for partitions with different spacing at i and i + 1 ages, it is necessary that the base be paired including the field representing the spacing, doubts about how to pair its base see the Fgmutils package
#' @param database data.frame, data.table or any database
#' @param parcel_name string containing the field name parcels in database 
#' @param spacing_age1 string containing the name of the field spacing in the first age
#' @param spacing_age2 string containing the name of the field spacing in the second age 
#' @param variables_to_group variable (s) that you want to group the result of the analysis, this can be a vector os strings or strign name to group
#' @import sqldf
#' @export
check_parcel_different_spacing = function(database,parcel_name,spacing_age1,spacing_age2,variables_to_group){
  if(check_variables(database,parcel_name)){
    if(check_variables(database,spacing_age1)){
      if(check_variables(database,spacing_age2)){
        query_one = mount_query(database,c(parcel_name,spacing_age1,spacing_age2),variables_to_group,1)
        result_query = sqldf(query_one)
        query_two = mount_query(result_query,parcel_name,c(spacing_age1,spacing_age2),3)
        return(as.data.frame(sqldf(query_two)))
      }
      else{
        find_missing_variable(database,spacing_age2)
      }
    }
    else{
      find_missing_variable(database,spacing_age1)
    }
  }
  else{
    find_missing_variable(database,parcel_name)
  }
}
