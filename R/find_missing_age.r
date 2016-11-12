#Português
#Descrição: Esta função identifica valores de idades inexistentes na base de dados e notifica os mesmos ao usuário
#Parametros: base de dados (database), nome do campo que representa idade (age_name), conjuntos de idades a serem checadas (ages_to_check)
#Exemplo de chamada:  find_missing_age(database,"idadearred",c(12,23,48,60,75))

#' @title find_missing_age
#' @description This function identifies the missing age values in the database and notifies them to the user.
#' @param database data.frame, data.table or any database
#' @param age_name string that contains the field name that represents age in database
#' @param ages_to_check vector containing the values of ages to be checked like c(12,23,48)
#' @import data.table
#' @export
find_missing_age = function(database, age_name, ages_to_check){
  if(check_variables(database,age_name)){
    missing_age = NULL
    data = as.data.table(database)
    for (age in ages_to_check) {
      if(!(age %in% data[,eval(parse(text=age_name))])){
        missing_age = paste(missing_age,age,sep=" ")
      }
    }
    return(paste("Age(s):",missing_age," not found in ",deparse(substitute(database)), "!",sep=""))

  }
  else{
    find_missing_variable(database,age_name)
  }
}
