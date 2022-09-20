require("dplyr")



add_ratios <- function(data, modelo, cantidad_importantes = 10){
  ### data -> se pone el dataset 
  ### modelo -> para saber las variables mas importantes hay que tener un modelo previo
  ### cantidad_importantes -> la cantidad de variables a usar en la combinatoria
  
  mas_importantes <- head(sort(modelo$variable.importance, decreasing = TRUE), cantidad_importantes)
  col_names <- combn(names(mas_importantes), 2, paste, collapse = "___")
  
  
  ratios <- combn(data %>% select(names(mas_importantes)),
                  2,
                  function(x)x[[1]]/x[[2]])
  
  colnames(ratios) <- col_names
  
  ratios[is.infinite(ratios)] <- NA
  
  return (cbind(data,ratios))
  #### devuelve el dataset entero, sumado los ratios calculados
  
}



### ejemplo implementacion
# 
# 
# modelo  <- ranger( formula= "clase_binaria ~ .",
#                    data=  dtrain,
#                    importance = "impurity",
#                    probability=   TRUE,  #para que devuelva las probabilidades
#                    num.trees=     param$num.trees,
#                    mtry=          param$mtry,
#                    min.node.size= param$min.node.size,
#                    max.depth=     param$max.depth
# )
# 
# 
# 
# dataset <- add_ratios(dataset,modelo, 20)
# dataset  <- na.roughfix( dataset )
# 
# dtrain  <- dataset[ foto_mes==202101 ]
# dapply  <- dataset[ foto_mes==202103 ]
# 
# modelo_features  <- ranger( formula= "clase_binaria ~ .",
#                             data=  dtrain,
#                             importance = "impurity",
#                             probability=   TRUE,  #para que devuelva las probabilidades
#                             num.trees=     param$num.trees,
#                             mtry=          param$mtry,
#                             min.node.size= param$min.node.size,
#                             max.depth=     param$max.depth
# )
# 
# 
# 
# 
# 
# 
# prediccion  <- predict( modelo_features, dapply )