# Este script esta pensado para correr en Google Cloud
# Eliminar la vm una vez que termine
#   8 vCPU
#  16 GB memoria RAM
# 256 GB espacio en disco

# Optimizacion Bayesiana de hiperparametros de  lightgbm, con el metodo TRADICIONAL de los hiperparametros originales de lightgbm
# 5-fold cross validation
# la probabilidad de corte es un hiperparametro

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection


require("dplyr")
require("rpart")
require("data.table")
require("rlist")

require("lightgbm")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

        add_ratios <- function(dataset, cantidad_importantes = 10){
        ### data -> se pone el dataset 
        ### modelo -> para saber las variables mas importantes hay que tener un modelo previo
        ### cantidad_importantes -> la cantidad de variables a usar en la combinatoria

        dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
        modelo  <- rpart(formula=    "clase01 ~ .", #quiero predecir clase_ternaria a partir de el resto de las variables
                        data =      dtrain,  #los datos donde voy a entrenar
                        xval=       0,
                        cp=        -1, #esto significa no limitar la complejidad de los splits
                        minsplit= 653,   #minima cantidad de registros para que se haga el split
                        minbucket=  317,   #tamaño minimo de una hoja
                        maxdepth=  7 )  #PRUEBO  una profundidad de 10
        
        mas_importantes <- head(sort(modelo$variable.importance, decreasing = TRUE), cantidad_importantes)
        col_names <- combn(names(mas_importantes), 2, paste, collapse = "___")
        
        
        ratios <- combn(dataset %>% select(names(mas_importantes)),
                        2,
                        function(x)x[[1]]/x[[2]])
        
        colnames(ratios) <- col_names
        
        ratios[is.infinite(ratios)] <- NA
        
        return (cbind(dataset,ratios))
        #### devuelve el dataset entero, sumado los ratios calculados
        
        }

setwd("C:/DataMining")  #Establezco el Working Directory
dataset  <- fread("C:/DataMining/datasets/competencia1_2022.csv")

#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ foto_mes==202101, clase01 := ifelse( clase_ternaria=="BAJA+2", 1L, 0L) ]

dataset[, clase_ternaria := NULL]


dataset <- add_ratios(dataset, 20)

##dim(dataset)
##dataset[, clase01]


dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo





kBO_iter  <- 100   #cantidad de iteraciones de la Optimizacion Bayesiana

#Aqui se cargan los hiperparametros
hs <- makeParamSet( 
         makeNumericParam("learning_rate",    lower=  0.001 , upper=    0.3),
         makeNumericParam("feature_fraction", lower=  0.2  , upper=    1.0),
         makeIntegerParam("min_data_in_leaf", lower=  0    , upper= 8000),
         makeIntegerParam("num_leaves",       lower= 16L   , upper= 2000L),
         makeNumericParam("prob_corte",       lower= 1/80  , upper=  1/20)  #esto sera visto en clase en gran detalle
        )

ksemilla_azar  <- 102191  #Aqui poner la propia semilla

#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./exp/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg), ext )

  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )

    cat( linea, file=archivo )
  }

  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )

  cat( linea, file=archivo, append=TRUE )  #grabo al archivo

  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------
#esta funcion calcula internamente la ganancia de la prediccion probs
fganancia_logistic_lightgbm   <- function( probs, datos) 
{
  vlabels  <- get_field(datos, "label")

  gan  <- sum( (probs > PROB_CORTE  ) *
               ifelse( vlabels== 1, 78000, -2000 ) )


  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales, la semilla del mal ...

EstimarGanancia_lightgbm  <- function( x )
{
  gc()  #libero memoria

  #llevo el registro de la iteracion por la que voy
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1

  PROB_CORTE <<- x$prob_corte   #asigno la variable global

  kfolds  <- 5   # cantidad de folds para cross validation

  param_basicos  <- list( objective= "binary",
                          metric= "custom",
                          first_metric_only= TRUE,
                          boost_from_average= TRUE,
                          feature_pre_filter= FALSE,
                          verbosity= -100,
                          max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                          min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                          lambda_l1= 0.0,         #por ahora, lo dejo fijo
                          lambda_l2= 0.0,         #por ahora, lo dejo fijo
                          max_bin= 31,            #por ahora, lo dejo fijo
                          num_iterations= 9999,   #un numero muy grande, lo limita early_stopping_rounds
                          force_row_wise= TRUE,   #para que los alumnos no se atemoricen con tantos warning
                          seed= ksemilla_azar
                        )

  #el parametro discolo, que depende de otro
  param_variable  <- list(  early_stopping_rounds= as.integer(50 + 5/x$learning_rate) )

  param_completo  <- c( param_basicos, param_variable, x )

  set.seed( ksemilla_azar )
  modelocv  <- lgb.cv( data= dtrain,
                       eval= fganancia_logistic_lightgbm,
                       stratified= TRUE, #sobre el cross validation
                       nfold= kfolds,    #folds del cross validation
                       param= param_completo,
                       verbose= -100
                      )

  #obtengo la ganancia
  ganancia  <- unlist(modelocv$record_evals$valid$ganancia$eval)[ modelocv$best_iter ]

  ganancia_normalizada  <-  ganancia* kfolds     #normailizo la ganancia

  #el lenguaje R permite asignarle ATRIBUTOS a cualquier variable
  attr(ganancia_normalizada ,"extras" )  <- list("num_iterations"= modelocv$best_iter)  #esta es la forma de devolver un parametro extra

  param_completo$num_iterations <- modelocv$best_iter  #asigno el mejor num_iterations
  param_completo["early_stopping_rounds"]  <- NULL     #elimino de la lista el componente  "early_stopping_rounds"

  #logueo 
  xx  <- param_completo
  xx$ganancia  <- ganancia_normalizada   #le agrego la ganancia
  xx$iteracion <- GLOBAL_iteracion
  loguear( xx, arch= klog )

  return( ganancia )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa

#Aqui se debe poner la carpeta de la computadora local
##setwd("~/buckets/b1/")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar el modelo
##dataset  <- fread("./datasets/competencia1_2022.csv")

#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/HT5330/", showWarnings = FALSE )
setwd("./exp/HT5330/")   #Establezco el Working Directory DEL EXPERIMENTO


#en estos archivos quedan los resultados
kbayesiana  <- "HT5330_v2.RDATA"
klog        <- "HT5330_v2.txt"


GLOBAL_iteracion  <- 0   #inicializo la variable global

#si ya existe el archivo log, traigo hasta donde llegue
if( file.exists(klog) )
{
  tabla_log  <- fread( klog )
  GLOBAL_iteracion  <- nrow( tabla_log )
}


#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase01") )

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ foto_mes==202101, campos_buenos, with=FALSE]),
                        label= dataset[ foto_mes==202101, clase01 ] )



#Aqui comienza la configuracion de la Bayesian Optimization
funcion_optimizar  <- EstimarGanancia_lightgbm   #la funcion que voy a maximizar

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar, #la funcion que voy a maximizar
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set=  hs,     #definido al comienzo del programa
              has.simple.signature = FALSE   #paso los parametros en una lista
             )

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= kbayesiana)  #se graba cada 600 segundos
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )   #cantidad de iteraciones
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI() )

#establezco la funcion que busca el maximo
surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if( !file.exists( kbayesiana ) ) {
  run  <- mbo(obj.fun, learner= surr.km, control= ctrl)
} else {
  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista
}


quit( save="no" )


# pero nosotros  NO nos vamos a quedar tranquilos sin cuestionar los hiperparametros originales
# min_data_in_leaf  y  num_leaves   estan relacionados entre ellos