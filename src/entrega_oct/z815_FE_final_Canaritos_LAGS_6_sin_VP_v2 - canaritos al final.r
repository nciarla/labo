# source( "~/labo/src/FeatureEngineering/z815_FE_final.r")
#Necesita para correr en Google Cloud
# 256 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("Rcpp")

require("ranger")
require("randomForest")  #solo se usa para imputar nulos

require("lightgbm")

options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})

#------------------------------------------------------------------------------

Corregir_MachineLearning  <- function( dataset )
{
  gc()

  #acomodo los errores del dataset

  dataset[ foto_mes==201901,  ctransferencias_recibidas  := NA ]
  dataset[ foto_mes==201901,  mtransferencias_recibidas  := NA ]

  dataset[ foto_mes==201902,  ctransferencias_recibidas  := NA ]
  dataset[ foto_mes==201902,  mtransferencias_recibidas  := NA ]

  dataset[ foto_mes==201903,  ctransferencias_recibidas  := NA ]
  dataset[ foto_mes==201903,  mtransferencias_recibidas  := NA ]

  dataset[ foto_mes==201904,  ctransferencias_recibidas  := NA ]
  dataset[ foto_mes==201904,  mtransferencias_recibidas  := NA ]
  dataset[ foto_mes==201904,  ctarjeta_visa_debitos_automaticos  :=  NA ]
  dataset[ foto_mes==201904,  mttarjeta_visa_debitos_automaticos := NA ]
  dataset[ foto_mes==201904,  Visa_mfinanciacion_limite := NA ]

  dataset[ foto_mes==201905,  ctransferencias_recibidas  := NA ]
  dataset[ foto_mes==201905,  mtransferencias_recibidas  := NA ]
  dataset[ foto_mes==201905,  mrentabilidad     := NA ]
  dataset[ foto_mes==201905,  mrentabilidad_annual     := NA ]
  dataset[ foto_mes==201905,  mcomisiones      := NA ]
  dataset[ foto_mes==201905,  mpasivos_margen  := NA ]
  dataset[ foto_mes==201905,  mactivos_margen  := NA ]
  dataset[ foto_mes==201905,  ctarjeta_visa_debitos_automaticos  := NA ]
  dataset[ foto_mes==201905,  ccomisiones_otras := NA ]
  dataset[ foto_mes==201905,  mcomisiones_otras := NA ]

  dataset[ foto_mes==201910,  mpasivos_margen   := NA ]
  dataset[ foto_mes==201910,  mactivos_margen   := NA ]
  dataset[ foto_mes==201910,  ccomisiones_otras := NA ]
  dataset[ foto_mes==201910,  mcomisiones_otras := NA ]
  dataset[ foto_mes==201910,  mcomisiones       := NA ]
  dataset[ foto_mes==201910,  mrentabilidad     := NA ]
  dataset[ foto_mes==201910,  mrentabilidad_annual        := NA ]
  dataset[ foto_mes==201910,  chomebanking_transacciones  := NA ]
  dataset[ foto_mes==201910,  ctarjeta_visa_descuentos    := NA ]
  dataset[ foto_mes==201910,  ctarjeta_master_descuentos  := NA ]
  dataset[ foto_mes==201910,  mtarjeta_visa_descuentos    := NA ]
  dataset[ foto_mes==201910,  mtarjeta_master_descuentos  := NA ]
  dataset[ foto_mes==201910,  ccajeros_propios_descuentos := NA ]
  dataset[ foto_mes==201910,  mcajeros_propios_descuentos := NA ]

  dataset[ foto_mes==202001,  cliente_vip   := NA ]

  dataset[ foto_mes==202006,  active_quarter   := NA ]
  #dataset[ foto_mes==202006,  internet   := NA ]
  dataset[ foto_mes==202006,  mrentabilidad   := NA ]
  dataset[ foto_mes==202006,  mrentabilidad_annual   := NA ]
  dataset[ foto_mes==202006,  mcomisiones   := NA ]
  dataset[ foto_mes==202006,  mactivos_margen   := NA ]
  dataset[ foto_mes==202006,  mpasivos_margen   := NA ]
  dataset[ foto_mes==202006,  mcuentas_saldo   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_debito_transacciones  := NA ]
  dataset[ foto_mes==202006,  mautoservicio   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_visa_transacciones   := NA ]
  dataset[ foto_mes==202006,  mtarjeta_visa_consumo   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_master_transacciones  := NA ]
  dataset[ foto_mes==202006,  mtarjeta_master_consumo   := NA ]
  dataset[ foto_mes==202006,  ccomisiones_otras   := NA ]
  dataset[ foto_mes==202006,  mcomisiones_otras   := NA ]
  dataset[ foto_mes==202006,  cextraccion_autoservicio   := NA ]
  dataset[ foto_mes==202006,  mextraccion_autoservicio   := NA ]
  dataset[ foto_mes==202006,  ccheques_depositados   := NA ]
  dataset[ foto_mes==202006,  mcheques_depositados   := NA ]
  dataset[ foto_mes==202006,  ccheques_emitidos   := NA ]
  dataset[ foto_mes==202006,  mcheques_emitidos   := NA ]
  dataset[ foto_mes==202006,  ccheques_depositados_rechazados   := NA ]
  dataset[ foto_mes==202006,  mcheques_depositados_rechazados   := NA ]
  dataset[ foto_mes==202006,  ccheques_emitidos_rechazados   := NA ]
  dataset[ foto_mes==202006,  mcheques_emitidos_rechazados   := NA ]
  dataset[ foto_mes==202006,  tcallcenter   := NA ]
  dataset[ foto_mes==202006,  ccallcenter_transacciones   := NA ]
  dataset[ foto_mes==202006,  thomebanking   := NA ]
  dataset[ foto_mes==202006,  chomebanking_transacciones   := NA ]
  dataset[ foto_mes==202006,  ccajas_transacciones   := NA ]
  dataset[ foto_mes==202006,  ccajas_consultas   := NA ]
  dataset[ foto_mes==202006,  ccajas_depositos   := NA ]
  dataset[ foto_mes==202006,  ccajas_extracciones   := NA ]
  dataset[ foto_mes==202006,  ccajas_otras   := NA ]
  dataset[ foto_mes==202006,  catm_trx   := NA ]
  dataset[ foto_mes==202006,  matm   := NA ]
  dataset[ foto_mes==202006,  catm_trx_other   := NA ]
  dataset[ foto_mes==202006,  matm_other   := NA ]
  dataset[ foto_mes==202006,  ctrx_quarter   := NA ]
  dataset[ foto_mes==202006,  tmobile_app   := NA ]
  dataset[ foto_mes==202006,  cmobile_app_trx   := NA ]


  #dataset[ foto_mes==202010,  internet  := NA ]
  #dataset[ foto_mes==202011,  internet  := NA ]
  #dataset[ foto_mes==202012,  internet  := NA ]
  #dataset[ foto_mes==202101,  internet  := NA ]
  #dataset[ foto_mes==202102,  internet  := NA ]
  #dataset[ foto_mes==202103,  internet  := NA ]

  dataset[ foto_mes==202009,  tmobile_app  := NA ]
  dataset[ foto_mes==202010,  tmobile_app  := NA ]
  dataset[ foto_mes==202011,  tmobile_app  := NA ]
  dataset[ foto_mes==202012,  tmobile_app  := NA ]
  dataset[ foto_mes==202101,  tmobile_app  := NA ]
  dataset[ foto_mes==202102,  tmobile_app  := NA ]
  dataset[ foto_mes==202103,  tmobile_app  := NA ]

}
#------------------------------------------------------------------------------
#Esta es la parte que los alumnos deben desplegar todo su ingenio

Ajuste_Inflacion  <- function( dataset )
{
  gc()
    ## Indexación
dataset[ foto_mes==201901,mrentabilidad:=mrentabilidad*2.36]
dataset[ foto_mes==201902,mrentabilidad:=mrentabilidad*2.3]
dataset[ foto_mes==201903,mrentabilidad:=mrentabilidad*2.21]
dataset[ foto_mes==201904,mrentabilidad:=mrentabilidad*2.11]
dataset[ foto_mes==201905,mrentabilidad:=mrentabilidad*2.05]
dataset[ foto_mes==201906,mrentabilidad:=mrentabilidad*1.98]
dataset[ foto_mes==201907,mrentabilidad:=mrentabilidad*1.93]
dataset[ foto_mes==201908,mrentabilidad:=mrentabilidad*1.89]
dataset[ foto_mes==201909,mrentabilidad:=mrentabilidad*1.82]
dataset[ foto_mes==201910,mrentabilidad:=mrentabilidad*1.72]
dataset[ foto_mes==201911,mrentabilidad:=mrentabilidad*1.66]
dataset[ foto_mes==201912,mrentabilidad:=mrentabilidad*1.59]
dataset[ foto_mes==202001,mrentabilidad:=mrentabilidad*1.54]
dataset[ foto_mes==202002,mrentabilidad:=mrentabilidad*1.5]
dataset[ foto_mes==202003,mrentabilidad:=mrentabilidad*1.47]
dataset[ foto_mes==202004,mrentabilidad:=mrentabilidad*1.42]
dataset[ foto_mes==202005,mrentabilidad:=mrentabilidad*1.4]
dataset[ foto_mes==202006,mrentabilidad:=mrentabilidad*1.38]
dataset[ foto_mes==202007,mrentabilidad:=mrentabilidad*1.35]
dataset[ foto_mes==202008,mrentabilidad:=mrentabilidad*1.33]
dataset[ foto_mes==202009,mrentabilidad:=mrentabilidad*1.29]
dataset[ foto_mes==202010,mrentabilidad:=mrentabilidad*1.26]
dataset[ foto_mes==202011,mrentabilidad:=mrentabilidad*1.21]
dataset[ foto_mes==202012,mrentabilidad:=mrentabilidad*1.17]
dataset[ foto_mes==202101,mrentabilidad:=mrentabilidad*1.13]
dataset[ foto_mes==201901,mcuentas_saldo:=mcuentas_saldo*2.36]
dataset[ foto_mes==201902,mcuentas_saldo:=mcuentas_saldo*2.3]
dataset[ foto_mes==201903,mcuentas_saldo:=mcuentas_saldo*2.21]
dataset[ foto_mes==201904,mcuentas_saldo:=mcuentas_saldo*2.11]
dataset[ foto_mes==201905,mcuentas_saldo:=mcuentas_saldo*2.05]
dataset[ foto_mes==201906,mcuentas_saldo:=mcuentas_saldo*1.98]
dataset[ foto_mes==201907,mcuentas_saldo:=mcuentas_saldo*1.93]
dataset[ foto_mes==201908,mcuentas_saldo:=mcuentas_saldo*1.89]
dataset[ foto_mes==201909,mcuentas_saldo:=mcuentas_saldo*1.82]
dataset[ foto_mes==201910,mcuentas_saldo:=mcuentas_saldo*1.72]
dataset[ foto_mes==201911,mcuentas_saldo:=mcuentas_saldo*1.66]
dataset[ foto_mes==201912,mcuentas_saldo:=mcuentas_saldo*1.59]
dataset[ foto_mes==202001,mcuentas_saldo:=mcuentas_saldo*1.54]
dataset[ foto_mes==202002,mcuentas_saldo:=mcuentas_saldo*1.5]
dataset[ foto_mes==202003,mcuentas_saldo:=mcuentas_saldo*1.47]
dataset[ foto_mes==202004,mcuentas_saldo:=mcuentas_saldo*1.42]
dataset[ foto_mes==202005,mcuentas_saldo:=mcuentas_saldo*1.4]
dataset[ foto_mes==202006,mcuentas_saldo:=mcuentas_saldo*1.38]
dataset[ foto_mes==202007,mcuentas_saldo:=mcuentas_saldo*1.35]
dataset[ foto_mes==202008,mcuentas_saldo:=mcuentas_saldo*1.33]
dataset[ foto_mes==202009,mcuentas_saldo:=mcuentas_saldo*1.29]
dataset[ foto_mes==202010,mcuentas_saldo:=mcuentas_saldo*1.26]
dataset[ foto_mes==202011,mcuentas_saldo:=mcuentas_saldo*1.21]
dataset[ foto_mes==202012,mcuentas_saldo:=mcuentas_saldo*1.17]
dataset[ foto_mes==202101,mcuentas_saldo:=mcuentas_saldo*1.13]
dataset[ foto_mes==201901,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*2.36]
dataset[ foto_mes==201902,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*2.3]
dataset[ foto_mes==201903,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*2.21]
dataset[ foto_mes==201904,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*2.11]
dataset[ foto_mes==201905,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*2.05]
dataset[ foto_mes==201906,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*1.98]
dataset[ foto_mes==201907,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*1.93]
dataset[ foto_mes==201908,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*1.89]
dataset[ foto_mes==201909,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*1.82]
dataset[ foto_mes==201910,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*1.72]
dataset[ foto_mes==201911,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*1.66]
dataset[ foto_mes==201912,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*1.59]
dataset[ foto_mes==202001,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*1.54]
dataset[ foto_mes==202002,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*1.5]
dataset[ foto_mes==202003,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*1.47]
dataset[ foto_mes==202004,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*1.42]
dataset[ foto_mes==202005,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*1.4]
dataset[ foto_mes==202006,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*1.38]
dataset[ foto_mes==202007,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*1.35]
dataset[ foto_mes==202008,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*1.33]
dataset[ foto_mes==202009,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*1.29]
dataset[ foto_mes==202010,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*1.26]
dataset[ foto_mes==202011,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*1.21]
dataset[ foto_mes==202012,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*1.17]
dataset[ foto_mes==202101,mtarjeta_visa_consumo:=mtarjeta_visa_consumo*1.13]
dataset[ foto_mes==201901,mtarjeta_master_consumo:=mtarjeta_master_consumo*2.36]
dataset[ foto_mes==201902,mtarjeta_master_consumo:=mtarjeta_master_consumo*2.3]
dataset[ foto_mes==201903,mtarjeta_master_consumo:=mtarjeta_master_consumo*2.21]
dataset[ foto_mes==201904,mtarjeta_master_consumo:=mtarjeta_master_consumo*2.11]
dataset[ foto_mes==201905,mtarjeta_master_consumo:=mtarjeta_master_consumo*2.05]
dataset[ foto_mes==201906,mtarjeta_master_consumo:=mtarjeta_master_consumo*1.98]
dataset[ foto_mes==201907,mtarjeta_master_consumo:=mtarjeta_master_consumo*1.93]
dataset[ foto_mes==201908,mtarjeta_master_consumo:=mtarjeta_master_consumo*1.89]
dataset[ foto_mes==201909,mtarjeta_master_consumo:=mtarjeta_master_consumo*1.82]
dataset[ foto_mes==201910,mtarjeta_master_consumo:=mtarjeta_master_consumo*1.72]
dataset[ foto_mes==201911,mtarjeta_master_consumo:=mtarjeta_master_consumo*1.66]
dataset[ foto_mes==201912,mtarjeta_master_consumo:=mtarjeta_master_consumo*1.59]
dataset[ foto_mes==202001,mtarjeta_master_consumo:=mtarjeta_master_consumo*1.54]
dataset[ foto_mes==202002,mtarjeta_master_consumo:=mtarjeta_master_consumo*1.5]
dataset[ foto_mes==202003,mtarjeta_master_consumo:=mtarjeta_master_consumo*1.47]
dataset[ foto_mes==202004,mtarjeta_master_consumo:=mtarjeta_master_consumo*1.42]
dataset[ foto_mes==202005,mtarjeta_master_consumo:=mtarjeta_master_consumo*1.4]
dataset[ foto_mes==202006,mtarjeta_master_consumo:=mtarjeta_master_consumo*1.38]
dataset[ foto_mes==202007,mtarjeta_master_consumo:=mtarjeta_master_consumo*1.35]
dataset[ foto_mes==202008,mtarjeta_master_consumo:=mtarjeta_master_consumo*1.33]
dataset[ foto_mes==202009,mtarjeta_master_consumo:=mtarjeta_master_consumo*1.29]
dataset[ foto_mes==202010,mtarjeta_master_consumo:=mtarjeta_master_consumo*1.26]
dataset[ foto_mes==202011,mtarjeta_master_consumo:=mtarjeta_master_consumo*1.21]
dataset[ foto_mes==202012,mtarjeta_master_consumo:=mtarjeta_master_consumo*1.17]
dataset[ foto_mes==202101,mtarjeta_master_consumo:=mtarjeta_master_consumo*1.13]
dataset[ foto_mes==201901,mprestamos_personales:=mprestamos_personales*2.36]
dataset[ foto_mes==201902,mprestamos_personales:=mprestamos_personales*2.3]
dataset[ foto_mes==201903,mprestamos_personales:=mprestamos_personales*2.21]
dataset[ foto_mes==201904,mprestamos_personales:=mprestamos_personales*2.11]
dataset[ foto_mes==201905,mprestamos_personales:=mprestamos_personales*2.05]
dataset[ foto_mes==201906,mprestamos_personales:=mprestamos_personales*1.98]
dataset[ foto_mes==201907,mprestamos_personales:=mprestamos_personales*1.93]
dataset[ foto_mes==201908,mprestamos_personales:=mprestamos_personales*1.89]
dataset[ foto_mes==201909,mprestamos_personales:=mprestamos_personales*1.82]
dataset[ foto_mes==201910,mprestamos_personales:=mprestamos_personales*1.72]
dataset[ foto_mes==201911,mprestamos_personales:=mprestamos_personales*1.66]
dataset[ foto_mes==201912,mprestamos_personales:=mprestamos_personales*1.59]
dataset[ foto_mes==202001,mprestamos_personales:=mprestamos_personales*1.54]
dataset[ foto_mes==202002,mprestamos_personales:=mprestamos_personales*1.5]
dataset[ foto_mes==202003,mprestamos_personales:=mprestamos_personales*1.47]
dataset[ foto_mes==202004,mprestamos_personales:=mprestamos_personales*1.42]
dataset[ foto_mes==202005,mprestamos_personales:=mprestamos_personales*1.4]
dataset[ foto_mes==202006,mprestamos_personales:=mprestamos_personales*1.38]
dataset[ foto_mes==202007,mprestamos_personales:=mprestamos_personales*1.35]
dataset[ foto_mes==202008,mprestamos_personales:=mprestamos_personales*1.33]
dataset[ foto_mes==202009,mprestamos_personales:=mprestamos_personales*1.29]
dataset[ foto_mes==202010,mprestamos_personales:=mprestamos_personales*1.26]
dataset[ foto_mes==202011,mprestamos_personales:=mprestamos_personales*1.21]
dataset[ foto_mes==202012,mprestamos_personales:=mprestamos_personales*1.17]
dataset[ foto_mes==202101,mprestamos_personales:=mprestamos_personales*1.13]
dataset[ foto_mes==201901,mprestamos_prendarios:=mprestamos_prendarios*2.36]
dataset[ foto_mes==201902,mprestamos_prendarios:=mprestamos_prendarios*2.3]
dataset[ foto_mes==201903,mprestamos_prendarios:=mprestamos_prendarios*2.21]
dataset[ foto_mes==201904,mprestamos_prendarios:=mprestamos_prendarios*2.11]
dataset[ foto_mes==201905,mprestamos_prendarios:=mprestamos_prendarios*2.05]
dataset[ foto_mes==201906,mprestamos_prendarios:=mprestamos_prendarios*1.98]
dataset[ foto_mes==201907,mprestamos_prendarios:=mprestamos_prendarios*1.93]
dataset[ foto_mes==201908,mprestamos_prendarios:=mprestamos_prendarios*1.89]
dataset[ foto_mes==201909,mprestamos_prendarios:=mprestamos_prendarios*1.82]
dataset[ foto_mes==201910,mprestamos_prendarios:=mprestamos_prendarios*1.72]
dataset[ foto_mes==201911,mprestamos_prendarios:=mprestamos_prendarios*1.66]
dataset[ foto_mes==201912,mprestamos_prendarios:=mprestamos_prendarios*1.59]
dataset[ foto_mes==202001,mprestamos_prendarios:=mprestamos_prendarios*1.54]
dataset[ foto_mes==202002,mprestamos_prendarios:=mprestamos_prendarios*1.5]
dataset[ foto_mes==202003,mprestamos_prendarios:=mprestamos_prendarios*1.47]
dataset[ foto_mes==202004,mprestamos_prendarios:=mprestamos_prendarios*1.42]
dataset[ foto_mes==202005,mprestamos_prendarios:=mprestamos_prendarios*1.4]
dataset[ foto_mes==202006,mprestamos_prendarios:=mprestamos_prendarios*1.38]
dataset[ foto_mes==202007,mprestamos_prendarios:=mprestamos_prendarios*1.35]
dataset[ foto_mes==202008,mprestamos_prendarios:=mprestamos_prendarios*1.33]
dataset[ foto_mes==202009,mprestamos_prendarios:=mprestamos_prendarios*1.29]
dataset[ foto_mes==202010,mprestamos_prendarios:=mprestamos_prendarios*1.26]
dataset[ foto_mes==202011,mprestamos_prendarios:=mprestamos_prendarios*1.21]
dataset[ foto_mes==202012,mprestamos_prendarios:=mprestamos_prendarios*1.17]
dataset[ foto_mes==202101,mprestamos_prendarios:=mprestamos_prendarios*1.13]
dataset[ foto_mes==201901,mprestamos_hipotecarios:=mprestamos_hipotecarios*2.36]
dataset[ foto_mes==201902,mprestamos_hipotecarios:=mprestamos_hipotecarios*2.3]
dataset[ foto_mes==201903,mprestamos_hipotecarios:=mprestamos_hipotecarios*2.21]
dataset[ foto_mes==201904,mprestamos_hipotecarios:=mprestamos_hipotecarios*2.11]
dataset[ foto_mes==201905,mprestamos_hipotecarios:=mprestamos_hipotecarios*2.05]
dataset[ foto_mes==201906,mprestamos_hipotecarios:=mprestamos_hipotecarios*1.98]
dataset[ foto_mes==201907,mprestamos_hipotecarios:=mprestamos_hipotecarios*1.93]
dataset[ foto_mes==201908,mprestamos_hipotecarios:=mprestamos_hipotecarios*1.89]
dataset[ foto_mes==201909,mprestamos_hipotecarios:=mprestamos_hipotecarios*1.82]
dataset[ foto_mes==201910,mprestamos_hipotecarios:=mprestamos_hipotecarios*1.72]
dataset[ foto_mes==201911,mprestamos_hipotecarios:=mprestamos_hipotecarios*1.66]
dataset[ foto_mes==201912,mprestamos_hipotecarios:=mprestamos_hipotecarios*1.59]
dataset[ foto_mes==202001,mprestamos_hipotecarios:=mprestamos_hipotecarios*1.54]
dataset[ foto_mes==202002,mprestamos_hipotecarios:=mprestamos_hipotecarios*1.5]
dataset[ foto_mes==202003,mprestamos_hipotecarios:=mprestamos_hipotecarios*1.47]
dataset[ foto_mes==202004,mprestamos_hipotecarios:=mprestamos_hipotecarios*1.42]
dataset[ foto_mes==202005,mprestamos_hipotecarios:=mprestamos_hipotecarios*1.4]
dataset[ foto_mes==202006,mprestamos_hipotecarios:=mprestamos_hipotecarios*1.38]
dataset[ foto_mes==202007,mprestamos_hipotecarios:=mprestamos_hipotecarios*1.35]
dataset[ foto_mes==202008,mprestamos_hipotecarios:=mprestamos_hipotecarios*1.33]
dataset[ foto_mes==202009,mprestamos_hipotecarios:=mprestamos_hipotecarios*1.29]
dataset[ foto_mes==202010,mprestamos_hipotecarios:=mprestamos_hipotecarios*1.26]
dataset[ foto_mes==202011,mprestamos_hipotecarios:=mprestamos_hipotecarios*1.21]
dataset[ foto_mes==202012,mprestamos_hipotecarios:=mprestamos_hipotecarios*1.17]
dataset[ foto_mes==202101,mprestamos_hipotecarios:=mprestamos_hipotecarios*1.13]
dataset[ foto_mes==201901,mplazo_fijo_dolares:=mplazo_fijo_dolares*2.36]
dataset[ foto_mes==201902,mplazo_fijo_dolares:=mplazo_fijo_dolares*2.3]
dataset[ foto_mes==201903,mplazo_fijo_dolares:=mplazo_fijo_dolares*2.21]
dataset[ foto_mes==201904,mplazo_fijo_dolares:=mplazo_fijo_dolares*2.11]
dataset[ foto_mes==201905,mplazo_fijo_dolares:=mplazo_fijo_dolares*2.05]
dataset[ foto_mes==201906,mplazo_fijo_dolares:=mplazo_fijo_dolares*1.98]
dataset[ foto_mes==201907,mplazo_fijo_dolares:=mplazo_fijo_dolares*1.93]
dataset[ foto_mes==201908,mplazo_fijo_dolares:=mplazo_fijo_dolares*1.89]
dataset[ foto_mes==201909,mplazo_fijo_dolares:=mplazo_fijo_dolares*1.82]
dataset[ foto_mes==201910,mplazo_fijo_dolares:=mplazo_fijo_dolares*1.72]
dataset[ foto_mes==201911,mplazo_fijo_dolares:=mplazo_fijo_dolares*1.66]
dataset[ foto_mes==201912,mplazo_fijo_dolares:=mplazo_fijo_dolares*1.59]
dataset[ foto_mes==202001,mplazo_fijo_dolares:=mplazo_fijo_dolares*1.54]
dataset[ foto_mes==202002,mplazo_fijo_dolares:=mplazo_fijo_dolares*1.5]
dataset[ foto_mes==202003,mplazo_fijo_dolares:=mplazo_fijo_dolares*1.47]
dataset[ foto_mes==202004,mplazo_fijo_dolares:=mplazo_fijo_dolares*1.42]
dataset[ foto_mes==202005,mplazo_fijo_dolares:=mplazo_fijo_dolares*1.4]
dataset[ foto_mes==202006,mplazo_fijo_dolares:=mplazo_fijo_dolares*1.38]
dataset[ foto_mes==202007,mplazo_fijo_dolares:=mplazo_fijo_dolares*1.35]
dataset[ foto_mes==202008,mplazo_fijo_dolares:=mplazo_fijo_dolares*1.33]
dataset[ foto_mes==202009,mplazo_fijo_dolares:=mplazo_fijo_dolares*1.29]
dataset[ foto_mes==202010,mplazo_fijo_dolares:=mplazo_fijo_dolares*1.26]
dataset[ foto_mes==202011,mplazo_fijo_dolares:=mplazo_fijo_dolares*1.21]
dataset[ foto_mes==202012,mplazo_fijo_dolares:=mplazo_fijo_dolares*1.17]
dataset[ foto_mes==202101,mplazo_fijo_dolares:=mplazo_fijo_dolares*1.13]
dataset[ foto_mes==201901,mplazo_fijo_pesos:=mplazo_fijo_pesos*2.36]
dataset[ foto_mes==201902,mplazo_fijo_pesos:=mplazo_fijo_pesos*2.3]
dataset[ foto_mes==201903,mplazo_fijo_pesos:=mplazo_fijo_pesos*2.21]
dataset[ foto_mes==201904,mplazo_fijo_pesos:=mplazo_fijo_pesos*2.11]
dataset[ foto_mes==201905,mplazo_fijo_pesos:=mplazo_fijo_pesos*2.05]
dataset[ foto_mes==201906,mplazo_fijo_pesos:=mplazo_fijo_pesos*1.98]
dataset[ foto_mes==201907,mplazo_fijo_pesos:=mplazo_fijo_pesos*1.93]
dataset[ foto_mes==201908,mplazo_fijo_pesos:=mplazo_fijo_pesos*1.89]
dataset[ foto_mes==201909,mplazo_fijo_pesos:=mplazo_fijo_pesos*1.82]
dataset[ foto_mes==201910,mplazo_fijo_pesos:=mplazo_fijo_pesos*1.72]
dataset[ foto_mes==201911,mplazo_fijo_pesos:=mplazo_fijo_pesos*1.66]
dataset[ foto_mes==201912,mplazo_fijo_pesos:=mplazo_fijo_pesos*1.59]
dataset[ foto_mes==202001,mplazo_fijo_pesos:=mplazo_fijo_pesos*1.54]
dataset[ foto_mes==202002,mplazo_fijo_pesos:=mplazo_fijo_pesos*1.5]
dataset[ foto_mes==202003,mplazo_fijo_pesos:=mplazo_fijo_pesos*1.47]
dataset[ foto_mes==202004,mplazo_fijo_pesos:=mplazo_fijo_pesos*1.42]
dataset[ foto_mes==202005,mplazo_fijo_pesos:=mplazo_fijo_pesos*1.4]
dataset[ foto_mes==202006,mplazo_fijo_pesos:=mplazo_fijo_pesos*1.38]
dataset[ foto_mes==202007,mplazo_fijo_pesos:=mplazo_fijo_pesos*1.35]
dataset[ foto_mes==202008,mplazo_fijo_pesos:=mplazo_fijo_pesos*1.33]
dataset[ foto_mes==202009,mplazo_fijo_pesos:=mplazo_fijo_pesos*1.29]
dataset[ foto_mes==202010,mplazo_fijo_pesos:=mplazo_fijo_pesos*1.26]
dataset[ foto_mes==202011,mplazo_fijo_pesos:=mplazo_fijo_pesos*1.21]
dataset[ foto_mes==202012,mplazo_fijo_pesos:=mplazo_fijo_pesos*1.17]
dataset[ foto_mes==202101,mplazo_fijo_pesos:=mplazo_fijo_pesos*1.13]
dataset[ foto_mes==201901,minversion1_pesos:=minversion1_pesos*2.36]
dataset[ foto_mes==201902,minversion1_pesos:=minversion1_pesos*2.3]
dataset[ foto_mes==201903,minversion1_pesos:=minversion1_pesos*2.21]
dataset[ foto_mes==201904,minversion1_pesos:=minversion1_pesos*2.11]
dataset[ foto_mes==201905,minversion1_pesos:=minversion1_pesos*2.05]
dataset[ foto_mes==201906,minversion1_pesos:=minversion1_pesos*1.98]
dataset[ foto_mes==201907,minversion1_pesos:=minversion1_pesos*1.93]
dataset[ foto_mes==201908,minversion1_pesos:=minversion1_pesos*1.89]
dataset[ foto_mes==201909,minversion1_pesos:=minversion1_pesos*1.82]
dataset[ foto_mes==201910,minversion1_pesos:=minversion1_pesos*1.72]
dataset[ foto_mes==201911,minversion1_pesos:=minversion1_pesos*1.66]
dataset[ foto_mes==201912,minversion1_pesos:=minversion1_pesos*1.59]
dataset[ foto_mes==202001,minversion1_pesos:=minversion1_pesos*1.54]
dataset[ foto_mes==202002,minversion1_pesos:=minversion1_pesos*1.5]
dataset[ foto_mes==202003,minversion1_pesos:=minversion1_pesos*1.47]
dataset[ foto_mes==202004,minversion1_pesos:=minversion1_pesos*1.42]
dataset[ foto_mes==202005,minversion1_pesos:=minversion1_pesos*1.4]
dataset[ foto_mes==202006,minversion1_pesos:=minversion1_pesos*1.38]
dataset[ foto_mes==202007,minversion1_pesos:=minversion1_pesos*1.35]
dataset[ foto_mes==202008,minversion1_pesos:=minversion1_pesos*1.33]
dataset[ foto_mes==202009,minversion1_pesos:=minversion1_pesos*1.29]
dataset[ foto_mes==202010,minversion1_pesos:=minversion1_pesos*1.26]
dataset[ foto_mes==202011,minversion1_pesos:=minversion1_pesos*1.21]
dataset[ foto_mes==202012,minversion1_pesos:=minversion1_pesos*1.17]
dataset[ foto_mes==202101,minversion1_pesos:=minversion1_pesos*1.13]
dataset[ foto_mes==201901,minversion1_dolares:=minversion1_dolares*2.36]
dataset[ foto_mes==201902,minversion1_dolares:=minversion1_dolares*2.3]
dataset[ foto_mes==201903,minversion1_dolares:=minversion1_dolares*2.21]
dataset[ foto_mes==201904,minversion1_dolares:=minversion1_dolares*2.11]
dataset[ foto_mes==201905,minversion1_dolares:=minversion1_dolares*2.05]
dataset[ foto_mes==201906,minversion1_dolares:=minversion1_dolares*1.98]
dataset[ foto_mes==201907,minversion1_dolares:=minversion1_dolares*1.93]
dataset[ foto_mes==201908,minversion1_dolares:=minversion1_dolares*1.89]
dataset[ foto_mes==201909,minversion1_dolares:=minversion1_dolares*1.82]
dataset[ foto_mes==201910,minversion1_dolares:=minversion1_dolares*1.72]
dataset[ foto_mes==201911,minversion1_dolares:=minversion1_dolares*1.66]
dataset[ foto_mes==201912,minversion1_dolares:=minversion1_dolares*1.59]
dataset[ foto_mes==202001,minversion1_dolares:=minversion1_dolares*1.54]
dataset[ foto_mes==202002,minversion1_dolares:=minversion1_dolares*1.5]
dataset[ foto_mes==202003,minversion1_dolares:=minversion1_dolares*1.47]
dataset[ foto_mes==202004,minversion1_dolares:=minversion1_dolares*1.42]
dataset[ foto_mes==202005,minversion1_dolares:=minversion1_dolares*1.4]
dataset[ foto_mes==202006,minversion1_dolares:=minversion1_dolares*1.38]
dataset[ foto_mes==202007,minversion1_dolares:=minversion1_dolares*1.35]
dataset[ foto_mes==202008,minversion1_dolares:=minversion1_dolares*1.33]
dataset[ foto_mes==202009,minversion1_dolares:=minversion1_dolares*1.29]
dataset[ foto_mes==202010,minversion1_dolares:=minversion1_dolares*1.26]
dataset[ foto_mes==202011,minversion1_dolares:=minversion1_dolares*1.21]
dataset[ foto_mes==202012,minversion1_dolares:=minversion1_dolares*1.17]
dataset[ foto_mes==202101,minversion1_dolares:=minversion1_dolares*1.13]
dataset[ foto_mes==201901,minversion2:=minversion2*2.36]
dataset[ foto_mes==201902,minversion2:=minversion2*2.3]
dataset[ foto_mes==201903,minversion2:=minversion2*2.21]
dataset[ foto_mes==201904,minversion2:=minversion2*2.11]
dataset[ foto_mes==201905,minversion2:=minversion2*2.05]
dataset[ foto_mes==201906,minversion2:=minversion2*1.98]
dataset[ foto_mes==201907,minversion2:=minversion2*1.93]
dataset[ foto_mes==201908,minversion2:=minversion2*1.89]
dataset[ foto_mes==201909,minversion2:=minversion2*1.82]
dataset[ foto_mes==201910,minversion2:=minversion2*1.72]
dataset[ foto_mes==201911,minversion2:=minversion2*1.66]
dataset[ foto_mes==201912,minversion2:=minversion2*1.59]
dataset[ foto_mes==202001,minversion2:=minversion2*1.54]
dataset[ foto_mes==202002,minversion2:=minversion2*1.5]
dataset[ foto_mes==202003,minversion2:=minversion2*1.47]
dataset[ foto_mes==202004,minversion2:=minversion2*1.42]
dataset[ foto_mes==202005,minversion2:=minversion2*1.4]
dataset[ foto_mes==202006,minversion2:=minversion2*1.38]
dataset[ foto_mes==202007,minversion2:=minversion2*1.35]
dataset[ foto_mes==202008,minversion2:=minversion2*1.33]
dataset[ foto_mes==202009,minversion2:=minversion2*1.29]
dataset[ foto_mes==202010,minversion2:=minversion2*1.26]
dataset[ foto_mes==202011,minversion2:=minversion2*1.21]
dataset[ foto_mes==202012,minversion2:=minversion2*1.17]
dataset[ foto_mes==202101,minversion2:=minversion2*1.13]
dataset[ foto_mes==201901,mpayroll:=mpayroll*2.36]
dataset[ foto_mes==201902,mpayroll:=mpayroll*2.3]
dataset[ foto_mes==201903,mpayroll:=mpayroll*2.21]
dataset[ foto_mes==201904,mpayroll:=mpayroll*2.11]
dataset[ foto_mes==201905,mpayroll:=mpayroll*2.05]
dataset[ foto_mes==201906,mpayroll:=mpayroll*1.98]
dataset[ foto_mes==201907,mpayroll:=mpayroll*1.93]
dataset[ foto_mes==201908,mpayroll:=mpayroll*1.89]
dataset[ foto_mes==201909,mpayroll:=mpayroll*1.82]
dataset[ foto_mes==201910,mpayroll:=mpayroll*1.72]
dataset[ foto_mes==201911,mpayroll:=mpayroll*1.66]
dataset[ foto_mes==201912,mpayroll:=mpayroll*1.59]
dataset[ foto_mes==202001,mpayroll:=mpayroll*1.54]
dataset[ foto_mes==202002,mpayroll:=mpayroll*1.5]
dataset[ foto_mes==202003,mpayroll:=mpayroll*1.47]
dataset[ foto_mes==202004,mpayroll:=mpayroll*1.42]
dataset[ foto_mes==202005,mpayroll:=mpayroll*1.4]
dataset[ foto_mes==202006,mpayroll:=mpayroll*1.38]
dataset[ foto_mes==202007,mpayroll:=mpayroll*1.35]
dataset[ foto_mes==202008,mpayroll:=mpayroll*1.33]
dataset[ foto_mes==202009,mpayroll:=mpayroll*1.29]
dataset[ foto_mes==202010,mpayroll:=mpayroll*1.26]
dataset[ foto_mes==202011,mpayroll:=mpayroll*1.21]
dataset[ foto_mes==202012,mpayroll:=mpayroll*1.17]
dataset[ foto_mes==202101,mpayroll:=mpayroll*1.13]
dataset[ foto_mes==201901,mpayroll2:=mpayroll2*2.36]
dataset[ foto_mes==201902,mpayroll2:=mpayroll2*2.3]
dataset[ foto_mes==201903,mpayroll2:=mpayroll2*2.21]
dataset[ foto_mes==201904,mpayroll2:=mpayroll2*2.11]
dataset[ foto_mes==201905,mpayroll2:=mpayroll2*2.05]
dataset[ foto_mes==201906,mpayroll2:=mpayroll2*1.98]
dataset[ foto_mes==201907,mpayroll2:=mpayroll2*1.93]
dataset[ foto_mes==201908,mpayroll2:=mpayroll2*1.89]
dataset[ foto_mes==201909,mpayroll2:=mpayroll2*1.82]
dataset[ foto_mes==201910,mpayroll2:=mpayroll2*1.72]
dataset[ foto_mes==201911,mpayroll2:=mpayroll2*1.66]
dataset[ foto_mes==201912,mpayroll2:=mpayroll2*1.59]
dataset[ foto_mes==202001,mpayroll2:=mpayroll2*1.54]
dataset[ foto_mes==202002,mpayroll2:=mpayroll2*1.5]
dataset[ foto_mes==202003,mpayroll2:=mpayroll2*1.47]
dataset[ foto_mes==202004,mpayroll2:=mpayroll2*1.42]
dataset[ foto_mes==202005,mpayroll2:=mpayroll2*1.4]
dataset[ foto_mes==202006,mpayroll2:=mpayroll2*1.38]
dataset[ foto_mes==202007,mpayroll2:=mpayroll2*1.35]
dataset[ foto_mes==202008,mpayroll2:=mpayroll2*1.33]
dataset[ foto_mes==202009,mpayroll2:=mpayroll2*1.29]
dataset[ foto_mes==202010,mpayroll2:=mpayroll2*1.26]
dataset[ foto_mes==202011,mpayroll2:=mpayroll2*1.21]
dataset[ foto_mes==202012,mpayroll2:=mpayroll2*1.17]
dataset[ foto_mes==202101,mpayroll2:=mpayroll2*1.13]
dataset[ foto_mes==201901,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*2.36]
dataset[ foto_mes==201902,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*2.3]
dataset[ foto_mes==201903,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*2.21]
dataset[ foto_mes==201904,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*2.11]
dataset[ foto_mes==201905,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*2.05]
dataset[ foto_mes==201906,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*1.98]
dataset[ foto_mes==201907,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*1.93]
dataset[ foto_mes==201908,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*1.89]
dataset[ foto_mes==201909,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*1.82]
dataset[ foto_mes==201910,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*1.72]
dataset[ foto_mes==201911,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*1.66]
dataset[ foto_mes==201912,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*1.59]
dataset[ foto_mes==202001,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*1.54]
dataset[ foto_mes==202002,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*1.5]
dataset[ foto_mes==202003,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*1.47]
dataset[ foto_mes==202004,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*1.42]
dataset[ foto_mes==202005,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*1.4]
dataset[ foto_mes==202006,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*1.38]
dataset[ foto_mes==202007,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*1.35]
dataset[ foto_mes==202008,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*1.33]
dataset[ foto_mes==202009,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*1.29]
dataset[ foto_mes==202010,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*1.26]
dataset[ foto_mes==202011,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*1.21]
dataset[ foto_mes==202012,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*1.17]
dataset[ foto_mes==202101,mcuenta_debitos_automaticos:=mcuenta_debitos_automaticos*1.13]
dataset[ foto_mes==201901,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*2.36]
dataset[ foto_mes==201902,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*2.3]
dataset[ foto_mes==201903,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*2.21]
dataset[ foto_mes==201904,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*2.11]
dataset[ foto_mes==201905,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*2.05]
dataset[ foto_mes==201906,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*1.98]
dataset[ foto_mes==201907,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*1.93]
dataset[ foto_mes==201908,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*1.89]
dataset[ foto_mes==201909,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*1.82]
dataset[ foto_mes==201910,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*1.72]
dataset[ foto_mes==201911,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*1.66]
dataset[ foto_mes==201912,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*1.59]
dataset[ foto_mes==202001,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*1.54]
dataset[ foto_mes==202002,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*1.5]
dataset[ foto_mes==202003,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*1.47]
dataset[ foto_mes==202004,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*1.42]
dataset[ foto_mes==202005,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*1.4]
dataset[ foto_mes==202006,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*1.38]
dataset[ foto_mes==202007,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*1.35]
dataset[ foto_mes==202008,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*1.33]
dataset[ foto_mes==202009,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*1.29]
dataset[ foto_mes==202010,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*1.26]
dataset[ foto_mes==202011,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*1.21]
dataset[ foto_mes==202012,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*1.17]
dataset[ foto_mes==202101,mttarjeta_visa_debitos_automaticos:=mttarjeta_visa_debitos_automaticos*1.13]
dataset[ foto_mes==201901,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*2.36]
dataset[ foto_mes==201902,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*2.3]
dataset[ foto_mes==201903,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*2.21]
dataset[ foto_mes==201904,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*2.11]
dataset[ foto_mes==201905,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*2.05]
dataset[ foto_mes==201906,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*1.98]
dataset[ foto_mes==201907,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*1.93]
dataset[ foto_mes==201908,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*1.89]
dataset[ foto_mes==201909,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*1.82]
dataset[ foto_mes==201910,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*1.72]
dataset[ foto_mes==201911,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*1.66]
dataset[ foto_mes==201912,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*1.59]
dataset[ foto_mes==202001,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*1.54]
dataset[ foto_mes==202002,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*1.5]
dataset[ foto_mes==202003,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*1.47]
dataset[ foto_mes==202004,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*1.42]
dataset[ foto_mes==202005,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*1.4]
dataset[ foto_mes==202006,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*1.38]
dataset[ foto_mes==202007,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*1.35]
dataset[ foto_mes==202008,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*1.33]
dataset[ foto_mes==202009,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*1.29]
dataset[ foto_mes==202010,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*1.26]
dataset[ foto_mes==202011,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*1.21]
dataset[ foto_mes==202012,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*1.17]
dataset[ foto_mes==202101,mttarjeta_master_debitos_automaticos:=mttarjeta_master_debitos_automaticos*1.13]
dataset[ foto_mes==201901,mpagodeservicios:=mpagodeservicios*2.36]
dataset[ foto_mes==201902,mpagodeservicios:=mpagodeservicios*2.3]
dataset[ foto_mes==201903,mpagodeservicios:=mpagodeservicios*2.21]
dataset[ foto_mes==201904,mpagodeservicios:=mpagodeservicios*2.11]
dataset[ foto_mes==201905,mpagodeservicios:=mpagodeservicios*2.05]
dataset[ foto_mes==201906,mpagodeservicios:=mpagodeservicios*1.98]
dataset[ foto_mes==201907,mpagodeservicios:=mpagodeservicios*1.93]
dataset[ foto_mes==201908,mpagodeservicios:=mpagodeservicios*1.89]
dataset[ foto_mes==201909,mpagodeservicios:=mpagodeservicios*1.82]
dataset[ foto_mes==201910,mpagodeservicios:=mpagodeservicios*1.72]
dataset[ foto_mes==201911,mpagodeservicios:=mpagodeservicios*1.66]
dataset[ foto_mes==201912,mpagodeservicios:=mpagodeservicios*1.59]
dataset[ foto_mes==202001,mpagodeservicios:=mpagodeservicios*1.54]
dataset[ foto_mes==202002,mpagodeservicios:=mpagodeservicios*1.5]
dataset[ foto_mes==202003,mpagodeservicios:=mpagodeservicios*1.47]
dataset[ foto_mes==202004,mpagodeservicios:=mpagodeservicios*1.42]
dataset[ foto_mes==202005,mpagodeservicios:=mpagodeservicios*1.4]
dataset[ foto_mes==202006,mpagodeservicios:=mpagodeservicios*1.38]
dataset[ foto_mes==202007,mpagodeservicios:=mpagodeservicios*1.35]
dataset[ foto_mes==202008,mpagodeservicios:=mpagodeservicios*1.33]
dataset[ foto_mes==202009,mpagodeservicios:=mpagodeservicios*1.29]
dataset[ foto_mes==202010,mpagodeservicios:=mpagodeservicios*1.26]
dataset[ foto_mes==202011,mpagodeservicios:=mpagodeservicios*1.21]
dataset[ foto_mes==202012,mpagodeservicios:=mpagodeservicios*1.17]
dataset[ foto_mes==202101,mpagodeservicios:=mpagodeservicios*1.13]
dataset[ foto_mes==201901,mpagomiscuentas:=mpagomiscuentas*2.36]
dataset[ foto_mes==201902,mpagomiscuentas:=mpagomiscuentas*2.3]
dataset[ foto_mes==201903,mpagomiscuentas:=mpagomiscuentas*2.21]
dataset[ foto_mes==201904,mpagomiscuentas:=mpagomiscuentas*2.11]
dataset[ foto_mes==201905,mpagomiscuentas:=mpagomiscuentas*2.05]
dataset[ foto_mes==201906,mpagomiscuentas:=mpagomiscuentas*1.98]
dataset[ foto_mes==201907,mpagomiscuentas:=mpagomiscuentas*1.93]
dataset[ foto_mes==201908,mpagomiscuentas:=mpagomiscuentas*1.89]
dataset[ foto_mes==201909,mpagomiscuentas:=mpagomiscuentas*1.82]
dataset[ foto_mes==201910,mpagomiscuentas:=mpagomiscuentas*1.72]
dataset[ foto_mes==201911,mpagomiscuentas:=mpagomiscuentas*1.66]
dataset[ foto_mes==201912,mpagomiscuentas:=mpagomiscuentas*1.59]
dataset[ foto_mes==202001,mpagomiscuentas:=mpagomiscuentas*1.54]
dataset[ foto_mes==202002,mpagomiscuentas:=mpagomiscuentas*1.5]
dataset[ foto_mes==202003,mpagomiscuentas:=mpagomiscuentas*1.47]
dataset[ foto_mes==202004,mpagomiscuentas:=mpagomiscuentas*1.42]
dataset[ foto_mes==202005,mpagomiscuentas:=mpagomiscuentas*1.4]
dataset[ foto_mes==202006,mpagomiscuentas:=mpagomiscuentas*1.38]
dataset[ foto_mes==202007,mpagomiscuentas:=mpagomiscuentas*1.35]
dataset[ foto_mes==202008,mpagomiscuentas:=mpagomiscuentas*1.33]
dataset[ foto_mes==202009,mpagomiscuentas:=mpagomiscuentas*1.29]
dataset[ foto_mes==202010,mpagomiscuentas:=mpagomiscuentas*1.26]
dataset[ foto_mes==202011,mpagomiscuentas:=mpagomiscuentas*1.21]
dataset[ foto_mes==202012,mpagomiscuentas:=mpagomiscuentas*1.17]
dataset[ foto_mes==202101,mpagomiscuentas:=mpagomiscuentas*1.13]
dataset[ foto_mes==201901,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*2.36]
dataset[ foto_mes==201902,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*2.3]
dataset[ foto_mes==201903,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*2.21]
dataset[ foto_mes==201904,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*2.11]
dataset[ foto_mes==201905,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*2.05]
dataset[ foto_mes==201906,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*1.98]
dataset[ foto_mes==201907,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*1.93]
dataset[ foto_mes==201908,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*1.89]
dataset[ foto_mes==201909,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*1.82]
dataset[ foto_mes==201910,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*1.72]
dataset[ foto_mes==201911,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*1.66]
dataset[ foto_mes==201912,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*1.59]
dataset[ foto_mes==202001,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*1.54]
dataset[ foto_mes==202002,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*1.5]
dataset[ foto_mes==202003,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*1.47]
dataset[ foto_mes==202004,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*1.42]
dataset[ foto_mes==202005,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*1.4]
dataset[ foto_mes==202006,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*1.38]
dataset[ foto_mes==202007,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*1.35]
dataset[ foto_mes==202008,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*1.33]
dataset[ foto_mes==202009,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*1.29]
dataset[ foto_mes==202010,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*1.26]
dataset[ foto_mes==202011,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*1.21]
dataset[ foto_mes==202012,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*1.17]
dataset[ foto_mes==202101,mcajeros_propios_descuentos:=mcajeros_propios_descuentos*1.13]
dataset[ foto_mes==201901,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*2.36]
dataset[ foto_mes==201902,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*2.3]
dataset[ foto_mes==201903,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*2.21]
dataset[ foto_mes==201904,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*2.11]
dataset[ foto_mes==201905,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*2.05]
dataset[ foto_mes==201906,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*1.98]
dataset[ foto_mes==201907,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*1.93]
dataset[ foto_mes==201908,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*1.89]
dataset[ foto_mes==201909,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*1.82]
dataset[ foto_mes==201910,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*1.72]
dataset[ foto_mes==201911,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*1.66]
dataset[ foto_mes==201912,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*1.59]
dataset[ foto_mes==202001,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*1.54]
dataset[ foto_mes==202002,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*1.5]
dataset[ foto_mes==202003,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*1.47]
dataset[ foto_mes==202004,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*1.42]
dataset[ foto_mes==202005,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*1.4]
dataset[ foto_mes==202006,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*1.38]
dataset[ foto_mes==202007,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*1.35]
dataset[ foto_mes==202008,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*1.33]
dataset[ foto_mes==202009,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*1.29]
dataset[ foto_mes==202010,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*1.26]
dataset[ foto_mes==202011,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*1.21]
dataset[ foto_mes==202012,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*1.17]
dataset[ foto_mes==202101,mtarjeta_visa_descuentos:=mtarjeta_visa_descuentos*1.13]
dataset[ foto_mes==201901,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*2.36]
dataset[ foto_mes==201902,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*2.3]
dataset[ foto_mes==201903,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*2.21]
dataset[ foto_mes==201904,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*2.11]
dataset[ foto_mes==201905,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*2.05]
dataset[ foto_mes==201906,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*1.98]
dataset[ foto_mes==201907,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*1.93]
dataset[ foto_mes==201908,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*1.89]
dataset[ foto_mes==201909,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*1.82]
dataset[ foto_mes==201910,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*1.72]
dataset[ foto_mes==201911,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*1.66]
dataset[ foto_mes==201912,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*1.59]
dataset[ foto_mes==202001,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*1.54]
dataset[ foto_mes==202002,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*1.5]
dataset[ foto_mes==202003,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*1.47]
dataset[ foto_mes==202004,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*1.42]
dataset[ foto_mes==202005,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*1.4]
dataset[ foto_mes==202006,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*1.38]
dataset[ foto_mes==202007,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*1.35]
dataset[ foto_mes==202008,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*1.33]
dataset[ foto_mes==202009,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*1.29]
dataset[ foto_mes==202010,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*1.26]
dataset[ foto_mes==202011,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*1.21]
dataset[ foto_mes==202012,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*1.17]
dataset[ foto_mes==202101,mtarjeta_master_descuentos:=mtarjeta_master_descuentos*1.13]
dataset[ foto_mes==201901,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*2.36]
dataset[ foto_mes==201902,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*2.3]
dataset[ foto_mes==201903,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*2.21]
dataset[ foto_mes==201904,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*2.11]
dataset[ foto_mes==201905,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*2.05]
dataset[ foto_mes==201906,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*1.98]
dataset[ foto_mes==201907,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*1.93]
dataset[ foto_mes==201908,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*1.89]
dataset[ foto_mes==201909,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*1.82]
dataset[ foto_mes==201910,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*1.72]
dataset[ foto_mes==201911,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*1.66]
dataset[ foto_mes==201912,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*1.59]
dataset[ foto_mes==202001,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*1.54]
dataset[ foto_mes==202002,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*1.5]
dataset[ foto_mes==202003,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*1.47]
dataset[ foto_mes==202004,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*1.42]
dataset[ foto_mes==202005,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*1.4]
dataset[ foto_mes==202006,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*1.38]
dataset[ foto_mes==202007,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*1.35]
dataset[ foto_mes==202008,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*1.33]
dataset[ foto_mes==202009,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*1.29]
dataset[ foto_mes==202010,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*1.26]
dataset[ foto_mes==202011,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*1.21]
dataset[ foto_mes==202012,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*1.17]
dataset[ foto_mes==202101,mcomisiones_mantenimiento:=mcomisiones_mantenimiento*1.13]
dataset[ foto_mes==201901,mforex_buy:=mforex_buy*2.36]
dataset[ foto_mes==201902,mforex_buy:=mforex_buy*2.3]
dataset[ foto_mes==201903,mforex_buy:=mforex_buy*2.21]
dataset[ foto_mes==201904,mforex_buy:=mforex_buy*2.11]
dataset[ foto_mes==201905,mforex_buy:=mforex_buy*2.05]
dataset[ foto_mes==201906,mforex_buy:=mforex_buy*1.98]
dataset[ foto_mes==201907,mforex_buy:=mforex_buy*1.93]
dataset[ foto_mes==201908,mforex_buy:=mforex_buy*1.89]
dataset[ foto_mes==201909,mforex_buy:=mforex_buy*1.82]
dataset[ foto_mes==201910,mforex_buy:=mforex_buy*1.72]
dataset[ foto_mes==201911,mforex_buy:=mforex_buy*1.66]
dataset[ foto_mes==201912,mforex_buy:=mforex_buy*1.59]
dataset[ foto_mes==202001,mforex_buy:=mforex_buy*1.54]
dataset[ foto_mes==202002,mforex_buy:=mforex_buy*1.5]
dataset[ foto_mes==202003,mforex_buy:=mforex_buy*1.47]
dataset[ foto_mes==202004,mforex_buy:=mforex_buy*1.42]
dataset[ foto_mes==202005,mforex_buy:=mforex_buy*1.4]
dataset[ foto_mes==202006,mforex_buy:=mforex_buy*1.38]
dataset[ foto_mes==202007,mforex_buy:=mforex_buy*1.35]
dataset[ foto_mes==202008,mforex_buy:=mforex_buy*1.33]
dataset[ foto_mes==202009,mforex_buy:=mforex_buy*1.29]
dataset[ foto_mes==202010,mforex_buy:=mforex_buy*1.26]
dataset[ foto_mes==202011,mforex_buy:=mforex_buy*1.21]
dataset[ foto_mes==202012,mforex_buy:=mforex_buy*1.17]
dataset[ foto_mes==202101,mforex_buy:=mforex_buy*1.13]
dataset[ foto_mes==201901,mforex_sell:=mforex_sell*2.36]
dataset[ foto_mes==201902,mforex_sell:=mforex_sell*2.3]
dataset[ foto_mes==201903,mforex_sell:=mforex_sell*2.21]
dataset[ foto_mes==201904,mforex_sell:=mforex_sell*2.11]
dataset[ foto_mes==201905,mforex_sell:=mforex_sell*2.05]
dataset[ foto_mes==201906,mforex_sell:=mforex_sell*1.98]
dataset[ foto_mes==201907,mforex_sell:=mforex_sell*1.93]
dataset[ foto_mes==201908,mforex_sell:=mforex_sell*1.89]
dataset[ foto_mes==201909,mforex_sell:=mforex_sell*1.82]
dataset[ foto_mes==201910,mforex_sell:=mforex_sell*1.72]
dataset[ foto_mes==201911,mforex_sell:=mforex_sell*1.66]
dataset[ foto_mes==201912,mforex_sell:=mforex_sell*1.59]
dataset[ foto_mes==202001,mforex_sell:=mforex_sell*1.54]
dataset[ foto_mes==202002,mforex_sell:=mforex_sell*1.5]
dataset[ foto_mes==202003,mforex_sell:=mforex_sell*1.47]
dataset[ foto_mes==202004,mforex_sell:=mforex_sell*1.42]
dataset[ foto_mes==202005,mforex_sell:=mforex_sell*1.4]
dataset[ foto_mes==202006,mforex_sell:=mforex_sell*1.38]
dataset[ foto_mes==202007,mforex_sell:=mforex_sell*1.35]
dataset[ foto_mes==202008,mforex_sell:=mforex_sell*1.33]
dataset[ foto_mes==202009,mforex_sell:=mforex_sell*1.29]
dataset[ foto_mes==202010,mforex_sell:=mforex_sell*1.26]
dataset[ foto_mes==202011,mforex_sell:=mforex_sell*1.21]
dataset[ foto_mes==202012,mforex_sell:=mforex_sell*1.17]
dataset[ foto_mes==202101,mforex_sell:=mforex_sell*1.13]
dataset[ foto_mes==201901,mtransferencias_recibidas:=mtransferencias_recibidas*2.36]
dataset[ foto_mes==201902,mtransferencias_recibidas:=mtransferencias_recibidas*2.3]
dataset[ foto_mes==201903,mtransferencias_recibidas:=mtransferencias_recibidas*2.21]
dataset[ foto_mes==201904,mtransferencias_recibidas:=mtransferencias_recibidas*2.11]
dataset[ foto_mes==201905,mtransferencias_recibidas:=mtransferencias_recibidas*2.05]
dataset[ foto_mes==201906,mtransferencias_recibidas:=mtransferencias_recibidas*1.98]
dataset[ foto_mes==201907,mtransferencias_recibidas:=mtransferencias_recibidas*1.93]
dataset[ foto_mes==201908,mtransferencias_recibidas:=mtransferencias_recibidas*1.89]
dataset[ foto_mes==201909,mtransferencias_recibidas:=mtransferencias_recibidas*1.82]
dataset[ foto_mes==201910,mtransferencias_recibidas:=mtransferencias_recibidas*1.72]
dataset[ foto_mes==201911,mtransferencias_recibidas:=mtransferencias_recibidas*1.66]
dataset[ foto_mes==201912,mtransferencias_recibidas:=mtransferencias_recibidas*1.59]
dataset[ foto_mes==202001,mtransferencias_recibidas:=mtransferencias_recibidas*1.54]
dataset[ foto_mes==202002,mtransferencias_recibidas:=mtransferencias_recibidas*1.5]
dataset[ foto_mes==202003,mtransferencias_recibidas:=mtransferencias_recibidas*1.47]
dataset[ foto_mes==202004,mtransferencias_recibidas:=mtransferencias_recibidas*1.42]
dataset[ foto_mes==202005,mtransferencias_recibidas:=mtransferencias_recibidas*1.4]
dataset[ foto_mes==202006,mtransferencias_recibidas:=mtransferencias_recibidas*1.38]
dataset[ foto_mes==202007,mtransferencias_recibidas:=mtransferencias_recibidas*1.35]
dataset[ foto_mes==202008,mtransferencias_recibidas:=mtransferencias_recibidas*1.33]
dataset[ foto_mes==202009,mtransferencias_recibidas:=mtransferencias_recibidas*1.29]
dataset[ foto_mes==202010,mtransferencias_recibidas:=mtransferencias_recibidas*1.26]
dataset[ foto_mes==202011,mtransferencias_recibidas:=mtransferencias_recibidas*1.21]
dataset[ foto_mes==202012,mtransferencias_recibidas:=mtransferencias_recibidas*1.17]
dataset[ foto_mes==202101,mtransferencias_recibidas:=mtransferencias_recibidas*1.13]
dataset[ foto_mes==201901,mtransferencias_emitidas:=mtransferencias_emitidas*2.36]
dataset[ foto_mes==201902,mtransferencias_emitidas:=mtransferencias_emitidas*2.3]
dataset[ foto_mes==201903,mtransferencias_emitidas:=mtransferencias_emitidas*2.21]
dataset[ foto_mes==201904,mtransferencias_emitidas:=mtransferencias_emitidas*2.11]
dataset[ foto_mes==201905,mtransferencias_emitidas:=mtransferencias_emitidas*2.05]
dataset[ foto_mes==201906,mtransferencias_emitidas:=mtransferencias_emitidas*1.98]
dataset[ foto_mes==201907,mtransferencias_emitidas:=mtransferencias_emitidas*1.93]
dataset[ foto_mes==201908,mtransferencias_emitidas:=mtransferencias_emitidas*1.89]
dataset[ foto_mes==201909,mtransferencias_emitidas:=mtransferencias_emitidas*1.82]
dataset[ foto_mes==201910,mtransferencias_emitidas:=mtransferencias_emitidas*1.72]
dataset[ foto_mes==201911,mtransferencias_emitidas:=mtransferencias_emitidas*1.66]
dataset[ foto_mes==201912,mtransferencias_emitidas:=mtransferencias_emitidas*1.59]
dataset[ foto_mes==202001,mtransferencias_emitidas:=mtransferencias_emitidas*1.54]
dataset[ foto_mes==202002,mtransferencias_emitidas:=mtransferencias_emitidas*1.5]
dataset[ foto_mes==202003,mtransferencias_emitidas:=mtransferencias_emitidas*1.47]
dataset[ foto_mes==202004,mtransferencias_emitidas:=mtransferencias_emitidas*1.42]
dataset[ foto_mes==202005,mtransferencias_emitidas:=mtransferencias_emitidas*1.4]
dataset[ foto_mes==202006,mtransferencias_emitidas:=mtransferencias_emitidas*1.38]
dataset[ foto_mes==202007,mtransferencias_emitidas:=mtransferencias_emitidas*1.35]
dataset[ foto_mes==202008,mtransferencias_emitidas:=mtransferencias_emitidas*1.33]
dataset[ foto_mes==202009,mtransferencias_emitidas:=mtransferencias_emitidas*1.29]
dataset[ foto_mes==202010,mtransferencias_emitidas:=mtransferencias_emitidas*1.26]
dataset[ foto_mes==202011,mtransferencias_emitidas:=mtransferencias_emitidas*1.21]
dataset[ foto_mes==202012,mtransferencias_emitidas:=mtransferencias_emitidas*1.17]
dataset[ foto_mes==202101,mtransferencias_emitidas:=mtransferencias_emitidas*1.13]
dataset[ foto_mes==201901,mextraccion_autoservicio:=mextraccion_autoservicio*2.36]
dataset[ foto_mes==201902,mextraccion_autoservicio:=mextraccion_autoservicio*2.3]
dataset[ foto_mes==201903,mextraccion_autoservicio:=mextraccion_autoservicio*2.21]
dataset[ foto_mes==201904,mextraccion_autoservicio:=mextraccion_autoservicio*2.11]
dataset[ foto_mes==201905,mextraccion_autoservicio:=mextraccion_autoservicio*2.05]
dataset[ foto_mes==201906,mextraccion_autoservicio:=mextraccion_autoservicio*1.98]
dataset[ foto_mes==201907,mextraccion_autoservicio:=mextraccion_autoservicio*1.93]
dataset[ foto_mes==201908,mextraccion_autoservicio:=mextraccion_autoservicio*1.89]
dataset[ foto_mes==201909,mextraccion_autoservicio:=mextraccion_autoservicio*1.82]
dataset[ foto_mes==201910,mextraccion_autoservicio:=mextraccion_autoservicio*1.72]
dataset[ foto_mes==201911,mextraccion_autoservicio:=mextraccion_autoservicio*1.66]
dataset[ foto_mes==201912,mextraccion_autoservicio:=mextraccion_autoservicio*1.59]
dataset[ foto_mes==202001,mextraccion_autoservicio:=mextraccion_autoservicio*1.54]
dataset[ foto_mes==202002,mextraccion_autoservicio:=mextraccion_autoservicio*1.5]
dataset[ foto_mes==202003,mextraccion_autoservicio:=mextraccion_autoservicio*1.47]
dataset[ foto_mes==202004,mextraccion_autoservicio:=mextraccion_autoservicio*1.42]
dataset[ foto_mes==202005,mextraccion_autoservicio:=mextraccion_autoservicio*1.4]
dataset[ foto_mes==202006,mextraccion_autoservicio:=mextraccion_autoservicio*1.38]
dataset[ foto_mes==202007,mextraccion_autoservicio:=mextraccion_autoservicio*1.35]
dataset[ foto_mes==202008,mextraccion_autoservicio:=mextraccion_autoservicio*1.33]
dataset[ foto_mes==202009,mextraccion_autoservicio:=mextraccion_autoservicio*1.29]
dataset[ foto_mes==202010,mextraccion_autoservicio:=mextraccion_autoservicio*1.26]
dataset[ foto_mes==202011,mextraccion_autoservicio:=mextraccion_autoservicio*1.21]
dataset[ foto_mes==202012,mextraccion_autoservicio:=mextraccion_autoservicio*1.17]
dataset[ foto_mes==202101,mextraccion_autoservicio:=mextraccion_autoservicio*1.13]
dataset[ foto_mes==201901,mcheques_depositados:=mcheques_depositados*2.36]
dataset[ foto_mes==201902,mcheques_depositados:=mcheques_depositados*2.3]
dataset[ foto_mes==201903,mcheques_depositados:=mcheques_depositados*2.21]
dataset[ foto_mes==201904,mcheques_depositados:=mcheques_depositados*2.11]
dataset[ foto_mes==201905,mcheques_depositados:=mcheques_depositados*2.05]
dataset[ foto_mes==201906,mcheques_depositados:=mcheques_depositados*1.98]
dataset[ foto_mes==201907,mcheques_depositados:=mcheques_depositados*1.93]
dataset[ foto_mes==201908,mcheques_depositados:=mcheques_depositados*1.89]
dataset[ foto_mes==201909,mcheques_depositados:=mcheques_depositados*1.82]
dataset[ foto_mes==201910,mcheques_depositados:=mcheques_depositados*1.72]
dataset[ foto_mes==201911,mcheques_depositados:=mcheques_depositados*1.66]
dataset[ foto_mes==201912,mcheques_depositados:=mcheques_depositados*1.59]
dataset[ foto_mes==202001,mcheques_depositados:=mcheques_depositados*1.54]
dataset[ foto_mes==202002,mcheques_depositados:=mcheques_depositados*1.5]
dataset[ foto_mes==202003,mcheques_depositados:=mcheques_depositados*1.47]
dataset[ foto_mes==202004,mcheques_depositados:=mcheques_depositados*1.42]
dataset[ foto_mes==202005,mcheques_depositados:=mcheques_depositados*1.4]
dataset[ foto_mes==202006,mcheques_depositados:=mcheques_depositados*1.38]
dataset[ foto_mes==202007,mcheques_depositados:=mcheques_depositados*1.35]
dataset[ foto_mes==202008,mcheques_depositados:=mcheques_depositados*1.33]
dataset[ foto_mes==202009,mcheques_depositados:=mcheques_depositados*1.29]
dataset[ foto_mes==202010,mcheques_depositados:=mcheques_depositados*1.26]
dataset[ foto_mes==202011,mcheques_depositados:=mcheques_depositados*1.21]
dataset[ foto_mes==202012,mcheques_depositados:=mcheques_depositados*1.17]
dataset[ foto_mes==202101,mcheques_depositados:=mcheques_depositados*1.13]
dataset[ foto_mes==201901,mcheques_emitidos:=mcheques_emitidos*2.36]
dataset[ foto_mes==201902,mcheques_emitidos:=mcheques_emitidos*2.3]
dataset[ foto_mes==201903,mcheques_emitidos:=mcheques_emitidos*2.21]
dataset[ foto_mes==201904,mcheques_emitidos:=mcheques_emitidos*2.11]
dataset[ foto_mes==201905,mcheques_emitidos:=mcheques_emitidos*2.05]
dataset[ foto_mes==201906,mcheques_emitidos:=mcheques_emitidos*1.98]
dataset[ foto_mes==201907,mcheques_emitidos:=mcheques_emitidos*1.93]
dataset[ foto_mes==201908,mcheques_emitidos:=mcheques_emitidos*1.89]
dataset[ foto_mes==201909,mcheques_emitidos:=mcheques_emitidos*1.82]
dataset[ foto_mes==201910,mcheques_emitidos:=mcheques_emitidos*1.72]
dataset[ foto_mes==201911,mcheques_emitidos:=mcheques_emitidos*1.66]
dataset[ foto_mes==201912,mcheques_emitidos:=mcheques_emitidos*1.59]
dataset[ foto_mes==202001,mcheques_emitidos:=mcheques_emitidos*1.54]
dataset[ foto_mes==202002,mcheques_emitidos:=mcheques_emitidos*1.5]
dataset[ foto_mes==202003,mcheques_emitidos:=mcheques_emitidos*1.47]
dataset[ foto_mes==202004,mcheques_emitidos:=mcheques_emitidos*1.42]
dataset[ foto_mes==202005,mcheques_emitidos:=mcheques_emitidos*1.4]
dataset[ foto_mes==202006,mcheques_emitidos:=mcheques_emitidos*1.38]
dataset[ foto_mes==202007,mcheques_emitidos:=mcheques_emitidos*1.35]
dataset[ foto_mes==202008,mcheques_emitidos:=mcheques_emitidos*1.33]
dataset[ foto_mes==202009,mcheques_emitidos:=mcheques_emitidos*1.29]
dataset[ foto_mes==202010,mcheques_emitidos:=mcheques_emitidos*1.26]
dataset[ foto_mes==202011,mcheques_emitidos:=mcheques_emitidos*1.21]
dataset[ foto_mes==202012,mcheques_emitidos:=mcheques_emitidos*1.17]
dataset[ foto_mes==202101,mcheques_emitidos:=mcheques_emitidos*1.13]
dataset[ foto_mes==201901,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*2.36]
dataset[ foto_mes==201902,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*2.3]
dataset[ foto_mes==201903,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*2.21]
dataset[ foto_mes==201904,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*2.11]
dataset[ foto_mes==201905,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*2.05]
dataset[ foto_mes==201906,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*1.98]
dataset[ foto_mes==201907,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*1.93]
dataset[ foto_mes==201908,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*1.89]
dataset[ foto_mes==201909,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*1.82]
dataset[ foto_mes==201910,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*1.72]
dataset[ foto_mes==201911,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*1.66]
dataset[ foto_mes==201912,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*1.59]
dataset[ foto_mes==202001,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*1.54]
dataset[ foto_mes==202002,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*1.5]
dataset[ foto_mes==202003,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*1.47]
dataset[ foto_mes==202004,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*1.42]
dataset[ foto_mes==202005,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*1.4]
dataset[ foto_mes==202006,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*1.38]
dataset[ foto_mes==202007,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*1.35]
dataset[ foto_mes==202008,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*1.33]
dataset[ foto_mes==202009,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*1.29]
dataset[ foto_mes==202010,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*1.26]
dataset[ foto_mes==202011,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*1.21]
dataset[ foto_mes==202012,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*1.17]
dataset[ foto_mes==202101,mcheques_depositados_rechazados:=mcheques_depositados_rechazados*1.13]
dataset[ foto_mes==201901,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*2.36]
dataset[ foto_mes==201902,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*2.3]
dataset[ foto_mes==201903,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*2.21]
dataset[ foto_mes==201904,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*2.11]
dataset[ foto_mes==201905,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*2.05]
dataset[ foto_mes==201906,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*1.98]
dataset[ foto_mes==201907,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*1.93]
dataset[ foto_mes==201908,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*1.89]
dataset[ foto_mes==201909,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*1.82]
dataset[ foto_mes==201910,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*1.72]
dataset[ foto_mes==201911,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*1.66]
dataset[ foto_mes==201912,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*1.59]
dataset[ foto_mes==202001,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*1.54]
dataset[ foto_mes==202002,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*1.5]
dataset[ foto_mes==202003,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*1.47]
dataset[ foto_mes==202004,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*1.42]
dataset[ foto_mes==202005,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*1.4]
dataset[ foto_mes==202006,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*1.38]
dataset[ foto_mes==202007,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*1.35]
dataset[ foto_mes==202008,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*1.33]
dataset[ foto_mes==202009,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*1.29]
dataset[ foto_mes==202010,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*1.26]
dataset[ foto_mes==202011,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*1.21]
dataset[ foto_mes==202012,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*1.17]
dataset[ foto_mes==202101,mcheques_emitidos_rechazados:=mcheques_emitidos_rechazados*1.13]
dataset[ foto_mes==201901,matm:=matm*2.36]
dataset[ foto_mes==201902,matm:=matm*2.3]
dataset[ foto_mes==201903,matm:=matm*2.21]
dataset[ foto_mes==201904,matm:=matm*2.11]
dataset[ foto_mes==201905,matm:=matm*2.05]
dataset[ foto_mes==201906,matm:=matm*1.98]
dataset[ foto_mes==201907,matm:=matm*1.93]
dataset[ foto_mes==201908,matm:=matm*1.89]
dataset[ foto_mes==201909,matm:=matm*1.82]
dataset[ foto_mes==201910,matm:=matm*1.72]
dataset[ foto_mes==201911,matm:=matm*1.66]
dataset[ foto_mes==201912,matm:=matm*1.59]
dataset[ foto_mes==202001,matm:=matm*1.54]
dataset[ foto_mes==202002,matm:=matm*1.5]
dataset[ foto_mes==202003,matm:=matm*1.47]
dataset[ foto_mes==202004,matm:=matm*1.42]
dataset[ foto_mes==202005,matm:=matm*1.4]
dataset[ foto_mes==202006,matm:=matm*1.38]
dataset[ foto_mes==202007,matm:=matm*1.35]
dataset[ foto_mes==202008,matm:=matm*1.33]
dataset[ foto_mes==202009,matm:=matm*1.29]
dataset[ foto_mes==202010,matm:=matm*1.26]
dataset[ foto_mes==202011,matm:=matm*1.21]
dataset[ foto_mes==202012,matm:=matm*1.17]
dataset[ foto_mes==202101,matm:=matm*1.13]
dataset[ foto_mes==201901,matm_other:=matm_other*2.36]
dataset[ foto_mes==201902,matm_other:=matm_other*2.3]
dataset[ foto_mes==201903,matm_other:=matm_other*2.21]
dataset[ foto_mes==201904,matm_other:=matm_other*2.11]
dataset[ foto_mes==201905,matm_other:=matm_other*2.05]
dataset[ foto_mes==201906,matm_other:=matm_other*1.98]
dataset[ foto_mes==201907,matm_other:=matm_other*1.93]
dataset[ foto_mes==201908,matm_other:=matm_other*1.89]
dataset[ foto_mes==201909,matm_other:=matm_other*1.82]
dataset[ foto_mes==201910,matm_other:=matm_other*1.72]
dataset[ foto_mes==201911,matm_other:=matm_other*1.66]
dataset[ foto_mes==201912,matm_other:=matm_other*1.59]
dataset[ foto_mes==202001,matm_other:=matm_other*1.54]
dataset[ foto_mes==202002,matm_other:=matm_other*1.5]
dataset[ foto_mes==202003,matm_other:=matm_other*1.47]
dataset[ foto_mes==202004,matm_other:=matm_other*1.42]
dataset[ foto_mes==202005,matm_other:=matm_other*1.4]
dataset[ foto_mes==202006,matm_other:=matm_other*1.38]
dataset[ foto_mes==202007,matm_other:=matm_other*1.35]
dataset[ foto_mes==202008,matm_other:=matm_other*1.33]
dataset[ foto_mes==202009,matm_other:=matm_other*1.29]
dataset[ foto_mes==202010,matm_other:=matm_other*1.26]
dataset[ foto_mes==202011,matm_other:=matm_other*1.21]
dataset[ foto_mes==202012,matm_other:=matm_other*1.17]
dataset[ foto_mes==202101,matm_other:=matm_other*1.13]
dataset[ foto_mes==201901,Master_msaldototal:=Master_msaldototal*2.36]
dataset[ foto_mes==201902,Master_msaldototal:=Master_msaldototal*2.3]
dataset[ foto_mes==201903,Master_msaldototal:=Master_msaldototal*2.21]
dataset[ foto_mes==201904,Master_msaldototal:=Master_msaldototal*2.11]
dataset[ foto_mes==201905,Master_msaldototal:=Master_msaldototal*2.05]
dataset[ foto_mes==201906,Master_msaldototal:=Master_msaldototal*1.98]
dataset[ foto_mes==201907,Master_msaldototal:=Master_msaldototal*1.93]
dataset[ foto_mes==201908,Master_msaldototal:=Master_msaldototal*1.89]
dataset[ foto_mes==201909,Master_msaldototal:=Master_msaldototal*1.82]
dataset[ foto_mes==201910,Master_msaldototal:=Master_msaldototal*1.72]
dataset[ foto_mes==201911,Master_msaldototal:=Master_msaldototal*1.66]
dataset[ foto_mes==201912,Master_msaldototal:=Master_msaldototal*1.59]
dataset[ foto_mes==202001,Master_msaldototal:=Master_msaldototal*1.54]
dataset[ foto_mes==202002,Master_msaldototal:=Master_msaldototal*1.5]
dataset[ foto_mes==202003,Master_msaldototal:=Master_msaldototal*1.47]
dataset[ foto_mes==202004,Master_msaldototal:=Master_msaldototal*1.42]
dataset[ foto_mes==202005,Master_msaldototal:=Master_msaldototal*1.4]
dataset[ foto_mes==202006,Master_msaldototal:=Master_msaldototal*1.38]
dataset[ foto_mes==202007,Master_msaldototal:=Master_msaldototal*1.35]
dataset[ foto_mes==202008,Master_msaldototal:=Master_msaldototal*1.33]
dataset[ foto_mes==202009,Master_msaldototal:=Master_msaldototal*1.29]
dataset[ foto_mes==202010,Master_msaldototal:=Master_msaldototal*1.26]
dataset[ foto_mes==202011,Master_msaldototal:=Master_msaldototal*1.21]
dataset[ foto_mes==202012,Master_msaldototal:=Master_msaldototal*1.17]
dataset[ foto_mes==202101,Master_msaldototal:=Master_msaldototal*1.13]
dataset[ foto_mes==201901,Master_msaldopesos:=Master_msaldopesos*2.36]
dataset[ foto_mes==201902,Master_msaldopesos:=Master_msaldopesos*2.3]
dataset[ foto_mes==201903,Master_msaldopesos:=Master_msaldopesos*2.21]
dataset[ foto_mes==201904,Master_msaldopesos:=Master_msaldopesos*2.11]
dataset[ foto_mes==201905,Master_msaldopesos:=Master_msaldopesos*2.05]
dataset[ foto_mes==201906,Master_msaldopesos:=Master_msaldopesos*1.98]
dataset[ foto_mes==201907,Master_msaldopesos:=Master_msaldopesos*1.93]
dataset[ foto_mes==201908,Master_msaldopesos:=Master_msaldopesos*1.89]
dataset[ foto_mes==201909,Master_msaldopesos:=Master_msaldopesos*1.82]
dataset[ foto_mes==201910,Master_msaldopesos:=Master_msaldopesos*1.72]
dataset[ foto_mes==201911,Master_msaldopesos:=Master_msaldopesos*1.66]
dataset[ foto_mes==201912,Master_msaldopesos:=Master_msaldopesos*1.59]
dataset[ foto_mes==202001,Master_msaldopesos:=Master_msaldopesos*1.54]
dataset[ foto_mes==202002,Master_msaldopesos:=Master_msaldopesos*1.5]
dataset[ foto_mes==202003,Master_msaldopesos:=Master_msaldopesos*1.47]
dataset[ foto_mes==202004,Master_msaldopesos:=Master_msaldopesos*1.42]
dataset[ foto_mes==202005,Master_msaldopesos:=Master_msaldopesos*1.4]
dataset[ foto_mes==202006,Master_msaldopesos:=Master_msaldopesos*1.38]
dataset[ foto_mes==202007,Master_msaldopesos:=Master_msaldopesos*1.35]
dataset[ foto_mes==202008,Master_msaldopesos:=Master_msaldopesos*1.33]
dataset[ foto_mes==202009,Master_msaldopesos:=Master_msaldopesos*1.29]
dataset[ foto_mes==202010,Master_msaldopesos:=Master_msaldopesos*1.26]
dataset[ foto_mes==202011,Master_msaldopesos:=Master_msaldopesos*1.21]
dataset[ foto_mes==202012,Master_msaldopesos:=Master_msaldopesos*1.17]
dataset[ foto_mes==202101,Master_msaldopesos:=Master_msaldopesos*1.13]
dataset[ foto_mes==201901,Master_msaldodolares:=Master_msaldodolares*2.36]
dataset[ foto_mes==201902,Master_msaldodolares:=Master_msaldodolares*2.3]
dataset[ foto_mes==201903,Master_msaldodolares:=Master_msaldodolares*2.21]
dataset[ foto_mes==201904,Master_msaldodolares:=Master_msaldodolares*2.11]
dataset[ foto_mes==201905,Master_msaldodolares:=Master_msaldodolares*2.05]
dataset[ foto_mes==201906,Master_msaldodolares:=Master_msaldodolares*1.98]
dataset[ foto_mes==201907,Master_msaldodolares:=Master_msaldodolares*1.93]
dataset[ foto_mes==201908,Master_msaldodolares:=Master_msaldodolares*1.89]
dataset[ foto_mes==201909,Master_msaldodolares:=Master_msaldodolares*1.82]
dataset[ foto_mes==201910,Master_msaldodolares:=Master_msaldodolares*1.72]
dataset[ foto_mes==201911,Master_msaldodolares:=Master_msaldodolares*1.66]
dataset[ foto_mes==201912,Master_msaldodolares:=Master_msaldodolares*1.59]
dataset[ foto_mes==202001,Master_msaldodolares:=Master_msaldodolares*1.54]
dataset[ foto_mes==202002,Master_msaldodolares:=Master_msaldodolares*1.5]
dataset[ foto_mes==202003,Master_msaldodolares:=Master_msaldodolares*1.47]
dataset[ foto_mes==202004,Master_msaldodolares:=Master_msaldodolares*1.42]
dataset[ foto_mes==202005,Master_msaldodolares:=Master_msaldodolares*1.4]
dataset[ foto_mes==202006,Master_msaldodolares:=Master_msaldodolares*1.38]
dataset[ foto_mes==202007,Master_msaldodolares:=Master_msaldodolares*1.35]
dataset[ foto_mes==202008,Master_msaldodolares:=Master_msaldodolares*1.33]
dataset[ foto_mes==202009,Master_msaldodolares:=Master_msaldodolares*1.29]
dataset[ foto_mes==202010,Master_msaldodolares:=Master_msaldodolares*1.26]
dataset[ foto_mes==202011,Master_msaldodolares:=Master_msaldodolares*1.21]
dataset[ foto_mes==202012,Master_msaldodolares:=Master_msaldodolares*1.17]
dataset[ foto_mes==202101,Master_msaldodolares:=Master_msaldodolares*1.13]
dataset[ foto_mes==201901,Master_mconsumospesos:=Master_mconsumospesos*2.36]
dataset[ foto_mes==201902,Master_mconsumospesos:=Master_mconsumospesos*2.3]
dataset[ foto_mes==201903,Master_mconsumospesos:=Master_mconsumospesos*2.21]
dataset[ foto_mes==201904,Master_mconsumospesos:=Master_mconsumospesos*2.11]
dataset[ foto_mes==201905,Master_mconsumospesos:=Master_mconsumospesos*2.05]
dataset[ foto_mes==201906,Master_mconsumospesos:=Master_mconsumospesos*1.98]
dataset[ foto_mes==201907,Master_mconsumospesos:=Master_mconsumospesos*1.93]
dataset[ foto_mes==201908,Master_mconsumospesos:=Master_mconsumospesos*1.89]
dataset[ foto_mes==201909,Master_mconsumospesos:=Master_mconsumospesos*1.82]
dataset[ foto_mes==201910,Master_mconsumospesos:=Master_mconsumospesos*1.72]
dataset[ foto_mes==201911,Master_mconsumospesos:=Master_mconsumospesos*1.66]
dataset[ foto_mes==201912,Master_mconsumospesos:=Master_mconsumospesos*1.59]
dataset[ foto_mes==202001,Master_mconsumospesos:=Master_mconsumospesos*1.54]
dataset[ foto_mes==202002,Master_mconsumospesos:=Master_mconsumospesos*1.5]
dataset[ foto_mes==202003,Master_mconsumospesos:=Master_mconsumospesos*1.47]
dataset[ foto_mes==202004,Master_mconsumospesos:=Master_mconsumospesos*1.42]
dataset[ foto_mes==202005,Master_mconsumospesos:=Master_mconsumospesos*1.4]
dataset[ foto_mes==202006,Master_mconsumospesos:=Master_mconsumospesos*1.38]
dataset[ foto_mes==202007,Master_mconsumospesos:=Master_mconsumospesos*1.35]
dataset[ foto_mes==202008,Master_mconsumospesos:=Master_mconsumospesos*1.33]
dataset[ foto_mes==202009,Master_mconsumospesos:=Master_mconsumospesos*1.29]
dataset[ foto_mes==202010,Master_mconsumospesos:=Master_mconsumospesos*1.26]
dataset[ foto_mes==202011,Master_mconsumospesos:=Master_mconsumospesos*1.21]
dataset[ foto_mes==202012,Master_mconsumospesos:=Master_mconsumospesos*1.17]
dataset[ foto_mes==202101,Master_mconsumospesos:=Master_mconsumospesos*1.13]
dataset[ foto_mes==201901,Master_mconsumosdolares:=Master_mconsumosdolares*2.36]
dataset[ foto_mes==201902,Master_mconsumosdolares:=Master_mconsumosdolares*2.3]
dataset[ foto_mes==201903,Master_mconsumosdolares:=Master_mconsumosdolares*2.21]
dataset[ foto_mes==201904,Master_mconsumosdolares:=Master_mconsumosdolares*2.11]
dataset[ foto_mes==201905,Master_mconsumosdolares:=Master_mconsumosdolares*2.05]
dataset[ foto_mes==201906,Master_mconsumosdolares:=Master_mconsumosdolares*1.98]
dataset[ foto_mes==201907,Master_mconsumosdolares:=Master_mconsumosdolares*1.93]
dataset[ foto_mes==201908,Master_mconsumosdolares:=Master_mconsumosdolares*1.89]
dataset[ foto_mes==201909,Master_mconsumosdolares:=Master_mconsumosdolares*1.82]
dataset[ foto_mes==201910,Master_mconsumosdolares:=Master_mconsumosdolares*1.72]
dataset[ foto_mes==201911,Master_mconsumosdolares:=Master_mconsumosdolares*1.66]
dataset[ foto_mes==201912,Master_mconsumosdolares:=Master_mconsumosdolares*1.59]
dataset[ foto_mes==202001,Master_mconsumosdolares:=Master_mconsumosdolares*1.54]
dataset[ foto_mes==202002,Master_mconsumosdolares:=Master_mconsumosdolares*1.5]
dataset[ foto_mes==202003,Master_mconsumosdolares:=Master_mconsumosdolares*1.47]
dataset[ foto_mes==202004,Master_mconsumosdolares:=Master_mconsumosdolares*1.42]
dataset[ foto_mes==202005,Master_mconsumosdolares:=Master_mconsumosdolares*1.4]
dataset[ foto_mes==202006,Master_mconsumosdolares:=Master_mconsumosdolares*1.38]
dataset[ foto_mes==202007,Master_mconsumosdolares:=Master_mconsumosdolares*1.35]
dataset[ foto_mes==202008,Master_mconsumosdolares:=Master_mconsumosdolares*1.33]
dataset[ foto_mes==202009,Master_mconsumosdolares:=Master_mconsumosdolares*1.29]
dataset[ foto_mes==202010,Master_mconsumosdolares:=Master_mconsumosdolares*1.26]
dataset[ foto_mes==202011,Master_mconsumosdolares:=Master_mconsumosdolares*1.21]
dataset[ foto_mes==202012,Master_mconsumosdolares:=Master_mconsumosdolares*1.17]
dataset[ foto_mes==202101,Master_mconsumosdolares:=Master_mconsumosdolares*1.13]
dataset[ foto_mes==201901,Master_mlimitecompra:=Master_mlimitecompra*2.36]
dataset[ foto_mes==201902,Master_mlimitecompra:=Master_mlimitecompra*2.3]
dataset[ foto_mes==201903,Master_mlimitecompra:=Master_mlimitecompra*2.21]
dataset[ foto_mes==201904,Master_mlimitecompra:=Master_mlimitecompra*2.11]
dataset[ foto_mes==201905,Master_mlimitecompra:=Master_mlimitecompra*2.05]
dataset[ foto_mes==201906,Master_mlimitecompra:=Master_mlimitecompra*1.98]
dataset[ foto_mes==201907,Master_mlimitecompra:=Master_mlimitecompra*1.93]
dataset[ foto_mes==201908,Master_mlimitecompra:=Master_mlimitecompra*1.89]
dataset[ foto_mes==201909,Master_mlimitecompra:=Master_mlimitecompra*1.82]
dataset[ foto_mes==201910,Master_mlimitecompra:=Master_mlimitecompra*1.72]
dataset[ foto_mes==201911,Master_mlimitecompra:=Master_mlimitecompra*1.66]
dataset[ foto_mes==201912,Master_mlimitecompra:=Master_mlimitecompra*1.59]
dataset[ foto_mes==202001,Master_mlimitecompra:=Master_mlimitecompra*1.54]
dataset[ foto_mes==202002,Master_mlimitecompra:=Master_mlimitecompra*1.5]
dataset[ foto_mes==202003,Master_mlimitecompra:=Master_mlimitecompra*1.47]
dataset[ foto_mes==202004,Master_mlimitecompra:=Master_mlimitecompra*1.42]
dataset[ foto_mes==202005,Master_mlimitecompra:=Master_mlimitecompra*1.4]
dataset[ foto_mes==202006,Master_mlimitecompra:=Master_mlimitecompra*1.38]
dataset[ foto_mes==202007,Master_mlimitecompra:=Master_mlimitecompra*1.35]
dataset[ foto_mes==202008,Master_mlimitecompra:=Master_mlimitecompra*1.33]
dataset[ foto_mes==202009,Master_mlimitecompra:=Master_mlimitecompra*1.29]
dataset[ foto_mes==202010,Master_mlimitecompra:=Master_mlimitecompra*1.26]
dataset[ foto_mes==202011,Master_mlimitecompra:=Master_mlimitecompra*1.21]
dataset[ foto_mes==202012,Master_mlimitecompra:=Master_mlimitecompra*1.17]
dataset[ foto_mes==202101,Master_mlimitecompra:=Master_mlimitecompra*1.13]
dataset[ foto_mes==201901,Master_mpagado:=Master_mpagado*2.36]
dataset[ foto_mes==201902,Master_mpagado:=Master_mpagado*2.3]
dataset[ foto_mes==201903,Master_mpagado:=Master_mpagado*2.21]
dataset[ foto_mes==201904,Master_mpagado:=Master_mpagado*2.11]
dataset[ foto_mes==201905,Master_mpagado:=Master_mpagado*2.05]
dataset[ foto_mes==201906,Master_mpagado:=Master_mpagado*1.98]
dataset[ foto_mes==201907,Master_mpagado:=Master_mpagado*1.93]
dataset[ foto_mes==201908,Master_mpagado:=Master_mpagado*1.89]
dataset[ foto_mes==201909,Master_mpagado:=Master_mpagado*1.82]
dataset[ foto_mes==201910,Master_mpagado:=Master_mpagado*1.72]
dataset[ foto_mes==201911,Master_mpagado:=Master_mpagado*1.66]
dataset[ foto_mes==201912,Master_mpagado:=Master_mpagado*1.59]
dataset[ foto_mes==202001,Master_mpagado:=Master_mpagado*1.54]
dataset[ foto_mes==202002,Master_mpagado:=Master_mpagado*1.5]
dataset[ foto_mes==202003,Master_mpagado:=Master_mpagado*1.47]
dataset[ foto_mes==202004,Master_mpagado:=Master_mpagado*1.42]
dataset[ foto_mes==202005,Master_mpagado:=Master_mpagado*1.4]
dataset[ foto_mes==202006,Master_mpagado:=Master_mpagado*1.38]
dataset[ foto_mes==202007,Master_mpagado:=Master_mpagado*1.35]
dataset[ foto_mes==202008,Master_mpagado:=Master_mpagado*1.33]
dataset[ foto_mes==202009,Master_mpagado:=Master_mpagado*1.29]
dataset[ foto_mes==202010,Master_mpagado:=Master_mpagado*1.26]
dataset[ foto_mes==202011,Master_mpagado:=Master_mpagado*1.21]
dataset[ foto_mes==202012,Master_mpagado:=Master_mpagado*1.17]
dataset[ foto_mes==202101,Master_mpagado:=Master_mpagado*1.13]
dataset[ foto_mes==201901,Master_mpagospesos:=Master_mpagospesos*2.36]
dataset[ foto_mes==201902,Master_mpagospesos:=Master_mpagospesos*2.3]
dataset[ foto_mes==201903,Master_mpagospesos:=Master_mpagospesos*2.21]
dataset[ foto_mes==201904,Master_mpagospesos:=Master_mpagospesos*2.11]
dataset[ foto_mes==201905,Master_mpagospesos:=Master_mpagospesos*2.05]
dataset[ foto_mes==201906,Master_mpagospesos:=Master_mpagospesos*1.98]
dataset[ foto_mes==201907,Master_mpagospesos:=Master_mpagospesos*1.93]
dataset[ foto_mes==201908,Master_mpagospesos:=Master_mpagospesos*1.89]
dataset[ foto_mes==201909,Master_mpagospesos:=Master_mpagospesos*1.82]
dataset[ foto_mes==201910,Master_mpagospesos:=Master_mpagospesos*1.72]
dataset[ foto_mes==201911,Master_mpagospesos:=Master_mpagospesos*1.66]
dataset[ foto_mes==201912,Master_mpagospesos:=Master_mpagospesos*1.59]
dataset[ foto_mes==202001,Master_mpagospesos:=Master_mpagospesos*1.54]
dataset[ foto_mes==202002,Master_mpagospesos:=Master_mpagospesos*1.5]
dataset[ foto_mes==202003,Master_mpagospesos:=Master_mpagospesos*1.47]
dataset[ foto_mes==202004,Master_mpagospesos:=Master_mpagospesos*1.42]
dataset[ foto_mes==202005,Master_mpagospesos:=Master_mpagospesos*1.4]
dataset[ foto_mes==202006,Master_mpagospesos:=Master_mpagospesos*1.38]
dataset[ foto_mes==202007,Master_mpagospesos:=Master_mpagospesos*1.35]
dataset[ foto_mes==202008,Master_mpagospesos:=Master_mpagospesos*1.33]
dataset[ foto_mes==202009,Master_mpagospesos:=Master_mpagospesos*1.29]
dataset[ foto_mes==202010,Master_mpagospesos:=Master_mpagospesos*1.26]
dataset[ foto_mes==202011,Master_mpagospesos:=Master_mpagospesos*1.21]
dataset[ foto_mes==202012,Master_mpagospesos:=Master_mpagospesos*1.17]
dataset[ foto_mes==202101,Master_mpagospesos:=Master_mpagospesos*1.13]
dataset[ foto_mes==201901,Master_mpagosdolares:=Master_mpagosdolares*2.36]
dataset[ foto_mes==201902,Master_mpagosdolares:=Master_mpagosdolares*2.3]
dataset[ foto_mes==201903,Master_mpagosdolares:=Master_mpagosdolares*2.21]
dataset[ foto_mes==201904,Master_mpagosdolares:=Master_mpagosdolares*2.11]
dataset[ foto_mes==201905,Master_mpagosdolares:=Master_mpagosdolares*2.05]
dataset[ foto_mes==201906,Master_mpagosdolares:=Master_mpagosdolares*1.98]
dataset[ foto_mes==201907,Master_mpagosdolares:=Master_mpagosdolares*1.93]
dataset[ foto_mes==201908,Master_mpagosdolares:=Master_mpagosdolares*1.89]
dataset[ foto_mes==201909,Master_mpagosdolares:=Master_mpagosdolares*1.82]
dataset[ foto_mes==201910,Master_mpagosdolares:=Master_mpagosdolares*1.72]
dataset[ foto_mes==201911,Master_mpagosdolares:=Master_mpagosdolares*1.66]
dataset[ foto_mes==201912,Master_mpagosdolares:=Master_mpagosdolares*1.59]
dataset[ foto_mes==202001,Master_mpagosdolares:=Master_mpagosdolares*1.54]
dataset[ foto_mes==202002,Master_mpagosdolares:=Master_mpagosdolares*1.5]
dataset[ foto_mes==202003,Master_mpagosdolares:=Master_mpagosdolares*1.47]
dataset[ foto_mes==202004,Master_mpagosdolares:=Master_mpagosdolares*1.42]
dataset[ foto_mes==202005,Master_mpagosdolares:=Master_mpagosdolares*1.4]
dataset[ foto_mes==202006,Master_mpagosdolares:=Master_mpagosdolares*1.38]
dataset[ foto_mes==202007,Master_mpagosdolares:=Master_mpagosdolares*1.35]
dataset[ foto_mes==202008,Master_mpagosdolares:=Master_mpagosdolares*1.33]
dataset[ foto_mes==202009,Master_mpagosdolares:=Master_mpagosdolares*1.29]
dataset[ foto_mes==202010,Master_mpagosdolares:=Master_mpagosdolares*1.26]
dataset[ foto_mes==202011,Master_mpagosdolares:=Master_mpagosdolares*1.21]
dataset[ foto_mes==202012,Master_mpagosdolares:=Master_mpagosdolares*1.17]
dataset[ foto_mes==202101,Master_mpagosdolares:=Master_mpagosdolares*1.13]
dataset[ foto_mes==201901,Master_mconsumototal:=Master_mconsumototal*2.36]
dataset[ foto_mes==201902,Master_mconsumototal:=Master_mconsumototal*2.3]
dataset[ foto_mes==201903,Master_mconsumototal:=Master_mconsumototal*2.21]
dataset[ foto_mes==201904,Master_mconsumototal:=Master_mconsumototal*2.11]
dataset[ foto_mes==201905,Master_mconsumototal:=Master_mconsumototal*2.05]
dataset[ foto_mes==201906,Master_mconsumototal:=Master_mconsumototal*1.98]
dataset[ foto_mes==201907,Master_mconsumototal:=Master_mconsumototal*1.93]
dataset[ foto_mes==201908,Master_mconsumototal:=Master_mconsumototal*1.89]
dataset[ foto_mes==201909,Master_mconsumototal:=Master_mconsumototal*1.82]
dataset[ foto_mes==201910,Master_mconsumototal:=Master_mconsumototal*1.72]
dataset[ foto_mes==201911,Master_mconsumototal:=Master_mconsumototal*1.66]
dataset[ foto_mes==201912,Master_mconsumototal:=Master_mconsumototal*1.59]
dataset[ foto_mes==202001,Master_mconsumototal:=Master_mconsumototal*1.54]
dataset[ foto_mes==202002,Master_mconsumototal:=Master_mconsumototal*1.5]
dataset[ foto_mes==202003,Master_mconsumototal:=Master_mconsumototal*1.47]
dataset[ foto_mes==202004,Master_mconsumototal:=Master_mconsumototal*1.42]
dataset[ foto_mes==202005,Master_mconsumototal:=Master_mconsumototal*1.4]
dataset[ foto_mes==202006,Master_mconsumototal:=Master_mconsumototal*1.38]
dataset[ foto_mes==202007,Master_mconsumototal:=Master_mconsumototal*1.35]
dataset[ foto_mes==202008,Master_mconsumototal:=Master_mconsumototal*1.33]
dataset[ foto_mes==202009,Master_mconsumototal:=Master_mconsumototal*1.29]
dataset[ foto_mes==202010,Master_mconsumototal:=Master_mconsumototal*1.26]
dataset[ foto_mes==202011,Master_mconsumototal:=Master_mconsumototal*1.21]
dataset[ foto_mes==202012,Master_mconsumototal:=Master_mconsumototal*1.17]
dataset[ foto_mes==202101,Master_mconsumototal:=Master_mconsumototal*1.13]
dataset[ foto_mes==201901,Master_mpagominimo:=Master_mpagominimo*2.36]
dataset[ foto_mes==201902,Master_mpagominimo:=Master_mpagominimo*2.3]
dataset[ foto_mes==201903,Master_mpagominimo:=Master_mpagominimo*2.21]
dataset[ foto_mes==201904,Master_mpagominimo:=Master_mpagominimo*2.11]
dataset[ foto_mes==201905,Master_mpagominimo:=Master_mpagominimo*2.05]
dataset[ foto_mes==201906,Master_mpagominimo:=Master_mpagominimo*1.98]
dataset[ foto_mes==201907,Master_mpagominimo:=Master_mpagominimo*1.93]
dataset[ foto_mes==201908,Master_mpagominimo:=Master_mpagominimo*1.89]
dataset[ foto_mes==201909,Master_mpagominimo:=Master_mpagominimo*1.82]
dataset[ foto_mes==201910,Master_mpagominimo:=Master_mpagominimo*1.72]
dataset[ foto_mes==201911,Master_mpagominimo:=Master_mpagominimo*1.66]
dataset[ foto_mes==201912,Master_mpagominimo:=Master_mpagominimo*1.59]
dataset[ foto_mes==202001,Master_mpagominimo:=Master_mpagominimo*1.54]
dataset[ foto_mes==202002,Master_mpagominimo:=Master_mpagominimo*1.5]
dataset[ foto_mes==202003,Master_mpagominimo:=Master_mpagominimo*1.47]
dataset[ foto_mes==202004,Master_mpagominimo:=Master_mpagominimo*1.42]
dataset[ foto_mes==202005,Master_mpagominimo:=Master_mpagominimo*1.4]
dataset[ foto_mes==202006,Master_mpagominimo:=Master_mpagominimo*1.38]
dataset[ foto_mes==202007,Master_mpagominimo:=Master_mpagominimo*1.35]
dataset[ foto_mes==202008,Master_mpagominimo:=Master_mpagominimo*1.33]
dataset[ foto_mes==202009,Master_mpagominimo:=Master_mpagominimo*1.29]
dataset[ foto_mes==202010,Master_mpagominimo:=Master_mpagominimo*1.26]
dataset[ foto_mes==202011,Master_mpagominimo:=Master_mpagominimo*1.21]
dataset[ foto_mes==202012,Master_mpagominimo:=Master_mpagominimo*1.17]
dataset[ foto_mes==202101,Master_mpagominimo:=Master_mpagominimo*1.13]
dataset[ foto_mes==201901,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*2.36]
dataset[ foto_mes==201902,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*2.3]
dataset[ foto_mes==201903,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*2.21]
dataset[ foto_mes==201904,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*2.11]
dataset[ foto_mes==201905,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*2.05]
dataset[ foto_mes==201906,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*1.98]
dataset[ foto_mes==201907,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*1.93]
dataset[ foto_mes==201908,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*1.89]
dataset[ foto_mes==201909,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*1.82]
dataset[ foto_mes==201910,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*1.72]
dataset[ foto_mes==201911,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*1.66]
dataset[ foto_mes==201912,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*1.59]
dataset[ foto_mes==202001,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*1.54]
dataset[ foto_mes==202002,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*1.5]
dataset[ foto_mes==202003,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*1.47]
dataset[ foto_mes==202004,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*1.42]
dataset[ foto_mes==202005,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*1.4]
dataset[ foto_mes==202006,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*1.38]
dataset[ foto_mes==202007,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*1.35]
dataset[ foto_mes==202008,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*1.33]
dataset[ foto_mes==202009,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*1.29]
dataset[ foto_mes==202010,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*1.26]
dataset[ foto_mes==202011,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*1.21]
dataset[ foto_mes==202012,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*1.17]
dataset[ foto_mes==202101,Visa_mfinanciacion_limite:=Visa_mfinanciacion_limite*1.13]
dataset[ foto_mes==201901,Visa_msaldototal:=Visa_msaldototal*2.36]
dataset[ foto_mes==201902,Visa_msaldototal:=Visa_msaldototal*2.3]
dataset[ foto_mes==201903,Visa_msaldototal:=Visa_msaldototal*2.21]
dataset[ foto_mes==201904,Visa_msaldototal:=Visa_msaldototal*2.11]
dataset[ foto_mes==201905,Visa_msaldototal:=Visa_msaldototal*2.05]
dataset[ foto_mes==201906,Visa_msaldototal:=Visa_msaldototal*1.98]
dataset[ foto_mes==201907,Visa_msaldototal:=Visa_msaldototal*1.93]
dataset[ foto_mes==201908,Visa_msaldototal:=Visa_msaldototal*1.89]
dataset[ foto_mes==201909,Visa_msaldototal:=Visa_msaldototal*1.82]
dataset[ foto_mes==201910,Visa_msaldototal:=Visa_msaldototal*1.72]
dataset[ foto_mes==201911,Visa_msaldototal:=Visa_msaldototal*1.66]
dataset[ foto_mes==201912,Visa_msaldototal:=Visa_msaldototal*1.59]
dataset[ foto_mes==202001,Visa_msaldototal:=Visa_msaldototal*1.54]
dataset[ foto_mes==202002,Visa_msaldototal:=Visa_msaldototal*1.5]
dataset[ foto_mes==202003,Visa_msaldototal:=Visa_msaldototal*1.47]
dataset[ foto_mes==202004,Visa_msaldototal:=Visa_msaldototal*1.42]
dataset[ foto_mes==202005,Visa_msaldototal:=Visa_msaldototal*1.4]
dataset[ foto_mes==202006,Visa_msaldototal:=Visa_msaldototal*1.38]
dataset[ foto_mes==202007,Visa_msaldototal:=Visa_msaldototal*1.35]
dataset[ foto_mes==202008,Visa_msaldototal:=Visa_msaldototal*1.33]
dataset[ foto_mes==202009,Visa_msaldototal:=Visa_msaldototal*1.29]
dataset[ foto_mes==202010,Visa_msaldototal:=Visa_msaldototal*1.26]
dataset[ foto_mes==202011,Visa_msaldototal:=Visa_msaldototal*1.21]
dataset[ foto_mes==202012,Visa_msaldototal:=Visa_msaldototal*1.17]
dataset[ foto_mes==202101,Visa_msaldototal:=Visa_msaldototal*1.13]
dataset[ foto_mes==201901,Visa_msaldopesos:=Visa_msaldopesos*2.36]
dataset[ foto_mes==201902,Visa_msaldopesos:=Visa_msaldopesos*2.3]
dataset[ foto_mes==201903,Visa_msaldopesos:=Visa_msaldopesos*2.21]
dataset[ foto_mes==201904,Visa_msaldopesos:=Visa_msaldopesos*2.11]
dataset[ foto_mes==201905,Visa_msaldopesos:=Visa_msaldopesos*2.05]
dataset[ foto_mes==201906,Visa_msaldopesos:=Visa_msaldopesos*1.98]
dataset[ foto_mes==201907,Visa_msaldopesos:=Visa_msaldopesos*1.93]
dataset[ foto_mes==201908,Visa_msaldopesos:=Visa_msaldopesos*1.89]
dataset[ foto_mes==201909,Visa_msaldopesos:=Visa_msaldopesos*1.82]
dataset[ foto_mes==201910,Visa_msaldopesos:=Visa_msaldopesos*1.72]
dataset[ foto_mes==201911,Visa_msaldopesos:=Visa_msaldopesos*1.66]
dataset[ foto_mes==201912,Visa_msaldopesos:=Visa_msaldopesos*1.59]
dataset[ foto_mes==202001,Visa_msaldopesos:=Visa_msaldopesos*1.54]
dataset[ foto_mes==202002,Visa_msaldopesos:=Visa_msaldopesos*1.5]
dataset[ foto_mes==202003,Visa_msaldopesos:=Visa_msaldopesos*1.47]
dataset[ foto_mes==202004,Visa_msaldopesos:=Visa_msaldopesos*1.42]
dataset[ foto_mes==202005,Visa_msaldopesos:=Visa_msaldopesos*1.4]
dataset[ foto_mes==202006,Visa_msaldopesos:=Visa_msaldopesos*1.38]
dataset[ foto_mes==202007,Visa_msaldopesos:=Visa_msaldopesos*1.35]
dataset[ foto_mes==202008,Visa_msaldopesos:=Visa_msaldopesos*1.33]
dataset[ foto_mes==202009,Visa_msaldopesos:=Visa_msaldopesos*1.29]
dataset[ foto_mes==202010,Visa_msaldopesos:=Visa_msaldopesos*1.26]
dataset[ foto_mes==202011,Visa_msaldopesos:=Visa_msaldopesos*1.21]
dataset[ foto_mes==202012,Visa_msaldopesos:=Visa_msaldopesos*1.17]
dataset[ foto_mes==202101,Visa_msaldopesos:=Visa_msaldopesos*1.13]
dataset[ foto_mes==201901,Visa_msaldodolares:=Visa_msaldodolares*2.36]
dataset[ foto_mes==201902,Visa_msaldodolares:=Visa_msaldodolares*2.3]
dataset[ foto_mes==201903,Visa_msaldodolares:=Visa_msaldodolares*2.21]
dataset[ foto_mes==201904,Visa_msaldodolares:=Visa_msaldodolares*2.11]
dataset[ foto_mes==201905,Visa_msaldodolares:=Visa_msaldodolares*2.05]
dataset[ foto_mes==201906,Visa_msaldodolares:=Visa_msaldodolares*1.98]
dataset[ foto_mes==201907,Visa_msaldodolares:=Visa_msaldodolares*1.93]
dataset[ foto_mes==201908,Visa_msaldodolares:=Visa_msaldodolares*1.89]
dataset[ foto_mes==201909,Visa_msaldodolares:=Visa_msaldodolares*1.82]
dataset[ foto_mes==201910,Visa_msaldodolares:=Visa_msaldodolares*1.72]
dataset[ foto_mes==201911,Visa_msaldodolares:=Visa_msaldodolares*1.66]
dataset[ foto_mes==201912,Visa_msaldodolares:=Visa_msaldodolares*1.59]
dataset[ foto_mes==202001,Visa_msaldodolares:=Visa_msaldodolares*1.54]
dataset[ foto_mes==202002,Visa_msaldodolares:=Visa_msaldodolares*1.5]
dataset[ foto_mes==202003,Visa_msaldodolares:=Visa_msaldodolares*1.47]
dataset[ foto_mes==202004,Visa_msaldodolares:=Visa_msaldodolares*1.42]
dataset[ foto_mes==202005,Visa_msaldodolares:=Visa_msaldodolares*1.4]
dataset[ foto_mes==202006,Visa_msaldodolares:=Visa_msaldodolares*1.38]
dataset[ foto_mes==202007,Visa_msaldodolares:=Visa_msaldodolares*1.35]
dataset[ foto_mes==202008,Visa_msaldodolares:=Visa_msaldodolares*1.33]
dataset[ foto_mes==202009,Visa_msaldodolares:=Visa_msaldodolares*1.29]
dataset[ foto_mes==202010,Visa_msaldodolares:=Visa_msaldodolares*1.26]
dataset[ foto_mes==202011,Visa_msaldodolares:=Visa_msaldodolares*1.21]
dataset[ foto_mes==202012,Visa_msaldodolares:=Visa_msaldodolares*1.17]
dataset[ foto_mes==202101,Visa_msaldodolares:=Visa_msaldodolares*1.13]
dataset[ foto_mes==201901,Visa_mconsumospesos:=Visa_mconsumospesos*2.36]
dataset[ foto_mes==201902,Visa_mconsumospesos:=Visa_mconsumospesos*2.3]
dataset[ foto_mes==201903,Visa_mconsumospesos:=Visa_mconsumospesos*2.21]
dataset[ foto_mes==201904,Visa_mconsumospesos:=Visa_mconsumospesos*2.11]
dataset[ foto_mes==201905,Visa_mconsumospesos:=Visa_mconsumospesos*2.05]
dataset[ foto_mes==201906,Visa_mconsumospesos:=Visa_mconsumospesos*1.98]
dataset[ foto_mes==201907,Visa_mconsumospesos:=Visa_mconsumospesos*1.93]
dataset[ foto_mes==201908,Visa_mconsumospesos:=Visa_mconsumospesos*1.89]
dataset[ foto_mes==201909,Visa_mconsumospesos:=Visa_mconsumospesos*1.82]
dataset[ foto_mes==201910,Visa_mconsumospesos:=Visa_mconsumospesos*1.72]
dataset[ foto_mes==201911,Visa_mconsumospesos:=Visa_mconsumospesos*1.66]
dataset[ foto_mes==201912,Visa_mconsumospesos:=Visa_mconsumospesos*1.59]
dataset[ foto_mes==202001,Visa_mconsumospesos:=Visa_mconsumospesos*1.54]
dataset[ foto_mes==202002,Visa_mconsumospesos:=Visa_mconsumospesos*1.5]
dataset[ foto_mes==202003,Visa_mconsumospesos:=Visa_mconsumospesos*1.47]
dataset[ foto_mes==202004,Visa_mconsumospesos:=Visa_mconsumospesos*1.42]
dataset[ foto_mes==202005,Visa_mconsumospesos:=Visa_mconsumospesos*1.4]
dataset[ foto_mes==202006,Visa_mconsumospesos:=Visa_mconsumospesos*1.38]
dataset[ foto_mes==202007,Visa_mconsumospesos:=Visa_mconsumospesos*1.35]
dataset[ foto_mes==202008,Visa_mconsumospesos:=Visa_mconsumospesos*1.33]
dataset[ foto_mes==202009,Visa_mconsumospesos:=Visa_mconsumospesos*1.29]
dataset[ foto_mes==202010,Visa_mconsumospesos:=Visa_mconsumospesos*1.26]
dataset[ foto_mes==202011,Visa_mconsumospesos:=Visa_mconsumospesos*1.21]
dataset[ foto_mes==202012,Visa_mconsumospesos:=Visa_mconsumospesos*1.17]
dataset[ foto_mes==202101,Visa_mconsumospesos:=Visa_mconsumospesos*1.13]
dataset[ foto_mes==201901,Visa_mconsumosdolares:=Visa_mconsumosdolares*2.36]
dataset[ foto_mes==201902,Visa_mconsumosdolares:=Visa_mconsumosdolares*2.3]
dataset[ foto_mes==201903,Visa_mconsumosdolares:=Visa_mconsumosdolares*2.21]
dataset[ foto_mes==201904,Visa_mconsumosdolares:=Visa_mconsumosdolares*2.11]
dataset[ foto_mes==201905,Visa_mconsumosdolares:=Visa_mconsumosdolares*2.05]
dataset[ foto_mes==201906,Visa_mconsumosdolares:=Visa_mconsumosdolares*1.98]
dataset[ foto_mes==201907,Visa_mconsumosdolares:=Visa_mconsumosdolares*1.93]
dataset[ foto_mes==201908,Visa_mconsumosdolares:=Visa_mconsumosdolares*1.89]
dataset[ foto_mes==201909,Visa_mconsumosdolares:=Visa_mconsumosdolares*1.82]
dataset[ foto_mes==201910,Visa_mconsumosdolares:=Visa_mconsumosdolares*1.72]
dataset[ foto_mes==201911,Visa_mconsumosdolares:=Visa_mconsumosdolares*1.66]
dataset[ foto_mes==201912,Visa_mconsumosdolares:=Visa_mconsumosdolares*1.59]
dataset[ foto_mes==202001,Visa_mconsumosdolares:=Visa_mconsumosdolares*1.54]
dataset[ foto_mes==202002,Visa_mconsumosdolares:=Visa_mconsumosdolares*1.5]
dataset[ foto_mes==202003,Visa_mconsumosdolares:=Visa_mconsumosdolares*1.47]
dataset[ foto_mes==202004,Visa_mconsumosdolares:=Visa_mconsumosdolares*1.42]
dataset[ foto_mes==202005,Visa_mconsumosdolares:=Visa_mconsumosdolares*1.4]
dataset[ foto_mes==202006,Visa_mconsumosdolares:=Visa_mconsumosdolares*1.38]
dataset[ foto_mes==202007,Visa_mconsumosdolares:=Visa_mconsumosdolares*1.35]
dataset[ foto_mes==202008,Visa_mconsumosdolares:=Visa_mconsumosdolares*1.33]
dataset[ foto_mes==202009,Visa_mconsumosdolares:=Visa_mconsumosdolares*1.29]
dataset[ foto_mes==202010,Visa_mconsumosdolares:=Visa_mconsumosdolares*1.26]
dataset[ foto_mes==202011,Visa_mconsumosdolares:=Visa_mconsumosdolares*1.21]
dataset[ foto_mes==202012,Visa_mconsumosdolares:=Visa_mconsumosdolares*1.17]
dataset[ foto_mes==202101,Visa_mconsumosdolares:=Visa_mconsumosdolares*1.13]
dataset[ foto_mes==201901,Visa_mlimitecompra:=Visa_mlimitecompra*2.36]
dataset[ foto_mes==201902,Visa_mlimitecompra:=Visa_mlimitecompra*2.3]
dataset[ foto_mes==201903,Visa_mlimitecompra:=Visa_mlimitecompra*2.21]
dataset[ foto_mes==201904,Visa_mlimitecompra:=Visa_mlimitecompra*2.11]
dataset[ foto_mes==201905,Visa_mlimitecompra:=Visa_mlimitecompra*2.05]
dataset[ foto_mes==201906,Visa_mlimitecompra:=Visa_mlimitecompra*1.98]
dataset[ foto_mes==201907,Visa_mlimitecompra:=Visa_mlimitecompra*1.93]
dataset[ foto_mes==201908,Visa_mlimitecompra:=Visa_mlimitecompra*1.89]
dataset[ foto_mes==201909,Visa_mlimitecompra:=Visa_mlimitecompra*1.82]
dataset[ foto_mes==201910,Visa_mlimitecompra:=Visa_mlimitecompra*1.72]
dataset[ foto_mes==201911,Visa_mlimitecompra:=Visa_mlimitecompra*1.66]
dataset[ foto_mes==201912,Visa_mlimitecompra:=Visa_mlimitecompra*1.59]
dataset[ foto_mes==202001,Visa_mlimitecompra:=Visa_mlimitecompra*1.54]
dataset[ foto_mes==202002,Visa_mlimitecompra:=Visa_mlimitecompra*1.5]
dataset[ foto_mes==202003,Visa_mlimitecompra:=Visa_mlimitecompra*1.47]
dataset[ foto_mes==202004,Visa_mlimitecompra:=Visa_mlimitecompra*1.42]
dataset[ foto_mes==202005,Visa_mlimitecompra:=Visa_mlimitecompra*1.4]
dataset[ foto_mes==202006,Visa_mlimitecompra:=Visa_mlimitecompra*1.38]
dataset[ foto_mes==202007,Visa_mlimitecompra:=Visa_mlimitecompra*1.35]
dataset[ foto_mes==202008,Visa_mlimitecompra:=Visa_mlimitecompra*1.33]
dataset[ foto_mes==202009,Visa_mlimitecompra:=Visa_mlimitecompra*1.29]
dataset[ foto_mes==202010,Visa_mlimitecompra:=Visa_mlimitecompra*1.26]
dataset[ foto_mes==202011,Visa_mlimitecompra:=Visa_mlimitecompra*1.21]
dataset[ foto_mes==202012,Visa_mlimitecompra:=Visa_mlimitecompra*1.17]
dataset[ foto_mes==202101,Visa_mlimitecompra:=Visa_mlimitecompra*1.13]
dataset[ foto_mes==201901,Visa_mpagado:=Visa_mpagado*2.36]
dataset[ foto_mes==201902,Visa_mpagado:=Visa_mpagado*2.3]
dataset[ foto_mes==201903,Visa_mpagado:=Visa_mpagado*2.21]
dataset[ foto_mes==201904,Visa_mpagado:=Visa_mpagado*2.11]
dataset[ foto_mes==201905,Visa_mpagado:=Visa_mpagado*2.05]
dataset[ foto_mes==201906,Visa_mpagado:=Visa_mpagado*1.98]
dataset[ foto_mes==201907,Visa_mpagado:=Visa_mpagado*1.93]
dataset[ foto_mes==201908,Visa_mpagado:=Visa_mpagado*1.89]
dataset[ foto_mes==201909,Visa_mpagado:=Visa_mpagado*1.82]
dataset[ foto_mes==201910,Visa_mpagado:=Visa_mpagado*1.72]
dataset[ foto_mes==201911,Visa_mpagado:=Visa_mpagado*1.66]
dataset[ foto_mes==201912,Visa_mpagado:=Visa_mpagado*1.59]
dataset[ foto_mes==202001,Visa_mpagado:=Visa_mpagado*1.54]
dataset[ foto_mes==202002,Visa_mpagado:=Visa_mpagado*1.5]
dataset[ foto_mes==202003,Visa_mpagado:=Visa_mpagado*1.47]
dataset[ foto_mes==202004,Visa_mpagado:=Visa_mpagado*1.42]
dataset[ foto_mes==202005,Visa_mpagado:=Visa_mpagado*1.4]
dataset[ foto_mes==202006,Visa_mpagado:=Visa_mpagado*1.38]
dataset[ foto_mes==202007,Visa_mpagado:=Visa_mpagado*1.35]
dataset[ foto_mes==202008,Visa_mpagado:=Visa_mpagado*1.33]
dataset[ foto_mes==202009,Visa_mpagado:=Visa_mpagado*1.29]
dataset[ foto_mes==202010,Visa_mpagado:=Visa_mpagado*1.26]
dataset[ foto_mes==202011,Visa_mpagado:=Visa_mpagado*1.21]
dataset[ foto_mes==202012,Visa_mpagado:=Visa_mpagado*1.17]
dataset[ foto_mes==202101,Visa_mpagado:=Visa_mpagado*1.13]
dataset[ foto_mes==201901,Visa_mpagospesos:=Visa_mpagospesos*2.36]
dataset[ foto_mes==201902,Visa_mpagospesos:=Visa_mpagospesos*2.3]
dataset[ foto_mes==201903,Visa_mpagospesos:=Visa_mpagospesos*2.21]
dataset[ foto_mes==201904,Visa_mpagospesos:=Visa_mpagospesos*2.11]
dataset[ foto_mes==201905,Visa_mpagospesos:=Visa_mpagospesos*2.05]
dataset[ foto_mes==201906,Visa_mpagospesos:=Visa_mpagospesos*1.98]
dataset[ foto_mes==201907,Visa_mpagospesos:=Visa_mpagospesos*1.93]
dataset[ foto_mes==201908,Visa_mpagospesos:=Visa_mpagospesos*1.89]
dataset[ foto_mes==201909,Visa_mpagospesos:=Visa_mpagospesos*1.82]
dataset[ foto_mes==201910,Visa_mpagospesos:=Visa_mpagospesos*1.72]
dataset[ foto_mes==201911,Visa_mpagospesos:=Visa_mpagospesos*1.66]
dataset[ foto_mes==201912,Visa_mpagospesos:=Visa_mpagospesos*1.59]
dataset[ foto_mes==202001,Visa_mpagospesos:=Visa_mpagospesos*1.54]
dataset[ foto_mes==202002,Visa_mpagospesos:=Visa_mpagospesos*1.5]
dataset[ foto_mes==202003,Visa_mpagospesos:=Visa_mpagospesos*1.47]
dataset[ foto_mes==202004,Visa_mpagospesos:=Visa_mpagospesos*1.42]
dataset[ foto_mes==202005,Visa_mpagospesos:=Visa_mpagospesos*1.4]
dataset[ foto_mes==202006,Visa_mpagospesos:=Visa_mpagospesos*1.38]
dataset[ foto_mes==202007,Visa_mpagospesos:=Visa_mpagospesos*1.35]
dataset[ foto_mes==202008,Visa_mpagospesos:=Visa_mpagospesos*1.33]
dataset[ foto_mes==202009,Visa_mpagospesos:=Visa_mpagospesos*1.29]
dataset[ foto_mes==202010,Visa_mpagospesos:=Visa_mpagospesos*1.26]
dataset[ foto_mes==202011,Visa_mpagospesos:=Visa_mpagospesos*1.21]
dataset[ foto_mes==202012,Visa_mpagospesos:=Visa_mpagospesos*1.17]
dataset[ foto_mes==202101,Visa_mpagospesos:=Visa_mpagospesos*1.13]
dataset[ foto_mes==201901,Visa_mpagosdolares:=Visa_mpagosdolares*2.36]
dataset[ foto_mes==201902,Visa_mpagosdolares:=Visa_mpagosdolares*2.3]
dataset[ foto_mes==201903,Visa_mpagosdolares:=Visa_mpagosdolares*2.21]
dataset[ foto_mes==201904,Visa_mpagosdolares:=Visa_mpagosdolares*2.11]
dataset[ foto_mes==201905,Visa_mpagosdolares:=Visa_mpagosdolares*2.05]
dataset[ foto_mes==201906,Visa_mpagosdolares:=Visa_mpagosdolares*1.98]
dataset[ foto_mes==201907,Visa_mpagosdolares:=Visa_mpagosdolares*1.93]
dataset[ foto_mes==201908,Visa_mpagosdolares:=Visa_mpagosdolares*1.89]
dataset[ foto_mes==201909,Visa_mpagosdolares:=Visa_mpagosdolares*1.82]
dataset[ foto_mes==201910,Visa_mpagosdolares:=Visa_mpagosdolares*1.72]
dataset[ foto_mes==201911,Visa_mpagosdolares:=Visa_mpagosdolares*1.66]
dataset[ foto_mes==201912,Visa_mpagosdolares:=Visa_mpagosdolares*1.59]
dataset[ foto_mes==202001,Visa_mpagosdolares:=Visa_mpagosdolares*1.54]
dataset[ foto_mes==202002,Visa_mpagosdolares:=Visa_mpagosdolares*1.5]
dataset[ foto_mes==202003,Visa_mpagosdolares:=Visa_mpagosdolares*1.47]
dataset[ foto_mes==202004,Visa_mpagosdolares:=Visa_mpagosdolares*1.42]
dataset[ foto_mes==202005,Visa_mpagosdolares:=Visa_mpagosdolares*1.4]
dataset[ foto_mes==202006,Visa_mpagosdolares:=Visa_mpagosdolares*1.38]
dataset[ foto_mes==202007,Visa_mpagosdolares:=Visa_mpagosdolares*1.35]
dataset[ foto_mes==202008,Visa_mpagosdolares:=Visa_mpagosdolares*1.33]
dataset[ foto_mes==202009,Visa_mpagosdolares:=Visa_mpagosdolares*1.29]
dataset[ foto_mes==202010,Visa_mpagosdolares:=Visa_mpagosdolares*1.26]
dataset[ foto_mes==202011,Visa_mpagosdolares:=Visa_mpagosdolares*1.21]
dataset[ foto_mes==202012,Visa_mpagosdolares:=Visa_mpagosdolares*1.17]
dataset[ foto_mes==202101,Visa_mpagosdolares:=Visa_mpagosdolares*1.13]
dataset[ foto_mes==201901,Visa_mconsumototal:=Visa_mconsumototal*2.36]
dataset[ foto_mes==201902,Visa_mconsumototal:=Visa_mconsumototal*2.3]
dataset[ foto_mes==201903,Visa_mconsumototal:=Visa_mconsumototal*2.21]
dataset[ foto_mes==201904,Visa_mconsumototal:=Visa_mconsumototal*2.11]
dataset[ foto_mes==201905,Visa_mconsumototal:=Visa_mconsumototal*2.05]
dataset[ foto_mes==201906,Visa_mconsumototal:=Visa_mconsumototal*1.98]
dataset[ foto_mes==201907,Visa_mconsumototal:=Visa_mconsumototal*1.93]
dataset[ foto_mes==201908,Visa_mconsumototal:=Visa_mconsumototal*1.89]
dataset[ foto_mes==201909,Visa_mconsumototal:=Visa_mconsumototal*1.82]
dataset[ foto_mes==201910,Visa_mconsumototal:=Visa_mconsumototal*1.72]
dataset[ foto_mes==201911,Visa_mconsumototal:=Visa_mconsumototal*1.66]
dataset[ foto_mes==201912,Visa_mconsumototal:=Visa_mconsumototal*1.59]
dataset[ foto_mes==202001,Visa_mconsumototal:=Visa_mconsumototal*1.54]
dataset[ foto_mes==202002,Visa_mconsumototal:=Visa_mconsumototal*1.5]
dataset[ foto_mes==202003,Visa_mconsumototal:=Visa_mconsumototal*1.47]
dataset[ foto_mes==202004,Visa_mconsumototal:=Visa_mconsumototal*1.42]
dataset[ foto_mes==202005,Visa_mconsumototal:=Visa_mconsumototal*1.4]
dataset[ foto_mes==202006,Visa_mconsumototal:=Visa_mconsumototal*1.38]
dataset[ foto_mes==202007,Visa_mconsumototal:=Visa_mconsumototal*1.35]
dataset[ foto_mes==202008,Visa_mconsumototal:=Visa_mconsumototal*1.33]
dataset[ foto_mes==202009,Visa_mconsumototal:=Visa_mconsumototal*1.29]
dataset[ foto_mes==202010,Visa_mconsumototal:=Visa_mconsumototal*1.26]
dataset[ foto_mes==202011,Visa_mconsumototal:=Visa_mconsumototal*1.21]
dataset[ foto_mes==202012,Visa_mconsumototal:=Visa_mconsumototal*1.17]
dataset[ foto_mes==202101,Visa_mconsumototal:=Visa_mconsumototal*1.13]
dataset[ foto_mes==201901,Visa_mpagominimo:=Visa_mpagominimo*2.36]
dataset[ foto_mes==201902,Visa_mpagominimo:=Visa_mpagominimo*2.3]
dataset[ foto_mes==201903,Visa_mpagominimo:=Visa_mpagominimo*2.21]
dataset[ foto_mes==201904,Visa_mpagominimo:=Visa_mpagominimo*2.11]
dataset[ foto_mes==201905,Visa_mpagominimo:=Visa_mpagominimo*2.05]
dataset[ foto_mes==201906,Visa_mpagominimo:=Visa_mpagominimo*1.98]
dataset[ foto_mes==201907,Visa_mpagominimo:=Visa_mpagominimo*1.93]
dataset[ foto_mes==201908,Visa_mpagominimo:=Visa_mpagominimo*1.89]
dataset[ foto_mes==201909,Visa_mpagominimo:=Visa_mpagominimo*1.82]
dataset[ foto_mes==201910,Visa_mpagominimo:=Visa_mpagominimo*1.72]
dataset[ foto_mes==201911,Visa_mpagominimo:=Visa_mpagominimo*1.66]
dataset[ foto_mes==201912,Visa_mpagominimo:=Visa_mpagominimo*1.59]
dataset[ foto_mes==202001,Visa_mpagominimo:=Visa_mpagominimo*1.54]
dataset[ foto_mes==202002,Visa_mpagominimo:=Visa_mpagominimo*1.5]
dataset[ foto_mes==202003,Visa_mpagominimo:=Visa_mpagominimo*1.47]
dataset[ foto_mes==202004,Visa_mpagominimo:=Visa_mpagominimo*1.42]
dataset[ foto_mes==202005,Visa_mpagominimo:=Visa_mpagominimo*1.4]
dataset[ foto_mes==202006,Visa_mpagominimo:=Visa_mpagominimo*1.38]
dataset[ foto_mes==202007,Visa_mpagominimo:=Visa_mpagominimo*1.35]
dataset[ foto_mes==202008,Visa_mpagominimo:=Visa_mpagominimo*1.33]
dataset[ foto_mes==202009,Visa_mpagominimo:=Visa_mpagominimo*1.29]
dataset[ foto_mes==202010,Visa_mpagominimo:=Visa_mpagominimo*1.26]
dataset[ foto_mes==202011,Visa_mpagominimo:=Visa_mpagominimo*1.21]
dataset[ foto_mes==202012,Visa_mpagominimo:=Visa_mpagominimo*1.17]
dataset[ foto_mes==202101,Visa_mpagominimo:=Visa_mpagominimo*1.13]
}


AgregarVariables  <- function( dataset )
{
  gc()
  #INICIO de la seccion donde se deben hacer cambios con variables nuevas

  #creo un ctr_quarter que tenga en cuenta cuando los clientes hace 3 menos meses que estan
  dataset[  , ctrx_quarter_normalizado := ctrx_quarter ]
  dataset[ cliente_antiguedad==1 , ctrx_quarter_normalizado := ctrx_quarter * 5 ]
  dataset[ cliente_antiguedad==2 , ctrx_quarter_normalizado := ctrx_quarter * 2 ]
  dataset[ cliente_antiguedad==3 , ctrx_quarter_normalizado := ctrx_quarter * 1.2 ]

  #variable extraida de una tesis de maestria de Irlanda
  dataset[  , mpayroll_sobre_edad  := mpayroll / cliente_edad ]

  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status
  dataset[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  dataset[ , mv_status02       := Master_status +  Visa_status ]
  dataset[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
  dataset[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
  dataset[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]

  dataset[ , mv_status06       := ifelse( is.na(Visa_status), 
                                          ifelse( is.na(Master_status), 10, Master_status), 
                                          Visa_status)  ]

  dataset[ , mv_status07       := ifelse( is.na(Master_status), 
                                          ifelse( is.na(Visa_status), 10, Visa_status), 
                                          Master_status)  ]


  #combino MasterCard y Visa
  dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

  dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
  dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
  dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
  dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
  dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
  dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  dataset[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

  #a partir de aqui juego con la suma de Mastercard y Visa
  dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
  dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
  dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
  dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
  dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
  dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
  dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
  dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
  dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
  dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
  dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
  dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]

  #Aqui debe usted agregar sus propias nuevas variables

  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
  infinitos_qty  <- sum( unlist( infinitos) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <<- NA
  }


  #valvula de seguridad para evitar valores NaN  que es 0/0
  #paso los NaN a 0 , decision polemica si las hay
  #se invita a asignar un valor razonable segun la semantica del campo creado
  nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
  nans_qty  <- sum( unlist( nans) )
  if( nans_qty > 0 )
  {
    cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
    cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <<- 0
  }

}

#------------------------------------------------------------------------------
#se calculan para los 6 meses previos el minimo, maximo y tendencia calculada con cuadrados minimos
#la formula de calculo de la tendencia puede verse en https://stats.libretexts.org/Bookshelves/Introductory_Statistics/Book%3A_Introductory_Statistics_(Shafer_and_Zhang)/10%3A_Correlation_and_Regression/10.04%3A_The_Least_Squares_Regression_Line
#para la maxíma velocidad esta funcion esta escrita en lenguaje C, y no en la porqueria de R o Python

cppFunction('NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde ) 
{
  /* Aqui se cargan los valores para la regresion */
  double  x[100] ;
  double  y[100] ;

  int n = pcolumna.size();
  NumericVector out( 5*n );

  for(int i = 0; i < n; i++)
  {
    //lag
    if( pdesde[i]-1 < i )  out[ i + 4*n ]  =  pcolumna[i-1] ;
    else                   out[ i + 4*n ]  =  NA_REAL ;


    int  libre    = 0 ;
    int  xvalor   = 1 ;

    for( int j= pdesde[i]-1;  j<=i; j++ )
    {
       double a = pcolumna[j] ;

       if( !R_IsNA( a ) ) 
       {
          y[ libre ]= a ;
          x[ libre ]= xvalor ;
          libre++ ;
       }

       xvalor++ ;
    }

    /* Si hay al menos dos valores */
    if( libre > 1 )
    {
      double  xsum  = x[0] ;
      double  ysum  = y[0] ;
      double  xysum = xsum * ysum ;
      double  xxsum = xsum * xsum ;
      double  vmin  = y[0] ;
      double  vmax  = y[0] ;

      for( int h=1; h<libre; h++)
      { 
        xsum  += x[h] ;
        ysum  += y[h] ; 
        xysum += x[h]*y[h] ;
        xxsum += x[h]*x[h] ;

        if( y[h] < vmin )  vmin = y[h] ;
        if( y[h] > vmax )  vmax = y[h] ;
      }

      out[ i ]  =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum) ;
      out[ i + n ]    =  vmin ;
      out[ i + 2*n ]  =  vmax ;
      out[ i + 3*n ]  =  ysum / libre ;
    }
    else
    {
      out[ i       ]  =  NA_REAL ; 
      out[ i + n   ]  =  NA_REAL ;
      out[ i + 2*n ]  =  NA_REAL ;
      out[ i + 3*n ]  =  NA_REAL ;
    }
  }

  return  out;
}')

#------------------------------------------------------------------------------
#calcula la tendencia de las variables cols de los ultimos 6 meses
#la tendencia es la pendiente de la recta que ajusta por cuadrados minimos
#La funcionalidad de ratioavg es autoria de  Daiana Sparta,  UAustral  2021

TendenciaYmuchomas  <- function( dataset, cols, ventana=6, tendencia=TRUE, minimo=TRUE, maximo=TRUE, promedio=TRUE, 
                                 ratioavg=FALSE, ratiomax=FALSE)
{
  gc()
  #Esta es la cantidad de meses que utilizo para la historia
  ventana_regresion  <- ventana

  last  <- nrow( dataset )

  #creo el vector_desde que indica cada ventana
  #de esta forma se acelera el procesamiento ya que lo hago una sola vez
  vector_ids   <- dataset$numero_de_cliente

  vector_desde  <- seq( -ventana_regresion+2,  nrow(dataset)-ventana_regresion+1 )
  vector_desde[ 1:ventana_regresion ]  <-  1

  for( i in 2:last )  if( vector_ids[ i-1 ] !=  vector_ids[ i ] ) {  vector_desde[i] <-  i }
  for( i in 2:last )  if( vector_desde[i] < vector_desde[i-1] )  {  vector_desde[i] <-  vector_desde[i-1] }

  for(  campo  in   cols )
  {
    nueva_col     <- fhistC( dataset[ , get(campo) ], vector_desde ) 

    if(tendencia)  dataset[ , paste0( campo, "_tend", ventana) := nueva_col[ (0*last +1):(1*last) ]  ]
    if(minimo)     dataset[ , paste0( campo, "_min", ventana)  := nueva_col[ (1*last +1):(2*last) ]  ]
    if(maximo)     dataset[ , paste0( campo, "_max", ventana)  := nueva_col[ (2*last +1):(3*last) ]  ]
    if(promedio)   dataset[ , paste0( campo, "_avg", ventana)  := nueva_col[ (3*last +1):(4*last) ]  ]
    if(ratioavg)   dataset[ , paste0( campo, "_ratioavg", ventana)  := get(campo) /nueva_col[ (3*last +1):(4*last) ]  ]
    if(ratiomax)   dataset[ , paste0( campo, "_ratiomax", ventana)  := get(campo) /nueva_col[ (2*last +1):(3*last) ]  ]
  }

}
#------------------------------------------------------------------------------
#agrega al dataset nuevas variables {0,1} que provienen de las hojas de un Random Forest

AgregaVarRandomForest  <- function( num.trees, max.depth, min.node.size, mtry)
{
  gc()
  dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

  campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria" ) )

  dataset_rf  <- copy( dataset[ , campos_buenos, with=FALSE] )
  azar  <- runif( nrow(dataset_rf) )
  dataset_rf[ , entrenamiento := as.integer( foto_mes>= 202009 &  foto_mes<= 202101 & ( clase01==1 | azar < 0.10 )) ]

  #imputo los nulos, ya que ranger no acepta nulos
  #Leo Breiman, ¿por que le temias a los nulos?
  dataset_rf  <- na.roughfix( dataset_rf )

  campos_buenos  <- setdiff( colnames(dataset_rf), c("clase_ternaria","entrenamiento" ) )
  modelo  <- ranger( formula= "clase01 ~ .",
                     data=  dataset_rf[ entrenamiento==1L, campos_buenos, with=FALSE  ] ,
                     classification= TRUE,
                     probability=   FALSE,
                     num.trees=     num.trees,
                     max.depth=     max.depth,
                     min.node.size= min.node.size,
                     mtry=          mtry
                   )

  rfhojas  <- predict( object= modelo, 
                       data= dataset_rf[ , campos_buenos, with=FALSE ],
                       predict.all= TRUE,    #entrega la prediccion de cada arbol
                       type= "terminalNodes" #entrega el numero de NODO el arbol
                     )

  for( arbol in 1:num.trees )
  {
    hojas_arbol  <- unique(  rfhojas$predictions[  , arbol  ] )

    for( pos in 1:length(hojas_arbol) )
    {
      nodo_id  <- hojas_arbol[ pos ]  #el numero de nodo de la hoja, estan salteados
      dataset[  ,  paste0( "rf_", sprintf( "%03d", arbol ), "_", sprintf( "%03d", nodo_id ) ) := 0L ]

      dataset[ which( rfhojas$predictions[ , arbol] == nodo_id ,  ), 
               paste0( "rf_", sprintf( "%03d", arbol ), "_", sprintf( "%03d", nodo_id ) ) := 1L ]
    }
  }

  rm( dataset_rf )
  dataset[ , clase01 := NULL ]

  gc()
}
#------------------------------------------------------------------------------
VPOS_CORTE  <- c()

fganancia_lgbm_meseta  <- function(probs, datos) 
{
  vlabels  <- get_field(datos, "label")
  vpesos   <- get_field(datos, "weight")

  tbl  <- as.data.table( list( "prob"=probs, "gan"= ifelse( vlabels==1 & vpesos > 1, 60000, -2000 ) ) )

  setorder( tbl, -prob )
  tbl[ , posicion := .I ]
  tbl[ , gan_acum :=  cumsum( gan ) ]
  setorder( tbl, -gan_acum )   #voy por la meseta

  gan  <- mean( tbl[ 1:500,  gan_acum] )  #meseta de tamaño 500

  pos_meseta  <- tbl[ 1:500,  median(posicion)]
  VPOS_CORTE  <<- c( VPOS_CORTE, pos_meseta )

  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#Elimina del dataset las variables que estan por debajo de la capa geologica de canaritos
#se llama varias veces, luego de agregar muchas variables nuevas, para ir reduciendo la cantidad de variables
# y así hacer lugar a nuevas variables importantes

GVEZ <- 1 

CanaritosAsesinos  <- function( canaritos_ratio=0.2 )
{
  gc()
  dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

  for( i  in 1:(ncol(dataset)*canaritos_ratio))  dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset))]

  campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", "foto_mes" ) )

  azar  <- runif( nrow(dataset) )
  dataset[ , entrenamiento := foto_mes>= 202008 &  foto_mes<= 202111  & ( clase01==1 | azar < 0.10 ) ]

  dtrain  <- lgb.Dataset( data=    data.matrix(  dataset[ entrenamiento==TRUE, campos_buenos, with=FALSE]),
                          label=   dataset[ entrenamiento==TRUE, clase01],
                          weight=  dataset[ entrenamiento==TRUE, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)],
                          free_raw_data= FALSE
                        )

  dvalid  <- lgb.Dataset( data=    data.matrix(  dataset[ foto_mes==202101, campos_buenos, with=FALSE]),
                          label=   dataset[ foto_mes==202101, clase01],
                          weight=  dataset[ foto_mes==202101, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)],
                          free_raw_data= FALSE
                          )


  param <- list( objective= "binary",
                 metric= "custom",
                 first_metric_only= TRUE,
                 boost_from_average= TRUE,
                 feature_pre_filter= FALSE,
                 verbosity= -100,
                 seed= 999983,
                 max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                 min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                 lambda_l1= 0.0,         #por ahora, lo dejo fijo
                 lambda_l2= 0.0,         #por ahora, lo dejo fijo
                 max_bin= 31,            #por ahora, lo dejo fijo
                 num_iterations= 9999,   #un numero muy grande, lo limita early_stopping_rounds
                 force_row_wise= TRUE,    #para que los alumnos no se atemoricen con tantos warning
                 learning_rate= 0.065, 
                 feature_fraction= 1.0,   #lo seteo en 1 para que las primeras variables del dataset no se vean opacadas
                 min_data_in_leaf= 260,
                 num_leaves= 60,
                 early_stopping_rounds= 200 )

  modelo  <- lgb.train( data= dtrain,
                        valids= list( valid= dvalid ),
                        eval= fganancia_lgbm_meseta,
                        param= param,
                        verbose= -100 )

  tb_importancia  <- lgb.importance( model= modelo )
  tb_importancia[  , pos := .I ]

  fwrite( tb_importancia, 
          file= paste0( "impo_", GVEZ ,".txt"),
          sep= "\t" )

  GVEZ  <<- GVEZ + 1

  umbral  <- tb_importancia[ Feature %like% "canarito", median(pos) + 2*sd(pos) ]  #Atencion corto en la mediana mas DOS desvios!!

  col_utiles  <- tb_importancia[ pos < umbral & !( Feature %like% "canarito"),  Feature ]
  col_utiles  <-  unique( c( col_utiles,  c("numero_de_cliente","foto_mes","clase_ternaria","mes") ) )
  col_inutiles  <- setdiff( colnames(dataset), col_utiles )

  dataset[  ,  (col_inutiles) := NULL ]

}
#------------------------------------------------------------------------------
#agrega para cada columna de cols una nueva variable _rank  que es un numero entre 0 y 1  del ranking de esa variable ese mes

Rankeador  <- function( cols )
{
  gc()
  sufijo  <- "_rank" 

  for( vcol in cols )
  {
     dataset[ , paste0( vcol, sufijo) := frank( get(vcol), ties.method= "random")/ .N, 
                by= foto_mes ]
  }
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

setwd( "~/buckets/b1/" )

#cargo el dataset
dataset  <- fread( "./datasets/competencia1_historia_2022.csv.gz" )

#Elimino los campos problematicos
dataset[  , internet := NULL ]

#creo la carpeta donde va el experimento
# FE  representa  Feature Engineering
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/FE8150/", showWarnings = FALSE )
setwd("./exp/FE8150/")   #Establezco el Working Directory DEL EXPERIMENTO



#corrijo los  < foto_mes, campo >  que fueron pisados con cero
# Atencion, cambiar si se desea el metodo VelenPennini
Corregir_MachineLearning( dataset )  
# Corregir_VelenPennini( dataset )

Ajuste_Inflacion(dataset) 


#Agrego las variables manuales al dataset
AgregarVariables( dataset )


#--------------------------------------
#estas son las columnas a las que se puede agregar lags o media moviles ( todas menos las obvias )
cols_lagueables  <- copy(  setdiff( colnames(dataset), c("numero_de_cliente", "foto_mes", "clase_ternaria")  ) )

#ordeno el dataset por <numero_de_cliente, foto_mes> para poder hacer lags
#  es MUY  importante esta linea
setorder( dataset, numero_de_cliente, foto_mes )

#------------------------------------------------------------------------------
#LAG y delta LAG 1 corte por canaritos
#------------------------------------------------------------------------------

#creo los campos lags de orden 1
dataset[ , paste0( cols_lagueables, "_lag1") := shift(.SD, 1, NA, "lag"), 
           by= numero_de_cliente, 
           .SDcols= cols_lagueables ]

#agrego los delta lags
for( vcol in cols_lagueables )
{
  dataset[ , paste0(vcol, "_delta1") := get(vcol)  - get(paste0( vcol, "_lag1"))  ]
}

#------------------------------------------------------------------------------
#LAG y delta LAG 2 corte por canaritos
#------------------------------------------------------------------------------
#creo los campos lags de orden 2
dataset[ , paste0( cols_lagueables, "_lag2") := shift(.SD, 2, NA, "lag"), 
           by= numero_de_cliente, 
           .SDcols= cols_lagueables ]

#agrego los delta lags
for( vcol in cols_lagueables )
{
  dataset[ , paste0(vcol, "_delta2") := get(vcol)  - get(paste0( vcol, "_lag2"))  ]
}



#------------------------------------------------------------------------------
#LAG y delta LAG 3 corte por canaritos
#------------------------------------------------------------------------------
#creo los campos lags de orden 3
dataset[ , paste0( cols_lagueables, "_lag3") := shift(.SD, 3, NA, "lag"), 
           by= numero_de_cliente, 
           .SDcols= cols_lagueables ]

#agrego los delta lags
for( vcol in cols_lagueables )
{
  dataset[ , paste0(vcol, "_delta3") := get(vcol)  - get(paste0( vcol, "_lag3"))  ]
}


#------------------------------------------------------------------------------
#LAG y delta LAG 4 corte por canaritos
#------------------------------------------------------------------------------
#creo los campos lags de orden 4
dataset[ , paste0( cols_lagueables, "_lag4") := shift(.SD, 4, NA, "lag"), 
           by= numero_de_cliente, 
           .SDcols= cols_lagueables ]

#agrego los delta lags
for( vcol in cols_lagueables )
{
  dataset[ , paste0(vcol, "_delta4") := get(vcol)  - get(paste0( vcol, "_lag4"))  ]
}


#------------------------------------------------------------------------------
#LAG y delta LAG 5 corte por canaritos
#------------------------------------------------------------------------------
#creo los campos lags de orden 5
dataset[ , paste0( cols_lagueables, "_lag5") := shift(.SD, 5, NA, "lag"), 
           by= numero_de_cliente, 
           .SDcols= cols_lagueables ]

#agrego los delta lags
for( vcol in cols_lagueables )
{
  dataset[ , paste0(vcol, "_delta5") := get(vcol)  - get(paste0( vcol, "_lag5"))  ]
}


#------------------------------------------------------------------------------
#LAG y delta LAG 6 corte por canaritos
#------------------------------------------------------------------------------
#creo los campos lags de orden 6
dataset[ , paste0( cols_lagueables, "_lag6") := shift(.SD, 6, NA, "lag"), 
           by= numero_de_cliente, 
           .SDcols= cols_lagueables ]

#agrego los delta lags
for( vcol in cols_lagueables )
{
  dataset[ , paste0(vcol, "_delta6") := get(vcol)  - get(paste0( vcol, "_lag6"))  ]
}


#agrego las tendencias

TendenciaYmuchomas( dataset, 
                    cols= cols_lagueables,
                    ventana=   6,      # 6 meses de historia
                    tendencia= TRUE,
                    minimo=    FALSE,
                    maximo=    FALSE,
                    promedio=  TRUE,
                    ratioavg=  FALSE,
                    ratiomax=  FALSE  )


#------------------------------------------------------------------------------
#Elimino las variables que no son tan importantes en el dataset

ncol( dataset )
CanaritosAsesinos( canaritos_ratio = 0.3 )
ncol( dataset )
#------------------------------------------------------------------------------

#Agrego variables a partir de las hojas de un Random Forest

AgregaVarRandomForest( num.trees = 40,
                       max.depth = 5,
                       min.node.size = 500,
                       mtry = 15 )

gc()

#------------------------------------------------------------------------------


#grabo el dataset
fwrite( dataset,
        "dataset_7130_Canaritos_LAGS6_v2_canaritos_al_final.csv.gz",
        logical01= TRUE,
        sep= "," )
