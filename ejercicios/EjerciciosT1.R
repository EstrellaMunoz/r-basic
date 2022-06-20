
opBasic = function(a,b){
  print("Suma")
  print(a+b)
  print("Resta")
  print(paste(sprintf("%i - %i = ",a,b),a-b))
  print(paste(sprintf("%i - %i = ",b,a),b-a))
  print("Producto")
  print(a*b)
  print("Cociente de la división entera")
  print(paste(sprintf("%i : %i = ",a,b),a%/%b))
  print(paste("con resto ",a%%b))
  print("Cociente de la división entera")
  print(paste(sprintf("%i : %i = ",b,a),b%/%a))
  print(paste("con resto ",b%%a))
}


sqrt(-5)
sqrt(as.complex(-5))

# Pregunta 1
# Si hubiéramos empezado a contar segundos a partir de las 12 campanadas que 
# marcan el inicio de 2018, ¿a
# qué hora de qué día de qué año llegaríamos a los 250 millones de segundos? 
# ¡Cuidado con los años bisiestos!

# Creo una función general donde el primer parámetro es el primer año a tener en 
# cuenta y el segundo parámetro son los segundos a tener en cuenta

# Año bisiesto = 31.622.400 segundos
# Año no bisiesto = 31.536.000 segundos
# Segundos día = 86.400 segundos

# Creo una función genérica que sirva para resolver el problema para cualquier
# año de partida y cualquier número de segundos.

calcularTiempo = function(ano_dado, tiempo_segundos){
  
  resto = tiempo_segundos
  
  #segundos
  segundos = resto%%60
  # Ahora el resto son minutos
  resto <- resto%/%60
  
  # minutos
  minutos = resto%%60
  # Ahora el resto son horas
  resto <- resto%/%60
  
  # horas
  hora = resto%%24
  # Ahora el resto son días
  resto <- resto%/%24
 
  salirAno = FALSE
  ano = ano_dado
  # Determino el año
  while (salirAno == FALSE){
    if (ano%%4 == 0){
      if(resto>=366){
        ano = ano + 1
        resto = resto - 366
        if (resto == 0){
          salirAno = TRUE
        }
      } else {
        salirAno = TRUE
      }
    } else {
      if (resto>=365){
        ano = ano +1
        resto = resto - 365
        if (resto == 0){
          salirAno = TRUE
        }
      } else {
        salirAno = TRUE
      }
    }
  }
  
  # El resto contiene ahora en número de días del año corriente, pero son días
  # completos, por lo que "el día" en el que estamos es un día más...
  #dias = resto
  
  dia = resto+1
  
  # Distingo si el año corriente es bisiesto o no, para determinar el mes y el día

  
  if(ano%%4 == 0){
    if (dia<=31) {
      mes = "Enero"
    } else {
      if (dia<=60) {
        mes = "Febrero"
        dia = dia-31
      } else {
        if (dia<=91) {
          mes = "Marzo"
          dia = dia-60
        } else {
          if (dia<=121) {
            mes = "Abril"
            dia = dia-91
          } else {
            if (dia<=152) {
              mes = "Mayo"
              dia = dia-120
            } else {
              if (dia<=182) {
                mes = "Junio"
                dia = dia-152
              } else {
                if (dia<=213) {
                  mes = "Julio"
                  dia = dia-182
                } else {
                  if (dia<=244) {
                    mes = "Agosto"
                    dia = dia-213
                  } else {
                    if (dia<=274) {
                      mes = "Septiembre"
                      dia = dia-244
                    } else {
                      if (dia<=305) {
                        mes = "Octubre"
                        dia = dia-274
                      } else {
                        if (dia<=335) {
                          mes = "Noviembre"
                          dia = dia-305
                        } else {
                          if (dia<=366) {
                            mes = "Diciembre"
                            dia = dia-335
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }

  } else {
    if (dia<=31) {
      mes = "Enero"
    } else {
      if (dia<=59) {
        mes = "Febrero"
        dia = dia-31
      } else {
        if (dia<=90) {
          mes = "Marzo"
          dia = dia-59
        } else {
          if (dia<=120) {
            mes = "Abril"
            dia = dia-90
          } else {
            if (dia<=151) {
              mes = "Mayo"
              dia = dia-120
            } else {
              if (dia<=181) {
                mes = "Junio"
                dia = dia-151
              } else {
                if (dia<=212) {
                  mes = "Julio"
                  dia = dia-181
                } else {
                  if (dia<=243) {
                    mes = "Agosto"
                    dia = dia-212
                  } else {
                    if (dia<=273) {
                      mes = "Septiembre"
                      dia = dia-243
                    } else {
                      if (dia<=304) {
                        mes = "Octubre"
                        dia = dia-273
                      } else {
                        if (dia<=334) {
                          mes = "Noviembre"
                          dia = dia-304
                        } else {
                          if (dia<=365) {
                            mes = "Diciembre"
                            dia = dia-334
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  
  print ("RESULTADO:")
  print(paste("Año: ", ano))
  print(paste("Mes: ", mes))
  print(paste("Día: ", dia))
  print(paste("Hora: ", hora))
  print(paste("Minutos: ", minutos))
  print(paste("Segundos: ", segundos))
}

calcularTiempo(2018, 250000000)
calcularTiempo(2017, 250000000)
calcularTiempo(2000, 25000000)
calcularTiempo(2000, 25000025)
calcularTiempo(2000, 1)
calcularTiempo(2000, 0)
calcularTiempo(2000, 2678400)
  
# Pregunta 2
# Crea una función en R que resuelva una ecuación de primer grado (de la forma 
# Ax+B=0). Es decir, los parámetros deben ser los coeficientes (en orden) y la 
# función tiene que devolver la solución. Por ejemplo, si la ecuación es 2x+4=0,
# la función tendría que devolver -2.  
  
# x = (c-b)/a

ec_primer_grado = function(a,b,c=0){

  print("ECUACIÓN:")
  print(paste(a,"x + ", b, " = ", c ))
  print("RESULTADO")
  print (paste("x = ", (c-b)/a))
}

ec_primer_grado (2,4)
ec_primer_grado(5,3)
ec_primer_grado(7,4,18)
ec_primer_grado(1,1,1)

# Pregunta 3
# Da una expresión para calcular 3e-π con R y a continuación, da el resultado 
# obtenido redondeado a 3 cifras decimales.

round(3*exp(1)-pi, 3)

# Pregunta 4
# Da una expresión para calcular el módulo del número complejo (2+3i)^2/(5+8i) y, 
#a continuación, da el resultado obtenido redondeado a 3 cifras decimales.

round(Mod((2+3i)^2/(5+8i)), 3)






