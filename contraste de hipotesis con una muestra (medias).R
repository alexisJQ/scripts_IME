library(ggpubr)

# El art�culo "Automatic Segmentation of Medical Images Using Image
# Registration: Diagnostic and Simulation Applications" (Journal of
# Medical Engeeniering and Technology 2005) propuso una nueva t�cnica
# para la identificaci�n autom�tica de los bordes de estructuras
# significativas en una imagen m�dica utilizando desplazamiento lineal
# promedio (ALD, por sus siglas en ingl�s).
# El art�culo dio las siguientes observaciones de ADL con una muestra
# de 49 ri�ones (en pixeles y usando punto en vez de coma decimal).

muestra <- c(1.38, 0.98, 1.09, 0.77, 0.66, 1.28, 0.61, 1.49, 0.81, 0.36,
             0.84, 0.83, 0.61, 0.64, 1.30, 0.57, 0.43, 0.62, 1.00, 1.05,
             0.82, 1.10, 0.65, 0.99, 0.56, 0.66, 0.64, 1.45, 0.82, 1.06,
             0.41, 0.58, 0.66, 1.14, 0.73, 0.59, 0.51, 1.04, 0.85, 0.45,
             0.82, 1.01, 1.11, 0.34, 1.25, 0.38, 1.44, 1.28, 0.91
)

# Los autores comentaron que el ALD medio estar�a alrededor de 1.0.
# �Los datos soportan esta afirmaci�n?

cat("\n\n")
cat("Identificandio bordes en im�genes\n")
cat("=================================\n")

# Dado que el tama�o de la muestra es grande, deber�amos intentar
# responder esta pregunta con un test Z, siempre y cuando se cumplan
# las condiciones: (OpenIntro Statistics p�g. 178)
# 1) The sample observations are independent.
# 2) The sample size is large: n ??? 30 is a good rule of thumb.
# 3) The population distribution is not strongly skewed. 
#    This condition can be difficult to evaluate, so just use your best
#    judgement. Additionally, the larger the sample size, the more
#    lenient we can be with the sample's skew.

# Condici�n 1: No nos dicen mucho de c�mo se seleccionaron las 49
# im�genes. Pero como se trata de un art�culo cient�fico, publicado en
# un journal serio, podemos suponer que los autores del art�culo se
# preocuparon de obtener una muestra de observaciones aleatorias e
# independientes.
# 
# Condici�n 2: Se cumple, puesto que tenemos 49 > 30 ejemplos.
# 
# Condici�n 3: Tenemos el problema de que no conocemos la poblaci�n !!!
# Podemos ver el comportamiento de la muestra usando un histograma y
# un gr�fico cuantil-cuantil (QQ plot).:

d <- data.frame(ADL = muestra)

p1.1 <- gghistogram(
  d, x = "ADL",
  binwidth = 0.15,
  color = "#6D9EC1",
  fill = "#BFD5E3"
)


p1.2 <- ggqqplot(
  d, x = "ADL",
  color = "#6D9EC1",
  fill = "#BFD5E3"
)

# La muestra no sigue una distribuci�n completamente sim�trica, tampoco
# parece absurdo suponer que la poblaci�n de la que proviene pueda ser
# m�s bien sim�trica. Luego, podemo seguir adelante con una prueba Z,
# aunque podemos ser un poco m�s exigente con el ?? que usaremos (m�s
# peque�o).

?? <- 0.025

# Ahora debemos plantear hip�tesis que permitan responder la
# pregunta planteada: Los datos �respaldan la idea de que el ADL medio
# est� alrededor de 1.0 [1/pixeles]?
# 
# Esta pregunta es sobre el par�metro ?? de la poblaci�n desconocida.
# Las hip�tesis ser�an:
# H0: ?? = 1  [1/pixeles]
# HA: ?? <> 1 [1/pixeles]

?? <- 1
tama�o.muestra <- length(muestra)
media.muestra <- mean(muestra)
desvest.muestra <- sd(muestra)

cat("\n")
cat("H0: ?? =", ??, "[1/pixeles]\n")
cat("HA: ?? <>", ??, "[1/pixeles]\n")
cat("Promedio muestral:", media.muestra, "[1/pixeles]\n")
cat("Desviaci�n est�ndar muestral:", desvest.muestra, "[1/pixeles]\n")
cat("Tama�o de la muestra:", tama�o.muestra, "[im�genes]\n")

# Obtenemos el estad�stidico y la probabilidad de ver este valor si H0
# fuera verdadero usando la distribuci�n normal est�ndar, considerando
# ambas colas.

z <- (media.muestra - ??) / (desvest.muestra / sqrt(tama�o.muestra))
p.valor <- pnorm(z)
# Pero multiplicamos esta probabilidad por 2, por tratarse de una
# prueba de 2 colas.
p.valor <- 2 * p.valor
# El est�ndar APA indica que deber�amos reportar hasta 3 decimales
p <- round(p.valor, 3)

cat("\n\n")
cat("Z test para ADL medio\n")
cat("---------------------\n")
cat("\n")
cat("Estad�stico z:", round(z, 3), "\n")
cat("Nivel de significaci�n:", ??, "\n")
if(p > 0) cat("p-valor:", p, "\n") else cat("p-valor: < .001\n")
if(p >= ??) {
  cat("Resultado: No hay suficiente evidencia para rechazar H0\n")
} else {
  cat("Resultado: Hay suficiente evidencia para rechazar H0 y aceptar HA\n")
}

# As�, los autores del art�culo no podr�an estar equivocados y los datos
# no sugieren que el ADL medio sea alrededor de 1.0 [1/pixeles]


# stop("*** Parada intermedia ***")


# Por otro lado, existe una funci�n para realizar un test Z en el
# paquete 'TeachingDemos'
library(TeachingDemos)

ztest1 <- z.test(muestra, mu = ??, sd = desvest.muestra,
                 alternative = "two.sided", conf.level = 1 - ??)

cat("\n\n")
cat("Z test implementada en biblioteca TeachingDemos\n")
cat("-----------------------------------------------\n")
print(ztest1)

# Esta funci�n devuelve un objeto de clase htest (como en muchas pruebas
# estad�sticas en R). Podemos ver su estructura, para irnos familiari-
# zando:
cat("\n")
print(str(ztest1))

# Notemos que se incluye el intervalo de 100(1 - ??)% confianza
cat("\n")
print(ztest1[["conf.int"]])

# Este intervalo, de forma consistente con el p-valor, no incluye el
# ?? = 1 teorizado.

# stop("*** Parada intermedia ***")




# ----------------------------------------------------------------



# El art�culo "An Introduction to Some Precision and Accuracy of 
# Measurement Problems" (Journal of Testing and Evaluation 1982)
# estudi� si la ropa de dormir de ni�os cumpl�a la norma que se�ala
# que el tiempo de permanencia de llamas ha de ser a lo m�s 9,75 s.
# Con una muestra de tiras de ropa tratada observaron los siguientes
# tiempos de permanencia de llamas (en segundos, usando punto en vez
# de coma decimal):

muestra <- c(9.85, 9.83, 9.94, 9.93, 9.88, 9.67, 9.93, 9.92, 9.85, 9.92,
             9.95, 9.87, 9.75, 9.74, 9.75, 9.89, 9.95, 9.67, 9.77, 9.99
)

# �A qu� conclusi�n deber�an haber llegado los autores del art�culo?

cat("\n\n")
cat("Pijamas seguros\n")
cat("===============\n")


# Como tenemos una muestra mediana, una prueba apropiada podr�a ser la
# T de Student. Veamos si se cumplen las condiciones:
# (OpenIntro Statistics pag. 223)
# 1) Observaciones son independientes
# 2) Observaciones tienen aproximadamente una distribuci�n normal

# Condici�n 1: Nuevamente solo nos queda presumir que los investigadores
# hicieron el trabajo bien y escogieron una muestra sin sesgos mezquinos.
# Tambi�n podemos suponer con seguridad que una muestra de tama�o 20,
# como en este caso, no llega al 10% de todos los pijamas infantiles en
# el mercado, por lo que no habr�a problemas por este lado.
# 
# Condici�n 2: el mismo texto nos sugiere:
# "This second condition is difficult to verify with small data sets.
# We often (i) take a look at a plot of the data for obvious departures
# from the normal model, and (ii) consider whether any previous
# experiences alert us that the data may not be nearly normal"
# 
# Hagamos eso:

d <- data.frame(Permanecia = muestra)

p2.1 <- gghistogram(
  d, x = "Permanecia",
  binwidth = 0.05,
  color = "#6D9EC1",
  fill = "#BFD5E3"
)


p2.2 <- ggqqplot(
  d, x = "Permanecia",
  color = "#6D9EC1",
  fill = "#BFD5E3"
)

# stop("*** Parada intermedia ***")


# Parece bastante en el l�mite la normalidad de la distribuci�n de las
# observaciones. Podemos proceder con el test T de Student, pero siendo
# bastante exigente con el nivel de confianza:

?? <- 0.01

# Ahora, nuestras hip�tesis:
# Si la ropa de dormir cumple la norma, el tiempo de permanencia 
# promedio debe ser menor a 9,75 segundos; si es mayor, debemos
# rechazar la idea de que la ropa cumple dicha norma. As�:
# H0: ?? = 9,75 s
# HA: ?? > 9,75 s

?? <- 9.75
tama�o.muestra <- length(muestra)
grados.de.libertad <- tama�o.muestra - 1
media.muestra <- mean(muestra)
desvest.muestra <- sd(muestra)

cat("\n")
cat("H0: ?? =", ??, "[s]\n")
cat("HA: ?? >", ??, "[s]\n")
cat("Promedio muestral:", media.muestra, "[s]\n")
cat("Desviaci�n est�ndar muestral:", desvest.muestra, "[s]\n")
cat("Tama�o de la muestra:", tama�o.muestra, "[pijamas]\n")
cat("\n")

# Obtenemos el estad�stidico y la probabilidad de ver este valor si H0
# fuera verdadero usando la distribuci�n T de Student, considerando
# una lado solamente.

t <- (media.muestra - ??) / (desvest.muestra / sqrt(tama�o.muestra))

p.valor <- ifelse(t < 0, pt(t, grados.de.libertad),
                  pt(t, grados.de.libertad, lower.tail = FALSE)
)
p <- round(p.valor, 3)

cat("\n\n")
cat("Student T test para tiempo medio de permanencia de llamas\n")
cat("---------------------------------------------------------\n")
cat("\n")
cat("Estad�stico t:", t, "\n")
cat("Nivel de significaci�n:", ??, "\n")
if(p > 0) cat("p-valor:", p, "\n") else cat("p-valor: < .001\n")
if(p >= ??) {
  cat("Resultado: No hay suficiente evidencia para rechazar H0\n")
} else {
  cat("Resultado: Hay suficiente evidencia para rechazar H0 y aceptar HA\n")
}
cat("\n")

# La funci�n t.test del "core" de R nos permite hacer esta prueba y
# obtener un intervalo de confianza f�cilmente (retorna un objeto htest).

ttest1 <- t.test(muestra, mu = ??,
                 alternative = "greater", conf.level = 1 - ??)

cat("\n\n")
cat("T test de Student de R\n")
cat("----------------------\n")
print(ttest1)

# El p-valor es muy bajo, por lo que hay una fuerte evidencia para
# rechazar H0.

# Notemos que se el intervalo de confianza obtenido dice que la
# verdadera media del tiempo de permanencia de las llamas es a lo menos
# 9,80 segundos:

cat("\n")
print(ttest1[["conf.int"]])

# Conclusi�n: la ropa de dormir de ni�os no pareciera estar cumpliendo
# la norma de seguridad anti llamas.

# stop("*** Parada intermedia ***")



# ----------------------------------------------------------------



# El art�culo "Engineering Properties of Soil" (Soil Science 1998) puso
# a prueba la idea generalizada de que el 3% del suelo corresponde a
# materia org�nica. Para esto, los autores obtuvieron una muestra
# aleatoria de espec�menes de suelo, determinando que la cantidad (%)
# de materia org�nica presente en cada esp�cimen era (usando punto en
# vez de coma decimal):

muestra <- c(3.10, 4.09, 2.97, 2.19, 2.60, 3.32, 0.55, 1.45, 0.14, 2.47,
             0.80, 3.50, 5.02, 4.67, 4.02, 2.69, 3.98, 3.17, 3.03, 2.21,
             3.69, 4.47, 3.31, 2.17, 2.76, 1.17, 1.57, 2.62, 1.66, 2.05
)

# �Qu� conclusi�n sugerir�a a los autores?

cat("\n\n")
cat("Materia org�nica en muestras de suelo\n")
cat("=====================================\n")


# Nuevamente el tama�o de la muestra es grande, por lo que deber�amos
# considerar un test Z, siempre y cuando se cumplan las condiciones:
# (OpenIntro Statistics p�g. 178)
# 
# Condici�n 1: Otra vez se trata de un art�culo cient�fico, publicado en
# un journal serio. Suponemos entonces que los autores del art�culo se
# preocuparon de obtener una muestra adecuada.
# 
# Condici�n 2: Est� en el l�mite de las 30 observaciones. Debemos 
# proceder con cautela.
# 
# Condici�n 3: Veamos el comportamiento de la muestra usando gr�ficos
# para ver si podemos suponer razonablemente que proviene de una 
# poblaci�n normal.


media.muestra <- mean(muestra)
desvest.muestra <- sd(muestra)

d <- data.frame(Pcto.Materia.org = muestra)

p3.1 <- gghistogram(
  d, x = "Pcto.Materia.org",
  binwidth = 0.4,
  color = "#6D9EC1",
  fill = "#BFD5E3"
)

p3.2 <- ggqqplot(
  d, x = "Pcto.Materia.org",
  color = "#6D9EC1",
  fill = "#BFD5E3"
)


# stop("*** Parada intermedia ***")


# Tiene un comportamiento bastante bueno. Por lo que podemos proceder
# con el test Z.
# Pero, debemos saber que esto no se da siempre. Lo usual es que
# porcentajes no formen distribuciones con comportameinto normal.
# Existe una transformaci�n bastante usada, que los convierten en
# una distribuci�n de �ngulos aproximadamente normal. Para tener en
# cuenta en otras ocasiones.
#
# Si bien podemos estar m�s o menos tranquilos en este caso, seamos
# cautos por el tama�o de la muestra:

?? <- 0.04

# Ahora debemos plantear hip�tesis que permitan responder la duda
# planteada en el problema: Los datos �respaldan la idea generalizada
# de que el 3% del suelo corresponde a materia org�nica?
# 
# Suponiendo una poblaci�n de porcentajes, pregunta es su par�metro ?? 
# hipot�tico. Las hip�tesis ser�an:
# H0: ?? = 3  [%]
# HA: ?? <> 3 [%]

?? <- 3
tama�o.muestra <- length(muestra)
media.muestra <- mean(muestra)
desvest.muestra <- sd(muestra)

cat("\n")
cat("H0: ?? =", ??, "[%]\n")
cat("HA: ?? <>", ??, "[%]\n")
cat("Promedio muestral:", media.muestra, "[%]\n")
cat("Desviaci�n est�ndar muestral:", desvest.muestra, "[%]\n")
cat("Tama�o de la muestra:", tama�o.muestra, "[muestras]\n")
cat("\n")

# Ahora podemos obtener el test Z

ztest2 <- z.test(muestra, mu = ??, sd = desvest.muestra,
                 alternative = "two.sided", conf.level = 1 - ??)
z <- ztest2[["statistic"]]
ci <- paste(round(ztest2[["conf.int"]], 3), collapse = ", ")
p.valor <- ztest2[["p.value"]]
p <- round(p.valor, 3)

cat("\n\n")
cat("Z test para el porcentaje de materia org�nica en el suelo\n")
cat("---------------------------------------------------------\n")
cat("\n")
cat("Estad�stico z:", round(z, 3), "\n")
cat("Nivel de significaci�n:", ??, "\n")
if(p > 0) cat("p-valor:", p, "\n") else cat("p-valor: < .001\n")
cat("Intervalo de confianza: [", ci, "]\n", sep = "")
if(p >= ??) {
  cat("Resultado: No hay suficiente evidencia para rechazar H0\n")
} else {
  cat("Resultado: Hay suficiente evidencia para rechazar H0 y aceptar HA\n")
}
cat("\n")

# Ya que estamos trabajando con el tama�o de muestra m�nimo para
# considerar una prueba Z, no es descabellado haber usado un test T de
# Student.

ttest2 <- t.test(muestra, mu = ??, conf.level = 1 - ??)

t <- ttest2[["statistic"]]
ci <- paste(round(ttest2[["conf.int"]], 3), collapse = ", ")
p.valor <- ttest2[["p.value"]]
p <- round(p.valor, 3)

cat("\n")
cat("T test de Student para el porcentaje de materia org�nica en el suelo\n")
cat("--------------------------------------------------------------------\n")
cat("\n")
cat("Estad�stico t:", round(t, 3), "\n")
cat("Nivel de significaci�n:", ??, "\n")
if(p > 0) cat("p-valor:", p, "\n") else cat("p-valor: < .001\n")
cat("Intervalo de confianza: [", ci, "]\n", sep = "")
if(p >= ??) {
  cat("Resultado: No hay suficiente evidencia para rechazar H0\n")
} else {
  cat("Resultado: Hay suficiente evidencia para rechazar H0 y aceptar HA\n")
}
cat("\n")

# Conclusi�n: Pareciera cumplirse la hip�tesis generalizada que el
# porcentaje de materia org�nica en el suelo est� en torno al 3%


# stop("*** Parada intermedia ***")



# ----------------------------------------------------------------



# Un laboratorio que fabrica aspirina en Chile llena los frascos por
# peso en lugar de por conteo. Cada frasco contiene 30 tabletas si es
# que se cumple el supuesto de que el peso promedio de las tableta es
# de 5 gramos. Se obtuvo la siguiente muestra de 100 tabletas:

muestra <- c(4.62, 4.43, 5.18, 4.89, 4.89, 5.41, 4.87, 5.07, 5.30, 4.98,
             4.54, 5.21, 4.60, 4.71, 4.58, 4.99, 5.05, 4.70, 4.63, 4.95,
             4.85, 4.19, 5.25, 4.69, 5.03, 4.74, 4.67, 4.85, 4.45, 4.93,
             4.42, 4.40, 5.59, 4.69, 5.42, 5.19, 4.99, 4.88, 4.03, 5.51,
             4.90, 4.43, 4.93, 4.84, 4.73, 4.89, 4.53, 4.97, 5.10, 5.95,
             4.95, 4.18, 4.91, 4.87, 5.38, 5.49, 4.96, 4.76, 4.76, 4.63,
             5.10, 4.84, 4.87, 4.39, 4.99, 5.03, 4.31, 5.05, 4.71, 4.78,
             4.90, 5.02, 4.84, 5.18, 4.79, 4.99, 4.55, 4.70, 4.74, 4.60,
             4.94, 5.25, 5.01, 4.95, 4.19, 5.27, 5.00, 5.15, 5.12, 4.34,
             4.27, 4.92, 4.98, 4.91, 5.05, 5.28, 4.29, 5.58, 5.55, 4.60
)

# �Proporciona esta informaci�n una fuerte evidencia para concluir que
# la compa��a no est� llenando sus frascos como lo anuncia?

cat("\n\n")
cat("Fabricando aspirinas\n")
cat("====================\n")

# Aqu� nuevamente una muestra grande, por lo que deber�amos inclinarnos
# a responder esta pregunta con un test Z. Revisemos las condiciones:
# 
# Condici�n 1: Nuevamente, mientras no sepamos la metodolog�a usada para
# obtener la muestra, solo nos queda suponer que el estudio fue �tico y
# se consigui� una muestra de observaciones aleatorias e independientes.
# 
# Condici�n 2: Se cumple, puesto que tenemos 100 > 30 ejemplos.
# 
# Condici�n 3: Usemos un histograma y un gr�fico cuantil-cuantil como
# hemos hecho anteriormente:

d <- data.frame(Peso.tableta = muestra)

p4.1 <- gghistogram(
  d, x = "Peso.tableta",
  binwidth = 0.25,
  color = "#6D9EC1",
  fill = "#BFD5E3"
)

p4.2 <- ggqqplot(
  d, x = "Peso.tableta",
  color = "#6D9EC1",
  fill = "#BFD5E3"
)


# stop("*** Parada intermedia ***")


# Bastante bien portados los datos recogidos. Podemos proceder con un
# test Z.

?? <- 0.05

# Para responder la pregunta del enunciado: �hay suficiente evidencia
# para decir que los frascos no tienen 30 tabletas?
# Esto ocurrir�a si el peso medio de las tabletas (todas las tabletas =
# poblaci�n) difiere de 5 g. Luego las hip�tesis ser�an:
# H0: ?? = 5  [g]
# HA: ?? <> 5 [g]

?? <- 5
tama�o.muestra <- length(muestra)
media.muestra <- mean(muestra)
desvest.muestra <- sd(muestra)

cat("\n")
cat("H0: ?? =", ??, "[g]\n")
cat("HA: ?? <>", ??, "[g]\n")
cat("Promedio muestral:", media.muestra, "[g]\n")
cat("Desviaci�n est�ndar muestral:", desvest.muestra, "[g]\n")
cat("Tama�o de la muestra:", tama�o.muestra, "[tabletas]\n")
cat("\n")

# Ahora ejecutemos el test Z

ztest3 <- z.test(muestra, mu = ??, sd = desvest.muestra,
                 alternative = "two.sided", conf.level = 1 - ??)
z <- ztest3[["statistic"]]
ci <- paste(round(ztest3[["conf.int"]], 3), collapse = ", ")
p.valor <- ztest3[["p.value"]]
p <- round(p.valor, 3)

cat("\n\n")
cat("Z test para el peso de las tabletas\n")
cat("-----------------------------------\n")
cat("\n")
cat("Estad�stico z:", round(z, 3), "\n")
cat("Nivel de significaci�n:", ??, "\n")
if(p > 0) cat("p-valor:", p, "\n") else cat("p-valor: < .001\n")
cat("Intervalo de confianza: [", ci, "]\n", sep = "")
if(p >= ??) {
  cat("Resultado: No hay suficiente evidencia para rechazar H0\n")
} else {
  cat("Resultado: Hay suficiente evidencia para rechazar H0 y aceptar HA\n")
}
cat("\n")

# �Qu� pasar�a si usamos el test T de Student?
# Con una muestra de este tama�o, es probable que se tengan resultados
# similares.

ttest3 <- t.test(muestra, mu = ??, conf.level = 1 - ??)

t <- ttest3[["statistic"]]
ci <- paste(round(ttest3[["conf.int"]], 3), collapse = ", ")
p.valor <- ttest3[["p.value"]]
p <- round(p.valor, 3)

cat("\n")
cat("T test de Student para el peso de las tabletas\n")
cat("----------------------------------------------\n")
cat("\n")
cat("Estad�stico t:", round(t, 3), "\n")
cat("Nivel de significaci�n:", ??, "\n")
if(p > 0) cat("p-valor:", p, "\n") else cat("p-valor: < .001\n")
cat("Intervalo de confianza: [", ci, "]\n", sep = "")
if(p >= ??) {
  cat("Resultado: No hay suficiente evidencia para rechazar H0\n")
} else {
  cat("Resultado: Hay suficiente evidencia para rechazar H0 y aceptar HA\n")
}
cat("\n")

# Conclusi�n: Es probable que los frascos contengan m�s de 30 tabletas,
# ya que estas en promedio pesan menos de los 5 g hipotizados.


# stop("*** Parada intermedia ***")



# ----------------------------------------------------------------



# Un estudio, encargado por bomberos, investig� si los sistemas
# aspersores de prevenci�n, que deben ser instalados en los edificios
# de m�s de 4 pisos construidos despu�s de 2001, cumplen con la norma
# que les obliga a que el tiempo promedio de activaci�n no sobrepase los
# 25 s. Con una serie de pruebas obtuvieron la siguiente muestra:

muestra <- c(27, 41, 22, 27, 23, 35, 30, 33, 24, 27, 28, 22, 24)

# El estudio sugiere que la norma no se est� cumpliendo.
# �Sugieren los datos esta conclusi�n?


cat("\n\n")
cat("Sistemas aspersores de prevenci�n de encendios\n")
cat("==============================================\n")


# Como tenemos una muestra m�s bien reducida, solo podemos considerar
# una prueba T de Student. Veamos si se cumplen las condiciones:
# (OpenIntro Statistics pag. 223)
# 
# Condici�n 1: Nuevamente vamos a suponer que los investigadores
# hicieron bien el trabajo y consiguieron una muestra sin sesgos.
# 
# Condici�n 2:  Veamos los gr�ficos:

d <- data.frame(Tiempo.act = muestra)

p5.1 <- gghistogram(
  d, x = "Tiempo.act",
  binwidth = 5,
  color = "#6D9EC1",
  fill = "#BFD5E3"
)

p5.2 <- ggqqplot(
  d, x = "Tiempo.act",
  color = "#6D9EC1",
  fill = "#BFD5E3"
)


# stop("*** Parada intermedia ***")


# Otra vez los datos no parecen seguir muy bien una distribuci�n normal,
# aunque con una muestra tan peque�a, tampoco es impensable que esto sea
# as�. Podemos proceder con el test T de Student, pero siendo exigente
# con el nivel de confianza:

?? <- 0.01

# Ahora, nuestras hip�tesis para la questi�n planteada: Si la norma no
# se est� cumpliendo, el tiempo promedio de activaci�n va a sobrepasar
# los 25 s. As�:
# H0: ?? = 25 s
# HA: ?? > 25 s

?? <- 25
tama�o.muestra <- length(muestra)
grados.de.libertad <- tama�o.muestra - 1
media.muestra <- mean(muestra)
desvest.muestra <- sd(muestra)

cat("\n")
cat("H0: ?? =", ??, "[s]\n")
cat("HA: ?? >", ??, "[s]\n")
cat("Promedio muestral:", media.muestra, "[s]\n")
cat("Desviaci�n est�ndar muestral:", desvest.muestra, "[s]\n")
cat("Tama�o de la muestra:", tama�o.muestra, "[pijamas]\n")
cat("\n")

ttest4 <- t.test(muestra, mu = ??,
                 alternative = "greater", conf.level = 1 - ??)
t <- ttest4[["statistic"]]
ci <- paste(round(ttest4[["conf.int"]], 3), collapse = ", ")
p.valor <- ttest4[["p.value"]]
p <- round(p.valor, 3)

cat("\n")
cat("T test de Student para el tiempo medio de activaci�n\n")
cat("----------------------------------------------------\n")
cat("\n")
cat("Estad�stico t:", round(t, 3), "\n")
cat("Nivel de significaci�n:", ??, "\n")
if(p > 0) cat("p-valor:", p, "\n") else cat("p-valor: < .001\n")
cat("Intervalo de confianza: [", ci, "]\n", sep = "")
if(p >= ??) {
  cat("Resultado: No hay suficiente evidencia para rechazar H0\n")
} else {
  cat("Resultado: Hay suficiente evidencia para rechazar H0 y aceptar HA\n")
}
cat("\n")

# Si bien el el p-valor no es m�s peque�o que el nivel de significaci�n
# definido, tampoco est� tan lejos. La conclusi�n en este caso deber�a
# ser que la muestra deber�a ser incrementada para estar seguros.

