library(datasets)
library(dplyr)
library(ggpubr)


# Se sabe que la lactancia estimula una p�rdida de masa �sea para
# proporcionar cantidades de calcio adecuadas para la producci�n de
# leche. Un estudio intent� determinar si madres adolescentes pod�an
# recuperar niveles m�s normales a pesar de no consumir suplementos
# (Amer. J. Clinical Nutr., 2004; 1322-1326).
# El estudio obtuvo las siguientes medidas del contenido total de
# minerales en los huesos del cuerpo (en gramos) para una muestra de
# madres adolescentes tanto durante la lactancia (6-24 semanas postparto)
# y posterior a ella (12-30 semana postparto):

lact <- c(1928, 2549, 2825, 1924, 1628, 2175, 2114, 2621, 1843, 2541)
posd <- c(2126, 2885, 2895, 1942, 1750, 2184, 2164, 2626, 2006, 2627)

# �Sugieren los datos que el contenido total de minerales en los huesos
# del cuerpo durante el posdestete excede el de la etapa de lactancia
# por m�s de 25 g? 

# Para analizar estos datos, lo m�s importante es darse cuente que se
# tratan de dos mediciones a los mismos sujetos de investigaci�n
# (personas). Eso significa que estamos frente a datos apareados.
# Este an�lisis aparece en la secci�n 5.2.1 de OpenIntro Statistics.
# Debemos obtener la diferencia de las medidas de cada persona:

dif <- posd - lact

# Grafiquemos la distribuci�n de las diferencias.
# (Para eso creamos un data frame con un solo grupo)

df.dif <- data.frame(
  Diferencia = dif,
  Grupo = 1
)
df.dif[["Grupo"]] <- factor(df.dif[["Grupo"]])

p1.1 <- ggboxplot(
  df.dif,
  x = "Grupo", y = "Diferencia",
  color = "Grupo",
  add = "jitter", add.params = list(color = "Grupo", fill = "Grupo"),
  title = "Diferencia en la medida de mineral �seo"
)
p1.1 <- p1.1 + rremove("legend")
p1.1 <- ggpar(p1.1, orientation = "horizontal")
p1.1 <- ggpar(p1.1, ylab = FALSE)
p1.1 <- p1.1 + rremove("y.text")
p1.1 <- p1.1 + rremove("y.ticks")


# Vemos que ninguna de las diferencias es menor a cero, por lo que
# efectivamente hay valores de minerales en los huesos m�s altos
# despu�s de terminada la lactancia.
# Pero el problema nos consulta si esta diferencia es mayor a 25 g.
# Eso es menos claro en el gr�fico, puesto si bien hay puntos que parecen
# estar bajo los 25 g, estos podr�an deberse a la muestra usada.
# Para confirmar la validez estad�stica de este posible aumento debemos
# usar una prueba T de Student, por el tama�o acotado de la muestra, y
# utilizar esta posibilidad como hip�tesis alternativa:
# H0: ?? = 25  
# H1: ?? > 25  
??0 <- 25

# El gr�fico tambi�n muestra que hay una leve asimetr�a, pero sin que se
# identifiquen valores at�picos (outliers), aunque hay uno que parece estar
# cerca del l�mite. Luego, basta con ser un poco m�s exigentes con el nivel
# de significaci�n:
?? <- 0.025

# Obtengamos el estad�stico
media.muestra <- mean(df.dif[["Diferencia"]])
desvest.muestra <- sd(df.dif[["Diferencia"]])
tama�o.muestra <- nrow(df.dif)
err.est <- desvest.muestra / sqrt(tama�o.muestra)
t <- (media.muestra - ??0) / err.est
gdl <- tama�o.muestra - 1

# Obtengamos el p-valor. En este caso, tenemos una prueba de una cola.
p.valor <- pt(t, gdl, lower.tail = FALSE)

cat("\n\n")
cat("Masa �sea\n")
cat("=========\n")

cat("\n\n")
cat("T test de Student para la diferencia de dos muestras relacionadas\n")
cat("-----------------------------------------------------------------\n")
cat("?? hipot�tico de las diferencias:", ??0, "g\n")
cat("Media observada de las diferencias:", media.muestra, "g\n")
cat("Desviaci�n est�ndar de las diferencias:", desvest.muestra, "g\n")
cat("Tama�o de la muestra:", tama�o.muestra, "[sujetos]\n")
cat("Error est�ndar:", err.est, "g\n")
cat("\n")
cat("Estad�stico t:", t, "\n")
cat("Grados de libertad:", gdl, "\n")
cat("Nivel de significaci�n:", ??, "\n")
cat("p-valor:", round(p.valor, 3), "\n")

# Vemos que el p.valor es menor que el ?? definido para la prueba, por lo
# que hay suficiente evidencia para rechazar H0.
# Conclusi�n: La diferencia en contenido total de minerales en los huesos
# de madres adolescentes entre el periodo de lactancia y luego que esta
# concluye es, en promedio, m�s de 25 g.


# stop("*** Parada intermedia ***")


# Pero este tipo de c�lculos manuales son raramente necesarios en R
# despu�s de todos los a�os que lleva desarroll�ndose. As�, existe la
# funci�n t.test() que puede usarse aqu� nuevamente:

ttest1 <- t.test(posd, lact, paired = TRUE,
                 mu = 25, alternative = "greater", conf.level = 1 - ??
)

# O tambi�n en la forma con f�rmula y data.frames:

d <- data.frame(
  Medici�n = c(posd, lact),
  Periodo = c(rep("P", length(posd)), rep("L", length(lact)))
)

# Notemos que la funci�n 'data.frame()' convierte variables de tipo 'string'
# en factores, que es otro nombre para variables categ�ricas (a menos que le
# especifiquemos que no lo haga dando valor 'FALSE' al argumento
# 'stringsAsFactors').
# Los factores en R se almacenan como un vector de enteros: 1, 2, ... que
# codifican los posibles valores que puede tomar la variable, tambi�n llamados
# niveles ('levels' en ingl�s).
# Cada nivel es acompa�ado de un nombre, tambien llamada etiqueta ('label' en
# ingl�s) que es un string.
# El problema es que cuando R convierte un vector de strings a factores, asigna
# los valores en orden alfab�tico. Por ejemplo:
# meses <- factor(c("febrero", "abril", "septiembre"))
# print(meses)
# print(str(meses))
# print(levels(meses))

# Eso usualmente funciona bien, pero en este caso queremos que se calcule
# la resta: valores del nivel 'P' menos valores del nivel 'L' (P - L)
# Pero por el orden usado para los niveles es alfab�tico, por lo que
# al decir "reste de acuerdo al factor", se va a calcular L - P, lo que
# no es correcto para nuestra hip�tesis ?? > 25.
# As�, o cambiamos la hip�tesis a ?? < 25 o cambiamos el orden de los niveles
# en el factor.
# Hag�mos esto �ltimo:

d[["Periodo"]] <- factor(d[["Periodo"]], levels = c("P", "L"))

# Ahora podemos usar esta otra versi�n de la funci�n t.test, con f�rmulas,
# las que son inevitables cuando trabajemos con m�todos m�s sofisticados. 
ttest2 <- t.test(Medici�n ~ Periodo, data = d, paired = TRUE,
                 mu = 25, alternative = "greater", conf.level = 1 - ??)

cat("\n\n")
cat("T test de Student para dos muestras correlacionadas en R\n")
cat("--------------------------------------------------------\n")
print(ttest1)
print(ttest2)


# Notemos que esta funci�n tambi�n nos da el intervalo (abierto) con 
# 97.5% de confianza, que nos indica que los datos indican que la 
# verdadera media de la diferencia ser�a al menos 31,4 g.



# stop("*** Parada intermedia ***")
# ----------------------------------------------------------------



# La avicultura de carne es un negocio muy lucrativo, y cualquier
# m�todo que ayude al r�pido crecimiento de los pollitos es beneficioso,
# tanto para las av�colas como para los consumidores. En el paquete
# dataset de R est�n los datos (chickwts) de un experimento hecho para
# medir la efectividad de varios suplementos alimenticios en la tasa
# de crecimiento de las aves. Pollitos reci�n nacidos se separaron
# aleatoriamente en 6 grupos y a cada grupo se le dio un suplemento
# distinto. Para productores de la 7ma regi�n, es especialmente
# importante saber si existe diferencia en la efectividad entre el
# alimento basado en linaza (linseed) y el basado en habas (horsebean).

cat("\n\n")
cat("Avicultura de carne\n")
cat("===================\n")

datos <- chickwts
datos <- datos %>% filter(feed == "horsebean" | feed == "linseed")
datos[["feed"]] <- factor(datos[["feed"]])

p2.1 <- ggboxplot(
  datos,
  x = "feed", y = "weight",
  add = "jitter", add.params = list(color = "feed", fill = "feed"),
  color = "feed",
  title = "Peso final por tipo de alimento",
  xlab = "Tipo de alimento", ylab = "Peso (g)"
)

# Parece haber una clara diferencia. Para confirmar la validez
# estad�stica de esta diferencia con un contraste de hip�tesis debemos
# formar conjeturar sobre la diferencias de las medias de ambas
# poblaciones (pollos nutridos con suplementos basados en linaza y
# pollos nutridos con suplementos basados en habas).
# Nos preguntan si hay evidencia de una diferencia en la efectividad de
# estos suplementos. Llamemos ??_linaza al peso medio que alcanzan (todos)
# los pollos alimentados con suplementos basados en linaza, y llamemos
# ??_habas a (todos) los pollos alimentados con suplementos basados en
# habas. Como no hay razones para sospechar que tal o cual va a ser
# menor/mayor que la otra, corresponde hacer una prueba de dos
# colas. Luego las hip�tesis ser�an:
# H0: ??_habas y ??_linaza no son diferentes (??_habas = ??_linaza)
# H1: ??_habas y ??_linaza s� son diferentes (??_habas <> ??_linaza)
# o en t�rminos de fiderencia de estas medias:
# H0: ??_habas - ??_linaza = 0  
# H1: ??_habas - ??_linaza <> 0
#
??0 <- 0

# Como las muestras no son tan grandes, nos corresponder�a hacer una
# prueba T de Student para diferencia de dos medias.
# Debemos entonces, revisar las condiciones para aplicar esta prueba.

# Condiciones (OpenIntro Statistics p�g. 231):
# 1) cada muestra cumple las coniciones para usar la distribuci�n T, y
# 2) las muestras son independientes.

# Dado que se trat� de un experimento dise�ado para evaluar los tipos
# de suplementos alimenticios, podemos suponer que la selecci�n de cada
# muestra estuvo acorde a los principios de independencia. Sin otra
# informaci�n, debemos suponer que la condici�n 2) est� cumplida.

# Al gr�fico de cajas le agregamos los puntos; no hay outliers y ambas
# muestras paracen bastante sim�tricas. Tambi�n podemos usar un gr�fico
# Q-Q para confirmar esta evaluaci�n:

p2.2 <- ggqqplot(
  datos,
  x = "weight",
  color = "feed"
)

# Vemos que, con la excepci�n de algunos cuantos puntos extremos, 
# las muestras parecen seguir aproximadamente una distribuci�n normal.
# Tal vez, considerando la existencia de estos valores "casi" at�picos
# y al tama�o reducido de las muestras, podemos ser un poco m�s
# exigentes con el nivel de significaci�n:

?? <- 0.03

# Otra forma com�n de evaluar la normalidad aproximada de nuestra
# muestra es usar una prueba de hip�tesis en que la H0 es "la muestra
# viene de una poblaci�n normal".
# Existen varias alternativas de este tipo de "pruebas de normalidad",
# pero suele recomendarse la prueba W de Shapiro-Wilk o la prueba de
# A� de Anderson-Darling, ambos implementados en R.
# [H.C. Thode (2011) Normality Tests. In: Lovric M. (eds) International
# Encyclopedia of Statistical Science. Springer]
# Usemos aqu� la prueba de Shapiro-Wilk:mada con una prueba W

es.habas <- datos[["feed"]] == "horsebean"
cat("\n")
print(shapiro.test(datos[es.habas, "weight"]))
cat("\n")
print(shapiro.test(datos[!es.habas, "weight"]))

# Adem�s, la separaci�n aleatoria de los pollitos y el reducido tama�o
# de la muestra respecto a la poblaci�n parecen asegurar la
# independencia de las observaciones en cada muestra.
#
# Luego la condici�n 1) parace cumplirse.



# stop("*** Parada intermedia ***")



# Los c�lculos para una prueba T de Student para diferencia de dos
# medias se encuentra en OpenIntro Statistics p�gs. 231-232.
# En realidad, lo que aparece all� corresponde a los c�lculos para la
# prueba de Welch, que es una modificaci�n de la prueba T de Student
# original en que no considera una varianza com�n (pooled variance),
# por lo que tambi�n se le llama "prueba T de Student con varianzas
# distintas".
# Esta prueba normalmente es m�s "robusta" (la probabilidad de cometer
# un error de tipo I se acerca al nivel de significaci�n definido) que
# el t-test original, sobre todo si el ta�ano de las muestras es
# desigual y las muestras no son muy reducidas. Ver:
#
# GD Ruxton (2006). The unequal variance t-test is an underused
# alternative to Student's t-test and the Mann-Whitney U test.
# Behavioral Ecology 17:688-690.
#
# B Derrick, D Toher, P White (2016). Why Welch's test is Type I error
# robust. The Quantitative Methods for Psychology 12(1):30-38.
# 

# Los c�lculos:

# Error est�ndar
var.habas <- var(datos[es.habas, "weight"])
var.linaza <- var(datos[!es.habas, "weight"])
n.habas <- length(datos[es.habas, "weight"])
n.linaza <- length(datos[!es.habas, "weight"])

varp.habas <- var.habas / n.habas
varp.linaza <- var.linaza / n.linaza

err.est <- sqrt(varp.habas + varp.linaza)


# Diferencia observada
media.habas <- mean(datos[es.habas, "weight"])
media.linaza <- mean(datos[!es.habas, "weight"])
dif.medias <- media.habas - media.linaza

# Estad�stico t
t <- (dif.medias - ??0) / err.est

# Grados de libertad (�se omite en el libro?)
gdl.habas <- n.habas - 1
gdl.linaza <- n.linaza - 1

gdl.numerador <- (varp.habas + varp.linaza)^2
gdl.denominador <- varp.habas^2 / gdl.habas + varp.linaza^2 / gdl.linaza
gdl <- gdl.numerador / gdl.denominador

# Obtenemos la probabilidad en la dist. t
p.valor <- ifelse(t < 0, pt(t, gdl),
                  pt(t, gdl, lower.tail = FALSE)
)
p.valor <- 2 * p.valor # 2 colas

cat("\n\n")
cat("T test de Student para dos muestras con varianzas distintas\n")
cat("-----------------------------------------------------------\n")
cat("Diferencia de ??'s hipot�tica:", ??0, "g\n")
cat("Diferencia de ??'s observada:", dif.medias, "g\n")
cat("Desviaci�n est�ndar muestra 1:", sqrt(varp.habas), "g\n")
cat("Tama�o de la muestra 1:", n.habas, "[pollitos]\n")
cat("Desviaci�n est�ndar muestra 2:", sqrt(varp.linaza), "g\n")
cat("Tama�o de la muestra 2:", n.linaza, "[pollitos]\n")
cat("Error est�ndar:", err.est, "g\n")
cat("\n")
cat("Estad�stico t:", t, "\n")
cat("Grados de libertad:", gdl, "\n")
cat("Nivel de significaci�n:", ??, "\n")
cat("p-valor:", round(p.valor, 3), "\n")



# stop("*** Parada intermedia ***")


# La funci�n t.test de R nos permite hacer esta prueba y obtener un
# intervalo de confianza f�cilmente. Aqu� usando la variante en que se
# especifica una f�rmula que relaciona columnas de un data.frame.

ttest3 <- t.test(weight ~ feed, data = datos, mu = ??0, conf.level = 1 - ??)

cat("\n\n")
cat("T test de Student para dos muestras con varianzas distintas en R\n")
cat("----------------------------------------------------------------\n")
print(ttest3)


# El p-valor es bastante menor que el ?? definido, por lo que hay una
# fuerte evidencia para rechazar H0.

# Conclusi�n: S� hay diferencia en la efectividad de los suplementos
# alimenticios (linaza es mejor que habas).



