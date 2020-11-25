

cat("\n\n")
cat("Búsqueda eficiente\n")
cat("==================\n")

# Se tiene una "tabla de dos vías" (o 2x2), que registra las
# frecuencias observadas para las posibles combinaciones de dos
# variables categóricas.
# Para esto existe un procedimiento ??^2, que se le conoce de forma 
# general como Prueba ??^2 de Asociación, aunque a veces también se 
# distinguen dos pruebas distintas: la Prueba ??^2 de Homogeneidad y la 
# Prueba ??^2 de Independencia. La diferencia es más bien conceptual, 
# no matemática, y tiene relación con cómo se miren las variables y las
# poblaciones involucradas en el problema.
# 
# Este caso parece enfrentarse mejor como una Prueba ??^2 de Homogeneidad,
# ya que podemos considerar varias muestras (una para el algoritmo actual,
# otra para el algoritmo nuevo 1 y una última para el algoritmo nuevo 2)
# para las cuales se midió un único factor, que podríamos llamar
# "eficiencia del algoritmo", con dos niveles:"una búsqueda" y "más de 
# una búsqueda".

cat("\n")
cat("H0: El rendimiento de los algoritmos es el mismo\n")
cat("HA: Al menos un algoritmo presenta una eficiencia disitnta al\n")
cat("    de los otros algoritmos\n")

# Menos correcto, pero posible, sería verlo como una Prueba ??^2 de
# Independencia, en donde hay dos factores ("algoritmos" y "eficiencia
# del algoritmo") que se miden en una misma población (de queries y
# documentos). Así:

cat("\n")
cat("H0: El nº de veces que se necesita hacer más de una búsqueda\n")
cat("    no depende del algoritmo usado\n")
cat("HA: El algoritmo usado incide en el nº de veces que se necesita\n")
cat("    hacer más de una búsqueda\n")

# Después de esta decisión conceptual, la matemática es la misma,
# aunque la interpretación de los resultados sea, obviamente,
# distinta.

# Veamos los datos
una.búsqueda <- c(3511, 1749, 1818)
más.de.una.búsqueda <- c(1489, 751, 682)

tabla.obs <- rbind(una.búsqueda, más.de.una.búsqueda)

margen.fila <- apply(tabla.obs, 1, sum)
margen.columna <- apply(tabla.obs, 2, sum)
n <- sum(tabla.obs)

tabla.esp <- margen.fila %*% t(margen.columna) / n

difs <- (tabla.obs - tabla.esp)^2 / tabla.esp
??.cuadrado <- sum(difs)

# Recordar que en este caso,
#     gdl = (nº de filas menos 1) · (nº de columnas menos 1)
gdl <- (length(margen.fila) - 1) * (length(margen.columna) - 1)

# Y ahora podemos obtener un p-valor

p.valor <- pchisq(??.cuadrado, gdl, lower.tail = FALSE)

cat("\n")
cat("Frecuencias observadas:\n")
print(tabla.obs)
cat("\n")
cat("Frecuencias esperadas:\n")
print(tabla.esp)
cat("\n")
cat("Estadístico ??^2:", ??.cuadrado, "\n")
cat("Grados de libertad:", gdl, "\n")
cat("P-valor:", p.valor, "\n")

# Lo mismo con la función chisq.test de R:

??.cuadrado.test <- chisq.test(x = tabla.obs)

cat("\n\n")
cat("Prueba ??^2 de Homogeneidad entre algorirtmos y más de una búsqueda\n")
cat("------------------------------------------------------------------\n")
print(??.cuadrado.test)
# stop()

# Luego, los datos sugieren que podría haber cierta diferencia de
# rendimiento entre los algoritmos con nivel de confianza 95%.
# Sería prudente hacer más experimentos.



cat("\n\n")
cat("Café y depresión\n")
cat("================\n")

# También es una tabla de contingencia de dos vías. Esta vez, la
# Prueba ??^2 de Asociación toma mejor más la idea de una Prueba ??^2 de
# Independencia.

cat("\n")
cat("H0: La incidencia de un cuadro depresivo en mujeres adultas no \n")
cat("    está asociada a la cantidad de café que estas consumen\n")
cat("HA: La cantidad de café consumido por una mujer adulta influye \n")
cat("    en su posibilidad de desarrollar un cuadro depresivo\n")

# Como dijimos, matemáticamente se maneja de manera idéntica
con.depresión <- c(670, 373, 905, 564, 95)
sin.depresión <- c(11545, 6244, 16329, 11726, 2288)

tabla.obs <- rbind(con.depresión, sin.depresión)

margen.fila <- apply(tabla.obs, 1, sum)
margen.columna <- apply(tabla.obs, 2, sum)
n <- sum(tabla.obs)

tabla.esp <- margen.fila %*% t(margen.columna) / n

difs <- (tabla.obs - tabla.esp)^2 / tabla.esp
??.cuadrado <- sum(difs)
gdl <- (length(margen.fila) - 1) * (length(margen.columna) - 1)
p.valor <- pchisq(??.cuadrado, gdl, lower.tail = FALSE)

cat("\n")
cat("Frecuencias observadas:\n")
print(tabla.obs)
cat("\n")
cat("Frecuencias esperadas:\n")
print(tabla.esp)
cat("\n")
cat("Estadístico ??^2:", ??.cuadrado, "\n")
cat("Grados de libertad:", gdl, "\n")
cat("P-valor:", p.valor, "\n")

# Ahora con la función chisq.test de R:

??.cuadrado.test <- chisq.test(x = tabla.obs)

cat("\n\n")
cat("Prueba ??^2 de Independencia entre café y depresión\n")
cat("--------------------------------------------------\n")
print(??.cuadrado.test)
# stop()

# Por lo tanto, los datos sugieren fuertemente que la cantidad de
# consumo de café tiene influencia en el desarrollo de cuadros
# depresivos.



cat("\n\n")
cat("Programme for International Student Assessment (PISA)\n")
cat("=====================================================\n")

# Nuevamente podemos pensar en una Prueba ??^2 de Homogeneidad, puesto
# que claramente existe una variable factor (niveles de desempeño) que
# se midió en dos poblaciones distintas (Chile y países con PIB similar).

cat("\n")
cat("H0: Chile obtiene en la prueba Pisa un rendimiento parecido a\n")
cat("    los países que tienen un PIB similar\n")
cat("HA: El rendimiento de Chile en la prueba Pisa no tiene el mismo\n")
cat("    comportamiento que los países que tienen un PIB similar\n")

# Definamos los datos
Chile <- 7053 * c(1.4, 23.8, 25.5, 49.3) / 100
PIB.similar <- 46994 * c(6.8, 37.0, 24.5, 31.7) / 100

# Y sigamos el mismo procedimiento matemático
tabla.obs <- rbind(Chile, PIB.similar)

margen.fila <- apply(tabla.obs, 1, sum)
margen.columna <- apply(tabla.obs, 2, sum)
n <- sum(tabla.obs)

tabla.esp <- margen.fila %*% t(margen.columna) / n

difs <- (tabla.obs - tabla.esp)^2 / tabla.esp
??.cuadrado <- sum(difs)

# Y determinemos el p-valor
gdl <- (length(margen.fila) - 1) * (length(margen.columna) - 1)
p.valor <- pchisq(??.cuadrado, gdl, lower.tail = FALSE)

cat("\n")
cat("Frecuencias observadas:\n")
print(tabla.obs)
cat("\n")
cat("Frecuencias esperadas:\n")
print(tabla.esp)
cat("\n")
cat("Estadístico ??^2:", ??.cuadrado, "\n")
cat("Grados de libertad:", gdl, "\n")
cat("P-valor:", p.valor, "\n")

# Lo mismo con la función chisq.test de R:

??.cuadrado.test <- chisq.test(x = tabla.obs)

cat("\n\n")
cat("Prueba ??^2 de Homogeneidad entre Chile y países con PIB similar\n")
cat("---------------------------------------------------------------\n")
print(??.cuadrado.test)
# stop()

# Así, es bastante improbable que el rendimiento de Chile en realidad se
# esté comportando de manera similar a los otros países que tienen un 
# PIB similar.



cat("\n\n")
cat("Evolución o creacionismo\n")
cat("========================\n")

# Este problema también puede enfrentarse con un procedimiento ??^2,
# que usualmente se le conoce como Prueba de Bondad de Ajuste, goodness-
# of-fit en inglés, en que se tabula las frecuencias observadas de una 
# variable en categorías y que se comparan con frecuencias esperadas en
# cada categoría de acuerdo a una distribución conocida de referencia, lo
# que permite, siguiendo la misma matemática, obtener un estadístico ??^2.

# En este caso, tenemos frecuencias observadas el 2010 y conocemos la
# distribución que tenía la "población" en 2007, lo que usaremos como
# referencia para la comparación.

frec.2010 <- c(387, 163, 408)
prop.2007 <- c(38, 14, 43) / 100

# Siguiendo las ideas expuestas en la sección 6.3 de OpenIntro
# [o alternativamente en 8.1 en Concepts & Applications of Inferential
# Statistics (vassarstats.net)], debemos calcular las desviaciones
# normalizadas cuadradas entre ambas "distribuciones".

n <- sum(frec.2010) + 61
frec.2007 <- prop.2007 * n

n.dif.2007 <- (frec.2010 - frec.2007)^2 / frec.2007
??.cuadrado.2007 <- sum(n.dif.2007)

# Podemos calcular un p-valor

gdl <- length(frec.2010) - 1
p.valor.2007 <- pchisq(??.cuadrado.2007, gdl, lower.tail = FALSE)

cat("\n")
cat("Frecuencias observadas en 2010:", frec.2010, "\n")
cat("Frecuencias esperadas según 2007:", frec.2007, "\n")
cat("Estadístico ??^2:", ??.cuadrado.2007, "\n")
cat("Grados de libertad:", gdl, "\n")
cat("P-valor:", p.valor.2007, "\n")

# Esto puede obtenerse en R fácilmente

??.cuadrado.test.2007 <- chisq.test(x = frec.2010, p = prop.2007,
                                   rescale.p = TRUE)

# Notemos que es necesario normalizar las proporciones esperadas
# porque no suman 1 al no considerar las personas que no responden.

cat("\n\n")
cat("Prueba ??^2 de Bondad del Ajuste según proporciones 2007\n")
cat("-------------------------------------------------------\n")
print(??.cuadrado.test.2007)

# Por lo tanto, los datos sugieren que la muestra podría venir de una
# población con las mismas creencias que en 2007 (con 8,5% de proba-
# bilidad) al considerar 95% confianza.


