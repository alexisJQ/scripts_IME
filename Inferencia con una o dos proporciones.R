

# Este script muestra ejemplos de soluci�n para problemas que involucra
# inferencias sobre proporciones.
# Usamos los mismos datos que en la actividad de la clase.

alcohol <-data.frame(
  consumo = factor(c("0", "1-9", "10-44", "45+")),
  casos = c(43, 89, 109, 242),
  controles = c(108, 141, 91, 107)
)

tabaco <-data.frame(
  consumo = factor(c("0", "1-19", "20-39", "40+")),
  casos = c(26, 66, 248, 143),
  controles = c(85, 97, 197, 68)
)


cat("\n")
cat("A1. Tama�o muestra\n")
cat("==================\n")

# A1. Suponiendo que la diferencia en la proporci�n de personas que
#     desarrollan la enfermedad entre quienes beben de 10 a 44 ml de
#     alcohol por d�a y aquellos que beben 45 o m�s ml al d�a es de 0.15.
#     �Cu�nta gente deber�amos entrevistar para obtener un intervalo
#     de confianza del 95% y poder estad�stico de 90%?

# Aqu� nos pregunta por el tama�o de la muestra. Se trata de una
# diferencia de proporciones entre dos grupos de personas que beben
# diferentes cantidades de alcohol regularmente.
# La hip�tesis nula es que quienes beben 45 ml o m�s al d�a tienen
# registran, al menos, 15% m�s casos de c�ncer oral que quienes beben
# de 10 a 44 ml de alcohol por d�a.
# Veamos las proporciones observadas:

n.10a44.obs <- alcohol[["casos"]][3] + alcohol[["controles"]][3]
p.10a44.obs <- alcohol[["casos"]][3] / n.10a44.obs

n.45om�s.obs <- alcohol[["casos"]][4] + alcohol[["controles"]][4]
p.45om�s.obs <- alcohol[["casos"]][4] / n.45om�s.obs

cat("\n")
cat("Casos observados 10-44 ml:", n.10a44.obs, "\n")
cat("Proporci�n observada 10-44 ml:", p.10a44.obs, "\n")
cat("\n")
cat("Casos observados 45+ ml:", n.45om�s.obs, "\n")
cat("Proporci�n observada 45+ ml:", p.45om�s.obs, "\n")

# Tratamos entonces de definir proporciones "esperadas" que se acerquen 
# a las observadas, pero con la diferencia establecida en la hip�tesis.
# Como no nos dan una direcci�n para la hip�tesis alternativa, lo
# dejamos con ambos lados:

p.10a44.esp <- .55
p.45om�s.esp <- .70
??.esp <- p.10a44.esp - p.45om�s.esp

# Para determinar el poder o el tama�o de una muestra para una
# diferencia de proporciones se suele usar el m�todo propuesto en
# Fleiss et al. (1980).
# [JL Fleiss, A Tytun & HK Ury (1980). A simple approximation for
# calculating sample sizes for comparing independent proportions.
# Biometrics, 343-346.]

?? <- 0.05
Z?? <- qnorm(1 - ?? / 2)
pow <- 0.9
?? <- 1 - pow
Z?? <- qnorm(1 - ??)

# Manejando el valor de r en el m�todo de Fleiss et al. (1980), se puede
# obtener tama�os distintos para cada muestra. Igual tama�o se consigue
# con r = 1.

r <- 1

# Se calcula una proporci�n combinada (que requiere correcci�n de
# continuidad cuando el valor de la proporci�n se acerca a 0 o a 1).

P <- (p.10a44.esp + r * p.45om�s.esp) / (r + 1)
Q <- 1 - P

# Luego se calcula los l�mites para la significaci�n y poder definidos

A <- Z?? * sqrt((r + 1) * P * Q)
B <- Z?? * sqrt(r * p.10a44.esp  * (1 - p.10a44.esp) +
                 p.45om�s.esp * (1 - p.45om�s.esp))

# Y se obtiene el tama�o de cada muestra

n1 <- (A + B)^2 / (r * ??.esp^2)
n2 <- r * n1


cat("\n")
cat("Factores con grupos de igual tama�o\n")
cat("-----------------------------------\n")
cat("Nivel de confianza:", 1 - ??, "\n")
cat("Poder estad�stico:", pow , "\n")
cat("n2 es", r, "veces n1", "\n")
cat("n1 = ", n1, " (", ceiling(n1), ")", "\n", sep = "")
cat("n2 = ", n2, " (", ceiling(n2), ")", "\n", sep = "")
# stop()


# Como hasta ahora, existe una funci�n en R que implementa este
# procedimiento: la funci�n power.prop.test() que trae R (base).

powtest1 <- power.prop.test(p1 = p.10a44.esp, p2 = p.45om�s.esp, 
                            sig.level = ??, power = pow,
                            alternative = "two.sided"
)
cat("\n")
cat("Usando la funci�n power.prop.test():\n")
cat("------------------------------------\n")
print(powtest1)
# stop()


# Pero tal vez es m�s representativo considerar muestras de diferente
# tama�o, porque tenemos antecedentes de que un grupo es m�s com�n que
# otro. En los datos de ejemplo, hay 200 personas que beben regularmente
# entre 10 y 44 ml, pero 349 personas que beben 45 o m�s ml de alcohol.
# Podemos mantener esta raz�n:

r <- n.45om�s.obs / n.10a44.obs

P <- (p.10a44.esp + r * p.45om�s.esp) / (r + 1)
Q <- 1 - P

A <- Z?? * sqrt((r + 1) * P * Q)
B <- Z?? * sqrt(r * p.10a44.esp  * (1 - p.10a44.esp) +
                 p.45om�s.esp * (1 - p.45om�s.esp))

n1 <- (A + B)^2 / (r * ??.esp^2)
n2 <- r * n1

cat("\n")
cat("Factores con grupos de tama�o distintos\n")
cat("---------------------------------------\n")
cat("Nivel de confianza:", 1 - ??, "\n")
cat("Poder estad�stico:", pow , "\n")
cat("n2 es", r, "veces n1", "\n")
cat("n1 = ", n1, " (", ceiling(n1), ")", "\n", sep = "")
cat("n2 = ", n2, " (", ceiling(n2), ")", "\n", sep = "")
# stop()

# Esto es posible de lograr con la funci�n bsamsize del paquete Hmisc
library(Hmisc)

powtest2 <- bsamsize(p1 = p.10a44.esp, p2 = p.45om�s.esp,
                     fraction = 1 / (r  + 1),
                     alpha = ??, power = pow
)

cat("\n")
cat("Usando la funci�n bsamsize():\n")
cat("-----------------------------\n")
print(powtest2)
# stop()




cat("\n\n")
cat("B2. Inferencia con una muestra\n")
cat("==============================\n")

# B2. Estudios previos hab�an determinado que la incidencia de c�ncer
#     oral en la poblaci�n que bebe regularmente entre 1 y 9 ml de
#     alcohol era de 25%. �Respaldan estos datos tal estimaci�n?

# Esto es una inferencia de una proporci�n con de una muestra, la que
# podemos enfrentar con las f�rmulas propuestas por Wilson que vimos al
# inicio de la  clase.

# Las hip�tesis ser�an:
cat("\n")
p0 <- 0.25
cat("H0: p =", p0, "\n")
cat("HA: p <>", p0, "\n")

# Hagamos el conteo de las frecuencias
casos <- alcohol[["casos"]][2]
controles <- alcohol[["controles"]][2]
n <- casos + controles
p.gorro <- casos / n

cat("\n")
cat("Casos observados:", casos, "\n")
cat("Controles observados:", controles, "\n")
cat("N� observaciones:", n, "\n")
cat("Proporci�n de c�cer observada (�xito):", p.gorro, "\n")
cat("N� de �xitos esperados:", p.gorro * n, "\n")
cat("N� de fracasos esperados:", (1 - p.gorro) * n, "\n")

# Luego se cumplen las condiciones para que la distribuci�n muestral
# de la proporci�n siga aproximadamente un modelo normal, que est�n
# especificadas en la secci�n 6.1.1 de OpenIntro.
# De este modo, y considerando que las muestras son m�s bien grandes,
# podemos definir un nivel de significaci�n relajado.

?? <- 0.05
z.??.medio <- qnorm(1 - ?? / 2)

p.prima.num <- p.gorro + (z.??.medio^2 / (2 * n))
p.prima.den <- 1 + (z.??.medio^2 / n)
p.prima <- p.prima.num / p.prima.den

s.prima.num.term1 <- (p.gorro * (1 - p.gorro)) / n
s.prima.num.term2 <- z.??.medio^2 / (4 * n^2)
s.prima.num <- sqrt(s.prima.num.term1 + s.prima.num.term2)
s.prima.den <- 1 + (z.??.medio^2 / n)
s.prima <- s.prima.num / s.prima.den

ic.low <- p.prima - z.??.medio * s.prima
ic.upp <- p.prima + z.??.medio * s.prima

cat("\n")
cat("Nivel de significaci�n:", ??, "\n")
cat("Intervalo con 100�(1-??)% de confianza: ")
cat("[", ic.low, ", ", ic.upp, "]", "\n", sep = "")
# stop()


# Vemos que el intervalo con 95% de confianza no incluye el valor p0.
# Por lo tanto, los datos no respaldan este valor hipot�tico para la
# verdadera proporci�n de c�ncer en este grupo de bebedores.

# Para obtener un p-valor, en este caso, podemos seguir las ideas
# en Openintro.

error.est�ndar <- sqrt(p0 * (1 - p0) / n)

# y obtener el estad�stico:

z <- (p.gorro - p0) / error.est�ndar

# y p-valor bilateral:

p.valor <- 2 * (1 - pnorm(z))

cat("\n")
cat("Estad�stico z:", round(z, 3), "\n")
cat("P-valor:", p.valor, "\n")
# stop()

# Esta probabilidad es mucho menor al nivel de significaci�n 0.05
# considerado, confirmando que es muy improbable ver estos n�meros
# en una muestra si el par�metro p fuera p0.

# Esta prueba est� implementada en R:


ptest1 <- prop.test(
  x = casos,
  n = n,
  p = p0,
  alternative = "two.sided",
  conf.level = 1 - ??,
  correct = FALSE
)

cat("\n")
cat("Usando la funci�n prop.test():\n")
cat("------------------------------\n")
print(ptest1)
# stop()


# Pero si nos fijamos, esta funci�n no usa un estad�stico z,
# sino que un estad�stico chi-cuadrado.

# En Openintro tambi�n se trata esta familia de pruebas, vemos
# que se calcula como la suma de las desviaciones cuadradas entre
# las frecuencias observadas y las frecuencias esperadas (normalizadas
# por las frecuencias esperadas). As�:

# observadas <- c(casos, controles)
# esperadas <- c(round(n * p0), round(n * (1 - p0)))

# luego:

# ??.cuadrado <- sum((observadas - esperadas)^2 / esperadas)
# gdl <- length(observadas) - 1

# con lo que podemos obtener un p-valor:

# p.valor <- pchisq(??.cuadrado, gdl, lower.tail = FALSE)
# 
# cat("\n")
# cat("Usando una prueba basada en ??^2:\n")
# cat("-------------------------------\n")
# cat("Estad�stico ??^2:", ??.cuadrado, "\n")
# cat("Grados de libertad:", gdl, "\n")
# cat("P-valor:", p.valor, "\n")
# stop()


# Esto ocurre porque una distribuci�n chi cuadrado con un grado de
# libertad corresponde a desviaciones normales al cuadrado.
# Pero esta es una coincidencia matem�tica solamente, ambas pruebas
# son fundamentalmente diferentes: esta �ltima prueba no define una
# hip�tesis sobre el par�metro p de la poblaci�n (ni de ning�n otro
# par�metro, por lo que "no es param�trico"), sino que contrasta
# hip�tesis m�s d�biles:
# H0: Las frecuencias observadas son las esperadas
# HA: Las frecuencias observadas no son las esperadas




cat("\n\n")
cat("C3. Inferencia con dos muestras\n")
cat("===============================\n")

# C3. Seg�n estos datos, �da lo mismo no fumar que hacerlo diariamente
#     consumiendo entre 1 y 19 cigarrillos?

# Esto es una prueba sobre la diferencia de dos proporciones.

# Las hip�tesis ser�an:
cat("\n")
cat("H0: p1 = p2, o p1 - p2 = 0\n")
cat("HA: p1 <> p2, o p1 - p2 <> 0\n")

n.0.obs <- tabaco[["casos"]][1] + tabaco[["controles"]][1]
p.0.obs <- tabaco[["casos"]][1] / n.0.obs

n.1a19.obs <- tabaco[["casos"]][2] + tabaco[["controles"]][2]
p.1a19.obs <- tabaco[["casos"]][2] / n.1a19.obs

??.obs <- p.0.obs - p.1a19.obs

cat("\n")
cat("Casos observados no fumadores:", n.0.obs, "\n")
cat("Proporci�n observada no fumadores:", p.0.obs, "\n")
cat("\n")
cat("Casos observados fumadores de menos de una cajetilla:", n.1a19.obs, "\n")
cat("Proporci�n observada fumadores de menos de una cajetilla:", p.1a19.obs, "\n")
cat("\n")
cat("Diferencia en proporciones observadas:", ??.obs, "\n")
# stop()

# Para poder realizar una prueba a la diferencia de estas proporciones
# es necesario que cada una de las proporciones cumpla la condici�n de
# �xito-fracaso (secci�n 6.2.1 en Openintro).
# Esto se cumple tanto para los no fumadores (26 y 85 casos) como para
# los que fuman menos de una cajetilla (66 y 97 casos).
# La otra condici�n, la independencia de ambos grupos, tendremos que
# suponer que se cumple dado que los datos los obtuvimos de una revista
# cient�fica seria que no publicar�a un estudio mal hecho.

# Primero obtengamos un intervalo de confianza como se explica en
# la secci�n 6.2 de OpenIntro.

var.est.1 <- p.0.obs * (1 - p.0.obs) / n.0.obs
var.est.2 <- p.1a19.obs * (1 - p.1a19.obs) / n.1a19.obs
err.est.?? <- sqrt(var.est.1 + var.est.2)

?? <- 0.05
z.??.medio <- qnorm(1 - ?? / 2)

if(??.obs > 0) {
  ic.low <- ??.obs - z.??.medio * err.est.??
  ic.upp <- ??.obs + z.??.medio * err.est.??
} else {
  ic.low <- ??.obs + z.??.medio * err.est.??
  ic.upp <- ??.obs - z.??.medio * err.est.??
}

cat("\n")
cat("Nivel de significaci�n:", ??, "\n")
cat("Intervalo con 100�(1-??)% de confianza: ")
cat("[", ic.low, ", ", ic.upp, "]", "\n", sep = "")
# stop()

# Vemos que el cero no est� incluido en el intervalo de confianza, por
# lo que podemos concluir que s� existe una diferencia entre los grupos.
# Es decir, la incidencia de c�ncer bucal en personas que no fuman es
# significativamente menor que en personas que fuman menos de una
# cajetilla de cigarrilos.

# Para obtener un p-valor, necesitamos calcular la proporci�n combinada
# (pooled proportion):

p.comb.num <- tabaco[["casos"]][1] + tabaco[["casos"]][2]
p.comb.den <- n.0.obs + n.1a19.obs
p.comb <- p.comb.num / p.comb.den

cat("\n")
cat("Proporci�n combinada:", p.comb, "\n")

# Esta proporci�n combinada debe verificar la condici�n de �xito-fracaso

e1 <- ceiling(p.comb * n.0.obs)
f1 <- ceiling((1 - p.comb) * n.0.obs)
e2 <- ceiling(p.comb * n.1a19.obs)
f2 <- ceiling((1 - p.comb) * n.1a19.obs)

cat("\n")
cat("�xitos esperados en no fumadores:", e1, "\n")
cat("Fracasos esperados en no fumadores:", f1, "\n")
cat("\n")
cat("�xitos esperados en fumadores de menos de una cajetilla:", e2, "\n")
cat("Fracasos esperados en fumadores de menos de una cajetilla:", f2, "\n")

# Podemos entonces asumir el modelo normal y calcular un p-valor.

var.comb.1 <- p.comb * (1 - p.comb) / n.0.obs
var.comb.2 <- p.comb * (1 - p.comb) / n.1a19.obs
err.est.comb <- sqrt(var.comb.1 + var.comb.2)
??0 <- 0

z <- (??.obs - ??0) / err.est.comb
p.valor <- 2 * pnorm(z)

cat("\n")
cat("Error est�ndar combinado:", err.est.comb, "\n")
cat("Estad�stico z:", round(z, 3), "\n")
cat("P-valor:", p.valor, "\n")
# stop()


# En este caso tambi�n podemos obtener un p-valor matem�ticamente
# equivalente, usando una prueba ??�. Si no se especifica el par�metro p,
# se asume que la hip�tesis nula es que todas las proporciones son
# iguales.

ptest2 <- prop.test(
  x = c(tabaco[["casos"]][1], tabaco[["casos"]][2]),
  n = c(n.0.obs, n.1a19.obs),
  alternative = "two.sided",
  conf.level = 1 - ??,
  correct = FALSE
)

cat("\n")
cat("Usando la funci�n prop.test():\n")
cat("------------------------------\n")
print(ptest2)

# Vemos que, con 95% de confianza, la verdadera diferencia entre las
# proporciones est� entre 6% y 28%, por lo que fumar entre uno y 19
# cigarrillos al d�a s� hace una diferencia (negativa) en cuanto al
# desarrollo de c�ncer oral.

