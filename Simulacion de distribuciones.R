library(ggplot2)
library(ggpubr)
library(tidyr)

# Indicar directorio
dir <- "~/Downloads"

# �Cu�nto tiempo de tardan desde sus casas a la universidad?
# Parece que m�s o menos ser�a:
tiempo.medio <- 60
tiempo.sd <- 20

# Ahora vamos a suponer que la distribuci�n del tiempo de desplazamiento
# requerido por los estudiantes de IME entre sus casas y la universidad
# sigue una distribuci�n normal con media y desviaci�n est�ndar especificadas
# arriba. Esto es N(60, 20).

# R tiene funciones nativas para las funciones de densidad, probabilidad y
# cuantil para las distribuciones m�s conocidas, las que se pueden acceder
# anteponiendo los prefijos d-, p- o q- al nombre (corto) de la distribuci�n
# (y proporcionando valores adecuados sus par�metros.
# Por ejemplo, para la distribuci�n nomal, se usa el nombre 'norm' y, as�, se
# tienen las funciones dnorm(), pnorm() y qnorm(), con par�metros 'mean' y
# 'sd'.

# As�, por ejemplo, si quisiera saber cu�n densa es la funci�n de probabilidad
# alrededor de los 20 minutos:
cat("f(X = 20 min.) =", dnorm(20, mean = tiempo.medio, sd = tiempo.sd), "\n")

# O saber la probabilidad de encontrar (al azar) un estudiante de IME que tarde
# menos de media hora en llegar a la universidad:
cat("P(X ??? 30 min.) =", pnorm(30, mean = tiempo.medio, sd = tiempo.sd), "\n")

# O saber cu�nto tiempo asegura que 75% de los estudiantes llegue a la
# universidad:
cat("Q(u = 75%) =", qnorm(0.75, mean = tiempo.medio, sd = tiempo.sd), "\n")

# Tambi�n se proporciona una funci�n para producir valores pseudoaleatorios
# con una distribuci�n dada, que tiene el prefijo r-.
# Luego, rnorm() es una funci�n generar n�meros que siguen una distribuci�n
# normal dada.

# As�, esta es la funci�n a usar para generar "una muestra" de la poblaci�n
# que sigue una distribuci�n normal.

# Pero primero, fijamos una semilla para la generaci�n de valores pseudo-
# aleatrorios con la finalidad de poder reproducir los resultados
# cada vez que se requieran.
set.seed(131)

# Ahora, podemos generar una muestra del tiempo que tardan
# un grupo de estudiantes de IME en viajar entre sus casas
# y la universidad.
datos1 <- rnorm(n = 18, mean = tiempo.medio, sd = tiempo.sd)

# Pero, para simular la "poblaci�n", podemos usar un n grande
set.seed(131)
pob <- rnorm(n = 25000, mean = tiempo.medio, sd = tiempo.sd)

# Coloquemos estos valores en un marco de datos en formato largo
df <- data.frame(
  Tiempo = c(datos1, pob),
  Fuente = c(rep("Muestra", length(datos1)), rep("Pob.", length(pob)))
)

# Con lo que podemos producir un gr�fico que compare la muestra
# con la poblaci�n

p <- gghistogram(
  data = df,
  x = "Tiempo",
  y = "..density..", # Esto porque el conteo es muy distinto
  bins = 31,
  fill = "Fuente"
)
p <- p + xlab("Tiempo de traslado [min.]")
p <- p + ylab("Densidad")
p <- p + ggtitle("Tiempo traslado estudiantes IME\n(Muestra y poblaci�n)")
print(p)
# La variable p tiene un histograma de la poblaci�n.


# Pero antes de que tuvieramos estas funciones en R,
# era complicado calcular densidades, probabilidades y
# cuartiles. Por eso exist�an "tablas".
# Y como no era posible hacer tablas para cualquier
# valor de par�metros, se hac�an tablas para "distribuciones
# est�dares". As�, conocimos la distribuci�n normal est�ndar
# o "distribuci�n Z" y las tablas para su densidad, probabilidad
# y cuartiles.

# Pero �c�mo podemos usar la distribuci�n Z con los tiempos de
# viaje de los estudiantes de IME?

# Tenemos que "normalizar" (o "estandarizar") nuestros datos

df.Z <- data.frame(
  Tiempo = (df[["Tiempo"]] - tiempo.medio) / tiempo.sd,
  Fuente = df[["Fuente"]]
)
p.Z <- gghistogram(
  data = df.Z,
  x = "Tiempo",
  y = "..density..", # Esto porque el conteo es muy distinto
  bins = 31,
  fill = "Fuente"
)
p.Z <- p.Z + xlab("Tiempo de traslado [Z]")
p.Z <- p.Z + ylab("Densidad")
p.Z <- p.Z + ggtitle("Tiempo de traslado normalizado")
print(p.Z)

# Podemos que ver que los datos tienen la misma "forma", pero ahora
# est�n centrados en cero (?? = 0) y desviaci�n est�ndar unitaria
# (?? = 1).

# Ahora podemos usar las tablas para determinar los mismos valores que
# obtuvimos anteriormente. Por ejemplo, para obtener P(X ??? 30 min.),
# primero estandarizamos:
z30 <- (30 - tiempo.medio) / tiempo.sd

# Y luego usamos las funciones para la distribuci�n Z est�ndar
cat("P(X ??? 30 min.) =", pnorm(z30), "(using Z)\n")


# Por supuesto existen infinitas secuencias de n valores z que en
# promedio dan cero. Pero las secuencias pueden ser muy distitntas en
# variaci�n. Por ejemplo las siguientes secuencias de 6 valores:
zetas1 <- c(-1.0, 0.5, 2, -0.5, 0.5, -1.5)
zetas2 <- c(0.15, -1.15, 4.60, -3.45, 2.15, -2.30)

# Obviamente la primera secuencia revolotea m�s cerca del valor cero
# que la segunda. Una forma de develar esta propiedad es sumar los
# cuadrados de estos valores z. Entre menor la suma, m�s cerca del
# cero (m�s cerca de la media en la distribuci�n original).

cat("SS(zetas1) =", sum(zetas1^2), "\n")
cat("SS(zetas2) =", sum(zetas2^2), "\n")

# Si uno toma la suma de todas las posibles secuencias de n valores
# z, se genera otra interesante distribuci�n de probabilidad est�ndar:
# una distribuci�n chi cuadrado con n grados de libertad.

# Claro que no vamos a producirla con nuesta poblaci�n ya que existen

cat(choose(length(pob), 6), "secuencias de 6 valores en la poblaci�n\n")

# Mejor seleccionemos aleatoriamente 30 valores (estand�rizados) de nuestra
# poblaci�n para crear una distribuci�n chi cuadrado con 6 grados de libertad:

set.seed(176)
x <- sample((pob - tiempo.medio) / tiempo.sd, 30)

# Obtenemos las secuencias
secuencias6 <- combn(x, 6)

# Y ahora las sumas del cuadrado de estas secuencias
chisq6 <- apply(secuencias6, 2, function(x) sum(x^2))

# La que podemos graficar a continuaci�n

df.chisq6 <- data.frame(
  Sumas = chisq6
)
p.chisq6 <- ggplot(df.chisq6, aes(x = Sumas))
p.chisq6 <- p.chisq6 + geom_histogram(
  aes(y = ..density..),
  binwidth = 1,
  col = "white", fill = "#000099"
)
p.chisq6 <- p.chisq6 + xlab("Sumas cuadradas de 6 valores z")
p.chisq6 <- p.chisq6 + ylab("Densidad")

# Notemos que este gr�fico es (m�s o menos) equivalente a usar
# el siguiente c�digo comentado con el paquete ggpubr.
# p2.chisq6 <- gghistogram(
#   data = df.chisq6,
#   x = "Sumas",
#   y = "..density..",
#   binwidth = 1,
#   col = "white", fill = "#000099"
# )

# Pero usamos funciones m�s b�sicas, del paquete ggplot2, para
# agregar la forma de la chi cuadrado te�rica, que solo obtendr�amos
# si us�ramos toda la distribuci�n z (o una buena aproximsaci�n si
# us�ramos la poblaci�n completa).

# Agreguemos la chi cuadrado te�rica:

f1 <- function(x, df){
  dchisq(x = x, df = df)
}

p.chisq6 <- p.chisq6 + stat_function(
  fun = f1,
  args = list(df = 6),
  color = "red", size = 2
)
p.chisq6 <- p.chisq6 + ggtitle("Sumas cuadradas [Chisq(6)]")
print(p.chisq6)


# �Pero ser� bueno usar conjuntos de 6 estudiantes? �O ser� mejor usar 3?
# Para saber, podr�amos comparar las distribuciones chi cuadrado que generan.

# Obtenemos las secuencias
secuencias3 <- combn(x, 3)

# Y ahora las sumas del cuadrado de estas secuencias
chisq3 <- apply(secuencias3, 2, function(x) sum(x^2))

# Pero tenemos un problema. Obvio que sumar 6 valores z^2 usualmente va a ser
# m�s grande que sumar 3 de estos valores.
# Es decir, necesitamos estandarizar.

chisq3.std <- chisq3 / 3
chisq6.std <- chisq6 / 6

# Una forma de comparar, que tiene sentido para los estad�sticos, es usar
# la raz�n de las sumas cuadradas estandarizadas. Si ambas sumas cuadrado son
# similares, entonces su raz�n deber�a estar alrededor de 1; en otros casos,
# la raz�n deber�a ser mayor o menor que la unidad.

# Hagamos esta divisi�n con grupos independientes (seleccionados al azar):

n <- min(length(chisq6.std), length(chisq3.std))
f.6.3 <- sample(chisq6.std, n) / sample(chisq3.std, n)

# Y grafiquemos estas razones:

df.f.6.3 <- data.frame(
  Razones = f.6.3
)
p.f.6.3 <- ggplot(df.f.6.3, aes(x = Razones))
p.f.6.3 <- p.f.6.3 + geom_histogram(
  aes(y = ..density..),
  binwidth = 0.25,
  col = "white", fill = "#000099"
)
p.f.6.3 <- p.f.6.3 +  xlim(0, 6)
p.f.6.3 <- p.f.6.3 + xlab("Raz�n de sumas cuadradas independientes")
p.f.6.3 <- p.f.6.3 + ylab("Densidad")

# El se�or Fisher (con la ayuda de un se�or Snedecor) caracteriz� hace mucho
# la distribuci�n de esta raz�n, que hoy llamamos distribuci�n F, que est�
# definida con dos par�metros: los grados de libertad de las sumas en el
# numerador (df1) y los grados de libertad de las sumas en el denominador
# (df2).

# Agreguemos la distribuci�n F te�rica:

f3 <- function(x, df1, df2){
  df(x = x, df1 = df1, df2 = df2)
}

p.f.6.3 <- p.f.6.3 + stat_function(
  fun = f3,
  args = list(df1 = 6, df2 = 3),
  color = "red", size = 2
)
p.f.6.3 <- p.f.6.3 + ggtitle("Raz�n de sumas cuadradas [F(6, 3)]")

print(p.f.6.3)

# Vemos, nuevamente, que la distribuci�n est� tomando la forma, pero que
# con tan solo unos pocos miles de casos, muestra desviaciones importantes.



#
# Pasamos ahora a recordar distribuciones discretas.
#



# Se le pregunt� a las y los estudiantes de IME si hab�an usado una
# consola de videojuegos durante el fin de semana pasado.
# Las siguientes fueron las respuestas:

consola <- c("s�", "s�", "no", "s�", "no", "s�", "no", "no", "s�", "no",
             "s�", "no", "no", "no", "s�", "no", "no", "no", "no", "no")

# As�, La siguiente es la proporci�n de estudiantes de IME que han
# usado una consola de videojuego durante un fin de semana anterior.

p <- sum(consola == "s�") / length(consola)

# Si suponemos que este estad�stico es representativo de *TODA* la
# poblaci�n de estudiantes de IME (para cualquier fin de semana del
# semestre), entonces esta ser�a la probabilidad de escoger al azar
# un/a estudiante de IME que haya usado consolas el fin de semana anterior.
# En lenguaje estad�stico, es el par�metro "probabilidad de �xito" de una
# distribuci�n (o proceso) de Bernoulli.

# Luego uno podr�a preguntarse: si elijo un grupo de 6 estudiantes de IME
# de forma aleatoria, �cu�ntos de ellos habr�n usado consolas el fin de
# semana pasado?

# Nuevamente se ha de computar todos los posibles grupos de 6 estudiantes
# de IME. Pero solo lo haremos con los actuales:

consolas6 <- combn(consola, 6)

# Ahora contamos cu�ntos �xitos se tienen en cada grupo:

exitos.consolas6 <- apply(consolas6, 2, function(x) sum(x == "s�"))

# Y dibujarlo:

df.consolas6 <- data.frame(
  Exitos = exitos.consolas6
)
p.consolas6 <- ggplot(df.consolas6, aes(x = Exitos))
p.consolas6 <- p.consolas6 + geom_histogram(
  aes(y = ..density..),
  binwidth = 1,
  col = "white", fill = "#009900"
)
p.consolas6 <- p.consolas6 + xlab("N� de estudiantes")
p.consolas6 <- p.consolas6 + ylab("Densidad")

# En teor�a, esta distribuci�n de n�mero de �xitos converge a una distribuci�n
# binomial con par�metros p y n, la que podemos agregar al gr�fico:

p.consolas6 <- p.consolas6 + geom_step(
  mapping = aes(x = x, y = y),
  data = data.frame(x = c(-0.5, (0:6)-0.5), y = c(0, dbinom(0:6, 6, p))),
  color = "red", size = 2
)
tit <- sprintf("N� de estudiantes que jugaron consola en conjuntos de 6 estudiantes\nB[%d, %.2f]", 6, p)
p.consolas6 <- p.consolas6 + ggtitle(tit)

print(p.consolas6)


# Otra forma de ver estas frecuencias es preguntarse algo similar:
# �cu�ntos estudiante debo consultar hasta encontrar uno que haya jugado
# consolas el fin de semana anterior?

# En este caso, debemos obtener todas las formas diferentes de ordenar
# nuestra poblaci�n de 20 estudiantes de IME (sus  permutaciones) y registrar
# la posici�n del primer estudiante que responda "s�".
# Nuevamente esto es computacionalmente prohibitivo, ya que una muestra de n
# elementos tiene n! permutaciones. As�, existen 2.432.902.008.176.640.000
# formas de ordenas los 20 estudiantes de IME.
# Una alternativa es usar "muchas" muestras de la poblaci�n.
# Por ejemplo, 5.000:

n <- 5000
set.seed(37)
perms <- sapply(1:n, function(i) sample(consola))

# Y ahora obtenermos un vector con la posici�n de primer s� en cada caso
geom <- apply(perms, 2, function(x) which(x == "s�")[1])

# Y dibujarlas:

df.geom <- data.frame(
  Intentos = geom
)
p.geom <- ggplot(df.geom, aes(x = Intentos))
p.geom <- p.geom + geom_histogram(
  aes(y = ..density..),
  binwidth = 1,
  col = "white", fill = "#009900"
)
p.geom <- p.geom + xlab("N� de intentos antes de un 's�'")
p.geom <- p.geom + ylab("Densidad")

# En teor�a, esta distribuci�n del "n� de intentos" antes de encontrar el
# primer �xito en un proceso Bernoulli tiende a la llamada distribuci�n
# geom�trica, con par�metro p, la que podemos agregar tambi�n al gr�fico:

p.geom <- p.geom + geom_step(
  mapping = aes(x = x, y = y),
  data = data.frame(x = c(0.5, (1:10)+0.5), y = dgeom(0:10, p)),
  color = "red", size = 2
)
tit <- sprintf("N� de intentos antes de encontrar un estudiante que jug� consola\nG[%.2f]", p)
p.geom <- p.geom + ggtitle(tit)

print(p.geom)



# Solo queda dejar unos desaf�os:
# 1. �C�mo generar distribuciones t a partir de poblaci�n de tiempo de viaje?
# 2. �Cu�ntos estudiantes jugadores encontraremos hasta juntar 6 estudiantes
#    que no jugaron consola? �Qu� curva te�rica tiene esta?

