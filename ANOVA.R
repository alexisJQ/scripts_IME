library(ggpubr)
library(knitr)
library(tidyr)


# Definimos los datos
duraciones <- c("0 día", "2 días", "4 días", "6 días")
t1 <- c(26, 27, 28, 28, 33)
t2 <- c(22, 23, 24, 27, 27)
t3 <- c(19, 20, 21, 23, 27)
t4 <- c(19, 20, 23, 24, 24)
dx4 <- list(t1, t2, t3, t4)
datos.wide <- data.frame(dx4)
colnames(datos.wide) <- duraciones


# Pero las rutinas para graficar requieren los datos en formalo
# largo (long).
dl <- gather(
  data = datos.wide,
  key = "Duración",
  value = "Errores",
  duraciones
)
dl[["Duración"]] <- factor(dl[["Duración"]])


# ¿Cuáles serían las hipótesis en este caso? 
# Usemos las definiciones en la sección 5.5 de OpenIntro Statistics:
# H0: The mean outcome is the same across all groups.
# HA: At least one mean is different.
# 
# Luego, en este caso:
# H0: La media del nº de pruebas unitarias falladas por sprint es la
#     misma en todos los grupos de desarrolladores 
# HA: La media de pruebas falladas de al menos un grupo de
#     desarrolladores es distinta 


# ----------------------------------------------------
# Procedimiento manual del capítulo 13 de VarssarStats
# ----------------------------------------------------

# Contamos observaciones por grupo y en total
N.por.grupo <- apply(datos.wide, 2, length)
N.total <- sum(N.por.grupo)
k <- ncol(datos.wide)

# Obtenemos la media de cada grupo y media global
media.por.grupo <- apply(datos.wide, 2, mean)
media.total <- mean(dl[["Errores"]])

# Obtenemos la suma de las desviaciones cuadradas observadas en cada
# grupo y globalmente.
SS.en.cada.grupo <- sapply(
  1:k,
  function(i) sum((datos.wide[, i] - media.por.grupo[i])^2)
)
SS.total <- sum((dl[["Errores"]] - media.total)^2)

# Obtenemos la suma de las desviaciones cuadradas al interior de los
# grupos.
SS.wg <- sum(SS.en.cada.grupo)

# Y podríamos obtener la suma de las desviaciones cuadradas entre los
# grupos: SS.bg <- SS.total - SS.wg

# Pero queda conceptualmente más claro si repetimos el procedimiento
# trabajando con las desviaciones de las medias de cada grupo.
S.de.cada.grupo <- (media.por.grupo - media.total)^2
S.ponderada.de.cada.grupo <- N.por.grupo * S.de.cada.grupo
SS.bg <- sum(S.ponderada.de.cada.grupo)

# Ahora, los grados de libertad
df.bg <- ncol(datos.wide) - 1
df.wg <- sum(N.por.grupo - 1)
# o equivalentemente: df.wg <- N.total - ncol(datos.wide)

# Podemos obtener las medias cuadradas
MS.bg <- SS.bg / df.bg
MS.wg <- SS.wg / df.wg

# Y ahora el estadístico
F <- MS.bg / MS.wg

# Ahora obtenemos un p-valor
Pr <- 1 -pf(F, df.bg, df.wg)
pv <- round(Pr, 3)
if(pv < 0.001) {
  pvs <- "<.001"
} else {
  pvs <- sub(pattern = "0.", replacement=".", x = sprintf("%1.3f", pv))
}


# Creamos una tabla con esta información
Source <- c("Between groups (effect)", "Within groups (error)", "TOTAL")
Df <- c(df.bg, df.wg, N.total - 1)
P <- c(pvs, "   ", "   ")

r1 <- round(c(SS.bg, MS.bg, F), 2)
r2 <- round(c(SS.wg, MS.wg, 0), 2)
r3 <- round(c(SS.total, 0, 0), 2)
rb <- rbind(r1, r2, r3)
colnames(rb) <- c("SS", "MS", "F")

tabla.aov <- data.frame(Source, Df, rb, P)
rownames(tabla.aov) <- NULL
# Y le damos un formato bonito
kt <- kable(
  tabla.aov,
  format = "pandoc",
  format.args = list(zero.print = FALSE)
)

cat("\n\n")
cat("Tabla ANOVA construida según VarssarStats\n")
cat("-----------------------------------------")
print(kt)
cat("\n\n")

