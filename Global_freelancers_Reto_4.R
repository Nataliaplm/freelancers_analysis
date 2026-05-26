# =============================================================================
# Práctica 2 - Tipología y Ciclo de Vida de los Datos
# Dataset: Global Freelancers (1000 freelancers)
# Autores:
#   Natalia Palomo Fuentes  - npalomof@uoc.edu
#   Guillermo Alfaro Cerda  - galfaroce@uoc.edu
# =============================================================================
# install.packages("factoextra")
library(factoextra)

df <- read.csv("global_freelancers_raw.csv")
head(df)
dim(df)
str(df)
summary(df)
df[df == "" | trimws(df) == ""] <- NA
colSums(is.na(df))

# =============================================================================
# Apartado 3.1 - Análisis y gestión de variables
# =============================================================================

# RATING - eliminar columna
df$rating <- NULL
names(df)

# YEARS OF EXPERIENCE - imputar NAs con la mediana
sum(is.na(df$years_of_experience))
hist(df$years_of_experience, 
     main = "Distribución de Years of Experience", 
     xlab = "Años de experiencia",
     ylab = "Frecuencia",
     col = "lightblue",
     border = "black")
# Solucionamos
mediana_exp <- median(df$years_of_experience, na.rm = TRUE)
df$years_of_experience[is.na(df$years_of_experience)] <- mediana_exp
df$years_of_experience <- round(df$years_of_experience, 0)
sum(is.na(df$years_of_experience))
summary(df$years_of_experience)

# EDAD - imputar NAs con la media
sum(is.na(df$age))
hist(df$age, 
     main = "Distribución de Edad", 
     xlab = "Edad",
     ylab = "Frecuencia",
     col = "lightblue",
     border = "black")
boxplot(df$age,
        main = "Boxplot de Edad",
        ylab = "Edad")
#Solucionamos
media_edad <- mean(df$age, na.rm = TRUE)
df$age[is.na(df$age)] <- media_edad
df$age <- round(df$age, 0) #entero
sum(is.na(df$age))

# HOURLY RATE - limpiar formato, convertir a numérico, imputar NAs y simplificar nombre
sum(is.na(df$hourly_rate..USD.))
df$hourly_rate..USD. <- as.numeric(gsub("[^0-9.]", "", df$hourly_rate..USD.))
mediana_rate <- median(df$hourly_rate..USD., na.rm = TRUE)
df$hourly_rate..USD.[is.na(df$hourly_rate..USD.)] <- mediana_rate
df$hourly_rate..USD. <- round(df$hourly_rate..USD., 2)  #2 decimales
names(df)[names(df) == "hourly_rate..USD."] <- "hourly_rate"
hist(df$hourly_rate, 
     main = "Distribución de Hourly Rate (antes de imputar)", 
     xlab = "Tarifa por hora (USD)",
     ylab = "Frecuencia",
     col = "lightblue",
     border = "black")
sum(is.na(df$hourly_rate))
summary(df$hourly_rate)

# NAME(hay duplicados)- Eliminamos filas duplicadas dejando nombres iguales
nrow(df)
df <- df[!duplicated(df), ]  
nrow(df)


# GENDER - estandarizar a male / female
sum(is.na(df$gender)) #Comprobamos que no tiene
df$gender <- tolower(trimws(df$gender))
df$gender[df$gender %in% c("m", "man", "male")] <- "male"
df$gender[df$gender %in% c("f", "woman", "female")] <- "female"
df$gender[!df$gender %in% c("male", "female")] <- NA
moda_gender <- names(sort(table(df$gender), decreasing = TRUE))[1]
df$gender[is.na(df$gender)] <- moda_gender
df$gender <- as.factor(df$gender)
table(df$gender)

# IS_ACTIVE - estandarizar a YES / NO
sum(is.na(df$is_active))
df$is_active <- tolower(trimws(df$is_active))
df$is_active[df$is_active %in% c("1", "y", "yes", "true")] <- "YES"
df$is_active[df$is_active %in% c("0", "n", "no", "false")] <- "NO"
df$is_active[!df$is_active %in% c("YES", "NO")] <- NA
moda_active <- names(sort(table(df$is_active), decreasing = TRUE))[1]
df$is_active[is.na(df$is_active)] <- moda_active
df$is_active <- as.factor(df$is_active)
sum(is.na(df$is_active))
table(df$is_active)

# CLIENT_SATISFACTION - limpiar %, convertir a numérico e imputar con media
sum(is.na(df$client_satisfaction))
df$client_satisfaction <- as.numeric(gsub("%", "", df$client_satisfaction))
hist( df$client_satisfaction,
      main = "Distribución de Client Satisfaction",
      xlab = "Satisfacción (%)",
      ylab = "Frecuencia",
      col = "lightblue",
      border = "black",
      xlim = c(0, 100))
media_satisfaction <- mean(df$client_satisfaction, na.rm = TRUE)
df$client_satisfaction[is.na(df$client_satisfaction)] <- media_satisfaction
df$client_satisfaction <- round(df$client_satisfaction, 0)  #entero
sum(is.na(df$client_satisfaction))
summary(df$client_satisfaction)

# Distribución de primary_skill
par(mar = c(10, 4, 4, 2))
barplot(sort(table(df$primary_skill), decreasing = TRUE),
        main  = "Distribución de habilidades principales",
        xlab  = "",
        ylab  = "Frecuencia",
        col   = "lightblue",
        border = "white",
        las   = 2,
        cex.names = 0.8)
par(mar = c(5, 4, 4, 2))

# -----------------------------------------------------------------------------
# VERIFICACIÓN FINAL
# -----------------------------------------------------------------------------
dim(df)
str(df)
colSums(is.na(df))
summary(df)

# -----------------------------------------------------------------------------
# EXPORTAR DATASET LIMPIO
# -----------------------------------------------------------------------------
# Session → Set Working Directory → To Source File Location (agregar al README)
write.csv(df, "global_freelancers_clean.csv", row.names = FALSE)


# =============================================================================
# Apartado 3.2 - Gestión de atributos
# =============================================================================
# Originales
str(df)

# Convertir categóricas a factor (para que R las trate como categorías, no como texto)
df$country <- as.factor(df$country)
df$language <- as.factor(df$language)
df$primary_skill <- as.factor(df$primary_skill)

# Verificar cambio
str(df)

# =============================================================================
# Apartado 3.3 - Valores extremos (outliers)
# =============================================================================

# Método del rango intercuartil (IQR) para detectar outliers
detectar_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  limite_inferior <- Q1 - 1.5 * IQR
  limite_superior <- Q3 + 1.5 * IQR
  return(x < limite_inferior | x > limite_superior)
}

# Aplicar a variables numéricas
outliers_age <- detectar_outliers(df$age)
outliers_years <- detectar_outliers(df$years_of_experience)
outliers_rate <- detectar_outliers(df$hourly_rate)
outliers_sat <- detectar_outliers(df$client_satisfaction)

# Mostrar resultados
cat("Outliers detectados:\n")
cat("Edad:", sum(outliers_age), "\n")
cat("Años de experiencia:", sum(outliers_years), "\n")
cat("Tarifa por hora:", sum(outliers_rate), "\n")
cat("Satisfacción:", sum(outliers_sat), "\n")

# Boxplots para visualizar
boxplot(df$age, df$years_of_experience, df$hourly_rate, df$client_satisfaction,
        names = c("Edad", "Experiencia", "Tarifa", "Satisfacción"),
        col = "lightblue",
        main = "Detección de valores extremos")

#No se elimina outliers, debe ser el mismo dataset

# =============================================================================
# 4.1 Modelo supervisado: Regresión Lineal
# =============================================================================

# Crear modelo
modelo_supervisado <- lm(client_satisfaction ~ age + years_of_experience + hourly_rate + gender, 
                         data = df)

# Resultados
cat("=== MODELO SUPERVISADO (REGRESIÓN LINEAL) ===\n")
cat("Variable objetivo: client_satisfaction\n")
cat("Predictores: age, years_of_experience, hourly_rate, gender\n\n")
summary(modelo_supervisado)

# =============================================================================
# 4.1 Modelo no supervisado: Clustering jerárquico
# Segmentar freelancers por edad, experiencia y tarifa
# =============================================================================

# Datos para clustering (solo numéricas)
datos_cluster <- df[, c("age", "years_of_experience", "hourly_rate", "client_satisfaction")]

# Escalamos los datos
datos_escalados <- scale(datos_cluster)

# Determnar número óptimo de clusters (método del codo)

wss <- sapply(1:10, function(k) {
  kmeans(datos_escalados, centers = k, nstart = 25)$tot.withinss
})

#Gráfica de codo
plot(1:10, wss, type = "b", pch = 19, col = "steelblue",
     main = "Método del codo",
     xlab = "Número de clusters",
     ylab = "WSS (varianza intra-cluster)")


# Probamos con clustering jerárquico
# Matriz de distancias y clustering jerárquico
distancia <- dist(datos_escalados, method = "euclidean")
hc <- hclust(distancia, method = "ward.D2")

# --- K = 3 ---
plot(hc, main = "Dendrograma de freelancers",
     xlab = "Freelancers", ylab = "Distancia",
     cex = 0.6, hang = -1)
rect.hclust(hc, k = 3, border = c("red", "blue", "green"))
df$cluster <- as.factor(cutree(hc, k = 3))
cat("Centroides de los clusters (k=3):\n")
centroides <- aggregate(datos_cluster, by = list(Cluster = df$cluster), FUN = mean)
print(centroides)

# --- K = 4 ---
plot(hc, main = "Dendrograma de freelancers",
     xlab = "Freelancers", ylab = "Distancia",
     cex = 0.6, hang = -1)
rect.hclust(hc, k = 4, border = c("red", "blue", "green", "purple"))
df$cluster4 <- as.factor(cutree(hc, k = 4))
cat("Centroides de los clusters (k=4):\n")
centroides4 <- aggregate(datos_cluster, by = list(Cluster = df$cluster4), FUN = mean)
print(centroides4)

# =============================================================================
# 4.2 Pruebas de hipótesis
# =============================================================================
#TEST 1 
#Analizar la diferencia significativa de la tarifa por hora entre hombres y mujeres
#--- Paso 1: Verificar normalidad (Shapiro-Wilk) ---
shapiro.test(df$hourly_rate[df$gender == "male"])
shapiro.test(df$hourly_rate[df$gender == "female"])

# --- Paso 2: Verificar homocedasticidad (Levene) ---
library(car)
leveneTest(hourly_rate ~ gender, data = df)

# --- Paso 3: Test según resultados anteriores ---
# Si normalidad y homocedasticidad → t-test
t.test(hourly_rate ~ gender, data = df, var.equal = TRUE)

# Si no hay normalidad → Wilcoxon (no paramétrico)
wilcox.test(hourly_rate ~ gender, data = df)

# Visualización
boxplot(hourly_rate ~ gender, data = df,
        main = "Tarifa por hora según género",
        xlab = "Género",
        ylab = "Tarifa por hora (USD)",
        col  = c("lightpink", "lightblue"))

# Gráfico de densidad por género
plot(density(df$hourly_rate[df$gender == "male"]),
     main = "Distribución de tarifa por hora según género",
     xlab = "Tarifa por hora (USD)",
     ylab = "Densidad",
     col  = "steelblue",
     lwd  = 2,
     ylim = c(0, 0.025))
lines(density(df$hourly_rate[df$gender == "female"]),
      col = "lightpink",
      lwd = 2)
legend("topright",
       legend = c("Male", "Female"),
       col    = c("steelblue", "lightpink"),
       lwd    = 2)

#TEST 2
#Analizar la diferencia significativa de la satisfacción entre hombres y mujeres
# --- Paso 1: Verificar normalidad (Shapiro-Wilk) ---
shapiro.test(df$client_satisfaction[df$gender == "male"])
shapiro.test(df$client_satisfaction[df$gender == "female"])

# --- Paso 2: Verificar homocedasticidad (Levene) ---
leveneTest(client_satisfaction ~ gender, data = df)

# --- Paso 3: Test según resultados anteriores ---
# Si normalidad y homocedasticidad → t-test
t.test(client_satisfaction ~ gender, data = df, var.equal = TRUE)

# Si no hay normalidad → Wilcoxon (no paramétrico)
wilcox.test(client_satisfaction ~ gender, data = df)

# Visualización
boxplot(client_satisfaction ~ gender, data = df,
        main = "Satisfacción del cliente según género",
        xlab = "Género",
        ylab = "Satisfacción (%)",
        col  = c("lightpink", "lightblue"))

# Gráfico de densidad por género
plot(density(df$client_satisfaction[df$gender == "male"]),
     main = "Distribución de satisfacción según género",
     xlab = "Satisfacción (%)",
     ylab = "Densidad",
     col  = "steelblue",
     lwd  = 2,
     ylim = c(0, 0.05))
lines(density(df$client_satisfaction[df$gender == "female"]),
      col = "lightpink",
      lwd = 2)
legend("topright",
       legend = c("Male", "Female"),
       col    = c("steelblue", "lightpink"),
       lwd    = 2)