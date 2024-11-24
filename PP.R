install.packages("ppcor")
library(readxl)
library(ppcor)
#cargar la base de datos
B_AC <- read_excel("Base_RF_CYG2024.xlsx")
# Seleccionar las columnas necesarias: País, Año y las últimas 7 variables
B_AC_subset <- B_AC[, c("País", "Año", tail(names(B_AC), 7))]
PXA<- aggregate(. ~ País + Año, data = B_AC_subset, FUN = mean)
head(PXA)
#Analisis de los grupos por año y por país
names(B_AC) <- make.names(names(B_AC))
variables <- tail(names(B_AC), 7)
resultados_kw <- lapply(variables, function(var) {
  kruskal.test(as.formula(paste(var, "~ interaction(País, Año)")), data = B_AC)
})
names(resultados_kw) <- variables
resultados_kw
resultados_posthoc <- lapply(variables, function(var) {
  pairwise.wilcox.test(B_AC[[var]], interaction(B_AC$País, B_AC$Año), 
                       p.adjust.method = "bonferroni")
})
names(resultados_posthoc) <- variables
resultados_posthoc
# Ahora, graficamos los boxplots usando la nueva columna 'País_Año'
B_AC_subset$País_Año <- factor(paste(B_AC_subset$País, B_AC_subset$Año, sep = " - "))
levels(B_AC_subset$País_Año)
# Boxplot para Desvin
ggplot(B_AC_subset, aes(x = País_Año, y = Desvin, color = País_Año)) +
  geom_boxplot() +
  labs(title = "Boxplot de Desvin por País y Año", x = "País y Año", y = "Desvin") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Boxplot para Part_Civil
ggplot(B_AC_subset, aes(x = País_Año, y = Part_Civil, color = País_Año)) +
  geom_boxplot() +
  labs(title = "Boxplot de Part_Civil por País y Año", x = "País y Año", y = "Part_Civil") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Boxplot para Part_Formal
ggplot(B_AC_subset, aes(x = País_Año, y = Part_Formal, color = País_Año)) +
  geom_boxplot() +
  labs(title = "Boxplot de Part_Formal por País y Año", x = "País y Año", y = "Part_Formal") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Boxplot para Activismo
ggplot(B_AC_subset, aes(x = País_Año, y = Activismo, color = País_Año)) +
  geom_boxplot() +
  labs(title = "Boxplot de Activismo por País y Año", x = "País y Año", y = "Activismo") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Boxplot para AutoE
ggplot(B_AC_subset, aes(x = País_Año, y = AutoE, color = País_Año)) +
  geom_boxplot() +
  labs(title = "Boxplot de AutoE por País y Año", x = "País y Año", y = "AutoE") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Boxplot para Rup-Tej
ggplot(B_AC_subset, aes(x = País_Año, y = `Rup-Tej`, color = País_Año)) +
  geom_boxplot() +
  labs(title = "Boxplot de Rup-Tej por País y Año", x = "País y Año", y = "Rup-Tej") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Boxplot para Rupt_Lider
ggplot(B_AC_subset, aes(x = País_Año, y = Rupt_Lider, color = País_Año)) +
  geom_boxplot() +
  labs(title = "Boxplot de Rupt_Lider por País y Año", x = "País y Año", y = "Rupt_Lider") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Correlaciones parciales
library(ppcor)
variables_interes <- c("Desvin", "Part_Civil", "Part_Formal", "Activismo", "AutoE", "Rup-Tej", "Rupt_Lider")
B_AC_subset[variables_interes] <- lapply(B_AC_subset[variables_interes], as.numeric)
B_AC_subset$País <- as.numeric(as.factor(B_AC_subset$País))
B_AC_subset$Año <- as.numeric(as.factor(B_AC_subset$Año))
resultados_correlaciones <- list()
for (i in 1:(length(variables_interes)-1)) {
  for (j in (i+1):length(variables_interes)) {
    var_x <- variables_interes[i]
    var_y <- variables_interes[j]
    correlacion_parcial <- pcor.test(B_AC_subset[[var_x]], B_AC_subset[[var_y]], 
                                     z = c(B_AC_subset$País, B_AC_subset$Año), 
                                     method = "spearman")
    resultados_correlaciones[[paste(var_x, var_y, sep = "_vs_")]] <- correlacion_parcial
  }
}
resultados_correlaciones



