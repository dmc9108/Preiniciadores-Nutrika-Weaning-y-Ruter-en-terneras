# Terneras granja lechera Saavedra

library(readxl)
tern <- read_excel("~/MEGA/Bases de datos/Preiniciadores en terneras/tern.xlsx", 
                   range = "A1:AF16")
View(tern)

rm(tern)

# Alimentos Nutrika y Ruter
barplot(table(tern$Alimento), main="Alimento",
        xlab="Alimentos", ylab="cantidad", 
        col=rainbow(length(table(tern$Alimento))))

help("hist") ayuda

# Peso al nacimiento
hist (x = tern$`Peso al nacimiento (kg)`, xlab= "Peso al nacimiento", ylab="Frecuencia",
      main = "Peso al nacimiento", col = "blue")
hist (x = tern$`Peso al nacimiento (kg)`, xlab= "Peso al nacimiento", ylab="Frecuencia",
      main = "Peso al nacimiento", col = "blue", 
      grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
hist (x = tern$`Peso al nacimiento (kg)`, xlab= "Peso al nacimiento", ylab="Frecuencia",
      main = "Peso al nacimiento", col = "blue", add = TRUE)
      
barplot(table(tern$Alimento,tern$`Peso al nacimiento (kg)`), 
        main="Peso al nacimiento por grupo", xlab="Peso", ylab="frecuencia", 
        col=c("blue","red"), 
        legend.text = rownames(table(tern$Alimento,tern$`Peso al nacimiento (kg)`)))
barplot(table(tern$Alimento,tern$`Peso al nacimiento (kg)`), 
        main="Peso al nacimiento por grupo", xlab="Peso", ylab="frecuencia", 
        col=c("blue","red"), 
        legend.text = rownames(table(tern$Alimento,tern$`Peso al nacimiento (kg)`)),
        grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
barplot(table(tern$Alimento,tern$`Peso al nacimiento (kg)`), 
        main="Peso al nacimiento por grupo", xlab="Peso", 
        ylab="frecuencia", col=c("blue","red"), 
        legend.text = rownames(table(tern$Alimento,tern$`Peso al nacimiento (kg)`)),
        add = TRUE)

pesnacprom <- tapply(X = tern$`Peso al nacimiento (kg)`, INDEX = tern$Alimento, FUN = mean)

barplot(pesnacprom, main="Peso promedio al nacimiento por grupo", 
        xlab="Peso promedio al nacimiento", ylab="Frecuencia", col=c("blue","red"),
        ylim = c(0, 50))
barplot(pesnacprom, main="Peso promedio al nacimiento por grupo", 
        xlab="Peso promedio al nacimiento", ylab="Frecuencia", col=c("blue","red"),
        ylim = c(0, 50), grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
barplot(pesnacprom, main="Peso promedio al nacimiento por grupo", 
        xlab="Peso promedio al nacimiento", ylab="Frecuencia", col=c("blue","red"),
        ylim = c(0, 50), add = TRUE)

library("FinCal") # Paquete coeficiente de variaci??n
library("psych") # Paquete asimetr??a y curtosis

mean(tern$`Peso al nacimiento (kg)`)
median(tern$`Peso al nacimiento (kg)`)
range(tern$`Peso al nacimiento (kg)`)
sd(tern$`Peso al nacimiento (kg)`)
coefficient.variation(sd=sd(tern$`Peso al nacimiento (kg)`), 
                      avg = mean(tern$`Peso al nacimiento (kg)`))
skew(tern$`Peso al nacimiento (kg)`)
kurtosi(tern$`Peso al nacimiento (kg)`)
skew(tern$`Peso al nacimiento (kg)`)/sqrt(6/15) 
kurtosi(tern$`Peso al nacimiento (kg)`)/sqrt(6/15) 

summary(tern$`Peso al nacimiento (kg)`)

shapiro.test(tern$`Peso al nacimiento (kg)`)

tapply(X = tern$`Peso al nacimiento (kg)`, INDEX = tern$Alimento, FUN = shapiro.test) # Test de normalidad

Peso_nacimiento_nutrika <- subset(tern$`Peso al nacimiento (kg)`, tern$Alimento=="Nutrika")
Peso_nacimiento_ruter <- subset(tern$`Peso al nacimiento (kg)`, tern$Alimento=="Ruter")

bartlett.test(list(Peso_nacimiento_nutriKa, Peso_nacimiento_ruter)) # Test de igualdad de varianzas


boxplot(Peso_nacimiento_nutrika, Peso_nacimiento_ruter, main = "Peso al nacimiento", 
        xlab = "Alimento", ylab = "Peso al nacimiento", col = c("blue","red"), 
        names = c("Nutrika", "Ruter"))
boxplot(Peso_nacimiento_nutrika, Peso_nacimiento_ruter, main = "Peso al nacimiento", 
        xlab = "Alimento", ylab = "Peso al nacimiento", col = c("blue","red"), 
        names = c("Nutrika", "Ruter"),
        grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
boxplot(Peso_nacimiento_nutrika, Peso_nacimiento_ruter, main = "Peso al nacimiento", 
        xlab = "Alimento", ylab = "Peso al nacimiento", col = c("blue","red"), 
        names = c("Nutrika", "Ruter"),  add = TRUE)


help("wilcox.test")
wilcox.test(Peso_nacimiento_ruter, Peso_nacimiento_nutrika, paired = FALSE, 
            alternative = "two.sided", conf.level = 0.95) # Prueba de Mann-Whitney-Wilcoxon

tapply(X = tern$`Peso al nacimiento (kg)`, INDEX = tern$Alimento, FUN = median)

library("tidyverse")
install.packages("car")
install.packages("ggpubr")

# Peso al destete

hist (x = tern$`Peso al destete (Kg)`, xlab= "Peso al destete", ylab="Frecuencia",
      main = "Peso al destete", col = "blue")
hist (x = tern$`Peso al destete (Kg)`, xlab= "Peso al destete", ylab="Frecuencia",
      main = "Peso al destete", col = "blue", 
      grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
hist (x = tern$`Peso al destete (Kg)`, xlab= "Peso al destete", ylab="Frecuencia",
      main = "Peso al destete", col = "blue", add = TRUE)


barplot(table(tern$Alimento,tern$`Peso al destete (Kg)`), 
        main="Peso al destete por grupo", xlab="Peso", ylab="Frecuencia", 
        col=c("blue","red"), 
        legend.text = rownames(table(tern$Alimento,tern$`Peso al destete (Kg)`)))
barplot(table(tern$Alimento,tern$`Peso al destete (Kg)`), 
        main="Peso al destetepor grupo", xlab="Peso", ylab="Frecuencia", 
        col=c("blue","red"), 
        legend.text = rownames(table(tern$Alimento,tern$`Peso al destete (Kg)`)),
        grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
barplot(table(tern$Alimento,tern$`Peso al destete (Kg)`), 
        main="Peso al destete por grupo", xlab="Peso", 
        ylab="Frecuencia", col=c("blue","red"), 
        legend.text = rownames(table(tern$Alimento,tern$`Peso al destete (Kg)`)),
        add = TRUE)


pesdestprom <- tapply(X = tern$`Peso al destete (Kg)`, INDEX = tern$Alimento, FUN = mean)


barplot(pesdestprom, main="Peso promedio al destete por grupo", 
        xlab="Peso promedio al destete", ylab="Frecuencia", col=c("blue","red"),
        ylim = c(0, 100))
barplot(pesdestprom, main="Peso promedio al destete por grupo",
        xlab="Peso promedio al destete", ylab="Frecuencia", col=c("blue","red"),
        ylim = c(0, 100), grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
barplot(pesdestprom, main="Peso promedio al destete por grupo", 
        xlab="Peso promedio al destete", ylab="Frecuencia", col=c("blue","red"),
        ylim = c(0, 100), add = TRUE)

mean(tern$`Peso al destete (Kg)`)
median(tern$`Peso al destete (Kg)`)
range(tern$`Peso al destete (Kg)`)
sd(tern$`Peso al destete (Kg)`)
coefficient.variation(sd=sd(tern$`Peso al destete (Kg)`), 
                      avg = mean(tern$`Peso al destete (Kg)`))
skew(tern$`Peso al destete (Kg)`)
kurtosi(tern$`Peso al destete (Kg)`)
skew(tern$`Peso al destete (Kg)`)/sqrt(6/15) 
kurtosi(tern$`Peso al destete (Kg)`)/sqrt(6/15) 

summary(tern$`Peso al destete (Kg)`)

shapiro.test(tern$`Peso al destete (Kg)`)

tapply(X = tern$`Peso al destete (Kg)`, INDEX = tern$Alimento, FUN = shapiro.test) # Test de normalidad

Peso_destete_nutrika <- subset(tern$`Peso al destete (Kg)`, tern$Alimento=="Nutrika")
Peso_destete_ruter <- subset(tern$`Peso al destete (Kg)`, tern$Alimento=="Ruter")

bartlett.test(list(Peso_destete_nutrika, Peso_destete_ruter)) # Test de igualdad de varianzas

boxplot(Peso_destete_nutrika, Peso_destete_ruter, main = "Peso al destete", 
        xlab = "Alimento", ylab = "Peso al destete", col = c("blue","red"), 
        names = c("Nutrika", "Ruter"))
boxplot(Peso_destete_nutrika, Peso_destete_ruter, main = "Peso al destete", 
        xlab = "Alimento", ylab = "Peso al destete", col = c("blue","red"), 
        names = c("Nutrika", "Ruter"),
        grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
boxplot(Peso_destete_nutrika, Peso_destete_ruter, main = "Peso al destete", 
        xlab = "Alimento", ylab = "Peso al destete", col = c("blue","red"), 
        names = c("Nutrika", "Ruter"),  add = TRUE)

wilcox.test(Peso_destete_ruter, Peso_destete_nutrika, paired = FALSE, 
            alternative = "two.sided", conf.level = 0.95) # Prueba de Mann-Whitney-Wilcoxon

tapply(X = tern$`Peso al destete (Kg)`, INDEX = tern$Alimento, FUN = median)


# Ganancia de peso al destete


hist (x = tern$`Ganancia de peso al destete (kg)`, xlab= "Ganancia de peso", ylab="Frecuencia",
      main = "Ganancia de peso al destete", col = "blue")
hist (x = tern$`Ganancia de peso al destete (kg)`, xlab= "Ganancia de peso", ylab="Frecuencia",
      main = "Ganancia de peso al destete", col = "blue", 
      grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
hist (x = tern$`Ganancia de peso al destete (kg)`, xlab= "Ganancia de peso", ylab="Frecuencia",
      main = "Ganancia de peso al destete", col = "blue", add = TRUE)

barp <- barplot(table(tern$Alimento, tern$`Ganancia de peso al destete (kg)`), 
                main="Ganancia de peso al destete", xlab="Peso", ylab="Frecuencia", 
                col=c("blue","red"), 
                legend.text = rownames(table(tern$Alimento, tern$`Ganancia de peso al destete (kg)`)))
barp <- barplot(table(tern$Alimento, tern$`Ganancia de peso al destete (kg)`), 
                main="Ganancia de peso al destete", xlab="Peso", ylab="Frecuencia", 
                col=c("blue","red"), 
                legend.text = rownames(table(tern$Alimento, tern$`Ganancia de peso al destete (kg)`)),
                grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
barp <- barplot(table(tern$Alimento, tern$`Ganancia de peso al destete (kg)`), 
                main="Ganancia de peso al destete", xlab="Peso", 
                ylab="Frecuencia", col=c("blue","red"), 
                legend.text = rownames(table(tern$Alimento, tern$`Ganancia de peso al destete (kg)`)),
                add = TRUE)
                
                
ganpesdestprom <- tapply(X = tern$`Ganancia de peso al destete (kg)`, 
                         INDEX = tern$Alimento, FUN = mean)
                

barplot(ganpesdestprom, main="Ganancia de peso promedio al destete por grupo", 
        xlab="Ganancia de peso promedio", ylab="Frecuencia", col=c("blue","red"),
        ylim = c(0, 60))
barplot(ganpesdestprom, main="Ganancia de peso promedio al destete por grupo", 
        xlab="Ganancia de peso promedio", ylab="Frecuencia", col=c("blue","red"),
        ylim = c(0, 60), grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
barplot(ganpesdestprom, main="Ganancia de peso promedio al destete por grupo", 
        xlab="Ganancia de peso promedio", ylab="Frecuencia", col=c("blue","red"),
        ylim = c(0, 60), add = TRUE)

mean(tern$`Ganancia de peso al destete (kg)`)
median(tern$`Ganancia de peso al destete (kg)`)
range(tern$`Ganancia de peso al destete (kg)`)
sd(tern$`Ganancia de peso al destete (kg)`)
coefficient.variation(sd=sd(tern$`Ganancia de peso al destete (kg)`), 
                      avg = mean(tern$`Ganancia de peso al destete (kg)`))
skew(tern$`Ganancia de peso al destete (kg)`)
kurtosi(tern$`Ganancia de peso al destete (kg)`)
skew(tern$`Ganancia de peso al destete (kg)`)/sqrt(6/15) 
kurtosi(tern$`Ganancia de peso al destete (kg)`)/sqrt(6/15) 

summary(tern$`Ganancia de peso al destete (kg)`)

shapiro.test(tern$`Ganancia de peso al destete (kg)`)

tapply(X = tern$`Ganancia de peso al destete (kg)`, INDEX = tern$Alimento, FUN = shapiro.test) # Test de normalidad

Ganancia_peso_nutrika <- subset(tern$`Ganancia de peso al destete (kg)`, tern$Alimento=="Nutrika")
Ganancia_peso_ruter <- subset(tern$`Ganancia de peso al destete (kg)`, tern$Alimento=="Ruter")

bartlett.test(list(Ganancia_peso_nutrika, Ganancia_peso_ruter)) # Test de igualdad de varianzas


boxplot(Ganancia_peso_nutrika, Ganancia_peso_ruter, main = "Ganancia de peso al destete", 
        xlab = "Alimento", ylab = "Ganancia de peso al destete", col = c("blue","red"), 
        names = c("Nutrika", "Ruter"))
boxplot(Ganancia_peso_nutrika, Ganancia_peso_ruter, main = "Ganancia de peso al destete", 
        xlab = "Alimento", ylab = "Ganancia de peso al destete", col = c("blue","red"), 
        names = c("Nutrika", "Ruter"),
        grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
boxplot(Ganancia_peso_nutrika, Ganancia_peso_ruter, main = "Ganancia de peso al destete", 
        xlab = "Alimento", ylab = "Ganancia de peso al destete", col = c("blue","red"), 
        names = c("Nutrika", "Ruter"),  add = TRUE)


wilcox.test(Ganancia_peso_ruter, Ganancia_peso_nutrika, paired = FALSE, 
            alternative = "two.sided", conf.level = 0.95) # Prueba de Mann-Whitney-Wilcoxon

tapply(X = tern$`Ganancia de peso al destete (kg)`, INDEX = tern$Alimento, FUN = median)

help("t.test")

t.test(Ganancia_peso_ruter, Ganancia_peso_nutrika, paired = FALSE, 
            alternative = "two.sided", conf.level = 0.95) # Prueba T

tapply(X = tern$`Ganancia de peso al destete (kg)`, 
       INDEX = tern$Alimento, FUN = mean) 


# Ganancia de peso a los 42 d??as


hist (x = tern$`Ganancia de peso a los 42 d??as (kg)`, xlab= "Ganancia de peso", ylab="Frecuencia",
      main = "Ganancia de peso al destete", col = "blue")
hist (x = tern$`Ganancia de peso a los 42 d??as (kg)`, xlab= "Ganancia de peso", ylab="Frecuencia",
      main = "Ganancia de peso al destete", col = "blue", 
      grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
hist (x = tern$`Ganancia de peso a los 42 d??as (kg)`, xlab= "Ganancia de peso", ylab="Frecuencia",
      main = "Ganancia de peso al destete", col = "blue", add = TRUE)


barplot(table(tern$Alimento, tern$`Ganancia de peso a los 42 d??as (kg)`), 
        main="Ganancia de peso al destete", xlab="Peso", ylab="Frecuencia", 
        col=c("blue","red"), 
        legend.text = rownames(table(tern$Alimento, 
                                     tern$`Ganancia de peso a los 42 d??as (kg)`)))
barplot(table(tern$Alimento, tern$`Ganancia de peso al destete (kg)`), 
        main="Ganancia de peso al destete", xlab="Peso", ylab="Frecuencia", 
        col=c("blue","red"), 
        legend.text = rownames(table(tern$Alimento, 
                                     tern$`Ganancia de peso a los 42 d??as (kg)`)),
        grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
barplot(table(tern$Alimento, tern$`Ganancia de peso a los 42 d??as (kg)`), 
        main="Ganancia de peso al destete", xlab="Peso", 
        ylab="Frecuencia", col=c("blue","red"), 
        legend.text = rownames(table(tern$Alimento, 
                                     tern$`Ganancia de peso a los 42 d??as (kg)`)),
        add = TRUE)


ganpes42prom <- tapply(X = tern$`Ganancia de peso a los 42 d??as (kg)`, 
                         INDEX = tern$Alimento, FUN = mean)


barplot(ganpes42prom , main="Ganancia de peso promedio a los 42 d??as por grupo", 
        xlab="Ganancia de peso promedio", ylab="Frecuencia", col=c("blue","red"),
        ylim = c(0, 40))
barplot(ganpes42prom , main="Ganancia de peso promedio a los 42 d??as por grupo", 
        xlab="Ganancia de peso promedio", ylab="Frecuencia", col=c("blue","red"),
        ylim = c(0, 4), grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
barplot(ganpes42prom , main="Ganancia de peso promedio a los 42 d??as por grupo", 
        xlab="Ganancia de peso promedio", ylab="Frecuencia", col=c("blue","red"),
        ylim = c(0, 40), add = TRUE)

mean(tern$`Ganancia de peso a los 42 d??as (kg)`)
median(tern$`Ganancia de peso a los 42 d??as (kg)`)
range(tern$`Ganancia de peso a los 42 d??as (kg)`)
sd(tern$`Ganancia de peso a los 42 d??as (kg)`)
coefficient.variation(sd=sd(tern$`Ganancia de peso a los 42 d??as (kg)`), 
                      avg = mean(tern$`Ganancia de peso a los 42 d??as (kg)`))
skew(tern$`Ganancia de peso a los 42 d??as (kg)`)
kurtosi(tern$`Ganancia de peso a los 42 d??as (kg)`)
skew(tern$`Ganancia de peso a los 42 d??as (kg)`)/sqrt(6/15) 
kurtosi(tern$`Ganancia de peso a los 42 d??as (kg)`)/sqrt(6/15) 

summary(tern$`Ganancia de peso a los 42 d??as (kg)`)

shapiro.test(tern$`Ganancia de peso a los 42 d??as (kg)`)

tapply(X = tern$`Ganancia de peso a los 42 d??as (kg)`, INDEX = tern$Alimento, FUN = shapiro.test) # Test de normalidad

Ganancia_42_nutrika <- subset(tern$`Ganancia de peso a los 42 d??as (kg)`, 
                              tern$Alimento=="Nutrika")
Ganancia_42_ruter <- subset(tern$`Ganancia de peso a los 42 d??as (kg)`, 
                            tern$Alimento=="Ruter")

bartlett.test(list(Ganancia_42_nutrika, Ganancia_42_ruter)) # Test de igualdad de varianzas


boxplot(Ganancia_42_nutrika, Ganancia_42_ruter, main = "Ganancia de peso a los 42 d??as", 
        xlab = "Alimento", ylab = "Ganancia de peso a los 42 d??as", col = c("blue","red"), 
        names = c("Nutrika", "Ruter"))
boxplot(Ganancia_42_nutrika, Ganancia_42_ruter, main = "Ganancia de peso a los 42 d??as", 
        xlab = "Alimento", ylab = "Ganancia de peso a los 42 d??as", col = c("blue","red"), 
        names = c("Nutrika", "Ruter"),
        grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
boxplot(Ganancia_42_nutrika, Ganancia_42_ruter, main = "Ganancia de peso a los 42 d??as", 
        xlab = "Alimento", ylab = "Ganancia de peso a los 42 d??as", col = c("blue","red"), 
        names = c("Nutrika", "Ruter"),  add = TRUE)


wilcox.test(Ganancia_42_ruter, Ganancia_42_nutrika, paired = FALSE, 
            alternative = "two.sided", conf.level = 0.95) # Prueba de Mann-Whitney-Wilcoxon

tapply(X = tern$`Ganancia de peso a los 42 d??as (kg)`, INDEX = tern$Alimento, FUN = median)

help("t.test")

t.test(Ganancia_peso_ruter, Ganancia_peso_nutrika, paired = FALSE, 
       alternative = "two.sided", conf.level = 0.95) # Prueba T

tapply(X = tern$`Ganancia de peso a los 42 d??as (kg)`, 
       INDEX = tern$Alimento, FUN = mean) 


tapply(X = tern$`Ganancia de peso a los 42 d??as (kg)`, INDEX = tern$Alimento, FUN = median)
tapply(X = tern$`Ganancia de peso a los 42 d??as (kg)`, INDEX = tern$Alimento, FUN = range)
tapply(X = tern$`Ganancia de peso a los 42 d??as (kg)`, INDEX = tern$Alimento, FUN = sd)


# Ganancia diaria de peso


gan_dia_60 <- tern$`Ganancia diaria de peso (kg/dia)`
gan_dia_42 <- tern$`Ganancia de peso a los 42 d??as (kg)`


shapiro.test(gan_dia_60)
shapiro.test(gan_dia_42) # Test de normalidad



bartlett.test(list(gan_dia_60, gan_dia_42)) # Test de igualdad de varianzas


cor(gan_dia_60, gan_dia_42, method = "spearman") # correlacion de Spearman

plot(x = tern$`Ganancia diaria de peso (kg/dia)`, 
     y = tern$`Ganancia de peso a los 42 d??as (kg)`, pch = 19,
     col=rainbow(1), abline (a = tern$`Ganancia diaria de peso (kg/dia)`, 
                             b = tern$`Ganancia de peso a los 42 d??as (kg)`), cex = 1,
     main = "Relaci??n entre el incremento de peso diario a los 42 y 60 d??as", 
     xlab = "Incremento de peso diario hasta los 60 d??as",
     ylab = "Incremento de peso diario hasta los 42")



wilcox.test(gan_dia_60, gan_dia_42, paired = FALSE, 
            alternative = "two.sided", conf.level = 0.95) # Prueba de Mann-Whitney-Wilcoxon


tern$`Ganancia diaria de peso (kg/dia)` <- tern$`Ganancia diaria de peso (kg/dia)`  

tern$`Ganancia diaria de peso (kg/dia)`<- round(tern$`Ganancia diaria de peso (kg/dia)`, 
                                                digits = 2)

hist (x = tern$`Ganancia diaria de peso (kg/dia)`, xlab= "Ganancia diaria de peso", ylab="Frecuencia",
      main = "Ganancia diaria de peso", col = "blue")
hist (x = tern$`Ganancia diaria de peso (kg/dia)`, xlab= "Ganancia diaria de peso", ylab="Frecuencia",
      main = "Ganancia diaria de peso", col = "blue", 
      grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
hist (x = tern$`Ganancia diaria de peso (kg/dia)`, xlab= "Ganancia diaria de peso", ylab="Frecuencia",
      main = "Ganancia diaria de peso", col = "blue", add = TRUE)

barplot(table(tern$Alimento, tern$`Ganancia diaria de peso (kg/dia)`), 
        main="Ganancia diaria de peso", xlab="Peso", ylab="Frecuencia", 
        col=c("blue","red"), 
        legend.text = rownames(table(tern$Alimento, tern$`Ganancia diaria de peso (kg/dia)`)))
barplot(table(tern$Alimento, tern$`Ganancia diaria de peso (kg/dia)`), 
        main="Ganancia diaria de peso", xlab="Peso", ylab="Frecuencia", 
        col=c("blue","red"), 
        legend.text = rownames(table(tern$Alimento, tern$`Ganancia diaria de peso (kg/dia)`)),
        grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
barplot(table(tern$Alimento, tern$`Ganancia diaria de peso (kg/dia)`), 
        main="Ganancia diaria de peso", xlab="Peso", 
        ylab="Frecuencia", col=c("blue","red"), 
        legend.text = rownames(table(tern$Alimento, tern$`Ganancia diaria de peso (kg/dia)`)),
        add = TRUE)

gandiariaprom <- tapply(X = tern$`Ganancia diaria de peso (kg/dia)`, 
                        INDEX = tern$Alimento, FUN = mean)


barplot(gandiariaprom, main="Promedio de ganancia diaria de peso al destete por grupo", 
        xlab="Promedio de ganancia diaria de peso", ylab="Frecuencia", col=c("blue","red"),
        ylim = c(0, 1))
barplot(gandiariaprom, main="Promedio de ganancia diaria de peso al destete por grupo", 
        xlab="Promedio de ganancia diaria de peso", ylab="Frecuencia", col=c("blue","red"),
        ylim = c(0, 1), grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
barplot(gandiariaprom, main="Promedio de ganancia diaria de peso al destete por grupo", 
        xlab="Promedio de ganancia diaria de peso", ylab="Frecuencia", col=c("blue","red"),
        ylim = c(0, 1), add = TRUE)

mean(tern$`Ganancia diaria de peso (kg/dia)`)
median(tern$`Ganancia diaria de peso (kg/dia)`)
range(tern$`Ganancia diaria de peso (kg/dia)`)
sd(tern$`Ganancia diaria de peso (kg/dia)`)
coefficient.variation(sd=sd(tern$`Ganancia diaria de peso (kg/dia)`), 
                      avg = mean(tern$`Ganancia diaria de peso (kg/dia)`))
skew(tern$`Ganancia diaria de peso (kg/dia)`)
kurtosi(tern$`Ganancia diaria de peso (kg/dia)`)
skew(tern$`Ganancia diaria de peso (kg/dia)`)/sqrt(6/15) 
kurtosi(tern$`Ganancia diaria de peso (kg/dia)`)/sqrt(6/15) 

summary(tern$`Ganancia diaria de peso (kg/dia)`)

shapiro.test(tern$`Ganancia diaria de peso (kg/dia)`)

tapply(X = tern$`Ganancia diaria de peso (kg/dia)`, 
       INDEX = tern$Alimento, FUN = shapiro.test) # Test de normalidad


Ganancia_diaria_nutrika <- subset(tern$`Ganancia diaria de peso (kg/dia)`, tern$Alimento=="Nutrika")
Ganancia_diaria_ruter <- subset(tern$`Ganancia diaria de peso (kg/dia)`, tern$Alimento=="Ruter")


bartlett.test(list(Ganancia_diaria_nutrika, Ganancia_diaria_ruter)) # Test de igualdad de varianzas


boxplot(Ganancia_diaria_nutrika, Ganancia_diaria_ruter, main = "Ganancia diaria de peso", 
        xlab = "Alimento", ylab = "Ganancia diaria de peso", col = c("blue","red"), 
        names = c("Nutrika", "Ruter"))
boxplot(Ganancia_diaria_nutrika,  Ganancia_diaria_ruter, main = "Ganancia diaria de peso", 
        xlab = "Alimento", ylab = "Ganancia diaria de peso", col = c("blue","red"), 
        names = c("Nutrika", "Ruter"),
        grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
boxplot(Ganancia_diaria_nutrika,  Ganancia_diaria_ruter, main = "Ganancia diaria de peso", 
        xlab = "Alimento", ylab = "Ganancia diaria de peso", col = c("blue","red"), 
        names = c("Nutrika", "Ruter"),  add = TRUE)


wilcox.test(Ganancia_diaria_ruter,  Ganancia_diaria_nutrika, paired = FALSE, 
            alternative = "two.sided", conf.level = 0.95) # Prueba de Mann-Whitney-Wilcoxon

tapply(X = tern$`Ganancia diaria de peso (kg/dia)`, INDEX = tern$Alimento, FUN = median)

help("t.test")

t.test(Ganancia_diaria_ruter, Ganancia_diaria_nutrika, paired = FALSE, 
       alternative = "two.sided", conf.level = 0.95) # Prueba T

tapply(X = tern$`Ganancia diaria de peso (kg/dia)`, 
       INDEX = tern$Alimento, FUN = mean) 

# Ganancia de peso diaria hasta los 42 d??as

tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`<- round(tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`, 
                                                digits = 2)

hist (x = tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`, xlab= "Ganancia diaria de peso", ylab="Frecuencia",
      main = "Ganancia diaria de peso", col = "blue")
hist (x = tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`, xlab= "Ganancia diaria de peso", ylab="Frecuencia",
      main = "Ganancia diaria de peso", col = "blue", 
      grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
hist (x = tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`, xlab= "Ganancia diaria de peso", ylab="Frecuencia",
      main = "Ganancia diaria de peso", col = "blue", add = TRUE)

barplot(table(tern$Alimento, tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`), 
        main="Ganancia diaria de peso", xlab="Peso", ylab="Frecuencia", 
        col=c("blue","red"), 
        legend.text = rownames(table(tern$Alimento, tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`)))
barplot(table(tern$Alimento, tern$`Ganancia diaria de peso (kg/dia)`), 
        main="Ganancia diaria de peso", xlab="Peso", ylab="Frecuencia", 
        col=c("blue","red"), 
        legend.text = rownames(table(tern$Alimento, tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`)),
        grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
barplot(table(tern$Alimento, tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`), 
        main="Ganancia diaria de peso", xlab="Peso", 
        ylab="Frecuencia", col=c("blue","red"), 
        legend.text = rownames(table(tern$Alimento, tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`)),
        add = TRUE)

gandiariaprom42 <- tapply(X = tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`, 
                        INDEX = tern$Alimento, FUN = mean)


barplot(gandiariaprom42, main="Promedio de ganancia diaria de peso al destete por grupo", 
        xlab="Promedio de ganancia diaria de peso", ylab="Frecuencia", col=c("blue","red"),
        ylim = c(0, 1))
barplot(gandiariaprom42, main="Promedio de ganancia diaria de peso al destete por grupo", 
        xlab="Promedio de ganancia diaria de peso", ylab="Frecuencia", col=c("blue","red"),
        ylim = c(0, 1), grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
barplot(gandiariaprom42, main="Promedio de ganancia diaria de peso al destete por grupo", 
        xlab="Promedio de ganancia diaria de peso", ylab="Frecuencia", col=c("blue","red"),
        ylim = c(0, 1), add = TRUE)

mean(tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`)
median(tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`)
range(tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`)
sd(tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`)
coefficient.variation(sd=sd(tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`), 
                      avg = mean(tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`))
skew(tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`)
kurtosi(tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`)
skew(tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`)/sqrt(6/15) 
kurtosi(tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`)/sqrt(6/15) 

summary(tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`)

shapiro.test(tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`)

tapply(X = tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`, 
       INDEX = tern$Alimento, FUN = shapiro.test) # Test de normalidad


Ganancia_diaria_nutrika42 <- subset(tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`, tern$Alimento=="Nutrika")
Ganancia_diaria_ruter42 <- subset(tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`, tern$Alimento=="Ruter")


bartlett.test(list(Ganancia_diaria_nutrika42, Ganancia_diaria_ruter42)) # Test de igualdad de varianzas


boxplot(Ganancia_diaria_nutrika42, Ganancia_diaria_ruter42, main = "Ganancia diaria de peso a los 42 d??as", 
        xlab = "Alimento", ylab = "Ganancia diaria de peso a los 42 d??as", col = c("blue","red"), 
        names = c("Nutrika", "Ruter"))
boxplot(Ganancia_diaria_nutrika42,  Ganancia_diaria_ruter42, main = "Ganancia diaria de peso a los 42 d??as", 
        xlab = "Alimento", ylab = "Ganancia diaria de peso a los 42 d??as", col = c("blue","red"), 
        names = c("Nutrika", "Ruter"),
        grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
boxplot(Ganancia_diaria_nutrika42,  Ganancia_diaria_ruter42, main = "Ganancia diaria de peso a los 42 d??as", 
        xlab = "Alimento", ylab = "Ganancia diaria de peso a los 42 d??as", col = c("blue","red"), 
        names = c("Nutrika", "Ruter"),  add = TRUE)


wilcox.test(Ganancia_diaria_ruter42,  Ganancia_diaria_nutrika42, paired = FALSE, 
            alternative = "two.sided", conf.level = 0.95) # Prueba de Mann-Whitney-Wilcoxon

tapply(X = tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`, INDEX = tern$Alimento, FUN = median)


tapply(X = tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`, 
       INDEX = tern$Alimento, FUN = mean) 

tapply(X = tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`, 
       INDEX = tern$Alimento, FUN = sd) 

tapply(X = tern$`Ganancia de peso diaria a los 42 d??as (Kg/d??a)`, 
       INDEX = tern$Alimento, FUN = range) 

# Conversi??n alimenticia

tern$`Conversi??n alimenticia`<- round(tern$`Conversi??n alimenticia`, digits = 2)

hist (x = tern$`Conversi??n alimenticia`, xlab= "Conversi??n alimenticia", ylab="Frecuencia",
      main = "Conversi??n alimenticia", col = "blue")
hist (x = tern$`Conversi??n alimenticia`, xlab= "Conversi??n alimenticia", ylab="Frecuencia",
      main = "Conversi??n alimenticia", col = "blue", 
      grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
hist (x = tern$`Conversi??n alimenticia`, xlab= "Conversi??n alimenticia", ylab="Frecuencia",
      main = "Conversi??n alimenticia", col = "blue", add = TRUE)

barplot(table(tern$Alimento, tern$`Conversi??n alimenticia`), 
        main="Conversi??n alimenticia", xlab="Conversi??n alimenticia", ylab="Frecuencia", 
        col=c("blue","red"), 
        legend.text = rownames(table(tern$Alimento, tern$`Conversi??n alimenticia`)))
barplot(table(tern$Alimento, tern$`Conversi??n alimenticia`), 
        main="Conversi??n alimenticia", xlab="Conversi??n alimenticia", ylab="Frecuencia", 
        col=c("blue","red"), 
        legend.text = rownames(table(tern$Alimento, tern$`Conversi??n alimenticia`)),
        grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
barplot(table(tern$Alimento, tern$`Conversi??n alimenticia`), 
        main="Conversi??n alimenticia", xlab="Conversi??n alimenticia", 
        ylab="Frecuencia", col=c("blue","red"), 
        legend.text = rownames(table(tern$Alimento, tern$`Conversi??n alimenticia`)),
        add = TRUE)

caprom <- tapply(X = tern$`Conversi??n alimenticia`, INDEX = tern$Alimento, FUN = mean)


barplot(caprom, main="Conversi??n alimenticia promedio al destete por grupo", 
        xlab="Conversi??n alimenticia promedio", ylab="Frecuencia", col=c("blue","red"),
        ylim = c(0, 5))
barplot(caprom, main="Conversi??n alimenticia promedio al destete por grupo", 
        xlab="Conversi??n alimenticia promedio", ylab="Frecuencia", col=c("blue","red"),
        ylim = c(0, 5), grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
barplot(caprom, main="Conversi??n alimenticia promedio al destete por grupo", 
        xlab="Conversi??n alimenticia promedio", ylab="Frecuencia", col=c("blue","red"),
        ylim = c(0, 5), add = TRUE)

mean(tern$`Conversi??n alimenticia`)
median(tern$`Conversi??n alimenticia`)
range(tern$`Conversi??n alimenticia`)
sd(tern$`Conversi??n alimenticia`)
coefficient.variation(sd=sd(tern$`Conversi??n alimenticia`), 
                      avg = mean(tern$`Conversi??n alimenticia`))
skew(tern$`Conversi??n alimenticia`)
kurtosi(tern$`Conversi??n alimenticia`)
skew(tern$`Conversi??n alimenticia`)/sqrt(6/15) 
kurtosi(tern$`Conversi??n alimenticia`)/sqrt(6/15) 

summary(tern$`Conversi??n alimenticia`)

shapiro.test(tern$`Conversi??n alimenticia`)

tapply(X = tern$`Conversi??n alimenticia`, INDEX = tern$Alimento, FUN = shapiro.test) # Test de normalidad

canutrika <- subset(tern$`Conversi??n alimenticia`, tern$Alimento=="Nutrika")
caruter <- subset(tern$`Conversi??n alimenticia`, tern$Alimento=="Ruter")

bartlett.test(list(canutrika, caruter)) # Test de igualdad de varianzas


boxplot(canutrika, caruter, main = "Conversi??n alimenticia", 
        xlab = "Alimento", ylab = "Conversi??n alimenticia", col = c("blue","red"), 
        names = c("Nutrika", "Ruter"))
boxplot(canutrika, caruter, main = "Conversi??n alimenticia", 
        xlab = "Alimento", ylab = "Conversi??n alimenticia", col = c("blue","red"), 
        names = c("Nutrika", "Ruter"),
        grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
boxplot(canutrika, caruter, main = "Conversi??n alimenticia", 
        xlab = "Alimento", ylab = "Conversi??n alimenticia", col = c("blue","red"), 
        names = c("Nutrika", "Ruter"),  add = TRUE)

wilcox.test(canutrika, caruter, paired = FALSE, 
            alternative = "two.sided", conf.level = 0.95) # Prueba de Mann-Whitney-Wilcoxon

tapply(X = tern$`Conversi??n alimenticia`, INDEX = tern$Alimento, FUN = median)
tapply(X = tern$`Conversi??n alimenticia`, INDEX = tern$Alimento, FUN = range)
tapply(X = tern$`Conversi??n alimenticia`, INDEX = tern$Alimento, FUN = mean)
tapply(X = tern$`Conversi??n alimenticia`, INDEX = tern$Alimento, FUN = sd)

# Morbilidad

barp <- barplot(table(tern$Morbilidad,tern$Alimento), main="Morbilidad", xlab="Alimento", 
                ylab="Cantidad", col=rainbow(length(table(tern$Morbilidad,tern$Alimento))), 
                legend.text = rownames(table(tern$Morbilidad,tern$Alimento)))
barp <- barplot(table(tern$Morbilidad,tern$Alimento), main="Morbilidad", xlab="Alimento", 
                ylab="Cantidad", col=rainbow(length(table(tern$Morbilidad,tern$Alimento))), 
                legend.text = rownames(table(tern$Morbilidad,tern$Alimento)),
                grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray"))
barp <- barplot(table(tern$Morbilidad,tern$Alimento), main="Morbilidad", xlab="Alimento", 
                ylab="Cantidad", col=rainbow(length(table(tern$Morbilidad,tern$Alimento))), 
                legend.text = rownames(table(tern$Morbilidad,tern$Alimento)), add = TRUE)

fisher.test(table(tern$Morbilidad,tern$Alimento))

help(fisher.test)
