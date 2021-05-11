
censo <- read.csv("~/Documents/Proyectos/itesm_segregacion/data/Clean data/restricciones.csv", 
                  na = c("*"))
cons <- censo[censo$NOM_MUN=="Monterrey", c(10,11,12,13,14)]
names(cons) <- c("F","M","0-14","15-64","64+") # renombrar columnas

enigh <- read.csv("~/Documents/Proyectos/itesm_segregacion/data/Clean data/poblacion_enigh_nuevoleon.csv",
                  na = c(""))
ind <- enigh[enigh$des_mun=="Monterrey", c(1,11,12,47)]
names(ind) <- c("ID","Sexo","Edad","NivelAprob")
ind_full <- ind

ind[is.na(ind)] <- 0 # llenar los valores nulos

brks <- c(0, 14, 64, 110) # crea los puntos de break
labs <- c("0-14","15-64","64+") # crea las etiquetas 
ind$Edad <- cut(ind$Edad, breaks = brks, labels = labs) # sobreescribir con las bandas

ind[ind$Sexo==1,"Sexo"] <- "M"
ind[ind$Sexo==2,"Sexo"] <- "F"

ind <- ind[complete.cases(ind), ]
cons <- cons[complete.cases(cons), ]

cat_edad <- model.matrix(~ind$Edad - 1)
cat_sexo <- model.matrix(~ind$Sexo - 1)

ind_cat <- cbind(cat_sexo, cat_edad)
ind_agg <- colSums(ind_cat)

weights <- matrix(data = 1, nrow = nrow(ind), ncol = nrow(cons)) # matrix de pesos
weights1 <- weights2 <- weights # copia de pesos
ind_agg0 <- t(apply(cons, 1, function(x) 1 * ind_agg)) 
colnames(ind_agg0) <- names(cons)

n_zone <- nrow(cons) # numero de zonas
n_ind <- nrow(ind) # numero de individuos
n_edad <- 3 # numero de categorias de edad
n_sexo <- 2 # numero de categorias de sexo

library(ipfp)

cons <- apply(cons, 2, as.numeric) # hacer valores numericos
ipfp(cons[2,], t(ind_cat), x0 = rep(1, n_ind), v = T) # corre IPF

ind_catt <- t(ind_cat)
x0 = rep(1, n_ind)

weights_maxit_2 <- weights # create a copy of the weights object

for(i in 1:ncol(weights)){
  weights_maxit_2[, i] <- ipfp(cons[i, ], ind_catt, x0, maxit = 200) 
}


















