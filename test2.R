library(mipfp)
library(stringi)

cons_full <- read.csv("~/Documents/Proyectos/itesm_segregacion/data/Clean data/cons.csv")
ind_full <- read.csv("~/Documents/Proyectos/itesm_segregacion/data/Clean data/ind.csv")

ind_full$Municipio <- stri_trans_general(str = ind_full$Municipio, id = "Latin-ASCII")
cons_full$Municipio <- stri_trans_general(str = cons_full$Municipio, id = "Latin-ASCII")

municipios_ind <- unique(ind_full$Municipio)
municipios_cons <- unique(cons_full$Municipio)

municipios <- municipios_cons[match(municipios_ind, municipios_cons)]
municipios <- municipios[is.na(municipios)==FALSE]

int_trs <- function(x){
  # For generalisation purpose, x becomes a vector
  xv <- as.vector(x) # allows trs to work on matrices
  xint <- floor(xv) # integer part of the weight
  r <- xv - xint # decimal part of the weight
  def <- round(sum(r)) # the deficit population
  # the weights be 'topped up' (+ 1 applied)
  topup <- sample(length(x), size = def, prob = r)
  xint[topup] <- xint[topup] + 1
  dim(xint) <- dim(x)
  dimnames(xint) <- dimnames(x)
  xint
}
int_expand_array <- function(x){
  # Transform the array into a dataframe
  count_data <- as.data.frame.table(x)
  # Store the indices of categories for the final population
  indices <- rep(1:nrow(count_data), count_data$Freq)
  # Create the final individuals
  ind_data <- count_data[indices, ]
  ind_data
}

nombres_municipios <- c("Monterrey", "San Pedro Garza Garcia", "San Nicolas de los Garza",
                        "Guadalupe", "Apodaca", "Escobedo","Santa Catarina" )

ind_zmm <- ind_full[Reduce(`|`, lapply(nombres_municipios, grepl, x = ind_full$Municipio)),]
cons_zmm <- cons_full[Reduce(`|`, lapply(nombres_municipios, grepl, x = cons_full$Municipio)),]

ind <- ind_zmm[, c("Ingreso.corriente"
                   , "Dorms"
                   , "Cuartos"
                   #, "TV"
                   #, "INT"
                   #, "PCs"
                   #, "Lavadoras"
                   #, "Autos"
                   #, "Consolas"
)]

cons <- cons_zmm[, c("X1dorm", "X2masdorm"
                     , "X1cuart", "X2cuart", "X3mascuart"
                     #,"TVPaga", "NoTVPaga"
                     #,"Internet", "NoInternet"
                     #,"PC", "NoPC"
                     #,"Lavadora", "NoLavadora"
                     #, "Auto", "NoAuto"
                     #,"Consola","NoConsola"
)]

# Enumeramos y seleccionamos las restricciones

con1 <- cons[,c("X1dorm","X2masdorm")]
con2 <- cons[,c("X1cuart", "X2cuart", "X3mascuart")]
con3 <- cons[,c("TVPaga","NoTVPaga")]
con4 <- cons[,c("Internet","NoInternet")]
con5 <- cons[,c("PC","NoPC")]
con6 <- cons[,c("Lavadora","NoLavadora")]
con7 <- cons[,c("Auto","NoAuto")]
con8 <- cons[,c("Consola","NoConsola")]

weight_init_zone <- table(ind) # tabla con los valores de los individuos
dimnames(weight_init_zone) # vemos el nombre de las variables a pesar

init_cells <- rep(weight_init_zone, each = nrow(cons)) # realizamos una matriz de 0s por todas las columnas

names <- c(list(rownames(cons)),
           as.list(dimnames(weight_init_zone))) # asignamos los nombres de las columnas correctas

weight_init <- array(init_cells, dim = 
                       c(nrow(cons), dim(weight_init_zone)),
                     dimnames = names) # hacemos la matriz completa

# (Zone, Ingreso, Dorm, Cuartos, TV, INT, PCs, Lavadoras, Consolas, Autos)

target <- list(as.matrix(con1), # dar un objetivo al codigo
               as.matrix(con2),
               as.matrix(con3),
               as.matrix(con4),
               as.matrix(con5),
               as.matrix(con6),
               as.matrix(con7),
               as.matrix(con8))

descript <- list(c(1,3), c(1,4), c(1,5), c(1,6), c(1,7), c(1,8), c(1,9), c(1,10)) # asignar las posiciones de las restricciones 

weight_mipfp <- Ipfp(weight_init, descript, target, iter = 1, print = T)

layout(matrix(c(1:9), 3, 3))
plot(as.matrix(con1), apply(weight_mipfp$x.hat,c(1, 3),sum), pch=20, xlab="Result of the simulation for constraint 1",
     ylab="Constraint 1",
     main="Simulation vs constraint 1")
plot(as.matrix(con2), apply(weight_mipfp$x.hat,c(1,4),sum), pch=20, xlab="Result of the simulation for constraint 2",
     ylab="Constraint 2",
     main="Simulation vs constraint 2")
plot(as.matrix(con3), apply(weight_mipfp$x.hat,c(1,5),sum), pch=20, xlab="Result of the simulation for constraint 3",
     ylab="Constraint 3",
     main="Simulation vs constraint 3")
plot(as.matrix(con4), apply(weight_mipfp$x.hat,c(1, 6),sum), pch=20, xlab="Result of the simulation for constraint 4",
     ylab="Constraint 4",
     main="Simulation vs constraint 4")
plot(as.matrix(con5), apply(weight_mipfp$x.hat,c(1,7),sum), pch=20, xlab="Result of the simulation for constraint 5",
     ylab="Constraint 5",
     main="Simulation vs constraint 5")
plot(as.matrix(con6), apply(weight_mipfp$x.hat,c(1,8),sum), pch=20, xlab="Result of the simulation for constraint 6",
     ylab="Constraint 6",
     main="Simulation vs constraint 6")
plot(as.matrix(con7), apply(weight_mipfp$x.hat,c(1, 9),sum), pch=20, xlab="Result of the simulation for constraint 7",
     ylab="Constraint 7",
     main="Simulation vs constraint 7")
plot(as.matrix(con8), apply(weight_mipfp$x.hat,c(1,10),sum), pch=20, xlab="Result of the simulation for constraint 8",
     ylab="Constraint 8",
     main="Simulation vs constraint 8")
plot(as.matrix(con9), apply(weight_mipfp$x.hat,c(1,11),sum), pch=20, xlab="Result of the simulation for constraint 9",
     ylab="Constraint 9",
     main="Simulation vs constraint 9")
















