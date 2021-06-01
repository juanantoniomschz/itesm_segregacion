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
int_expand_vector <- function(x){
  index <- 1:length(x)
  rep(index, round(x))
}

nombres_municipios <- c("Monterrey", "San Pedro Garza Garcia", "San Nicolas de los Garza",
                        "Guadalupe", "Apodaca", "Escobedo","Santa Catarina" )
ind_cols <- c("Dorms"
                , "Cuartos"
                , "TV"
                , "INT"
                #, "PCs"
                #, "Lavadoras"
                #, "Autos"
                #, "Consolas"
                ,"Ingreso.corriente")

cons_cols <- c("X1dorm", "X2masdorm"
               , "X1cuart", "X2cuart", "X3mascuart"
               ,"TVPaga", "NoTVPaga"
               ,"Internet", "NoInternet"
               #,"PC", "NoPC"
               #,"Lavadora", "NoLavadora"
               #, "Auto", "NoAuto"
               #,"Consola","NoConsola"
               )
               


ind_zmm <- ind_full[Reduce(`|`, lapply(nombres_municipios, grepl, x = ind_full$Municipio)),]
cons_zmm <- cons_full[Reduce(`|`, lapply(nombres_municipios, grepl, x = cons_full$Municipio)),]

ind_zmm$ID <- seq(1,nrow(ind_zmm))

ind <- ind_zmm[, ind_cols]
cons <- cons_zmm[, cons_cols]

n_zone <- nrow(cons)
n_ind <- nrow(ind)

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

target <- list(as.matrix(con1)
               , as.matrix(con2)
               #, as.matrix(con3)
               #, as.matrix(con4)
               #, as.matrix(con5)
               #, as.matrix(con6)
               #, as.matrix(con7)
               #, as.matrix(con8)
               )

descript <- list(c(1,2), c(1,3)) #, c(1,5), c(1,6), c(1,7), c(1,8), c(1,9), c(1,10)) # asignar las posiciones de las restricciones 

weight_mipfp <- Ipfp(weight_init, descript, target, iter = 100, print = T)

layout(matrix(c(1:9), 3, 3))
plot(as.matrix(con1), apply(weight_mipfp$x.hat,c(1, 2),sum), pch=20, xlab="Result of the simulation for constraint 1",
     ylab="Constraint 1",
     main="Simulation vs constraint 1")
plot(as.matrix(con2), apply(weight_mipfp$x.hat,c(1,3),sum), pch=20, xlab="Result of the simulation for constraint 2",
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

weight_init <- table(ind)

Names <- list(1:n_zone, colnames(con1), colnames(con2), colnames(con3), colnames(con4))
Mipfp_Tab <- array(data = NA, dim = c(n_zone,2, 3, 2, 2), 
                   dimnames = Names)

descript <- list(1,2,3,4)

for (zone in 1:n_zone){
  # Adapt the constraint to the zone
  con1_1 <- data.matrix(con1[zone,])
  con2_2 <- data.matrix(con2[zone,])
  con3_3 <- data.matrix(con3[zone,])
  con4_4 <- data.matrix(con4[zone,])
  target <- list(con1_1, con2_2, con3_3, con4_4)
  
  
  # Calculate the weights
  res <- Ipfp(weight_init, descript, target, tol = 1e-5, print = T)
  
  # Complete the array of calculated weights
  Mipfp_Tab[zone,,,,] <- apply(res$x.hat,c(1,2,3,4),sum)
  
  print(paste("Zona numero",zone, "terminada!"))
}

# Repeat the initial matrix n_zone times
init_cells <- rep(weight_init, each = n_zone)

# Define the names
names <- c(list(1:n_zone), as.list(dimnames(weight_init)))

# Structure the data
mipfp_zones <- array(init_cells,
                     dim = c(n_zone, ncol(con1), ncol(con2),  ncol(con3), 20),
                     dimnames = names)


target <- list(data.matrix(con1),
               data.matrix(con2),
               data.matrix(con3))

descript <- list (c(1,2), c(1,3), c(1,4))

res <- Ipfp(mipfp_zones, descript, target, tol = 1e-5, print = T)

weights_mipfp <- matrix(nrow = nrow(ind), ncol = nrow(cons))
Ind_Tab <- table(ind[,c(1,2,3)])

for (zone in 1:n_zone){
  
  # Transformation into individual weights
  for (i in 1:n_ind){
    
    # weight of the category
    weight_ind <- Mipfp_Tab[zone, ind[i, 1], ind[i, 2],  ind[i, 3]]
    
    # number of ind in the category
    sample_ind <- Ind_Tab[ind[i, 1], ind[i, 2],  ind[i, 3]]
    
    # distribute the weight to the ind of this category
    weights_mipfp[i,zone] <- weight_ind / sample_ind
    
  }
  
  print(paste("Zona numero",zone, "terminada!"))
  
}

ints_df <- NULL
set.seed(42)

for(i in 1:n_zone){
  
  # Integerise and expand
  ints <- int_expand_vector(int_trs(weights_mipfp[, i]))
  
  name_CVEGEO <- unique(cons_zmm$CVEGEO)[i]
  rep_CVEGEO <- rep(name_CVEGEO, length(ints))
  
  # Take the right individuals
  data_frame <- data.frame(ind[ints,], CVEGEO = rep_CVEGEO, id = ints)
  ints_df <- rbind(ints_df, data_frame)
  
  print(paste("Zona numero", i, "terminada!"))
  
}
