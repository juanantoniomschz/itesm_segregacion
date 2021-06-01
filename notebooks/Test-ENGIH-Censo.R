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

micro_sim <- NULL

for(municipio in municipios){
  
  ind_est <- ind_full[ind_full$Municipio==municipio,]
  cons_est <- cons_full[cons_full$Municipio==municipio,]
  
  ind <- ind_est[ind_est$Municipio==municipio, c("Ingreso.corriente", "Dorm", "TV", "INT", "PCs", "Lavadoras"
                                                 , "Consolas", "Autos")]
  
  cons <- cons_est[cons_est$Municipio==municipio, c("X1dorm", "X2masdorm","TVPaga", "NoTVPaga","Internet", "NoInternet",
                                                    "PC", "NoPC", "Lavadora", "NoLavadora",
                                                    "Auto", "NoAuto", "Consola","NoConsola")]
  
  weight_init_zone <- table(ind) # tabla con los valores de los individuos
  
  init_cells <- rep(weight_init_zone, each = nrow(cons)) # realizamos una matriz de 0s por todas las columnas
  
  names <- c(list(rownames(cons)),
             as.list(dimnames(weight_init_zone))) # asignamos los nombres de las columnas correctas
  
  weight_init <- array(init_cells, dim = 
                         c(nrow(cons), dim(weight_init_zone)),
                       dimnames = names) # hacemos la matriz completa
  
  con1 <- cons[,c('X1dorm','X2masdorm')] 
  con2 <- cons[,c('TVPaga','NoTVPaga')]
  con3 <- cons[,c('Internet','NoInternet')]
  con4 <- cons[,c('PC','NoPC')]
  con5 <- cons[,c('Lavadora','NoLavadora')]
  con6 <- cons[,c('Consola','NoConsola')]
  con7 <- cons[,c('Auto','NoAuto')]
  
  target <- list(as.matrix(con1), as.matrix(con2), as.matrix(con3), as.matrix(con4), as.matrix(con5),
                 as.matrix(con6), as.matrix(con7))
  
  descript <- list(c(1,3), c(1,4), c(1,5), c(1,6), c(1,7), 
                   c(1,8), c(1,9)) # asignar las posiciones de las restricciones 
  
  weight_mipfp <- Ipfp(weight_init, descript, target, iter = 1, print = T)
  
  
  set.seed(42)
  
  # tenemos que hacer enteros antes de realizar la expansión por loop
  mipfp_int <- int_trs(weight_mipfp$x.hat) 
  
  ints_df <- NULL
  
  for(i in 1:nrow(cons)){
    
    # Expandir
    ints <- int_expand_array(mipfp_int[i,,,,,,,,])
    
    rep_municipio <- rep(municipio, dim(ints)[1])
    
    name_ageb <- unique(cons_est$AGEB)[i]
    rep_ageb <- rep(name_ageb, dim(ints)[1])
    
    # Take the right individuals
    data_frame <- data.frame( Municipio = rep_municipio, AGEB = rep_ageb, ints)
    ints_df <- rbind(ints_df, data_frame)
    
  }
  
  micro_sim <- rbind(micro_sim, ints_df)
  
  write.csv(micro_sim,'microsim_nuevo_leon.csv')
  
}

# Seleccionamos las columnas

nombres_municipios <- c("Monterrey", "San Pedro Garza Garcia", "San Nicolas de los Garza",
                        "Guadalupe", "Apodaca", "Escobedo","Santa Catarina" )

ind_zmm <- ind_full[Reduce(`|`, lapply(nombres_municipios, grepl, x = ind_full$Municipio)),]
cons_zmm <- cons_full[Reduce(`|`, lapply(nombres_municipios, grepl, x = cons_full$Municipio)),]

ind <- ind_zmm[, c("Ingreso.corriente", "Dorms", "Cuartos", "TV", "INT",
                    "PCs", "Lavadoras", "Autos", "Consolas")]

cons <- cons_zmm[, c("X1dorm", "X2masdorm", "X1cuart", "X2cuart", "X3mascuart","TVPaga", "NoTVPaga","Internet", "NoInternet","PC", "NoPC"
                     ,"Lavadora", "NoLavadora", "Auto", "NoAuto","Consola","NoConsola")]


# Enumeramos y seleccionamos las restricciones

con1 <- cons[,c("X1dorm","X2masdorm")]
con2 <- cons[,c("X1cuart", "X2cuart", "X3mascuart")]
con3 <- cons[,c("TVPaga","NoTVPaga")]
con4 <- cons[,c("Internet","NoInternet")]
con5 <- cons[,c("PC","NoPC")]
con6 <- cons[,c("Lavadora","NoLavadora")]
con7 <- cons[,c("Auto","NoAuto")]
con8 <- cons[,c("Consola","NoConsola")]

n_zonas <- dim(cons)[1]
n_ind <- dim(ind)[1]

weight_init <- table(ind[, c(2,3,4,5,6,7,8,9,10)])

Names <- list(1:dim(cons)[1], colnames(con1), colnames(con2), colnames(con3), colnames(con4),
              colnames(con5), colnames(con6), colnames(con7), colnames(con8))

Mipfp_Tab <- array(data = NA, dim = c(n_zonas, 2, 3, 2, 2, 2, 2, 2, 2), 
                   dimnames = Names)
 
descript <- list (1, 2, 3, 4, 5, 6, 7, 8)

# Loop over the zones and execute Ipfp
for (zone in 1:n_zonas){
  # Adapt the constraint to the zone
  con_1_m <- data.matrix(con1[zone,])
  con_2_m <- data.matrix(con2[zone,])
  con_3_m <- data.matrix(con3[zone,])
  con_4_m <- data.matrix(con4[zone,])
  con_5_m <- data.matrix(con5[zone,])
  con_6_m <- data.matrix(con6[zone,])
  con_7_m <- data.matrix(con7[zone,])
  con_8_m <- data.matrix(con8[zone,])
  
  target <- list(con_1_m, con_2_m, con_3_m, con_4_m, con_5_m, con_6_m, con_7_m, con_8_m)
  
  # Calculate the weights
  res <- Ipfp(weight_init, descript, target, tol = 1e-5, print = T)
  
  # Complete the array of calculated weights
  Mipfp_Tab[zone,,,,,,,,] <- apply(res$x.hat,c(1,2,3,4,5,6,7,8),sum)
  
  print(paste("Zona numero",zone, "terminada!"))
}


init_cells <- rep(weight_init, each = n_zonas)

names <- c(list(seq(n_zonas)), as.list(dimnames(weight_init)))

mipfp_zones <- array(init_cells,
                     dim = c(n_zonas, 2, 3, 2, 2, 2, 2, 2, 2, 20),
                     dimnames = names)

table(mipfp_zones[1,,,,,,,,,] == weight_init) # comprobamos que las celdas en la zona 1 son iguales a los valores en la matriz anterior

target <- list(data.matrix(con1),
               data.matrix(con2),
               data.matrix(con3),
               data.matrix(con4),
               data.matrix(con5),
               data.matrix(con6),
               data.matrix(con7),
               data.matrix(con8))

descript <- list(c(1,2),c(1,3), c(1,4), c(1,5), c(1,6), c(1,7), c(1,8), c(1,9))

res <- Ipfp(mipfp_zones, descript, target, tol = 1e-5, print = T, iter = 10)

weights_mipfp <- matrix(nrow = nrow(ind), ncol = nrow(cons))
Ind_Tab <- table(ind[,c(2,3,4,5,6,7,8,9)])

for (zone in 1:n_zonas){
  
  # Transformation into individual weights
  for (i in 1:n_ind){
    # weight of the category
    weight_ind <- Mipfp_Tab[zone, ind[i, 2], ind[i, 3],
                            ind[i, 4], ind[i, 5], ind[i, 6],
                            ind[i, 7], ind[i, 8], ind[i, 9]]
    
    # number of ind in the category
    sample_ind <- Ind_Tab[ind[i, 2], ind[i, 3],
                          ind[i, 4], ind[i, 5], ind[i, 6],
                          ind[i, 7], ind[i, 8], ind[i, 9]]
    
    # distribute the weight to the ind of this category
    weights_mipfp[i,zone] <- weight_ind / sample_ind
  }
  print(paste("Zona numero",zone, "terminada!"))
}

restructure <- function(Mipfp_Tab, weights, ind, zindex = 1){
  n_zone <- dim(Mipfp_Tab)[zindex] # number of zones
  for (zone in 1:n_zone){
    for (i in 1:nrow(ind)){
      # weight of the category
      weight_ind <- Mipfp_Tab[zone, ind[i, 2], ind[i, 3]]
      
      # number of ind in the category
      sample_ind <- Ind_Tab[ind[i, 2], ind[i, 3]]
      
      # distribute the weight to the ind of this category
      weights_mipfp[i,zone] <- weight_ind / sample_ind
    }
  }
  weights_mipfp
}

# Save the matrix in a new variable
int_mipfp <- res$x.hat
trs <- int_trs(int_mipfp)

# Integerise zone per zone
for (i in 1:n_zonas){
  int_mipfp[i,,,,,,,,,] <- int_trs(int_mipfp[i,,,,,,,,,])
  print(paste("Zona numero",i, "terminada!"))
}

indiv_mipfp <- int_expand_array(int_mipfp)
Names <- c("Zone", "Dorms", "Cuartos", "TV", "INT","PCs", "Lavadoras", "Autos", "Consolas","Ingreso.corriente", "Freq")
colnames(indiv_mipfp) <- Names

ints_df <- NULL

for(i in 1:nrow(cons)){
  
  # Expandir
  ints <- int_expand_array(int_mipfp[i,,,,,,,,,])
  ints$NuevoIndex <- tibble::rownames_to_column(ints, "Index")$Index
  
  name_cve <- unique(cons_zmm$CVEGEO)[1]
  rep_cve <- rep(name_cve, dim(ints)[1])
  
  # Toma los individuos correctos
  data_frame <- data.frame(CVEGEO = rep_cve, ints)
  ints_df <- rbind(ints_df, data_frame)
  print(paste("Zona numero",i, "terminada!"))
}

write.csv(ints_df,"ZMM_microsim.csv")

#-------------------------------------------------------------------------------------------------------------------

weight_init_zone <- table(ind) # tabla con los valores de los individuos
dimnames(weight_init_zone) # vemos el nombre de las variables a pesar

init_cells <- rep(weight_init_zone, each = nrow(cons)) # realizamos una matriz de 0s por todas las columnas

names <- c(list(rownames(cons)),
           as.list(dimnames(weight_init_zone))) # asignamos los nombres de las columnas correctas

weight_init <- array(init_cells, dim = 
                       c(nrow(cons), dim(weight_init_zone)),
                     dimnames = names) # hacemos la matriz completa

# Enumeramos y seleccionamos las restricciones
con1 <- cons[,c('X1dorm','X2masdorm')] 
con2 <- cons[,c('TVPaga','NoTVPaga')]
con3 <- cons[,c('Internet','NoInternet')]
con4 <- cons[,c('PC','NoPC')]
con5 <- cons[,c('Lavadora','NoLavadora')] 
con6 <- cons[,c('Consola','NoConsola')]
con7 <- cons[,c('Auto','NoAuto')]
con8 <- cons[,c("Consola","NoConsola")]

# (Zone, Ingreso, Dorm, TV, INT, PCs, Lavadoras, Consolas, Autos)

target <- list(as.matrix(con1), # dar un objetivo al codigo
               as.matrix(con2),
               as.matrix(con3),
               as.matrix(con4),
               as.matrix(con5),
               as.matrix(con6),
               as.matrix(con7))

descript <- list(c(1,4), c(1,5), c(1,6), c(1,7), c(1,8), c(1,9), c(1,10)) # asignar las posiciones de las restricciones 

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

set.seed(42)

# tenemos que hacer enteros antes de realizar la expansión por loop
mipfp_int <- int_trs(weight_mipfp$x.hat) 

ints_df <- NULL

for(i in 1:nrow(cons)){
  
  # Integerise and expand
  ints <- int_expand_array(mipfp_int[i,,,,,,,,])
  
  name_ageb <- unique(cons_zmm$AGEB)[i]
  rep_ageb <- rep(name_ageb, dim(ints)[1])
  
  # Take the right individuals
  data_frame <- data.frame(ints, AGEB = rep_ageb)
  ints_df <- rbind(ints_df, data_frame)
  
}

library("ipfp")


M1 <- model.matrix(~ind$Dorms-1)
M2 <- model.matrix(~ind$Cuartos -1)
M3 <- model.matrix(~ind$TV -1)
M4 <- model.matrix(~ind$INT-1)
M5 <- model.matrix(~ind$PCs-1)
M6 <- model.matrix(~ind$Lavadoras-1)
M7 <- model.matrix(~ind$Autos-1)
M8 <- model.matrix(~ind$Consolas-1)
ind_cat <- data.frame(cbind(M1, M2, M3, M4, M5, M6, M7, M8))
rm(M1, M2, M3, M4, M5, M6, M7, M8)


cons <- apply(cons, 2, as.numeric) 

weights <- array(NA, dim=c(nrow(ind),nrow(cons))) 
ind_agg <- matrix(colSums(ind_cat), nrow(cons), ncol(cons), byrow = T)

ind_catt <- t(ind_cat) # transpose the dummy variables for ipfp
x0 <- rep(1, nrow(ind))
weights <- apply(cons, 1, function(x) ipfp(x, ind_catt, x0, maxit = 20))

ind_agg <- t(apply(weights, 2, function(x) colSums(x * ind_cat)))


int_expand_vector <- function(x){
  index <- 1:length(x)
  rep(index, round(x))
}

ints <- unlist(apply(weights, 2, function(x) int_expand_vector(int_trs(x))))




