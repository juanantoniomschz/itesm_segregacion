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
                                           ,"Refrigeradoes", "Tel", "Consolas", "Autos")]
  
  cons <- cons_est[cons_est$Municipio==municipio, c("X1dorm", "X2masdorm","TVPaga", "NoTVPaga","Internet", "NoInternet",
                                              "PC", "NoPC", "Lavadora", "NoLavadora", "Refri", "NoRefri",
                                              "Telefono","NoTelefono","Auto", "NoAuto", "Consola","NoConsola")]
  
  weight_init_zone <- table(ind) # tabla con los valores de los individuos
  
  init_cells <- rep(weight_init_zone, each = nrow(cons)) # realizamos una matriz de 0s por todas las columnas
 
  names <- c(list(rownames(cons)),
             as.list(dimnames(weight_init_1zone))) # asignamos los nombres de las columnas correctas
  
  weight_init <- array(init_cells, dim = 
                         c(nrow(cons), dim(weight_init_1zone)),
                       dimnames = names) # hacemos la matriz completa
  
  con1 <- cons[,c('X1dorm','X2masdorm')] 
  con2 <- cons[,c('TVPaga','NoTVPaga')]
  con3 <- cons[,c('Internet','NoInternet')]
  con4 <- cons[,c('PC','NoPC')]
  con5 <- cons[,c('Lavadora','NoLavadora')]
  con6 <- cons[,c('Refri','NoRefri')]
  con7 <- cons[,c('Telefono','NoTelefono')]
  con8 <- cons[,c('Consola','NoConsola')]
  con9 <- cons[,c('Auto','NoAuto')]
  
  target <- list(as.matrix(con1), as.matrix(con2), as.matrix(con3), as.matrix(con4), as.matrix(con5),
                 as.matrix(con6), as.matrix(con7), as.matrix(con8), as.matrix(con9))
  
  descript <- list(c(1,3), c(1,4), c(1,5), c(1,6), c(1,7), 
                   c(1,8), c(1,9), c(1,10), c(1,11)) # asignar las posiciones de las restricciones 
  
  weight_mipfp <- Ipfp(weight_init, descript, target, iter = 1, print = T)
  
  
  set.seed(42)
  
  # tenemos que hacer enteros antes de realizar la expansión por loop
  mipfp_int <- int_trs(weight_mipfp$x.hat) 
  
  ints_df <- NULL
  
  for(i in 1:nrow(cons)){
    
    # Expandir
    ints <- int_expand_array(mipfp_int[i,,,,,,,,,,])
    
    rep_municipio <- rep(municipio, dim(ints)[1])
    
    name_ageb <- unique(cons_est$AGEB)[i]
    rep_ageb <- rep(name_ageb, dim(ints)[1])
    
    # Take the right individuals
    data_frame <- data.frame( Municipio = rep_municipio, AGEB = rep_ageb, ints)
    ints_df <- rbind(ints_df, data_frame)
    
  }
  
  micro_sim <- rbind(micro_sim, ints_df)
  
}


ind <- ind_full[ind_full$Municipio=='Monterrey', c("Ingreso.corriente"
                                         ,"Dorm"
                                         , "TV"
                                         , "INT"
                                         ,"PCs"
                                         ,"Lavadoras"
                                         ,"Refrigeradoes"
                                         ,"Tel"
                                         ,"Consolas"
                                         ,"Autos"
                                         )]

cons <- cons_full[cons_full$Municipio=='Monterrey', c("X1dorm", "X2masdorm"
                                            ,"TVPaga", "NoTVPaga"
                                            ,"Internet", "NoInternet" 
                                            ,"PC", "NoPC"
                                            ,"Lavadora", "NoLavadora"
                                            ,"Refri", "NoRefri"
                                            ,"Telefono","NoTelefono"
                                            ,"Auto", "NoAuto"
                                            ,"Consola","NoConsola"
                                            )]

weight_init_1zone <- table(ind) # tabla con los valores de los individuos
dimnames(weight_init_1zone) # vemos el nombre de las variables a pesar

init_cells <- rep(weight_init_1zone, each = nrow(cons)) # realizamos una matriz de 0s por todas las columnas

names <- c(list(rownames(cons)),
           as.list(dimnames(weight_init_1zone))) # asignamos los nombres de las columnas correctas

weight_init <- array(init_cells, dim = 
                       c(nrow(cons), dim(weight_init_1zone)),
                     dimnames = names) # hacemos la matriz completa

library(mipfp)

# enumeramos las restricciones
con1 <- cons[,c('X1dorm','X2masdorm')] 
con2 <- cons[,c('TVPaga','NoTVPaga')]
con3 <- cons[,c('Internet','NoInternet')]
con4 <- cons[,c('PC','NoPC')]
con5 <- cons[,c('Lavadora','NoLavadora')]
con6 <- cons[,c('Refri','NoRefri')]
con7 <- cons[,c('Telefono','NoTelefono')]
con8 <- cons[,c('Consola','NoConsola')]
con9 <- cons[,c('Auto','NoAuto')]

# (Zone, Ingreso, Dorm, TV, INT, PCs, Lavadoras, Refri, Tel, Consolas, Autos)

target <- list(as.matrix(con1), # dar un objetivo al codigo
               as.matrix(con2),
               as.matrix(con3),
               as.matrix(con4),
               as.matrix(con5),
               as.matrix(con6),
               as.matrix(con7),
               as.matrix(con8),
               as.matrix(con9)
               )

descript <- list(c(1,3), c(1,4), c(1,5), c(1,6), c(1,7), c(1,8), c(1,9), c(1,10), c(1,11)) # asignar las posiciones de las restricciones 

weight_mipfp <- Ipfp(weight_init, descript, target, iter = 10, print = T)

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
  ints <- int_expand_array(mipfp_int[i,,,,,,,,,,])
  zone <- rep(i, dim(ints)[1])
  
  # Take the right individuals
  data_frame <- data.frame(ints, zone = zone)
  ints_df <- rbind(ints_df, data_frame)
  
}

