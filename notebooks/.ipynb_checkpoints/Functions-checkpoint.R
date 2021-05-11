





# Integerisation 

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

# Expansion for Weights for Individuals

int_expand_vector <- function(x){ index <- 1:length(x)
rep(index, round(x))
}

# Example Expansion for Weights for Individuals
exp_indices <- int_expand_vector(int_weight1) exp_indices
ind_full[exp_indices,]

# Expansion for Weights for Categories

int_expand_array <- function(x){
  # Transform the array into a dataframe
  count_data <- as.data.frame.table(x)
  # Store the indices of categories for the final population indices <- rep(1:nrow(count_data), count_data$Freq)
  # Create the final individuals
  ind_data <- count_data[indices,]
  ind_data
}

# Example for Weights for Categories

ind_mipfp <- int_expand_array(mipfp_int)

# Integerisation and Expansion Method 1: using a for loop
ints_df <- NULL 
set.seed(42)

for(i in 1:nrow(cons)){
  # Integerise and expand
  ints <- int_expand_vector(int_trs(weights[, i])) # Take the right individuals
  data_frame <- data.frame(ind_full[ints,]) ints_df <- rbind(ints_df, data_frame, zone = i)
}

# Integerisation and Expansion Method 2: using apply
set.seed(42)
ints_df2 <- NULL

# Take the right indices
ints2 <- unlist(apply(weights, 2, function(x)
  int_expand_vector(int_trs(x))))

# Generate the individuals
ints_df2 <- data.frame(ind_full[ints2,],
                       zone = rep(1:nrow(cons), colSums(weights)))
