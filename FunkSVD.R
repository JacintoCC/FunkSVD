SGD.feature <- function(R, U, Vt,f,
                        lr, gamma,
                        baseline){
   for(a in sample(nrow(R))){
      for(i in sample(ncol(R))){
         r_ai <- R[a,i]
         if(!is.na(r_ai)){
            p_ai <- baseline[a,i] + U[a, ] %*% Vt[ ,i]
            e_ai <- r_ai - p_ai
            temp_u_af <- U[a,f]
            U[a,f] <- U[a,f] + lr * (e_ai * Vt[f,i] - gamma * U[a,f])
            Vt[f,i] <- Vt[f,i] + lr * (e_ai * temp_u_af - gamma * Vt[f,i])
         }
      }
   }
   
   return(list(U, Vt))
}

GD.feature <- function(R, U, Vt,f,
                       lr, gamma,
                       baseline){

   prediction <- baseline + U %*% Vt
   error <- R - prediction
   
   temp_u_f <- U[ ,f]
   U[ ,f] <- U[ ,f] + lr * 
      (apply(error, 1, function(E_i) sum(E_i * Vt[f, ], na.rm = T)) - gamma * U[,f])
   Vt[f,] <- Vt[f,] + lr * 
      (apply(error, 2, function(E_a) sum(E_a *temp_u_f, na.rm = T)) - gamma * Vt[f,])
   
   return(list(U = U,Vt = Vt))
}


baseline <- function(R){
   offset.users <- apply(tra, 1, mean, na.rm = T) - mean(apply(tra, 1, mean, na.rm = T), na.rm = T)
   baseline.movies <- apply(tra, 2, mean, na.rm = T)
   
   prediction <- matrix(offset.users, ncol = ncol(R), nrow = nrow(R)) + 
      matrix(baseline.movies, ncol = ncol(R), nrow = nrow(R), byrow = T)
}

#'@function FunkSVD
#'@description Compute approximate SVD decomposition through SGD
#'
#'@param R Matrix of ratings
#'@param k Number of features
#'@param initialization Matrix initialization
#'@param lr Learning Rate
#'@param gamma Regularization term
#'@param epsilon Limit for convergence
#'@return U and V matrices 
funkSVD <- function(R, k, initialization = 0.1, 
                    lr = 0.001, gamma = 0.02, 
                    max.iter = 10,
                    verbose = T, actualization.feature = GD.feature){
   
   # Initialization of matrix
   U <- matrix(initialization, nrow = nrow(R), ncol = k)
   Vt <- matrix(initialization, nrow = k, ncol = ncol(R))

   # Baseline
   base <- baseline(R)
   # For all the features
   for(f in sample(k)){
      if(verbose)
         print(paste("Training feature", f))
      #Until convergence
      for(l in 1:max.iter){
         sgd.feature <- actualization.feature(R, U, Vt, f,
                                              lr, gamma,
                                              base)
         U <- sgd.feature$U
         Vt <- sgd.feature$Vt
      }
   }
   
   return(list(U = U, 
               V = t(Vt)))
}



#'@function Error in prediction
#'@description Error in prediction
#'
#'@param original Original matrix
#'@param funk U and V matrices
#'@param train Train matrix to regularize
#'@return RMSE 
error.prediction <- function(original, funk, train){
   # Baseline preduction
   baseline <- baseline(train)
   prediction <- baseline + funk$U %*% t(funk$V)
   # RMSE
   sqrt(mean((original-prediction)^2,na.rm=T))
}