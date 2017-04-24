#'@function FunkSVD
#'@description Compute approximate SVD descomposition through SGD
#'
#'@param R Matrix of ratings
#'@param k Number of features
#'@param initialization Matrix initialization
#'@param lr Learning Rate
#'@param gamma Regularization term
#'@param epsilon Limit for convergence
#'@return U, Sigma and V matrices 
funkSVD <- function(R, k, initialization = 0.1, 
                    lr = 0.001, gamma = 0.02, epsilon = 0.005){
   
   # Initialization of matrix
   U <- matrix(0.1, nrow = nrow(R), ncol = k)
   Vt <- matrix(0.1, nrow = k, ncol = ncol(R))
  
   # For all the features
   for(f in 1:k){
      # Convergence variables
      error  <- 1
      prev.error <- 2
      
      #Until convergence
      while(prev.error - error > epsilon){
         prev.error <- error
         error <- 0
         
         for(a in 1:nrow(R)){
            for(i in 1:ncol(R)){
               r_ai <- R[a,i]
               if(!is.na(r_ai)){
                  p_ai <- U[a, ] %*% Vt[ ,i]
                  e_ai <- r_ai - p_ai
                  temp_u_af <- U[a,f]
                  U[a,f] <- U[a,f] + lr * (e_ai * Vt[f,i] - gamma * U[a,f])
                  Vt[f,i] <- Vt[f,i] + lr * (e_ai * U[a,f] - gamma * Vt[f,i])
                  error <- error + e_ai
               }
            }
         }
      } 
   }
   
   return(list(U = U, 
               V = t(Vt)))
}

error.prediction(original, prediction){
   sqrt(mean((original-prediction)^2,na.rm=T))
}