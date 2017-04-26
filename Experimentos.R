source("FunkSVD.R")

make.experiment <- function(train, test, parameters){
   funk.svd <- do.call(funkSVD, append(list(R = train),
                                       parameters))
   return(c("E.in" = error.prediction(train, funk.svd, train),
            "E.out"= error.prediction(test, funk.svd, train)))
}

make.experiment(tra, tst, parameters = list(k = 2))

param.grid <- expand.grid(k = seq(2,40, by = 4),
                          initialization = 0.1,
                          lr = seq(0.0005,0.002, length.out = 4),
                          gamma = seq(0.01, 0.03, length.out = 3),
                          max.iter= 10,
                          verbose = F)
param <- mapply(list, 
                k = param.grid$k, 
                initialization = param.grid$initialization,
                lr = param.grid$lr,
                gamma = param.grid$gamma,
                max.iter = param.grid$max.iter,
                verbose = param.grid$verbose,
                SIMPLIFY = F)

library(pbmcapply)
results <- pbmclapply(param, function(param.combination){
   timestamp()
   cat(unlist(param.combination))
   single.res <- make.experiment(tra,tst,
                                 parameters = append(param.combination, 
                                                     list(actualization.feature = SGD.feature)))
}, mc.cores = 3, mc.preschedule = T, mc.silent = F)
