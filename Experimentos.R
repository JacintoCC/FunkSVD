source("FunkSVD.R")

make.experiment <- function(train, test, parameters){
   funk.svd <- do.call(funkSVD, append(list(R = train),
                                       parameters))
   return(c("E.in" = error.prediction(train, funk.svd, train),
            "E.out"= error.prediction(test, funk.svd, train)))
}

make.experiment(tra, tst, parameters = list(k = 2))

param.grid <- expand.grid(k = seq(2,40,by=4),
                          initialization = 0.1,
                          lr = seq(0.0005,0.002, length.out = 4),
                          gamma = seq(0.01, 0.03, length.out = 3),
                          max.iter= c(10,50),
                          verbose = F)

results <- apply(param.grid, 1, function(param.combination){
   print(param.combination)
   make.experiment(tra,tst,param.combination)
})
