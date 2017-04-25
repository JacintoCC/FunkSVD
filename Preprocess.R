ratings <- read.csv("Data/ratings.csv")
ratings$movieId <- factor(ratings$movieId)
set.seed(918273645)
shuffle.idx <- sample(nrow(ratings))
train.idx <- shuffle.idx[1:(0.7*length(shuffle.idx))]
test.idx <- shuffle.idx[(0.7*length(shuffle.idx)):length(shuffle.idx)]

tra <- matrix(NA, nrow = max(ratings$userId), 
              ncol = length(unique(ratings$movieId)))

tst <- matrix(NA, nrow = max(ratings$userId), 
              ncol = length(unique(ratings$movieId)))

tra[cbind(ratings[train.idx, 1], ratings[train.idx, 2])] <- ratings[train.idx, 3]
tst[cbind(ratings[test.idx, 1], ratings[test.idx, 2])] <- ratings[test.idx, 3]

colnames(tra) <- levels(ratings$movieId)
colnames(tst) <- levels(ratings$movieId)
