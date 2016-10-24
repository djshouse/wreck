require(data.table)
require(recommenderlab)
require(recosystem)
require(lsa)

setwd('~/Documents/Learning/wreck/')
ratings <- read.delim('ml-100k/u.data',
                      header = FALSE,
                      col.names = c('user_id', 
                                    'item_id', 
                                    'rating', 
                                    'timestamp'
                                    )
                      )
ratings <- data.table(ratings)

movies <- read.delim('ml-100k/u.item',
                     sep = '|',
                     header = FALSE,
                     col.names = c('movie_id',
                                   'movie_title',
                                   'release_date',
                                   'video_release_date',
                                   'imdb_url',
                                   'unknown',
                                   'action',
                                   'adventure',
                                   'animation',
                                   'childrens',
                                   'comedy',
                                   'crime',
                                   'documentary',
                                   'drama',
                                   'fantasy',
                                   'film_noir',
                                   'horror',
                                   'musical',
                                   'mystery',
                                   'romance',
                                   'sci_fi',
                                   'thriller',
                                   'war',
                                   'western'
                                   )
                     )
movies <- data.table(movies)
setkey(movies, movie_id)

ratings[, mu := mean(rating)]
ratings[, bu := sum(rating - mu) / .N, keyby = user_id]
ratings[, bi := sum(rating - bu - mu) / .N, keyby = item_id]
ratings[, norm_rating := rating - mu - bu - bi]

setkeyv(ratings, c('item_id', 'user_id'))
full_ratings <- ratings[CJ(unique(item_id), unique(user_id))]
full_ratings[is.na(norm_rating), norm_rating := 0]

M <- as.matrix(dcast(full_ratings, 
                     user_id ~ item_id, 
                     value.var = 'norm_rating'
                     )
               )[, -1]

simMat <- cosine(M)

myratings <- rep(NA,1682)
myratings[28] <- 4
myratings[82] <- 5
myratings[64] <- 5
myratings[78] <- 2
myratings[682] <- 4
myratings[313] <- 3
myratings[252] <- 2
myratings[132] <- 3
myratings[127] <- 5
myratings[257] <- 3
myratings[121] <- 4
myratings[871] <- 4
myratings[523] <- 4
myratings[66] <- 1
myratings[105] <- 4
myratings[435] <- 5
myratings[801] <- 2
myratings[755] <- 4
myratings[323] <- 3
myratings[510] <- 4

mynormratings <- myratings - unique(ratings$mu)
mynormratings <- mynormratings - (sum(mynormratings, na.rm = TRUE) / length(mynormratings))
mynormratings <- mynormratings - full_ratings[user_id == 1, bi]
mynormratings[which(is.na(mynormratings))] <- 0

nonzeros <- which(myratings != 0)
zeros <- which(myratings == 0)

myratings[nonzeros] <- myratings[nonzeros] - mean(myratings[nonzeros])

predictRating <- function(movieID, ratingvec) {
  if (movieID == 1) {
    simvec <- c(1, simMat[movieID, seq(movieID + 1, 1682)])
  } else if (movieID == 1682) {
    simvec <- c(simMat[seq(movieID - 1), movieID], 1)
  } else {
    simvec <- c(simMat[seq(movieID - 1), movieID], 1, simMat[movieID, seq(movieID + 1, 1682)])
  }
  t(ratingvec) %*% simvec
}

preds <- sapply(zeros, predictRating, ratingvec = myratings)
top10 <- order(-preds)[seq(10)]
bottom10 <- order(preds)[seq(10)]
movies[top10, .(movie_id, movie_title)]
movies[bottom10, .(movie_id, movie_title)]

### LSA
tm <- as.textmatrix(t(M))

decomp <- lsa(tm)

myratingstm <- as.textmatrix(as.matrix(mynormratings))
duh <- fold_in(myratingstm, decomp)
duh <- cbind(duh, seq(length(duh)))
top10 <- head(intersect(order(-duh), which(is.na(myratings))), 10)
bottom10 <- tail(intersect(order(-duh), which(is.na(myratings))), 10)
movies[top10, movie_title]
movies[bottom10, movie_title]


### Matrix Factorization
withme <- rbind(ratings[, .(user_id, item_id, rating)],
                myratings_dt)

train <- data_memory(withme[, user_id],
                     withme[, item_id],
                     withme[, rating],
                     index1 = TRUE)
test <- data_memory(rep(944, 1662),
                    which(is.na(myratings)),
                    index1 = TRUE)

r <- Reco()
opts <- r$tune(train, list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                           costp_l1 = 0, costq_l1 = 0,
                           nthread = 1, niter = 10))
opts

r$train(train, opts = c(opts$min, nthread = 1, niter = 1000))
preds <- r$predict(test, out_memory())
head(preds)

myratings_dt <- data.table('user_id' = 944, 
                           'item_id' = which(myratings > 0), 
                           'rating' = myratings[which(myratings > 0)])

ord_preds <- order(-preds)
movies[movie_id %in% head(ord_preds, 10), .(movie_id, movie_title)]
