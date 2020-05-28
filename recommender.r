#Installing and importing essential packages 
install.packages('ggplot2')
install.packages('data.table')
install.packages('reshape2')

library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)

#Retrieving the data as well as displaying the structure and important information
movie = read.csv('movies.csv', stringsAsFactors = F)
rating = read.csv('ratings.csv', stringsAsFactors = F)

str(movie)
head(movie)
summary(movie)

str(rating)
head(rating)
summary(rating)

#Preprocessing data 
#It is apparent that the userId column, as well as the movieId column, consist of integers. 
#Based on that we need to convert the genres present in our movie dataframe into a more usable format by the users. 
#our first task is creating a one-hot encoding to create a matrix of the respective genres for each of the films.

mgenre = as.data.frame(movie$genres, stringsAsFactors = F)

library(data.table)
mgenre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[|]', 
                                        type.convert=TRUE), 
                              stringsAsFactors=FALSE) 
colnames(mgenre2) <- c(1:10)

lgenre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")

genre_mat1 = matrix(0,10330,18)

genre_mat1[1,] <- lgenre

colnames(genre_mat1) <- lgenre

for (index in 1:nrow(mgenre2)) {
  for (col in 1:ncol(mgenre2)) {
    gen_col = which(genre_mat1[1,] == mgenre2[index,col]) 
    genre_mat1[index+1,gen_col] <- 1
  }
}

#remove first row & change the characters into integers
genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors=FALSE) 
for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col]) 
} 
str(genre_mat2)

SearchMatrix <- cbind(movie[,1:2], genre_mat2[])
head(SearchMatrix) 

ratingMatrix <- dcast(rating, userId~movieId, value.var = "rating", na.rm=FALSE)

#remove the userIDs
ratingMatrix <- as.matrix(ratingMatrix[,-1]) 

#Convert rating matrix into a recommenderlab sparse matrix
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
ratingMatrix

library(recommenderlab)
recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)

lapply(recommendation_model, "[[", "description")

#CHecking the structure of our realRatingMatrix

rate = ratingMatrix

as(rate,'matrix')[1:5, 1:5]

dim(rate)

str(rate)

image(rate[1:5,1:5])

as(rate,'matrix')[1:5, 1:5]

getRatingMatrix(rate)[1:5, 1:5]

sum(!is.na(as(rate,'matrix')))
sum(is.na(as(rate,'matrix')))           

mean(as(rate,'matrix')[1,],na.rm=T)  

#Split the dataset, retaining 80% in the train sample
set.seed(617)
split = sample(x = nrow(rate),size = 0.8*nrow(rate))
train = rate[split,]
test = rate[-split,]

#Examining the number of movies rated by users
summary(rowCounts(train))

library(ggplot2)
ggplot(data=data.frame(no_of_movies_rated = rowCounts(train)),aes(x=no_of_movies_rated))+
  geom_histogram(bins=50,fill='blue')+ylab('Number of Raters')+xlab('Number of Movies rated')+
  scale_x_continuous(breaks = seq(0,100,20),limits=c(0,110))+
  scale_y_continuous(breaks=seq(0,50,5),limits=c(0,50))

summary(colCounts(train))

head(colMeans(train))

#Examining the number of ratings  received
library(ggplot2)
ggplot(data=data.frame(movie_ratings = colMeans(train)),aes(x=movie_ratings))+
  geom_histogram(fill='blue')

ggplot(data=data.frame(movie_ratings = colMeans(train)),aes(x=movie_ratings))+
  geom_density(color='blue', size=1.2)

#Average rating of the first six Jokes
head(rowMeans(train))

#Examining the distribution of the average ratings of the movies
library(ggplot2)
ggplot(data=data.frame(movie_ratings = rowMeans(train)),aes(x=movie_ratings))+
  geom_histogram(fill='blue')

summary(rowMeans(train))

#Examining the distribution of the normalized average ratings of the movies
summary(rowMeans(normalize(train,method='center')))

ggplot(data=data.frame(movie_ratings = rowMeans(normalize(train))),aes(x=movie_ratings))+
  geom_histogram(fill='blue')

#Examining the effect of centering on the movie ratings
getRatings(train[1:25,2])

getRatings(train[,2])

getRatings(train[1,])

summary(rowMeans(train))

#Data after standardizing
getRatings(normalize(train, method='Z-score')[1,])

summary(getRatings(normalize(train, method='Z-score')[1,]))

#Examining the similarity 
similarity(normalize(train)[1:5,],method = 'euclidean')

similarity(normalize(train)[1:5,],method = 'cosine')

similarity(normalize(train)[1:5,],method = 'pearson')

#Non-personalized recommendations: Popular
recommenderRegistry$get_entries(dataType='realRatingMatrix')$POPULAR_realRatingMatrix

recom_popular = Recommender(train,
                            method='POPULAR',
                            parameter=list(normalize='center'))

recom_popular
class(recom_popular)

#Evaluating the predictions
pred_pop <- predict(object = recom_popular,
                     newdata = test, n = 10)
pred_pop

#Recommendation for the first user
user1 <- pred_pop@items[[1]] 
popmovies1 <- pred_pop@itemLabels[user1]
popmovies2 <- popmovies1
for (index in 1:10){
  popmovies2[index] <- as.character(subset(movie,
                                             movie$movieId == popmovies1[index])$title)
}

#Top 10 recommmendations for the user using the popular recommendation 
popmovies2

#matrix with the recommendations for each user
recom_pop2 <- sapply(pred_pop@items,
                       function(x){ as.integer(colnames(rate)[x]) }) 


#Now, instead of generating the Top n recommendations for each user, we can also 
#generate ratings for each movie. To do this, we use the type, ‘ratings’ instead of ‘topNList’

pred_popular = predict(recom_popular,newdata=test,type='ratings')

as(test,'matrix')[1,]

#Predicted ratings for movies not rated by 1
as(pred_popular,'matrix')[1,]

#User-based collaborative filtering

#Examine parameters for UBCF
recommenderRegistry$get_entries(data='realRatingMatrix')$UBCF_realRatingMatrix

#Building the recommender 
recom_ubcf = Recommender(train,
                         method='UBCF',
                         parameter=list(method='cosine',nn=25, normalize='center'))

#Evaluating the predictions
pred_ubcf <- predict(object = recom_ubcf,
                    newdata = test, n = 10)
pred_ubcf

#Recommendation for the first user using UBCF
user1 <- pred_ubcf@items[[1]] 
ubcfmovies1 <- pred_ubcf@itemLabels[user1]
ubcfmovies2 <- ubcfmovies1
for (index in 1:10){
  ubcfmovies2[index] <- as.character(subset(movie,
                                           movie$movieId == ubcfmovies1[index])$title)
}

#Top 10 recommmendations for the user using the UCBF recommender
ubcfmovies2

#matrix with the recommendations for each user
recom_ubcf2 <- sapply(pred_ubcf@items,
                     function(x){ as.integer(colnames(rate)[x]) }) 

#Movies rated
as(test,'matrix')[5,]

#Recommendations for Jokes not rated
as(pred_ubcf,'matrix')[5,]

#Item-based collaborative filtering

#Evaluate parameters for IBCF
recommenderRegistry$get_entries(data='realRatingMatrix')$IBCF_realRatingMatrix

#Building the recommender
library(recommenderlab)
recom_ibcf = Recommender(train, method='IBCF',
                         parameter=list(k=30, method='cosine',
                                        normalize='center'))
recom_ibcf
class(recom_ibcf)

#Evaluating the predictions
pred_ibcf <- predict(object = recom_ibcf,
                                     newdata = test,
                                     n = 10)
pred_ibcf

#Recommendation for the first user using IBCF
user1 <- pred_ibcf@items[[5]] 
ibcfuser1 <- pred_ibcf@itemLabels[user1]
ibcfuser2 <- ibcfuser1
for (index in 1:10){
  ibcfuser2[index] <- as.character(subset(movie,
                                             movie$movieId == ibcfuser1[index])$title)
}

#Top 10 recommmendations for the user using the UCBF recommender
ibcfuser2


recom_ibcf2 <- sapply(pred_ibcf@items,
                       function(x){ as.integer(colnames(rate)[x]) }) # matrix with the recommendations for each user

#Dimensions of recommendation matrix
dim(recom_matrix)
recom_matrix


#Evaluation Scheme 
min(rowCounts(rate))

es = evaluationScheme(rate,method='split',train=0.8, given=20)

getData(es,'train')

getData(es,'known')

getData(es,'unknown')

nratings(rate) ==
  nratings(getData(es,'train')) +
  nratings(getData(es,'known')) +
  nratings(getData(es,'unknown'))

rowCounts(getData(es,'known'))[1:20]

rowCounts(getData(es,'unknown'))[1:20]

#Build recommender
es_ubcf = Recommender(data = getData(es,'train'),
                         method='UBCF',
                         parameter = list(method='cosine',nn=25,normalize='center'))
#Generate Predictions
esu_pred = predict(es_ubcf,newdata=getData(es,'known'), type='ratings')

#Evaluate Predictions
calcPredictionAccuracy(es_ubcf,data = getData(es,'unknown'))

#Build ibcf recommender
es_ibcf = Recommender(data = getData(es,'train'),
                         method='IBCF',
                         parameter = list(method='cosine',k=30,normalize='center'))

#Generate predictions
esi_pred = predict(es_ibcf,newdata=getData(es,'known'), type='ratings')

#Evaluate predictions
calcPredictionAccuracy(esi_pred,data = getData(es,'unknown'))

#Using K-fold validation 
set.seed(617)
es = evaluationScheme(rate,method='cross-validation',k=10,given=20)
ev = evaluate(x = es,method='UBCF',parameter=list(method='cosine',nn=25), type='ratings')

avg(ev)

#Going further by using k-fold cross-validation to evaluate a set of recommenders
recommender_total = list(random = list(name='RANDOM'),
                              popular = list(name='POPULAR'),
                              ubcf = list(name='UBCF'),
                              ubcf_50 = list(name='UBCF',parameters=list(nn=50)),
                              ubcf_100 = list(name='UBCF',parameters=list(nn=100)),
                              ibcf = list(name='IBCF'),
                              ibcf_10 = list(name='IBCF', parameters=list(k=10)))

ev = evaluate(x = es,method=recommender_total, type='ratings')

#Evaluating recommenders as a condensed matrix 
results = matrix(unlist(avg(ev)),ncol=3,byrow = T)
colnames(results) = c('RMSE','MSE','MAE')
rownames(results) = c('random','popular','ubcf','ubcf_50','ubcf_100','ibcf','ibcf_10')
results

#Plotting the evaluations
plot(ev)
