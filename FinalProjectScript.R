library(recommenderlab)
library(data.table)
library(reshape2)
library(ggplot2)
library(plyr)
library(dplyr)
library(caret)
library(forecast)
library(tidyr)
library(rpart)
library(rpart.plot)

setwd("D:/Utkarsh/MSBA/Data Mining/Project")
options(scipen=999) 
options(stringsAsFactors = TRUE)

########################Execute only once, flattening the data#########################################
##Flattening the data
file1<- read.csv("combined_data_1.txt", header = FALSE,col.names=c('Cust_Id', 'Rating',"Rating_date"))

head(file1)
file1$Movie_id<-0
n <- which(is.na(file1$Rating))
head(file1)
head(n)


for( i in 1:(length(n)-1) ){
  movieId <- as.character(file1$Cust_Id[n[i]])
  file1$Movie_id[n[i]:(n[i+1]-1)]<-as.numeric(strsplit(movieId,"\\:"))
}

head(file1)

for(i in n){
  file1<-file1[-i,]
}

write.csv(file1,'Combined_Files.csv')

########################Execute only once#########################################

movie_data <- read.csv("movie_titles.csv", header =FALSE,stringsAsFactors=FALSE,col.names=c("Movie_Id","Year","Title"))
str(movie_data)
head(movie_data)

summary(movie_data)
rating_data <- read.csv("Combined_Files.csv",nrows = 1000000)
rating_data<-rating_data[,-1]
head(rating_data)
str(rating_data)
unique(rating_data$Movie_id)# in this we have 225 movies

# check for duplicate rows 
x<-(rating_data %>% group_by(Cust_Id, Movie_id) %>% mutate(n=n()))
check.duplicate<- x[x$n>1,] 
dim(check.duplicate) # this shows that there are no duplicate rows 

# distribution of mean rating 
rating_data %>% 
  group_by(Cust_Id) %>% 
  summarize(mean_user_rating = mean(Rating)) %>% 
  ggplot(aes(mean_user_rating)) +
  geom_histogram(fill = "cadetblue3", color = "grey20",bins=15)


# average rating for a movie
#memory.limit(size=8000000)
mydata <- dcast(rating_data, Cust_Id~Movie_id, value.var = "Rating", fill = 0)
sums <- data.frame(colSums(mydata[,2:dim(mydata)[2]]))
nonzero <- ldply(mydata[,2:dim(mydata)[2]], function(c){sum(c!=0)})[2]
average.rating <- sums/nonzero
colnames(average.rating) <- "Rating"
average.rating$movie <- colnames(mydata)[2:dim(mydata)[2]]
average.rating$Rating<-round(average.rating$Rating,3)


# number of rating per movie
movie.rating.count<- data.frame(average.rating$movie,nonzero)
movie.rating.count<-rename(movie.rating.count, Movie_id = average.rating.movie)
movie.rating.count<-rename(movie.rating.count, Count = V1)
head(movie.rating.count)
combine<-merge(x=movie.rating.count, y=average.rating, by.x = c("Movie_id"),by.y=c("movie")) 
head(combine)

# top 10 most popular movie by
combine %>% arrange(-Count) %>% top_n(10,wt=Count)

# top ten movie with highest average rating 
combine %>% arrange(-Rating) %>% top_n(10,wt = Rating) 

# this shows thats movies which are most popular are not highly rated and movies which are rated by most custermer don't have highest avg rating 

average.rating$overall<-.bincode(average.rating$Rating,c(0,1,2,3,4,5)) # for calculation of averating between o-1,1-2 and so on
p<-aggregate(data.frame(count = average.rating$overall),list(value = average.rating$overall),length)
p
ggplot(p, aes(x = value, y = count)) +
  geom_bar(stat="identity", fill = 'steelblue') +
  geom_text(aes(label=count), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Average rating  plot of movies")+ylab("No of Movies")
# this plot shows that out of 225 movies 134 movies received average rating between 3 $ 4
#ratingMatrix <- as.matrix(mydata)

ratingMatrix <- as.matrix(mydata[,-1]) #remove userIds

Freq1<-length(which(ratingMatrix ==1))
Freq2<-length(which(ratingMatrix ==2))
Freq3<-length(which(ratingMatrix ==3))
Freq4<-length(which(ratingMatrix ==4))
Freq5<-length(which(ratingMatrix ==5))

freq.table<- data.frame(rating=c(1,2,3,4,5),count=c(Freq1,Freq2,Freq3,Freq4,Freq5))

ggplot(data=freq.table, aes(x=rating, y=count,fill = factor(count))) +
  geom_bar(stat="identity",color = "grey20")+ geom_text(aes(label=count),vjust=-0.3, size=3.5)+
  scale_fill_brewer(palette = "YlGnBu")+ guides(fill = FALSE)
# we can see that most of the movie got rating 4. This shows that either customer who give rating like there movie or all the customers are liking there movies

#########viewership model####
getDateColumn <- function(str.date, index){
  unlist(strsplit(as.character(str.date), "\\/"))[index]
}

##Convert date
formatYear <- function(str.year){
  year <- as.integer(str.year)
  if (is.na(year)){
   "1700"
  }
  else if (year > 20){
    paste('19',str.year,sep = "")
  }else{
    paste('20',str.year,sep = "")
  }
}

rating_data_viewership <-rating_data
head(rating_data_viewership)

rating_data_viewership$Rating_Month <- as.numeric(sapply(rating_data_viewership$Rating_date,getDateColumn,index=1))
rating_data_viewership$Rating_Year <- as.numeric(sapply(sapply(rating_data_viewership$Rating_date,getDateColumn,index=3), formatYear))
head(rating_data_viewership)

###Removing missing data
rating_data_viewership<- rating_data_viewership[!(rating_data_viewership$Rating_Year==1700),]
head(rating_data_viewership)

unique(rating_data_viewership$Rating_Month)

data.for.plot <- aggregate(rating_data_viewership$Rating, by = list(rating_data_viewership$Rating_Month,rating_data_viewership$Rating_Year), FUN = mean)
names(data.for.plot) <- c("Rating_Month", "Rating_Year","Rating") 
head(data.for.plot)

##Average rating for each year and month
barplot(data.for.plot$Rating,  names.arg = data.for.plot$Rating_Month + data.for.plot$Rating_Year, 
        xlab = "Rating months (in number)", ylab = "Rating (1 to 5)", ylim=range(pretty(c(0, data.for.plot$Rating))))

##Movie count rated for each month with highest in October
data.for.plot_movie_count <- aggregate(rating_data_viewership$Movie_id, by = list(rating_data_viewership$Rating_Month,rating_data_viewership$Rating_Year), FUN = length)
names(data.for.plot_movie_count) <- c("Rating_Month", "Rating_Year","Movie_Count") 

barplot(data.for.plot_movie_count$Movie_Count,  names.arg = data.for.plot_movie_count$Rating_Month + data.for.plot_movie_count$Rating_Year, 
        xlab = "Rating months (in number)", ylab = "Movie count in months")

month_movie_cust_agg <- aggregate(rating_data_viewership$Cust_Id, by = list(rating_data_viewership$Rating_Month,rating_data_viewership$Movie_id ), FUN = length)
colnames(month_movie_cust_agg)<- c('Rating.Month','Movie.Id','Cust_count')

movie_cust_agg <- aggregate(rating_data_viewership$Cust_Id, by = list(rating_data_viewership$Movie_id), FUN = length)
colnames(movie_cust_agg)<- c('Movie.Id','total_cust_count')

movie_month_cust_agg <- merge(month_movie_cust_agg, movie_cust_agg, by=c('Movie.Id'))
movie_month_cust_agg$CustLiked <- ifelse(movie_month_cust_agg$Cust_count/movie_month_cust_agg$total_cust_count>=0.1,1,0)
head(movie_month_cust_agg)

s<- c(1,2,5)
train_rows <- sample(1:dim(movie_month_cust_agg)[1],0.6*dim(movie_month_cust_agg)[1])
train.df <- movie_month_cust_agg[train_rows,c(1,2,5)]
valid.df <- movie_month_cust_agg[-train_rows,c(1,2,5)]
summary(train.df)
dim(valid.df)

ggplot(movie_month_cust_agg, aes(x=Rating.Month , y=Cust_count, group=Movie.Id)) +geom_line(aes(color=Movie.Id))+ geom_point(aes(color=Movie.Id))

newdfknn <- knn(train=train.df[,-3], test=valid.df[,-3], cl=train.df[, 3], k = 3)
confusionMatrix(newdfknn, as.factor(valid.df[, 3]))

#########Classification Tree######

year_movie_cust_agg <- aggregate(rating_data_viewership$Cust_Id, by = list(rating_data_viewership$Rating_Year,rating_data_viewership$Movie_id ), FUN = length)
colnames(year_movie_cust_agg)<- c('Rating.Year','Movie.Id','Cust_count')
head(year_movie_cust_agg)

movie_cust_agg <- aggregate(rating_data_viewership$Cust_Id, by = list(rating_data_viewership$Movie_id), FUN = length)
colnames(movie_cust_agg)<- c('Movie.Id','total_cust_count')

year_cust_agg <- merge(year_movie_cust_agg, movie_cust_agg, by=c('Movie.Id'))

year_cust_agg_with_release <- merge(year_cust_agg,movie.details,  by='Movie.Id') 
head(movie.details)
summary(year_cust_agg_with_release)

year_cust_agg_with_release$CustLiked <- ifelse(year_cust_agg_with_release$Cust_count/year_cust_agg_with_release$total_cust_count>=0.1,1,0)

summary(year_cust_agg_with_release)

selectedFeatures <- c('Movie.Id ','Rating.Year', 'Movie_Year','CustLiked')
s<- c(1,2,5,6)
train_rows <- sample(1:dim(year_cust_agg_with_release)[1],0.6*dim(year_cust_agg_with_release)[1])
train.df <- year_cust_agg_with_release[train_rows,c(1,2,5,6)]
valid.df <- year_cust_agg_with_release[-train_rows,c(1,2,5,6)]
summary(train.df)
dim(valid.df)

library(rpart)
library(rpart.plot)

movieRated.tree <- rpart(CustLiked ~ ., data = train.df,  method = "class",cp=0)

prp(movieRated.tree, type = 1, extra = 1, split.font = 1, varlen = -10)  
movieRating.pred.valid <- predict(movieRated.tree,valid.df[,-4])
accuracy(as.factor(movieRating.pred.valid), as.factor(valid.df$CustLiked))
ggplot(year_cust_agg_with_release, aes(x= Rating.Year, y=Cust_count, group=Movie_Year)) +geom_line(aes(color=Movie_Year))+ geom_point(aes(color=Movie_Year))

#####Viewership model###

###Apriori Algorithm #####
head(rating_data)

netflix_matrix<-as.matrix(mydata[,-1])
netflix_matrix[ is.na(netflix_matrix)] <- 0

netflix_trx <- as(netflix_matrix, "transactions")

moviesrules <- apriori(netflix_trx, parameter = list(supp = 0.1, conf = 0.2,  target = "rules"))
inspect(moviesrules)

####End of Apriori Alogrithm###

# Part I
dimension_names <- list(user_id = mydata[,1], movie_Id = colnames(mydata)[2:(dim(mydata)[2])])
dimnames(ratingMatrix) <- dimension_names
ratingMatrix[1:10,1:10]

#Step 1: Find similar users
similar.user <- "16272"
item.rated<-as.data.frame(ratingMatrix[similar.user,])
item.rated <- which(item.rated$`ratingMatrix[similar.user, ]`!=0)
selected_users <- names(which(apply(ratingMatrix[ ,item.rated], 1, sum) >= 2))
head(selected_users, 40)
# this is the list of user who have rated at least one movie similar to our current user 


# Finding similarity between user 1 and other user
rmat <- ratingMatrix[selected_users,item.rated ]
similarities <- cor(t(rmat[rownames(rmat)!=similar.user, ]), rmat[similar.user, ], use = 'all.obs')
sim <- as.vector(similarities)
names(sim) <- rownames(similarities)
res <- sort(sim, decreasing = TRUE)
head(res, 10)

users <- names(res)
similar_users_ratings <- data.frame(item = rep(colnames(ratingMatrix), length(users)), rating = c(t(as.data.frame(ratingMatrix[users,])))) %>% filter((rating)!=0)
current_user_ratings <- data.frame(item = colnames(rmat), rating = rmat[similar.user,]) %>% filter((rating)!=0)

predictions <- similar_users_ratings %>% 
  filter(!(item %in% current_user_ratings$item)) %>% 
  group_by(item) %>% summarize(mean_rating = mean(rating))

# top 10 movies for the user 
top10<-head(data.frame(predictions %>% arrange(-mean_rating)),10)
movie_data[movie_data$Movie_Id %in% top10$item,]


sparse_ratings <- as(ratingMatrix, "sparseMatrix")
train.real.matrix <- as(sparse_ratings, "realRatingMatrix")
# user based 
recommen_model_user_based <- Recommender(data = train.real.matrix,method = "UBCF",parameter = list(method="pearson",nn=1000000))
predicted_recommendations_user_based <- predict(object = recommen_model_user_based,newdata = train.real.matrix[similar.user,],type = "ratings")
predicted_recommendations_user_based 

top10.algo<- as(predicted_recommendations_user_based,'data.frame')%>% arrange(-rating) %>% .[1:10,]
movie_data[movie_data$Movie_Id %in% top10.algo$item,]

# this shows that we have 80 % similar prediction.  


# Item based 
recommen_model_Item_based <- Recommender(data = train.real.matrix,method = "IBCF")
predicted_recommendations_Item_based <- predict(object = recommen_model_Item_based,newdata = train.real.matrix,type = "ratings")


item<- as(predicted_recommendations_Item_based,'data.frame')%>% arrange(-rating)
top10.item<- data.frame(head(unique(item$item),10))
movie_data[movie_data$Movie_Id %in% top10.item$head.unique.item.item...10.,]

# top 10 item based movies 


# evaluation of model 


ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
ratingMatrix
eval.model<- evaluationScheme(ratingMatrix[1:50000,], method="cross-validation",k=100, given=-1, goodRating=5)

algo.eval <- list("random" = list(name = "RANDOM", param = NULL),
                  "POPULAR" = list(name = "POPULAR", param = NULL),
                   "UBCF" = list(name = "UBCF", param =  list(method="pearson")),
                   "ITEM" = list(name = "IBCF"))


results <- evaluate(eval.model, algo.eval, type = "ratings")

plot(results,annotate = 1:4, legend="topright")


