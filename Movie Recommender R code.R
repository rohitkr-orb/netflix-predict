setwd('/Users/pogupta/lecture/dataMining/archive')

library(dummies)
library(scales)
options(scipen=999)
##Flattening the data
file1<- read.csv("combined_data_1.txt", header = FALSE,col.names=c('Cust_Id', 'Rating',"Rating_date"))

head(file1)
file1$Movie.Id<-0
n <- which(is.na(file1$Rating))
head(file1)
head(n)


for( i in 1:(length(n)-1) ){
  movieId <- as.character(file1$Cust_Id[n[i]])
  file1$Movie.Id[n[i]:(n[i+1]-1)]<-as.numeric(strsplit(movieId,"\\:"))
}

head(file1)

for(i in n){
  file1<-file1[-i,]
}

write.csv(file1,'file2_r.csv')


##Union the data with movie details
#file2<- read.csv("movie_titles.csv",header = FALSE,col.names=c('Movie.Id', 'Year_Release',"Summary"))
movie.rating<- read.csv("file2_r.csv", header = TRUE)
movie.details<- read.csv("movie_titles.csv", header = FALSE)
names(movie.details) <- c("Movie.Id", "Movie_Year") 
movie.details <- na.omit(movie.details)
movie.details$Movie_Year <- as.numeric(unfactor(movie.details$Movie_Year))

tail(as.integer(movie.details$Movie_Year))
#movie.rating[movie.id !=c(4388,4794,),]

library(varhandle)
y <- as.numeric(unfactor(movie.details$Movie_Year))

##Remving all nulls
movie.rating <- movie.rating[complete.cases(movie.rating$Rating), ]

head(movie.rating)
#Aggregate the data
getDateColumn <- function(str.date, index){
  unlist(strsplit(as.character(str.date), "\\/"))[index]
}

##Convert date
formatYear <- function(str.year){
  year <- as.integer(str.year)
  if (year>20){
    paste('19',str.year,sep = "")
  }else{
    paste('20',str.year,sep = "")
  }
  
}


movie.rating$Rating_Month <- as.numeric(sapply(movie.rating$Rating_date,getDateColumn,index=1))
movie.rating$Rating_Year <- as.numeric(sapply(sapply(movie.rating$Rating_date,getDateColumn,index=3), formatYear))

unique(movie.rating$Rating_Year)

data.for.plot <- aggregate(movie.rating$Rating, by = list(movie.rating$Rating_Month,movie.rating$Rating_Year), FUN = mean)
names(data.for.plot) <- c("Rating_Month", "Rating") 


##Average rating is same for months
barplot(data.for.plot$Rating,  names.arg = data.for.plot$Rating_Month + data.for.plot$Rating_Year, 
        xlab = "Rating months (in number)", ylab = "Rating (1 to 5)", ylim=range(pretty(c(0, data.for.plot$Rating))))


##Movie count rated for each month with highest in October
data.for.plot_movie_count <- aggregate(movie.rating$Movie.Id, by = list(movie.rating$Rating_Month,movie.rating$Rating_Year), FUN = length)
names(data.for.plot_movie_count) <- c("Rating_Month", "Rating_Year","Movie_Count") 

barplot(data.for.plot_movie_count$Movie_Count,  names.arg = data.for.plot_movie_count$Rating_Month + data.for.plot_movie_count$Rating_Year, 
        xlab = "Rating months (in number)", ylab = "Movie count in months")

##########################Seasonal Based Movie prediction###############################
#Create a data with month, movie.Id, 0/1 if predicted in that month or not
month_movie_cust_agg <- aggregate(movie.rating$Cust_Id, by = list(movie.rating$Rating_Month,movie.rating$Movie.Id ), FUN = length)
colnames(month_movie_cust_agg)<- c('Rating.Month','Movie.Id','Cust_count')

movie_cust_agg <- aggregate(movie.rating$Cust_Id, by = list(movie.rating$Movie.Id), FUN = length)
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

library(FNN)
library(caret)
library(forecast)

newdfknn <- knn(train=train.df[,-3], test=valid.df[,-3], cl=train.df[, 3], k = 3)
confusionMatrix(newdfknn, as.factor(valid.df[, 3]))

##############################################################################################

year_movie_cust_agg <- aggregate(movie.rating$Cust_Id, by = list(movie.rating$Rating_Year,movie.rating$Movie.Id ), FUN = length)
colnames(year_movie_cust_agg)<- c('Rating.Year','Movie.Id','Cust_count')
head(year_movie_cust_agg)

movie_cust_agg <- aggregate(movie.rating$Cust_Id, by = list(movie.rating$Movie.Id), FUN = length)
colnames(movie_cust_agg)<- c('Movie.Id','total_cust_count')

year_cust_agg <- merge(year_movie_cust_agg, movie_cust_agg, by=c('Movie.Id'))

year_cust_agg_with_release <- merge(year_cust_agg,movie.details,  by='Movie.Id') 
head(movie.details)
summary(year_cust_agg_with_release$Movie.Id)

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
accuracy(movieRating.pred.valid, valid.df$CustLiked)
ggplot(year_cust_agg_with_release, aes(x= Rating.Year, y=Cust_count, group=Movie_Year)) +geom_line(aes(color=Movie_Year))+ geom_point(aes(color=Movie_Year))
head(year_cust_agg_with_release)

########################Prediction End ###################################################

#Timeseries data -- Data various in each quarters differently
rating.ts <- ts(movie.rating$Rating, start=c(2003,1), end=c(2008,4), frequency = 4)
plot(rating.ts, xlab="Year",ylab="Movie Rating")

#Get movie rating counts
a <- table(movie.rating$Rating)

################Generating Movie data
# get movie count
movie.count <- length(unique(movie.rating$Movie.Id))

# get customer count
customer.count <- length(unique(movie.rating$Cust_Id))
customer.count

##Find info from movie summary
df_movie_summary <- aggregate(movie.rating$Rating, by = list(movie.rating$Movie.Id), FUN = mean)


cf <- movie.rating[sample(nrow(movie.rating), 1000), ]

summary(cf$Cust_Id, cf$Rating, cf$Movie.Id)


##Creating pivot table
library(recommenderlab)
library(reshape2)
f.mat<-acast(cf,Cust_Id~Movie.Id,value.var="Rating")
head(f.mat)
class(f.mat)

R<-as.matrix(f.mat)

mm<-as(R,"realRatingMatrix")

###Look into mm
as(mm, "matrix") 

#############User based collaborative filter##############

IB.Rec <- Recommender(mm, "UBCF",param=list(method="Cosine",nn=3, minRating=1))
pred <- predict(IB.Rec, mm[3],n=3, type = "topNList")
as(pred, "matrix")


