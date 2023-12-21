#install & load libraries
install.packages("tidyverse")
library(tidyverse)
install.packages("skimr")
library(skimr)
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("googledrive")
library(googledrive)

#import file from google drive 
mydrive <- drive_find(n_max = 10) #this will direct your gdrive browser for permission
id <- "1TRPPHAA2nn2NtpHn5GZaxKYVKzj27jQ5"
youtube_df <- read.csv(paste0("https://drive.google.com/uc?id=",id,
                              "&export=download"),na.strings = c(""))

#Q1.understanding structure of dataframe and key variables & check for missing values
as_tibble(youtube_df)
as_tibble(head(youtube_df))
as_tibble(tail(youtube_df))
summary(youtube_df)
glimpse(youtube_df)
skim_without_charts(youtube_df)
colnames(youtube_df)

#only taking distinct values
youtube_df<- youtube_df[!duplicated(youtube_df$Username),]

#check for missing values
colSums(is.na(youtube_df))

#method 1 
#to remove na
new_df <- youtube_df[complete.cases(youtube_df), ]
as_tibble(new_df)

#drop na value
youtube_df <- na.omit(youtube_df)

#print data with missing values as tibble
as_tibble(youtube_df[!complete.cases(youtube_df), ])
#method 2
#replace 'na' with 'entertainment'
youtube_df$Categories <- youtube_df$Categories %>%
  replace_na('Entertainment')



#check
colSums(is.na(youtube_df))

#rename coloumn name 'Suscribers' to 'Subscriber'
youtube_df <- rename(youtube_df, Subscribers = Suscribers)
str(youtube_df)

#check for outliers
boxplot(youtube_df$Subscribers)




#Q2.Trend Analysis

  #Streamers' Top Categories distribution
top100_categories <- head(youtube_df,100)
top100 <- top100_categories%>%
  group_by(Categories)%>%
  summarise(no_of_streamer = n())%>%
  arrange(-no_of_streamer)

  #plotting streamers' top categories
ggplot(data = top100, aes(no_of_streamer,reorder(Categories,no_of_streamer)))+
  geom_col(fill="purple")+
  labs(title = "Top Categories Among Top Streamers",caption="Taking Top 100 Streamers",x="No. of Streamer",y="Categories")


#correlation between the number of subscribers and the number of likes or comments
new_youtube_df <- top100_categories %>%
  group_by(Categories) %>%
  summarise(Total_Subscribers = sum(Subscribers), Total_Likes = sum(Likes),
            Total_Comments = sum(Comments), Total_Visits = sum(Visits))%>%
  arrange(-Total_Subscribers)

#check for correlation between Total subscribers & Total_Likes
cor_matrix <- cor(new_youtube_df[sapply(new_youtube_df,is.numeric)]) 

#visualization of correlation matrix
ggcorrplot(cor_matrix, method = "square",lab = TRUE)

#Q3.Audience Study

country_categories <- youtube_df %>%
  group_by(Country,Categories)%>%
  summarise(Total_visits = sum(Visits), Total_Likes = sum(Likes),
            Total_Subscibers = sum(Subscribers),.groups = "drop")

ggplot(data = country_categories)+
  geom_point(aes(Categories,Country))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Distribution of Content Categories by Country",x="Categories",
       y="Country")

#Q4.Performance Matrices

avg_matrices <- youtube_df %>%
  group_by(Categories) %>%
  summarise(Avg_Subscribers = mean(Subscribers), Avg_Likes = mean(Likes),
            Avg_Comments = mean(Comments), Avg_Visits = mean(Visits))
  #cal correlation matrix
avg_cor <- cor(avg_matrices[sapply(avg_matrices,is.numeric)])
round(avg_cor,2)
  #visualize average correlation matrix
ggcorrplot(avg_cor,method = "square",lab = TRUE,title = "Performance Matrix")

  #top10 categories by Likes
top10_L <- head(arrange(avg_matrices,-Avg_Likes),10)
ggplot(data = top10_L,mapping = aes(Avg_Likes,reorder(Categories,Avg_Likes)))+
  geom_col(fill="pink")+
  labs(title = "Top 10 Categories by Likes", x ="Avg Likes", y ="Categories")

  #Top10 Categories by Comments
top10_C <- head(arrange(avg_matrices,-Avg_Comments),10)
ggplot(data = top10_C, mapping = aes(Avg_Comments,reorder(Categories,Avg_Comments)))+
  geom_col(fill="skyblue")+
  labs(title = "Top 10 Categories by Comments", x ="Avg Comments", y ="Categories")

#Q5.Content Categories

Content_cat <- youtube_df %>%
  group_by(Categories)%>%
  summarise(No_of_Streamer = n())
arrange(Content_cat,-No_of_Streamer)

ggplot(data = Content_cat, mapping = aes(reorder(Categories,-No_of_Streamer),No_of_Streamer))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Distribution of Content Categories", x ="Categories", y ="No of Streamers")

#Q6.Top Streamer for Brand Collaboration

#creating proxy performance matrix 
per_matrix <- youtube_df%>%
  mutate(Brand_collab = Likes+Comments+Visits+Subscribers)
top10_streamer <- head(arrange(per_matrix,-Brand_collab),10)

ggplot(data = top10_streamer, aes(Brand_collab,reorder(Username,Brand_collab)))+
  geom_col(fill="light green")+
  labs(title = "Top Streamer for Brand Collaboration",x="Performance(Proxy)",
       y="Top Streamer")

#Q7.Benchmarking
  #calculating mean values of subscribers,visits,likes & comments

a <- mean(youtube_df$Subscribers)
b <- mean(youtube_df$Visits)
c <- mean(youtube_df$Likes)
d <- mean(youtube_df$Comments)

above_avg_streamer <- filter(youtube_df, Subscribers >a & Visits > b & Likes > c
                             & Comments > d)
print(select(above_avg_streamer,Username,Subscribers,Visits,Likes,Comments))

#Q8.Content Recommendations

var1 <- readline("Search by Categories:");

for (i in 1:nrow(above_avg_streamer))
{
cat <- above_avg_streamer[i,3]
if(str_detect(cat,var1))
{
  u_name <- above_avg_streamer[i,2]
  print(u_name)
}
}

#finding close competitor for given user

#Taking input frome user
var <- readline("search username:")
k <- youtube_df$Username
j <- which(k == var)
l <- youtube_df[j,3]
m <- youtube_df[j,4]

filtered_data <- filter(youtube_df, Categories == l)

num <- numeric()
for (i in 1:nrow(filtered_data)) {
  sub <- filtered_data[i,4]
  x <- abs(sub - m)
  num <- c(num,x)
}
y <- which.min(num)
print(paste("Close Competitor of", var, "is",(filtered_data[(y+1),2])))



