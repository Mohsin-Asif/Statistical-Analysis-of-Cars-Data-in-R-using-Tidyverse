library(tidyverse)
cardata<-read_csv("D:/Box Sync/MS IS/R Projects/Data Wrangling in R/cars.csv")
glimpse(cardata) #for overview of the dataset
# We can see that the entire dataset is in just one column and we need to separate data. 
#we will use read_delim to separate the data.
caradata<-read_delim("D:/Box Sync/MS IS/R Projects/Data Wrangling in R/cars.csv",";")
glimpse(caradata)
# We can see that the first line in our column is column types(string, integer, etc) written in the dataset. We need to read the file again and skip the columsn. 
# We will also change the names of the variables of our dataset in the same command.

varnames<-c("car","mpg","cylinders","displacement","horsepower","weight","acceleration","model","origin")
cardata<-read_delim("D:/Box Sync/MS IS/R Projects/Data Wrangling in R/cars.csv",";", skip=2, col_names = varnames)
# Now we take a look at the data again using glimpse and head.
head(cardata, 10)

which(is.na(cardata)) # Selecting which values in our dataset are NA
# Making quick bargraphs to analyse outliers in the dataset
ggplot(cardata, aes(mpg))+geom_bar(fill="dark green")
ggplot(cardata, aes(cylinders))+geom_bar(fill="navy")
ggplot(cardata, aes(displacement))+geom_bar(fill="blue")
ggplot(cardata, aes(horsepower))+geom_bar(fill="orange")
ggplot(cardata, aes(weight))+geom_bar(fill="#6299BA")
ggplot(cardata, aes(acceleration))+geom_bar(fill="#A3A144")
ggplot(cardata, aes(model))+geom_bar(fill="#6C4921")
ggplot(cardata, aes(origin))+geom_bar(fill="#59516D")


remove<-which(cardata$mpg==0) # storing tibble where mpg=0.
cardata<-cardata[-remove,] # Removing rows from dataset

ggplot(cardata, aes(mpg))+geom_bar() # Creating barchart to confirm that there are no more outliers

remove2<-which(cardata$horsepower==0) # Storing tibble where horsepower=0.

cardata<-cardata[-remove2,]

ggplot(cardata, aes(horsepower))+geom_bar()

summary(cardata) #Overview of cleaned dataset

cor(cardata$horsepower, cardata$mpg) # Finding relation between horsepower and mpg variable.

ggplot(cardata, aes(horsepower, mpg ))+geom_point(aes(color=origin))+
  labs(x="Horsepower", y="MPG", title="Corelation between MPG and Horsepower") # Creating a scatterplot to visulize the correlation.

cor(cardata$weight, cardata$mpg)
ggplot(cardata, aes(weight, mpg ))+geom_point(aes(color=origin))+
  labs(x="Weight", y="MPG", title="Corelation between MPG and Weight")

cor(cardata$displacement, cardata$mpg)

ggplot(cardata, aes(displacement, mpg ))+geom_point(aes(color=origin))+
  labs(x="Displacement", y="MPG", title="Corelation between MPG and Displacement")


cor(cardata$acceleration, cardata$mpg)

ggplot(cardata, aes(acceleration, mpg ))+geom_point(aes(color=origin))+
  labs(x="Acceleration", y="MPG", title="Corelation between MPG and Acceleration")


# Analysing cars on various parameters.

arrange(cardata, desc(cardata$mpg))%>%
  select(car, mpg)%>%
  head(5) # most fuel efficient cars in our dataset

arrange(cardata, desc(cardata$mpg))%>%
    select(car, mpg)%>%
      tail(5) # least fuel efficient cars in our dataset


arrange(cardata, desc(cardata$horsepower))%>%
  select(car, horsepower)%>%
  head(5) # most powerful in our dataset

arrange(cardata, desc(cardata$horsepower))%>%
  select(car, horsepower)%>%
  tail(5) # least powerful cars in our dataset

arrange(cardata, desc(cardata$mpg), desc(horsepower))%>%
  select(car, horsepower, mpg)%>%
  head(5) # selecting most powerful car that gives best mpg