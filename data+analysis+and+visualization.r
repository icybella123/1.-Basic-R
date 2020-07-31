# data analysis and visualization

# string operation
year = 2014
is.character(year)
year.char = as.character(year)
is.character(year.char)
nchar(year.char)

first = 'The'
second = 'Chemical'
third = 'Statistician'
my.name = paste(first, second, third, sep = ' ')
my.name
my.name1 = paste(first, second, third, sep = ',')
my.name1

parts = unlist(strsplit(my.name, split = ' '))
parts
parts[1]

y = "GGACTCTAAATCCGTACTATCGTCATCGTTTTTCCT"
substr(y, 4, nchar(y))
substr(y, 1, 1)

x = 'ATCG'
grep('A', x)
x1 = 'AATCG'
grepl('A', x1)
y = 'GGACTCTAAATCCGTACTATCGTCATCGTTTTTCCT'
z1='atcggg'
toupper(z1)
z = 'CTATCGGGTAGCT'
tolower(z)
z='ctatcgggtagct'
grepl(x,y)
grepl(x, c(y, z))

x='zhang'
gsub('z', 'c', x)

# vector operation
probes <- rep(TRUE, 15)
probes
probes <- append(probes, FALSE, after=5)
probes
probes <- append(probes, FALSE, after=11)
probes

x <- sample(1:10)
x
match(c(4,8),x)

x <- sample(1:4,10,replace=TRUE)
x
which(x %in% c(2,4))

str1= c("no abstract available", "A", "A", "B", "no abstract available")
str1
str2=sapply(str1, function(x){gsub(pattern = "no",
                        replacement = "yes", x)})
class(str2)
# install.packages('stringr')
library(stringr)
str3 <- str_replace(str1, "no", "yes")
str3

numbers <- c(4,23,4,23,5,43,54,56,657,67,67,435,
             453,435,324,34,456,56,567,65,34,435)
length(numbers)
table(numbers)
as.data.frame(table(numbers))
max(table(numbers))
table(numbers) == max(table(numbers))
which(table(numbers) == max(table(numbers)))
names(which(table(numbers) == max(table(numbers))))
as.numeric(names(which(table(numbers) == max(table(numbers)))))
sort(table(numbers))

# data manipulation
library(nycflights13)
dim(flights)
head(flights)
summary(flights)
str(flights)
View(flights)
flights$flight=as.character(flights$flight)

library(dplyr)
# january 1th, 19 columns
set1=filter(flights, month == 1, day == 1)
View(set1)
# 19 columns, 3 columns: year month day
select(flights, year, month, day)
# sort data frame by year, month, day
arrange(flights, year, month, day)
# add 2 new columns
flights$gain=flights$arr_delay-flights$dep_delay
set2=mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)
dim(set2)

# aggregation 
summarise(flights,
          delay = mean(dep_delay, na.rm = TRUE))

# calculate mean(delay) every month 2013 year
flights%>%
  filter(year==2013)%>%
  group_by(month)%>%
  summarise(delay = mean(dep_delay, na.rm = TRUE))

# data merge
df1 = data.frame(CustomerId = c(1:6), Product = c(rep("Toaster", 3), rep("Radio", 3)))
df2 = data.frame(CustomerId = c(2, 4, 6), State = c(rep("Alabama", 2), rep("Ohio", 1)))
df1
df2
inner_join(df1, df2, by='CustomerId')
left_join(df1, df2, by='CustomerId')
right_join(df1, df2, by='CustomerId')
full_join(df1, df2, by='CustomerId')

merge(x=df1, y=df2, by='CustomerId') # inner_join
merge(x=df1, y=df2, by='CustomerId', all.x = TRUE) # left_join
merge(x=df1, y=df2, by='CustomerId', all.y=TRUE) # right_join
merge(x=df1, y=df2, by='CustomerId', all = TRUE) # full_join

# ggplot2
setwd("/downloads/")
housing=read.csv('landdata-states.csv', header=TRUE, stringsAsFactors = FALSE)
dim(housing)
head(housing[1:5])
str(housing)
library(ggplot2)
# scatter 
# compare MA and TX
ggplot(data=filter(housing, State %in% c("MA", "TX")), # State=='MA' | State=='TX'
       aes(x=Date,
           y=Home.Value,
           color=State))+
  geom_point()

# relationship between land value and structure cost 
hp2001Q1 <- subset(housing, Date == 2001.25) # filter
ggplot(hp2001Q1,
       aes(y = Structure.Cost, x = Land.Value)) +
  geom_point()
ggplot(hp2001Q1,
       aes(y = Structure.Cost, x = log(Land.Value))) +
  geom_point()
ggplot(hp2001Q1, aes(x = log(Land.Value), y = Structure.Cost))+
  geom_point(aes(color = Home.Value))+
  geom_smooth()

# line chart
ggplot(housing, aes(x = Date, y = Home.Value))+
  geom_line(aes(color = State))
ggplot(housing, aes(x = Date, y = Home.Value))+
  geom_line() +
  facet_wrap(~State, ncol = 10)

# histogram
ggplot(housing, aes(x = Home.Value)) +
  geom_histogram(stat = "bin", binwidth=4000)

# bar chart
df=housing%>%
  group_by(State)%>%
  summarise(avg=mean(Home.Value))
df

ggplot(df, aes(x=reorder(State,avg), y=avg))+
  geom_bar(stat="identity")

# box plot
ggplot(housing, aes(x=State, y=Home.Value))+
  geom_boxplot()


