# Question 1
CL = read.csv("Champions.csv")
library(dplyr)
CL = tbl_df(CL)

# 1.1
CL1 <- filter(CL, HomeGoal > AwayGoal)
head(CL1, 5)

# 1.2
CL2 <- filter(CL, HomeTeam %in% c("Barcelona", "Real Madrid"))
head(CL2, 5)

CL3 = select(CL, starts_with("Home"))
head(CL3, 5)

CL4 = select(CL, contains("Team"), contains("Goal"), contains("Corner"))
head(CL4, 5)

# 1.3
CL %>% arrange(desc(HomeGoal)) %>%
  select(contains("Team"), contains("Goal"), contains("Corner"))

# 1.4
CL %>% group_by(HomeTeam) %>%
  summarise_each(funs(mean), HomeGoal, HomePossession, HomeYellow)
# funs Create a list of functions calls.
# 1.5
temp = mutate(CL, score = ifelse(HomeGoal > AwayGoal,
                                 paste(HomeGoal, AwayGoal, sep = "-"),
                                 paste(AwayGoal, HomeGoal, sep = "-")),
              n = 1)
temp = group_by(temp, score)
temp = arrange(summarise(temp, n = sum(n)), desc(n))
temp[1:5, ]


## Another solution using apply
cl_sub2=select(cl,contains("Goal"))
# Nice solution by transpose the matrix.
all_score<-t(apply(cl_sub2,1,sort))
all<-data.frame(score=apply(all_score,1,paste,collapse=""))

score_frq<-all %>%
  group_by(.,score)%>%
  summarise(.,count=n()) %>%
  arrange(.,desc(count))

score_frq[1:5,]


# Qustion 2

library(ggplot2)
data(cars)
plot(cars$speed,cars$dist)

ggplot(data = cars , aes(x = speed, y=dist))+
  geom_point(col="red",pch = 17)+
  xlab("Speed (mpg)")+
  ylab("Stopping Distance (ft)")+
  ggtitle("Relationship between Speed and Stopping Distance ")
