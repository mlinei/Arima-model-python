library(dplyr)
library(readxl)
library(tidyverse)
library(dplyr)
library(reshape)
library(tidyr)
wc <- read.csv("https://github.com/mlinei/datasets/blob/main/WorldCups.csv", sep = ";")
players <- read.csv("https://github.com/mlinei/datasets/blob/main/WorldCupPlayers.csv", sep = ";")
matches <- read.csv("https://github.com/mlinei/datasets/blob/main/WorldCupMatches.csv", sep = ';')
#Who have won the most FIFA World Cups and how many?
ListMaxWCWinner <- arrange(as.data.frame(table(wc$Winner)), desc(Freq))
View(ListMaxWCWinner)
#Which country has the most games won?
WonGames <- c()
matches <- matches[!duplicated(matches),]
for(i in 1:nrow(matches)) {
  row <- matches[i,]
  if(row['Home.Team.Goals'] > row['Away.Team.Goals']) {
    WonGames <- c(WonGames, row['Home.Team.Name'][1,1])
  } else if(row['Home.Team.Goals'] < row['Away.Team.Goals']) {
    WonGames <- c(WonGames, row['Away.Team.Name'][1,1])
  }
}
WonGames <- table(WonGames)
WonGames[which.max(WonGames)]
#And the most lost games?
LostGames <- c()
for(i in 1:nrow(matches)) {
  row <- matches[i,]
  if(row['Home.Team.Goals'] > row['Away.Team.Goals']) {
    LostGames <- c(LostGames, row['Away.Team.Name'][1,1])
  } else if(row['Home.Team.Goals'] < row['Away.Team.Goals']) {
    LostGames <- c(LostGames, row['Home.Team.Name'][1,1])
  }
}
LostGames <- table(LostGames)
LostGames[which.max(LostGames)]
#Which player has the most games played?
players <- players[!duplicated(players),]
table(players['Player.Name'])[which.max(table(players['Player.Name']))]
#Most goals per game player
GamesPerPlayer <- players[players$Line.up == "S",]
GamesPerPlayer<- as.data.frame(table(GamesPerPlayer$Player.Name))
colnames(GamesPerPlayer) <- c("Player","Games")
GoalsPerPlayer <- merge(players, matches, by = c("RoundID", "MatchID"))
GoalsPerPlayer <- GoalsPerPlayer[grep("G",GoalsPerPlayer$Event),c(7,9)]
GoalsPerPlayer <- as.data.frame(table(GoalsPerPlayer$Player.Name))
colnames(GoalsPerPlayer) <- c("Player","Goals")
GoalsPerPlayer <- merge_recurse(list(GoalsPerPlayer, GamesPerPlayer))
PlayersGoalGamesRatio <- cbind(GoalsPerPlayer, GoalsPerPlayer$Goals/GoalsPerPlayer$Games)
colnames(PlayersGoalGamesRatio)[4] <- "Goals/Games Ratio"
MaxPlayerGoalGamesRatio<-PlayersGoalGamesRatio[which.max(PlayersGoalGamesRatio$`Goals/Games Ratio`),]
View(MaxPlayerGoalGamesRatio)

#Let's get some data from the 2014 world cup 

#Games played
games2014<-matches[matches$Year == 2014,]
games2014<-games2014[!duplicated(games2014),]
gamespercountry2014<-as.data.frame(table(
  rbind(games2014$Home.Team.Name,
        games2014$Away.Team.Name)))
colnames(gamespercountry2014) <- c("Country", "Games")
View(gamespercountry2014)

#Won Games
HomeWins2014 <- games2014[
  games2014$Home.Team.Goals>
    games2014$Away.Team.Goals,6]
AwayWins2014 <- games2014[games2014$Away.Team.Goals>
                                  games2014$Home.Team.Goals,9]
WonGamesPerCountry2014 <- as.data.frame(table(
  c(AwayWins2014,HomeWins2014)))
colnames(WonGamesPerCountry2014) <- c("Country", "Won Games")
View(WonGamesPerCountry2014)

#Lost Games

HomeDefeats2014 <- games2014[
  games2014$Home.Team.Goals<
    games2014$Away.Team.Goals,6]
AwayDefeats2014 <- games2014[games2014$Away.Team.Goals<
                                     games2014$Home.Team.Goals,9]
LostGamesPerCountry2014 <- as.data.frame(table(
  c(AwayDefeats2014,HomeDefeats2014)))
colnames(LostGamesPerCountry2014) <- c("Country", "Lost Games")
View(LostGamesPerCountry2014)

#Draws

GamesDF <- list(gamespercountry2014,
                WonGamesPerCountry2014, LostGamesPerCountry2014  
                )
GamesDF <- merge_recurse(GamesDF)
GamesDF[is.na(GamesDF)] <- 0
DrawsPerCountry2014 <- cbind(
  GamesDF, (GamesDF$Games-(GamesDF$`Won Games`+ GamesDF$`Lost Games`)))
DrawsPerCountry2014 <- DrawsPerCountry2014[,c(1,5)]
colnames(DrawsPerCountry2014) <- c("Country","Draws")
GamesDF <- list(GamesDF, DrawsPerCountry2014)
GamesDF <- merge_recurse(GamesDF)
View(DrawsPerCountry2014)

#Scored goals per country

HomeGoals2014 <- games2014[,6:7]
AwayGoals2014 <- games2014[,8:9]
HomeGoals2014 <- HomeGoals2014 %>%
  group_by(Home.Team.Name) %>%
  summarise(Home.Team.Goals = sum(Home.Team.Goals)) %>%
  as.data.frame()
AwayGoals2014 <- AwayGoals2014 %>%
  group_by(Away.Team.Name) %>%
  summarise(Away.Team.Goals = sum(Away.Team.Goals)) %>%
  as.data.frame()
GoalsPerCountry2014 <- merge(
  HomeGoals2014,AwayGoals2014, by.x = c("Home.Team.Name"),
  by.y = c("Away.Team.Name"))
GoalsPerCountry2014 <- cbind(GoalsPerCountry2014$Home.Team.Name,
                             rowSums(subset(GoalsPerCountry2014, select = -c(Home.Team.Name))))
colnames(GoalsPerCountry2014) <- c("Country", "Goals")
View(GoalsPerCountry2014)

#Goals conceded per country

HomeGoalsAgainst2014 <- subset(
  games2014, select = c(Home.Team.Name, Away.Team.Goals))
AwayGoalsAgainst2014 <- subset(
  games2014, select = c(Away.Team.Name, Home.Team.Goals))
HomeGoalsAgainst2014 <- HomeGoalsAgainst2014 %>%
  group_by(Home.Team.Name) %>%
  summarise(Away.Team.Goals = sum(Away.Team.Goals)) %>%
  as.data.frame()
AwayGoalsAgainst2014 <- AwayGoalsAgainst2014 %>%
  group_by(Away.Team.Name) %>%
  summarise(Home.Team.Goals = sum(Home.Team.Goals)) %>%
  as.data.frame()
GoalsConcededPerCountry2014 <- merge(
  HomeGoalsAgainst2014, AwayGoalsAgainst2014, by.x = c(
    "Home.Team.Name"), by.y = c("Away.Team.Name"))
GoalsConcededPerCountry2014 <- cbind(
  GoalsConcededPerCountry2014$Home.Team.Name, rowSums(subset(
    GoalsConcededPerCountry2014, select = -c(Home.Team.Name))))
colnames(GoalsConcededPerCountry2014) <- c("Country", "Goals Conceded")
View(GoalsConcededPerCountry2014)

#Merge everything
GamesDF <- list(GamesDF,GoalsPerCountry2014, GoalsConcededPerCountry2014)
GamesDF <- merge_recurse(GamesDF)

#Now let's perform a logistic regression to get the probability to win a game for each country
#First we have to create the 1/0 variable to train our model. 1 is going to be used if the country won the game and 0 if lost 

modeldf <- data.frame()

for (i in 1:nrow(GamesDF)) {
  Country <- rep(GamesDF[i, "Country"], GamesDF[i, "Games"])
  won <- rep(1, GamesDF[i, "Won Games"])
  lost <- rep(0, GamesDF[i, "Games"] - GamesDF[i, "Won Games"])
  
  result <- c(won, lost)
  
  temp_df <- data.frame(Country, won = result)
  modeldf <- rbind(modeldf, temp_df)
}

#Perform the logistic regression

model <- glm(won ~ Country, data = modeldf,
             family = binomial(link = 'logit'))
predictions <- predict(model, modeldf, type = 'response')

WinProb <- cbind(modeldf, predictions)
WinProb <- WinProb[!duplicated(WinProb),]
WinProb$predictions <- round(as.numeric(WinProb$predictions),2)
View(WinProb)









