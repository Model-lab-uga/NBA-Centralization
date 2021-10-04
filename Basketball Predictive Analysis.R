load("gameDat.RData")

## Create Training Data
trainDat <- gameDat3[1:151,]

## Estimate 4 factor model
trainMod <- lm(ptDif ~ cEFG + cTOV + cORB + cFTFGA, trainDat)
summary(trainMod)
trainDat$fitted <- as.numeric(fitted(trainMod))

accTrain <- sum(trainDat[,"fitted"] > 0 & trainDat[,"ptDif"] > 0) + sum(trainDat[,"fitted"] < 0 &
                                                                          trainDat[,"ptDif"] < 0)
accTrain/nrow(trainDat) #percentage of games correctly predicted
# Make predictions on remaining data in gameDat3
testDat <- gameDat3[152:nrow(gameDat3),]
testDat$fitted <- predict(trainMod, newdata = testDat)

cor(testDat[,"fitted"],testDat[,"ptDif"])^2

accTest <- sum(testDat[,"fitted"] > 0 & testDat[,"ptDif"] > 0) + sum(testDat[,"fitted"] < 0 & 
                                                                       testDat[,"ptDif"] < 0)
accTest/nrow(testDat) #percentage of games correctly predicted


## Calculate frequencies of teams
table(gameDat$Team[1:302])

gameDat4 <- gameDat

## Add point differential to gameDat
awayScr <- gameDat4$Score[seq(1,nrow(gameDat4),2)]
homScr <- gameDat4$Score[seq(2,nrow(gameDat4),2)]
gameDat4$ptDif <- 0
gameDat4$ptDif[seq(1,nrow(gameDat4),2)] <- awayScr - homScr 
gameDat4$ptDif[seq(2,nrow(gameDat4),2)] <- homScr - awayScr 


# Find means of 4 factors for each team
## One by one
gameDat5 <- gameDat4[1:302,]

teamNames <- unique(gameDat5$Team)

gameDat6 <- t(sapply(teamNames, function(x){colMeans(gameDat5[gameDat5$Team== x, c(4:7, 10:13, 17)])}))
# create z scores for teams 4 factors
gameDat6 <- data.frame(gameDat6[,1:9], scale(gameDat6[,1:8]))
names(gameDat6)[10:17] <- c("zEFG", "zTOV", "zORB", "zFTFGA", "zDeFG", "zDTOV", "zDRB", "zDFTFGA")
gameDat6$cEFG <- gameDat6$zEFG - gameDat6$zDeFG
gameDat6$cTOV <- gameDat6$zDTOV - gameDat6$zTOV
gameDat6$cORB <- gameDat6$zORB + gameDat6$zDRB
gameDat6$cFTFGA <- gameDat6$zFTFGA - gameDat6$zDFTFGA

model5 <- lm(ptDif ~ cEFG + cTOV + cORB + cFTFGA, gameDat6)
summary(model5)


#Predicting Game 151
gameDat4[301:302,]
gameDat6[row.names(gameDat6)=="SAC",]
gameDat6[row.names(gameDat6)=="TOR",]

##Approach 1
# Static four factors
# Static regression coefficients

output <- rep(NA, 650)
nIter <- 0

for (i in seq(303, 1602, 2)) {
  awayTeam <- gameDat4[i,]$Team
  homeTeam <- gameDat4[i+1,]$Team
  
  nIter <- nIter +1 
  
  ###### Approach to predicting game 151
  ### Logic behind computations: 
  # zEFG: positive values indiciate better than average performance
  # zDeFG: negative values indicate better than average performance 
  # zEFG + zDeFG: positive values indicate better than average performance from the perspective of the offensive team
  # zTOV: negative values indicate better than average performance 
  # zDTOV: positive values indicate better than average performance 
  # zTOV + zDTOV: positive values indicate better than average performance from the perspective of the defensive team
  # zORB: positive values indicate better than average performance
  # zDRB: positive values indicate better than average performance
  # zORB - zDRB: positive values indicate better than average performance from the perspective of the offensive team
  # zFTFGA: positive values indicate better than average performance 
  # zDFTFGA: negative values indicate better than average performance 
  # zFTFGA + zDFTFGA: positive values indicate better than average performance from the perspective of the offensive team
  
  ## predAway = what happens in a game from the road team perspective (offense)
  predAway <- coef(model5)[1] + coef(model5)[2]*(gameDat6[row.names(gameDat6)==awayTeam,"zEFG"] + gameDat6[row.names(gameDat6)==homeTeam,"zDeFG"])-
    coef(model5)[3]*(gameDat6[row.names(gameDat6)==awayTeam,"zTOV"] + gameDat6[row.names(gameDat6)==homeTeam,"zDTOV"]) +
    coef(model5)[4]*(gameDat6[row.names(gameDat6)==awayTeam,"zORB"] - gameDat6[row.names(gameDat6)==homeTeam,"zDRB"]) +
    coef(model5)[5]*(gameDat6[row.names(gameDat6)==awayTeam,"zFTFGA"] + gameDat6[row.names(gameDat6)==homeTeam,"zDFTFGA"]) 
  
  ## predHom = what happens in a game from the home team perspective (offense) 
  predHom <- coef(model5)[1] + coef(model5)[2]*(gameDat6[row.names(gameDat6)==homeTeam,"zEFG"] + gameDat6[row.names(gameDat6)==awayTeam,"zDeFG"])-
    coef(model5)[3]*(gameDat6[row.names(gameDat6)==homeTeam,"zTOV"] + gameDat6[row.names(gameDat6)==awayTeam,"zDTOV"]) +
    coef(model5)[4]*(gameDat6[row.names(gameDat6)==homeTeam,"zORB"] - gameDat6[row.names(gameDat6)==awayTeam,"zDRB"]) +
    coef(model5)[5]*(gameDat6[row.names(gameDat6)==homeTeam,"zFTFGA"] + gameDat6[row.names(gameDat6)==awayTeam,"zDFTFGA"]) 
  
  ## Compute the relative edge on each of the four factors
  # Positive values indicate road team wins the game (by how many points)
  # Negative values indicate road team loses the game (by how many points)
  output[nIter] <- predAway - predHom
}

## R^2 statistic for our predictions 
cor(gameDat2[152:801,"ptDif"], output)^2

## Accuracy statistic
AccTest <- (sum(gameDat2[152:801, "ptDif"] < 0 & output < 0)) +  sum(gameDat2[152:801,"ptDif"] > 0 & output > 0)
AccTest/length(output) 

# Save #1
save(gameDat,gameDat2,gameDat3,gameDat4,gameDat5,gameDat6, file = "gameDat.rdata")

###Predictive approach number 2: Running (dynamic) average of the 4 factors
## Static regression coefficients: Regression model built on the first 150 games
## Dynamic four factors

output2 <- rep(NA, 650)
nIter <- 0

for (i in seq(303, 1602, 2)) {
  awayTeam <- gameDat4[i,]$Team
  homeTeam <- gameDat4[i+1,]$Team
  
  nIter <- nIter + 1
  
  predAway <- coef(model5)[1] + coef(model5)[2]*(gameDat6[row.names(gameDat6)==awayTeam,"zEFG"] + gameDat6[row.names(gameDat6)==homeTeam,"zDeFG"])-
    coef(model5)[3]*(gameDat6[row.names(gameDat6)==awayTeam,"zTOV"] + gameDat6[row.names(gameDat6)==homeTeam,"zDTOV"]) +
    coef(model5)[4]*(gameDat6[row.names(gameDat6)==awayTeam,"zORB"] - gameDat6[row.names(gameDat6)==homeTeam,"zDRB"]) +
    coef(model5)[5]*(gameDat6[row.names(gameDat6)==awayTeam,"zFTFGA"] + gameDat6[row.names(gameDat6)==homeTeam,"zDFTFGA"]) 

## predHom = what happens in a game from the home team perspective (offense) 
  predHom <- coef(model5)[1] + coef(model5)[2]*(gameDat6[row.names(gameDat6)==homeTeam,"zEFG"] + gameDat6[row.names(gameDat6)==awayTeam,"zDeFG"])-
    coef(model5)[3]*(gameDat6[row.names(gameDat6)==homeTeam,"zTOV"] + gameDat6[row.names(gameDat6)==awayTeam,"zDTOV"]) +
    coef(model5)[4]*(gameDat6[row.names(gameDat6)==homeTeam,"zORB"] - gameDat6[row.names(gameDat6)==awayTeam,"zDRB"]) +
    coef(model5)[5]*(gameDat6[row.names(gameDat6)==homeTeam,"zFTFGA"] + gameDat6[row.names(gameDat6)==awayTeam,"zDFTFGA"]) 

  output2[nIter] <- predAway - predHom
  
  if (i < 1601 & gameDat4[i+2,"Date"] != gameDat4[i,"Date"]) {
    gameDat5 <- gameDat4[1:(302 + 2*nIter),] 
    teamNames <- unique(gameDat5$Team)
    gameDat6 <- t(sapply(teamNames, function(x){colMeans(gameDat5[gameDat5$Team== x, c(4:7, 10:13, 17)])}))
    gameDat6 <- data.frame(gameDat6[,1:9], scale(gameDat6[,1:8]))
    names(gameDat6)[10:17] <- c("zEFG", "zTOV", "zORB", "zFTFGA", "zDeFG", "zDTOV", "zDRB", "zDFTFGA")
  }
}


## R^2 statistic for our predictions
cor(gameDat2[152:801,"ptDif"], output2)^2

## Accuracy statistic
AccTest <- (sum(gameDat2[152:801, "ptDif"] < 0 & output2 < 0)) +  sum(gameDat2[152:801,"ptDif"] > 0 & output2 > 0)
AccTest/length(output2) 


### Approach 3
## Regression coefficients dynamic
## Four factors dynamic

output3 <- rep(NA, 650)
nIter <- 0

for (i in seq(303, 1602, 2)) {
  awayTeam <- gameDat4[i,]$Team
  homeTeam <- gameDat4[i+1,]$Team
  
  nIter <- nIter + 1
  
  predAway <- coef(model5)[1] + coef(model5)[2]*(gameDat6[row.names(gameDat6)==awayTeam,"zEFG"] + gameDat6[row.names(gameDat6)==homeTeam,"zDeFG"])-
    coef(model5)[3]*(gameDat6[row.names(gameDat6)==awayTeam,"zTOV"] + gameDat6[row.names(gameDat6)==homeTeam,"zDTOV"]) +
    coef(model5)[4]*(gameDat6[row.names(gameDat6)==awayTeam,"zORB"] - gameDat6[row.names(gameDat6)==homeTeam,"zDRB"]) +
    coef(model5)[5]*(gameDat6[row.names(gameDat6)==awayTeam,"zFTFGA"] + gameDat6[row.names(gameDat6)==homeTeam,"zDFTFGA"]) 
  
  ## predHom = what happens in a game from the home team perspective (offense) 
  predHom <- coef(model5)[1] + coef(model5)[2]*(gameDat6[row.names(gameDat6)==homeTeam,"zEFG"] + gameDat6[row.names(gameDat6)==awayTeam,"zDeFG"])-
    coef(model5)[3]*(gameDat6[row.names(gameDat6)==homeTeam,"zTOV"] + gameDat6[row.names(gameDat6)==awayTeam,"zDTOV"]) +
    coef(model5)[4]*(gameDat6[row.names(gameDat6)==homeTeam,"zORB"] - gameDat6[row.names(gameDat6)==awayTeam,"zDRB"]) +
    coef(model5)[5]*(gameDat6[row.names(gameDat6)==homeTeam,"zFTFGA"] + gameDat6[row.names(gameDat6)==awayTeam,"zDFTFGA"]) 
  
  output3[nIter] <- predAway - predHom
  
  if (i < 1601 & gameDat4[i+2,"Date"] != gameDat4[i,"Date"]) {
    gameDat5 <- gameDat4[1:(302 + 2*nIter),] 
    teamNames <- unique(gameDat5$Team)
    gameDat6 <- t(sapply(teamNames, function(x){colMeans(gameDat5[gameDat5$Team== x, c(4:7, 10:13, 16)])}))
    gameDat6 <- data.frame(gameDat6[,1:9], scale(gameDat6[,1:8]))
    names(gameDat6)[10:17] <- c("zEFG", "zTOV", "zORB", "zFTFGA", "zDeFG", "zDTOV", "zDRB", "zDFTFGA")
    gameDat6$cEFG <- gameDat6$zEFG - gameDat6$zDeFG
    gameDat6$cTOV <- gameDat6$zDTOV - gameDat6$zTOV
    gameDat6$cORB <- gameDat6$zORB + gameDat6$zDRB
    gameDat6$cFTFGA <- gameDat6$zFTFGA - gameDat6$zDFTFGA
    
    model5 <- lm(ptDif ~ cEFG + cTOV + cORB + cFTFGA, gameDat6)
  }
}

###creating new output 3 to account for correction
output3h <- output3 + mean(gameDat3$ptDif)
## R^2 statistic for our predictions
cor(gameDat2[152:801,"ptDif"], output3)^2

## Accuracy statistic
AccTest <- (sum(gameDat2[152:801, "ptDif"] < 0 & output3 < 0)) +  sum(gameDat2[152:801,"ptDif"] > 0 & output3 > 0)
AccTest/length(output3) 


######## Setting up Monte Carlo Approach
gameDat5 <- gameDat4[1:302,]
gameDat5 <- data.frame(gameDat5, scale(gameDat5[,c(4:7,10:13)]))
names(gameDat5)[18:ncol(gameDat5)] <- c("zEFG", "zTOV", "zORB", "zFTFGA", "zDeFG", "zDTOV", "zDRB", "zDFTFGA")
gameDat5$cEFG <- gameDat5$zEFG - gameDat5$zDeFG
gameDat5$cTOV <- gameDat5$zDTOV - gameDat5$zTOV
gameDat5$cORB <- gameDat5$zORB + gameDat5$zDRB
gameDat5$cFTFGA <- gameDat5$zFTFGA - gameDat5$zDFTFGA

model6z <- lm(ptDif ~ zEFG + zTOV + zORB + zFTFGA + zDeFG + zDTOV + zDRB + zDFTFGA, gameDat5[seq(1,nrow(gameDat5),2),])
summary(model6z)
model6 <- lm(ptDif ~ cEFG + cTOV + cORB + cFTFGA, gameDat5[seq(1,nrow(gameDat5),2),])
summary(model6)

#####Beginning for loop
output4 <- rep(NA, 650)
nIter <- 0


gamePred <- function(awayTeam, homeTeam) {
  away4 <- gameDat5[gameDat5$Team == awayTeam, ]
  home4 <- gameDat5[gameDat5$Team == homeTeam, ]
  
  awaySamp <- away4[sample(nrow(away4),1),]
  homeSamp <- home4[sample(nrow(home4),1),]
  
  
  
  predAway <- coef(model6)[1] + coef(model6)[2]*(awaySamp[,"zEFG"] + homeSamp[,"zDeFG"])-
    coef(model6)[3]*(awaySamp[,"zTOV"] + homeSamp[,"zDTOV"]) +
    coef(model6)[4]*(awaySamp[,"zORB"] - homeSamp[,"zDRB"]) +
    coef(model6)[5]*(awaySamp[,"zFTFGA"] + homeSamp[,"zDFTFGA"])
  
  ## predHom = what happens in a game from the home team perspective (offense) 
  predHom <- coef(model6)[1] + coef(model6)[2]*(homeSamp[,"zEFG"] + awaySamp[,"zDeFG"]) - 
    coef(model6)[3]*(homeSamp[,"zTOV"] + awaySamp[,"zDTOV"]) +
    coef(model6)[4]*(homeSamp[,"zORB"] - awaySamp[,"zDRB"]) +
    coef(model6)[5]*(homeSamp[,"zFTFGA"] + awaySamp[,"zDFTFGA"])
  
  pred <- predAway - predHom
  return(as.numeric(pred))
}

for (i in seq(303, 1602, 2)) {
  
  awayTeam <- gameDat4[i,]$Team
  homeTeam <- gameDat4[i+1,]$Team
  nIter <- nIter + 1
  
  preds <- replicate(1000, gamePred(awayTeam,homeTeam))
  output4[nIter] <- mean(preds)
  
  
  if (i < 1601 & gameDat4[i+2,"Date"] != gameDat4[i,"Date"]) {
    gameDat5 <- gameDat4[1:(302 + 2*nIter),]   
    gameDat5 <- data.frame(gameDat5, scale(gameDat5[,c(4:7,10:13)]))
    names(gameDat5)[18:ncol(gameDat5)] <- c("zEFG", "zTOV", "zORB", "zFTFGA", "zDeFG", "zDTOV", "zDRB", "zDFTFGA")
    gameDat5$cEFG <- gameDat5$zEFG - gameDat5$zDeFG
    gameDat5$cTOV <- gameDat5$zDTOV - gameDat5$zTOV
    gameDat5$cORB <- gameDat5$zORB + gameDat5$zDRB
    gameDat5$cFTFGA <- gameDat5$zFTFGA - gameDat5$zDFTFGA
    
    model6 <- lm(ptDif ~ cEFG + cTOV + cORB + cFTFGA, gameDat5[seq(1,nrow(gameDat5),2),])
  }
}
## R^2 statistic for our predictions
cor(gameDat2[152:801,"ptDif"], output4)^2

## Accuracy statistic
AccTest <- (sum(gameDat2[152:801, "ptDif"] < 0 & output4 < 0)) +  sum(gameDat2[152:801,"ptDif"] > 0 & output4 > 0)
AccTest/length(output4) 
####### Using Approach 3 as base to build this approach
###### Sampling one set of factors from both teams (from their past games)
##### Combining these factors into one score (similar to before) and predict the outcome for the game
#### Interatively repeat above for a set number of samples (we're starting with 10)
### Average of these predicted outcomes will be our prediction of the game
## 