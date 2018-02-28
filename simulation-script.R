## Simulation Project ##
# Rachel, Derek, Patrick

# Load Libraries #
library(tidyverse)

# Define suits, cards, values
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
cards <- c("Ace", "Deuce", "Three", "Four","Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
values <- c(0, 2:9, rep(10, 4))
totalNumOfDecks <- 5

# Build deck, replicated proper number of times
deck <- expand.grid(cards=cards, suits=suits)
deck$value <- values
deck <- deck[rep(seq(nrow(deck)), totalNumOfDecks),]

# Set Values of Ace to 11 #
for (i in 1:nrow(deck)) {
        if(deck$cards[i] == "Ace"){
                deck$value[i] <- 11
        } else {
                next()
        }
}

## Create Player and Dealer Variables ##
# Dealer #
y <- NULL 
# 3 Players #
x1 <- NULL
x2 <- NULL
x3 <- NULL

# Create Wins Variable #
ywin <- 0
x1win <- 0
x2win <- 0
x3win <- 0

# Create Wallet Variable with Player Money Amount #
#x1wal <- 100
#x2wal <- 100
#x3wal <- 100

## Shuffle Deck ##
deck1 <- deck[sample(nrow(deck)),]

## Set Value to Number of games to simulate ##
n <- 100

#### Rule where Players stay on 16 or higher ####
for (i in 1:n) {

  if(nrow(deck1) < 65){
    deck1 <- deck[sample(nrow(deck)),]
  } else{
    deck1 <- deck1
  }

# Dealer #
y <- NULL 
# 3 Players #
x1 <- NULL
x2 <- NULL
x3 <- NULL

## Simluate the Initial Deal ##
for (i in 1:nrow(deck1)) {
        ## Assign Each Player and Dealer First Card
        x1 <- deck1[sample(nrow(deck1), 1), ]
        deck1 <- deck1[-as.integer(rownames(x1[1,])),]
        x2 <- deck1[sample(nrow(deck1), 1), ]
        deck1 <- deck1[-as.integer(rownames(x2[1,])),]
        x3 <- deck1[sample(nrow(deck1), 1), ]
        deck1 <- deck1[-as.integer(rownames(x3[1,])),]
        y <- deck1[sample(nrow(deck1), 1), ]
        deck1 <- deck1[-as.integer(rownames(y[1,])),]
        
        ## Assign Each Player and Dealer Second Card
        x1 <- rbind(x1, deck1[sample(nrow(deck1), 1), ])
        deck1 <- deck1[-as.integer(rownames(x1[2,])),]
        x2 <- rbind(x2, deck[sample(nrow(deck1), 1), ])
        deck1 <- deck1[-as.integer(rownames(x2[2,])),]
        x3 <- rbind(x3, deck[sample(nrow(deck1), 1), ])
        deck1 <- deck1[-as.integer(rownames(x3[2,])),]
        y <- rbind(y, deck[sample(nrow(deck1), 1), ])
        deck1 <- deck1[-as.integer(rownames(y[2,])),]
        
        break
}

## Simluate Hitting or Staying for Round ##
# With rule that if player has less then 16, then hit, else stay #
for (i in 1:nrow(deck1)) {
        if(sum(y[,3]) == 21){
                ywin <- ywin + 1
                break
        } else if(sum(x1[,3]) < 16) {
                x1 <- rbind(x1, deck[sample(nrow(deck1), 1), ])
                deck1 <- deck1[-as.integer(rownames(x1)),]
        } else if(sum(x2[,3]) < 16){
                x2 <- rbind(x2, deck[sample(nrow(deck1), 1), ])
                deck1 <- deck1[-as.integer(rownames(x2)),]
        } else if(sum(x3[,3]) < 16){
                x3 <- rbind(x3, deck[sample(nrow(deck1), 1), ])
                deck1 <- deck1[-as.integer(rownames(x3)),]
        } else if(sum(y[,3]) < 16){
                y <- rbind(y, deck[sample(nrow(deck1), 1), ])
                deck1 <- deck1[-as.integer(rownames(y)),]
        } else {
                break
        }
        
}

## Decide on Winners of the Hand ##
x1sum <- ifelse(sum(x1[,3]) > 21, 0, sum(x1[,3])) 
x2sum <- ifelse(sum(x2[,3]) > 21, 0, sum(x2[,3])) 
x3sum <- ifelse(sum(x3[,3]) > 21, 0, sum(x3[,3])) 
ysum <- ifelse(sum(y[,3]) > 21, 0, sum(y[,3])) 

x1win <- ifelse(x1sum > ysum, (x1win + 1), x1win)
x2win <- ifelse(x2sum > ysum, (x2win + 1), x2win)
x3win <- ifelse(x3sum > ysum, (x3win + 1), x3win)

}

playerwins <- rbind(x1win, x2win, x3win, ywin)


