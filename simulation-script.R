## Simulation Project ##
# Rachel, Derek, Patrick

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
deck <- deck[sample(nrow(deck)),]

## Simluate the Initial Deal ##
for (i in 1:nrow(deck)) {
        ## Assign Each Player and Dealer First Card
        x1 <- deck[sample(nrow(deck), 1), ]
        x2 <- deck[sample(nrow(deck), 1), ]
        x3 <- deck[sample(nrow(deck), 1), ]
        y <- deck[sample(nrow(deck), 1), ]
        
        if((nrow(y) < 3) == TRUE){
                
                ## Assign Each Player and Dealer Second Card
                x1 <- rbind(x1, deck[sample(nrow(deck), 1), ])
                x2 <- rbind(x2, deck[sample(nrow(deck), 1), ])
                x3 <- rbind(x3, deck[sample(nrow(deck), 1), ])
                y <- rbind(y, deck[sample(nrow(deck), 1), ])
        } else {
                break
        }
}

## Simluate Hitting or Staying for Round ##
# With rule that if player has less then 16, then hit, else stay #
for (i in 1:nrow(deck)) {
        if(sum(y[,3]) == 21){
                ywin <- ywin + 1
                break
        } else if(sum(x1[,3]) < 16) {
                x1 <- rbind(x1, deck[sample(nrow(deck), 1), ])    
        } else if(sum(x2[,3]) < 16){
                x2 <- rbind(x2, deck[sample(nrow(deck), 1), ])
        } else if(sum(x3[,3]) < 16){
                x3 <- rbind(x3, deck[sample(nrow(deck), 1), ])
        } else if(sum(y[,3]) < 16){
                y <- rbind(y, deck[sample(nrow(deck), 1), ])
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