library(readr)
library(tidyverse)
library(stringr)

# Read in the regular season data
reg_season <- read_csv("PrelimData2018/RegularSeasonDetailedResults_Prelim2018.csv")

# For now, focus on 2018 season
seas_2018 <- reg_season %>% 
  filter(Season == "2018")

# Pull out columns that are relevant to winning and losing teams
seas_2018_N <- seas_2018 %>% 
  select(Season, DayNum, NumOT)

# Create df of winning teams
seas_2018_W <- seas_2018 %>% 
  select_at(vars(starts_with("W"))) %>% 
  mutate(type = "W")

# Create df of losing teams
seas_2018_L <- seas_2018 %>% 
  select_at(vars(starts_with("L"))) %>% 
  mutate(type = "L")

# Define location for losing teams (based on opposite of win location)
seas_2018_L$WLoc <- ifelse(seas_2018_W$WLoc == "H", "A", ifelse(seas_2018_W$WLoc == "A", "H", "N"))

# Combind win and lose dfs with neutral info
seas_2018_L <- cbind(seas_2018_N,seas_2018_L)
seas_2018_W <- cbind(seas_2018_N,seas_2018_W)

# Reorder columns for losing df
seas_2018_L <- seas_2018_L %>% 
  select(Season, DayNum, NumOT, LTeamID, LScore, WLoc, LFGM, LFGA, LFGM3, LFGA3, LFTM, LFTA, LOR, LDR, LAst, LTO, LStl, LBlk, LPF, type)

# Rename columns
colnames(seas_2018_W) <- c("Season", "DayNum", "NumOT", "TeamID", "Score", "WLoc", "FGM", "FGA", "FGM3", "FGA3", "FTM", "FTA", "OR", "DR", "Ast", "TO", "Stl", "Blk", "PF", "type") 
colnames(seas_2018_L) <- c("Season", "DayNum", "NumOT", "TeamID", "Score", "WLoc", "FGM", "FGA", "FGM3", "FGA3", "FTM", "FTA", "OR", "DR", "Ast", "TO", "Stl", "Blk", "PF", "type") 

# Combine win and loss data
seas_2018 <- rbind(seas_2018_W, seas_2018_L)

# Read in sample tournament bracket
sample_bracket <- read_csv("SampleTourney2018/NCAATourneySeeds_SampleTourney2018.csv")

# Focus on 2018 for now
bracket_2018 <- sample_bracket %>% 
  filter(Season == "2018")

# Split seed info into relevant columns
bracket_2018$Region <- str_sub(bracket_2018$Seed, 1, 1)
bracket_2018$SeedNum <- str_sub(bracket_2018$Seed, 2, 3)
bracket_2018$PlayIn <- ifelse(str_sub(bracket_2018$Seed, 4, 4) != "", "Yes", "NA")

# Define number of simulations
num_sims <- 1000

for(i in 1:num_sims) {
  # Region by Region until final 4
  bracket_region_W <- bracket_2018 %>% 
    filter(Region == "W")
  
  # Determine if there are play in games
  if (nrow(bracket_region_W) > 16) {
    
    playin_game <- bracket_region_W %>% 
      filter(PlayIn != "NA")
    
    team1_seas_dat <- seas_2018 %>% 
      filter(TeamID == playin_game$TeamID[1])
    
    team2_seas_dat <- seas_2018 %>% 
      filter(TeamID == playin_game$TeamID[2])
    
    team1_density <- density(team1_seas_dat$Score)
    team2_density <- density(team2_seas_dat$Score)
    
    team1_wins <- 0
    team2_wins <- 0
    
    for (j in 1:500) {
      
      team1_score <- sample(rnorm(500, mean = mean(team1_density$x), sd = sd(team1_density$x)), 1)
      team2_score <- sample(rnorm(500, mean = mean(team2_density$x), sd = sd(team2_density$x)), 1)
      
      if(team1_score > team2_score) {
        team1_wins <- team1_wins + 1
      } else if (team2_score > team1_score) {
        team2_wins <- team2_wins +1
      } else skip
    }
  } 
}

    
    
    
  }
  
}

