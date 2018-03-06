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

# Build a single game helper function

single_game <- function(teamID_1, teamID_2) {
  
  team1_seas_dat <- seas_2018 %>% 
    filter(TeamID == teamID_1)
  
  team2_seas_dat <- seas_2018 %>% 
    filter(TeamID == teamID_2)
  
  team1_density <- density(team1_seas_dat$Score)
  team2_density <- density(team2_seas_dat$Score)

  team1_wins <- 0
  team2_wins <- 0
  
  for (j in 1:1000) {
    
    team1_score <- sample(team1_density$x, 1, prob = team1_density$y)
    team2_score <- sample(team2_density$x, 1, prob = team2_density$y)
    
    if(team1_score > team2_score) {
      team1_wins <- team1_wins + 1
    } else if (team2_score > team1_score) {
      team2_wins <- team2_wins +1
    } else skip
  }
  
  if(team1_wins > team2_wins) {
    winner <- teamID_1
  } else {
    winner <- teamID_2
  }
  
  return(winner)
}



  # Region by Region until final 4
  bracket_region_W <- bracket_2018 %>% 
    filter(Region == "W")
  
  # Determine if there are play in games
  if (nrow(bracket_region_W) > 16) {
    
    playin_game <- bracket_region_W %>% 
      filter(PlayIn != "NA")
    
    # How many play in games?
    playin_seeds <- unique(playin_game$SeedNum)
    
    seed_winner <- NULL
    playin_winners <- data.frame()
    
    for (seeds in seq_along(playin_seeds)) {
      
      playin_dat <- filter(playin_game, SeedNum == playin_seeds[[seeds]])
      
      seed_winner[[seeds]] <- single_game(playin_dat$TeamID[1], playin_dat$TeamID[2])
      
      seed_winner_dat <- filter(playin_game, TeamID == seed_winner[[seeds]])
      
      playin_winners <- rbind(playin_winners, seed_winner_dat)
    }
    
    other_dat <- filter(bracket_region_W, PlayIn == "NA")
    
    tournament_dat <- rbind(other_dat, playin_winners)
    
    tournament_dat$Seed <- NULL
    tournament_dat$PlayIn <- NULL
    
  } else {
    
    tournament_dat <- bracket_region_W
    
    tournament_dat$Seed <- NULL
    tournament_dat$PlayIn <- NULL
    
  } 
    
    # Now we start the tournament
    
    # Round 1
    
    game_1_v_16_dat_W <- filter(tournament_dat, SeedNum == "01" | SeedNum == "16")
    game_1_v_16_winner_W <- single_game(game_1_v_16_dat_W$TeamID[1], game_1_v_16_dat_W$TeamID[2]) 
    
    game_2_v_15_dat_W <- filter(tournament_dat, SeedNum == "02" | SeedNum == "15")
    game_2_v_15_winner_W <- single_game(game_2_v_15_dat_W$TeamID[1], game_2_v_15_dat_W$TeamID[2])
    
    game_3_v_14_dat_W <- filter(tournament_dat, SeedNum == "03" | SeedNum == "14")
    game_3_v_14_winner_W <- single_game(game_3_v_14_dat_W$TeamID[1], game_3_v_14_dat_W$TeamID[2])
    
    game_4_v_13_dat_W <- filter(tournament_dat, SeedNum == "04" | SeedNum == "13")
    game_4_v_13_winner_W <- single_game(game_4_v_13_dat_W$TeamID[1], game_4_v_13_dat_W$TeamID[2])
    
    game_5_v_12_dat_W <- filter(tournament_dat, SeedNum == "05" | SeedNum == "12")
    game_5_v_12_winner_W <- single_game(game_5_v_12_dat_W$TeamID[1], game_5_v_12_dat_W$TeamID[2])
    
    game_6_v_11_dat_W <- filter(tournament_dat, SeedNum == "06" | SeedNum == "11")
    game_6_v_11_winner_W <- single_game(game_6_v_11_dat_W$TeamID[1], game_6_v_11_dat_W$TeamID[2])
    
    game_7_v_10_dat_W <- filter(tournament_dat, SeedNum == "07" | SeedNum == "10")
    game_7_v_10_winner_W <- single_game(game_7_v_10_dat_W$TeamID[1], game_7_v_10_dat_W$TeamID[2])
    
    game_8_v_9_dat_W <- filter(tournament_dat, SeedNum == "08" | SeedNum == "09")
    game_8_v_9_winner_W <- single_game(game_8_v_9_dat_W$TeamID[1], game_8_v_9_dat_W$TeamID[2])
    
    # Round 2
    
    game_1v16_v_8v9_winner_W <- single_game(game_1_v_16_winner_W, game_8_v_9_winner_W)
    
    game_2v15_v_7v10_winner_W <- single_game(game_2_v_15_winner_W, game_7_v_10_winner_W)
  
    game_3v14_v_6v11_winner_W <- single_game(game_3_v_14_winner_W, game_6_v_11_winner_W)  
  
    game_4v13_v_5v12_winner_W <- single_game(game_4_v_13_winner_W, game_5_v_12_winner_W)
    
    # Sweet Sixteen
    
    sweet_sixteen_1v16v8v9v5v12v4v13_winner_W <- single_game(game_1v16_v_8v9_winner_W, game_4v13_v_5v12_winner_W)
    
    sweet_sixteen_6v11v3v14v7v10v2v15_winner_W <- single_game(game_3v14_v_6v11_winner_W, game_2v15_v_7v10_winner_W)
    
    # Final Four
    
    final_four_region_W <- single_game(sweet_sixteen_1v16v8v9v5v12v4v13_winner_W, sweet_sixteen_6v11v3v14v7v10v2v15_winner_W) 

    
# Build out a "bracket"    
    
    `Round 1` <- c(game_1_v_16_dat_W$TeamID[1], game_1_v_16_dat_W$TeamID[2], "",
                   game_8_v_9_dat_W$TeamID[1], game_8_v_9_dat_W$TeamID[2], "",
                   game_5_v_12_dat_W$TeamID[1], game_5_v_12_dat_W$TeamID[2], "",
                   game_4_v_13_dat_W$TeamID[1], game_4_v_13_dat_W$TeamID[2], "",
                   game_6_v_11_dat_W$TeamID[1], game_6_v_11_dat_W$TeamID[2], "",
                   game_3_v_14_dat_W$TeamID[1], game_3_v_14_dat_W$TeamID[2], "",
                   game_7_v_10_dat_W$TeamID[1], game_7_v_10_dat_W$TeamID[2], "",
                   game_2_v_15_dat_W$TeamID[1], game_2_v_15_dat_W$TeamID[2])    
    
    `Round 2` <- c(game_1_v_16_winner_W, "", "",
                   game_8_v_9_winner_W, "", "",
                   game_5_v_12_winner_W, "", "",
                   game_4_v_13_winner_W, "", "",
                   game_6_v_11_winner_W, "", "",
                   game_3_v_14_winner_W, "", "",
                   game_7_v_10_winner_W, "", "",
                   game_2_v_15_winner_W, "")

    `Sweet 16` <- c("",game_1v16_v_8v9_winner_W, "","","","","",
                   game_4v13_v_5v12_winner_W, "","","","","",
                   game_3v14_v_6v11_winner_W, "","","","","",
                   game_2v15_v_7v10_winner_W, "", "", "")
    
    `Elite 8` <- c("","","","",sweet_sixteen_1v16v8v9v5v12v4v13_winner_W,
                   "","","","","","","","","","","",
                   sweet_sixteen_6v11v3v14v7v10v2v15_winner_W, "","","","","","")
    
    `Final 4` <- c("","","","","","","","","","","", final_four_region_W,
                   "","","","","","","","","","","")

    region_W_final_bracket <- r1 <- data.frame(`Round 1`, `Round 2`,`Sweet 16`, `Elite 8`, `Final 4`)
    