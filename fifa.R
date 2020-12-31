# KEY LIBRARIES
library(dplyr)
library(readr)

# KEY VARIABLES
N <- 5000 # number of simulations
PLAYER.WEIGHT <- 0.90 # % of score based on players
H2H.WEIGHT <- 0.10 # % of score based on previous head-to-head
STD <- 10 # "randomness" sampled from a normal distribution with standard deviation 

# READ DATA
fifa <- read_csv('players_20.csv')
orgs <- read_csv('organizations.csv')
game.matrix <- read_csv('game_matrix.csv')

# FINDING INTERNATIONAL REPRESENTATION (# players representing a country)
representation <- fifa$nationality %>% table() %>% as.matrix()
representation.df <- data.frame(Country = rownames(representation), 
                                Players = representation) %>% as_tibble()

# FILTER COUNTRIES WITH LESS THAN 15 PLAYERS
viable.countries <- representation.df %>% filter(Players >= 15)
viable.fifa <- fifa %>% filter(nationality %in% viable.countries$Country)

countries <- c()
team.averages <- c()
for(i in viable.fifa$nationality %>% unique()) {
  sub <- viable.fifa %>% filter(nationality == i) %>% head(30)
  
  countries[i] <- i
  team.averages[i] <- sub$overall %>% mean()
}

averages.df <- data.frame(Country = countries, Averages = team.averages) %>% 
  as_tibble() %>% arrange(desc(Averages)) %>% left_join(orgs, 'Country')

# SELECT WINNERS FOR EACH REGULATORY BODY AND FOR THE INTERFEDERATION CUP
winners <- data.frame(Country = c(), Averages = c(), Organization = c())
interfederation <- winners
organization.df <- data.frame(Organization = c('CONMEBOL', 
                                               'AFC', 
                                               'CONCACAF', 
                                               'OFC', 
                                               'CAF', 
                                               'UEFA'),
                              Spots = c(4, 4, 3, 0, 5, 13))
for(i in 1 : length(organization.df$Organization)) {
  sub <- averages.df %>% filter(Organization == organization.df$Organization[i])
  winners <- rbind(winners, head(sub, organization.df$Spots[i]))
  if(!(organization.df$Organization[i] %in% c('CAF', 'UEFA'))) {
    interfederation <- rbind(interfederation, sub[organization.df$Spots[i] + 1, ])
  }
}
interfederation <- interfederation %>% arrange(desc(Averages))

# ADD WINNERS OF INTERFEDERATION AND QATAR
final.teams <- winners %>% rbind(interfederation[1 : 2, ]) %>% rbind(c('Qatar', 0, 'AFC'))
final.teams$Averages <- final.teams$Averages %>% as.numeric()

# FUNCTION TO COMPARE INDIVIDUAL COUNTRIES IN A H2H MATCH
compare.teams <- function(country1, country2) {
  country1.average <- final.teams %>% filter(Country == country1) %>% select('Averages') %>% as.numeric()
  country2.average <- final.teams %>% filter(Country == country2) %>% select('Averages') %>% as.numeric()
  country1.h2h <- game.matrix %>% filter(Country == country1) %>% select(country2) %>% as.numeric()
  country2.h2h <- game.matrix %>% filter(Country == country2) %>% select(country1) %>% as.numeric()
  
  # if two countries have never played each other, both will have NA values
  if(is.na(country1.h2h)) {
    # weighting the averages according to the key variables
    country1.score <- PLAYER.WEIGHT * country1.average + (1 - PLAYER.WEIGHT) * rnorm(1, 0, STD)
    country2.score <- PLAYER.WEIGHT * country2.average + (1 - PLAYER.WEIGHT) * rnorm(1, 0, STD)
  }
  else {
    country1.score <- PLAYER.WEIGHT * country1.average + country1.h2h * H2H.WEIGHT * country1.average + RANDOM.WEIGHT * rnorm(1, 0, STD)
    country2.score <- PLAYER.WEIGHT * country2.average + country2.h2h * H2H.WEIGHT * country2.average + RANDOM.WEIGHT * rnorm(1, 0, STD)
  }
  # return the winner
  ifelse(country1.score > country2.score, return(country1), return(country2))
}

# MONTE CARLO GROUPING AND SIMULATION
wins <- data.frame(Country = final.teams$Country, Wins = rep(0, length(final.teams$Country))) %>%
  as_tibble()
whole.wins <- wins # this variable represents World Cup wins
for(i in 1 : N) {
  print(paste('Iteration', i, 'of', N, '.'))
  new.final <- final.teams %>% sample_frac(1L)
  group1 <- new.final[1 : 4, ]
  group2 <- new.final[5 : 8, ]
  group3 <- new.final[9 : 12, ]
  group4 <- new.final[13 : 16, ]
  group5 <- new.final[17 : 20, ]
  group6 <- new.final[21 : 24, ]
  group7 <- new.final[25 : 28, ]
  group8 <- new.final[29 : 32, ]
  groups <- list(group1, group2, group3, group4, group5, group6, group7, group8)
  group.winners <- c()
  for(group in groups) {
    random.teams <- sample(1 : 4, 2, replace = TRUE)
    group$Averages[random.teams[1]] <- group$Averages[random.teams[1]] + 1
    group$Averages[random.teams[2]] <- group$Averages[random.teams[2]] + 1
    sorted.group <- group %>% arrange(desc(Averages))
    wins$Wins[wins$Country == sorted.group$Country[1]] <- wins$Wins[wins$Country == sorted.group$Country[1]] + 1
    wins$Wins[wins$Country == sorted.group$Country[2]] <- wins$Wins[wins$Country == sorted.group$Country[2]] + 1
    group.winners <- append(group.winners, c(sorted.group$Country[1], sorted.group$Country[2]))
  }
  
  # HARD-CODING TOURNAMENT BRACKET MATCHES
  # ROUND 1
  round1.winners <- c()
  a1 <- group.winners[1] #A1
  b2 <- group.winners[4] #B2
  w <- compare.teams(a1, b2)
  round1.winners <- append(round1.winners, w)
  
  a2 <- group.winners[2] #A2
  b1 <- group.winners[3] #B1
  w <- compare.teams(a2, b1)
  round1.winners <- append(round1.winners, w)
  
  c1 <- group.winners[5] #C1
  d2 <- group.winners[8] #D2
  w <- compare.teams(c1, d2)
  round1.winners <- append(round1.winners, w)
  
  c2 <- group.winners[6] #C2
  d1 <- group.winners[7] #D1
  w <- compare.teams(c2, d1)
  round1.winners <- append(round1.winners, w)
  
  e1 <- group.winners[9] #E1
  f2 <- group.winners[12] #F2
  w <- compare.teams(e1, f2)
  round1.winners <- append(round1.winners, w)
  
  e2 <- group.winners[10] #E2
  f1 <- group.winners[11] #F1
  w <- compare.teams(e2, f1)
  round1.winners <- append(round1.winners, w)
  
  g1 <- group.winners[13] #G1
  h2 <- group.winners[16] #H2
  w <- compare.teams(g1, h2)
  round1.winners <- append(round1.winners, w)
  
  g2 <- group.winners[14] #G2
  h1 <- group.winners[15] #H1
  w <- compare.teams(g2, h1)
  round1.winners <- append(round1.winners, w)
  
  # ROUND 2
  round2.winners <- c()
  a1b2 <- round1.winners[1]
  c1d2 <- round1.winners[3]
  w <- compare.teams(a1b2, c1d2)
  round2.winners <- append(round2.winners, w)
  
  a2b1 <- round1.winners[2]
  c2d1 <- round1.winners[4]
  w <- compare.teams(a2b1, c2d1)
  round2.winners <- append(round2.winners, w)
  
  e1f2 <- round1.winners[5]
  g1h2 <- round1.winners[7]
  w <- compare.teams(e1f2, g1h2)
  round2.winners <- append(round2.winners, w)
  
  e2f1 <- round1.winners[6]
  g2h1 <- round1.winners[8]
  w <- compare.teams(e2f1, g2h1)
  round2.winners <- append(round2.winners, w)
  
  # ROUND 3
  round3.winners <- c()
  c1 <- round2.winners[1]
  c2 <- round2.winners[3]
  w <- compare.teams(c1, c2)
  round3.winners <- append(round3.winners, w)
  
  c1 <- round2.winners[2]
  c2 <- round2.winners[4]
  w <- compare.teams(c1, c2)
  round3.winners <- append(round3.winners, w)
  
  # FINAL
  c1 <- round3.winners[1]
  c2 <- round3.winners[2]
  w <- compare.teams(c1, c2)
  whole.wins$Wins[whole.wins$Country == w] <- whole.wins$Wins[whole.wins$Country == w] + 1
}
wins$Wins <- wins$Wins / N * 100
whole.wins$Wins <- whole.wins$Wins / N * 100
group.probabilities <- wins %>% arrange(desc(Wins))
win.probabilities <- whole.wins %>% arrange(desc(Wins))

write.csv(group.probabilities, 'outcomes.csv', row.names = F)
write.csv(win.probabilities, 'champions.csv', row.names = F)