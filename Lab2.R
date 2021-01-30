library(dplyr)
path <- 'C:/Users/Ioan/Desktop/Essex/MA334/us-president-election-results-1976-2020.csv'

df <- read.csv(path)
head(df)

years <- seq(1976,2020,4)

overall_usa <- c("D","R","R","R","D","D","R","R","D","D","R","D")

df_winner <- data.frame(years,overall_usa)

ohio <- c()

for (y in years) {
  dem_wins <- select(filter(df, year==y, state=='Ohio', party=='D'), win)
  rep_wins <- select(filter(df, year==y, state=='Ohio',party=='R'), win)
  
  if (dem_wins == TRUE) {
    ohio <- c(ohio,c('D'))
  }
  else {
    ohio <- c(ohio,c('R'))
  }
}

df_winner$ohio <- ohio
df_winner$match <- overall_usa == ohio

ohio_matches <- sum(df_winner$match)
match_pctg <- ohio_matches/length(df_winner$match)*100

ans_text <- paste0("Bellwether in ", round(match_pctg,2), "% of cases")

print("Q1): Ohio bellwether"); ans_text

############swing state


head(df)

states <- unique(df$state)
years <- unique(df$year)

flips <- c()

for (s in states){
  last_win <- ""
  num_flips <- -1
  for (y in years){
    dem_win <- select(filter(df, state==s, year==y, party=='D'), win)
    rep_win <- select(filter(df, state==s, year==y, party=='R'), win)
    
    if (dem_win == TRUE){
      winn <- 'D'
    }
    else {
      winn <- 'R'
    }
    
  if (last_win != winn) {
    num_flips <- num_flips + 1
  }
  last_win <- winn
  } 
  flips <- c(flips, (num_flips))
}

state_flips <- data.frame(states,flips)

swing_states <- select(head(arrange(state_flips, desc(flips)),6),states)
ans_text <- ""
for(s in swing_states){
  ans_text <- paste0(ans_text,s)
}

print("Q2): Swing states")
print(ans_text)
