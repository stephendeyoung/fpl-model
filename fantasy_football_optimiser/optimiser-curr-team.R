library(lpSolve)
library(RCurl)
library(stringr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(ggalt)

data <- "http://localhost:3000/" %>% 
  getURL %>% 
  fromJSON

teamData <- "http://localhost:3000/teams" %>% 
  getURL %>% 
  fromJSON %>% 
  data.frame

df <- data %>% 
  .$elements %>% 
  data.frame

df$current_team <- 0
df$keep <- 0
df$discard <- 0

### Algorithm team ###

################### Current Team #################

# Watkins
# df$current_team[df$code == 178301] <- 1
# df$now_cost[df$code == 178301] <- 61
# # Wood
# df$current_team[df$code == 60689] <- 1
# df$now_cost[df$code == 60689] <- 62
# # Abraham
# df$current_team[df$code == 173879] <- 1
# df$now_cost[df$code == 173879] <- 72
# 
# # Fernandes
# df$current_team[df$code == 141746] <- 1
# df$now_cost[df$code == 141746] <- 112
# # Harrison
# df$current_team[df$code == 221399] <- 1
# df$now_cost[df$code == 221399] <- 54
# # Salah
# df$current_team[df$code == 118748] <- 1
# df$now_cost[df$code == 118748] <- 122
# # Raphina
# df$current_team[df$code == 219961] <- 1
# df$now_cost[df$code == 219961] <- 54
# # Soucek
# df$current_team[df$code == 215439] <- 1
# df$now_cost[df$code == 215439] <- 52
# 
# # James
# df$current_team[df$code == 225796] <- 1
# df$now_cost[df$code == 225796] <- 51
# # Cancelo
# df$current_team[df$code == 121145] <- 1
# df$now_cost[df$code == 121145] <- 58
# # TAA
# df$current_team[df$code == 169187] <- 1
# df$now_cost[df$code == 169187] <- 72
# # Robertson
# df$current_team[df$code == 122798] <- 1
# df$now_cost[df$code == 122798] <- 73
# #df$discard[df$code == 216054] <- 1
# # Coufal
# df$current_team[df$code == 164555] <- 1
# df$now_cost[df$code == 164555] <- 47
# #df$keep[df$code == 244723] <- 1
# 
# # Johnstone
# df$current_team[df$code == 101982] <- 1
# df$keep[df$code == 101982] <- 1
# df$now_cost[df$code == 98770] <- 45
# # McCarthy
# df$current_team[df$code == 58376] <- 1
# df$keep[df$code == 58376] <- 1
# df$now_cost[df$code == 58376] <- 46
# # 
# # ################### Wanted Team ##################
# Smith-Rowe
#df$keep[df$code == 209289] <- 1
# # Mitchell
# df$keep[df$code == 244723] <- 1
# # # 
# # # Ramsdale
# # # #df$current_team[df$code == 101982] <- 1
# df$keep[df$code == 225321] <- 1
# # # # Nyland
# # #df$current_team[df$code == 98770] <- 1
# df$keep[df$code == 98770] <- 1
# 
# #################################################

###################################################

### Fanteam ###

################### Current Team #################

# # Bamford
# df$current_team[df$code == 106617] <- 1
# df$now_cost[df$code == 106617] <- 71
# # Watkins
# df$current_team[df$code == 178301] <- 1
# df$now_cost[df$code == 178301] <- 62
# # Rodrigo
# df$current_team[df$code == 80954] <- 1
# df$now_cost[df$code == 80954] <- 58
# 
# # Soucek
# df$current_team[df$code == 215439] <- 1
# df$now_cost[df$code == 215439] <- 51
# # Salah
# df$current_team[df$code == 118748] <- 1
# df$now_cost[df$code == 118748] <- 123
# # KDB
# df$current_team[df$code == 61366] <- 1
# df$now_cost[df$code == 61366] <- 119
# df$discard[df$code == 61366] <- 1
# # Fernandes
# df$current_team[df$code == 141746] <- 1
# df$now_cost[df$code == 141746] <- 114
# # Raphina
# df$current_team[df$code == 219961] <- 1
# df$now_cost[df$code == 219961] <- 56
# 
# # Dias
# df$current_team[df$code == 171314] <- 1
# df$now_cost[df$code == 171314] <- 52
# # Cancelo
# df$current_team[df$code == 121145] <- 1
# df$now_cost[df$code == 121145] <- 51
# # Chilwell
# df$current_team[df$code == 172850] <- 1
# df$now_cost[df$code == 172850] <- 55
# # Semedo
# df$current_team[df$code == 200402] <- 1
# df$now_cost[df$code == 200402] <- 46
# #df$keep[df$code == 103914] <- 1
# # Mitchell
# df$current_team[df$code == 244723] <- 1
# df$now_cost[df$code == 244723] <- 42
# df$keep[df$code == 244723] <- 1
# 
# # Nyland
# df$current_team[df$code == 98770] <- 1
# df$keep[df$code == 98770] <- 1
# df$now_cost[df$code == 98770] <- 40
# # McCarthy
# df$current_team[df$code == 58376] <- 1
# df$keep[df$code == 58376] <- 1
# df$now_cost[df$code == 58376] <- 46
# 
# ################### Wanted Team ##################
# # Bamford
# #df$current_team[df$code == 106617] <- 1
# # Adams
# #df$current_team[df$code == 200439] <- 1
# # Abraham
# #df$current_team[df$code == 173879] <- 1
# 
# # Rashford
# #df$current_team[df$code == 176297] <- 1
# # Mane
# df$keep[df$code == 110979] <- 1
# # Salah
# df$keep[df$code == 118748] <- 1
# # Soucek
# #df$current_team[df$code == 215439] <- 1
# # Harrison
# #df$current_team[df$code == 221399] <- 1
# 
# # Robertson
# #df$keep[df$code == 122798] <- 1
# # Coufal
# #df$current_team[df$code == 164555] <- 1
# # Cresswell
# #df$current_team[df$code == 55459] <- 1
# # James
# #df$current_team[df$code == 225796] <- 1
# # Alioski
# #df$current_team[df$code == 105377] <- 1
# 
# # Johnstone
# #df$current_team[df$code == 101982] <- 1
# df$keep[df$code == 101982] <- 1
# # McCarthy
# #df$current_team[df$code == 58376] <- 1
# #df$keep[df$code == 58376] <- 1
# 
# # Fernandes
# df$keep[df$code == 141746] <- 1
# 
# #################################################


### FPL ###

################### Current Team #################

# Bamford
df$current_team[df$code == 106617] <- 1
df$now_cost[df$code == 106617] <- 64
# Antonio
df$current_team[df$code == 57531] <- 1
df$now_cost[df$code == 57531] <- 63
# Wood
df$current_team[df$code == 60689] <- 1
df$now_cost[df$code == 60689] <- 62

# Raphina
df$current_team[df$code == 219961] <- 1
df$now_cost[df$code == 219961] <- 54
# Fernandes
df$current_team[df$code == 141746] <- 1
df$now_cost[df$code == 141746] <- 113
#df$discard[df$code == 61366] <- 1
# Salah
df$current_team[df$code == 118748] <- 1
df$now_cost[df$code == 118748] <- 122
# Mane
df$current_team[df$code == 110979] <- 1
df$now_cost[df$code == 110979] <- 118
# Soucek
df$current_team[df$code == 215439] <- 1
df$now_cost[df$code == 215439] <- 52

# Cancelo
df$current_team[df$code == 121145] <- 1
df$now_cost[df$code == 121145] <- 57
# James
df$current_team[df$code == 225796] <- 1
df$now_cost[df$code == 225796] <- 51
# Alioski
df$current_team[df$code == 105377] <- 1
df$now_cost[df$code == 105377] <- 44
# Stones
df$current_team[df$code == 97299] <- 1
df$now_cost[df$code == 97299] <- 51
# Coufal
df$current_team[df$code == 164555] <- 1
df$now_cost[df$code == 164555] <- 47

# Johnstone
df$current_team[df$code == 101982] <- 1
df$keep[df$code == 101982] <- 1
df$now_cost[df$code == 101982] <- 45
# McCarthy
df$current_team[df$code == 58376] <- 1
df$keep[df$code == 58376] <- 1
df$now_cost[df$code == 58376] <- 46
# 
# ################### Wanted Team ##################
# # Bamford
# #df$current_team[df$code == 106617] <- 1
# # Adams
# #df$current_team[df$code == 200439] <- 1
# # Abraham
# #df$current_team[df$code == 173879] <- 1
# 
# # Rashford
# #df$current_team[df$code == 176297] <- 1
# # Mane
# df$keep[df$code == 110979] <- 1
# # Salah
# df$keep[df$code == 118748] <- 1
# # Soucek
# #df$current_team[df$code == 215439] <- 1
# # Harrison
# #df$current_team[df$code == 221399] <- 1
# 
# # Robertson
# df$keep[df$code == 122798] <- 1
# # Coufal
# #df$current_team[df$code == 164555] <- 1
# # Cresswell
# #df$current_team[df$code == 55459] <- 1
# Smith-Rowe
# df$keep[df$code == 209289] <- 1
# # Mitchell
# df$keep[df$code == 244723] <- 1
# # 
# # Ramsdale
# # #df$current_team[df$code == 101982] <- 1
# df$keep[df$code == 225321] <- 1
# # # Nyland
# #df$current_team[df$code == 98770] <- 1
# df$keep[df$code == 98770] <- 1
# 
# # Fernandes
# df$keep[df$code == 141746] <- 1

##################################################


########### Best model team ################
# # Anguissa
# df$keep[df$code == 203325] <- 1
# 
# # Mitchell
# df$keep[df$code == 244723] <- 1
# 
# # Nyland
# #df$current_team[df$code == 98770] <- 1
# df$keep[df$code == 98770] <- 1
# # Sanchez
# #df$current_team[df$code == 215059] <- 1
# df$keep[df$code == 215059] <- 1

############################################

dfFilter <- df %>%
  filter(#player_season_minutes >= 800
         #minutes >= 800
         #& element_type == "4"
         #& now_cost <= 6.2
         team_code == 43 |
         team_code == 35   
         ) %>%
  select(web_name,
         #points_per_90,
         keep,
         #bonus_per_90,
         expected_points_per_90,
         expected.points.total,
         element_type,
         now_cost,
         total_points,
         #gw15.expected.points,
         #gw16.expected.points,
         #gw17.expected.points,
         #gw18.expected.points,
         #gw19.expected.points,
         gw21.expected.points,
         #expected_points,
         code,
         minutes,
         current_team,
         team_code
  )

print(dfFilter %>% { sum(.$gw20.expected.points) })

print(dfFilter %>% { mean(.$gw20.expected.points) })

dfFilter$value <- (dfFilter$expected.points.total / dfFilter$now_cost)

dfAll <- data %>%
  .$elements %>%
  data.frame %>%
  select(sort(names(.)))

df <- df %>%
  select(sort(names(.)))
col_idx <- grep("web_name", names(df))
df <- df[, c(col_idx, (1:ncol(df))[-col_idx])]

df <- df %>%
  filter(
         #web_name != 'Mousset' &
         #web_name != 'Stones' &
         #web_name != 'Alexander-Arnold' &
         #web_name != 'Jota' &
         #web_name != 'Walcott' &
         web_name != 'Mitrović' &
         #web_name != 'Werner' &
         web_name != 'Walker' &
         web_name != 'Masuaku' &
         web_name != 'Balbuena' &
         web_name != 'Vardy' &
         #web_name != 'Foden' &
         #web_name != 'Iheanacho' &
         #web_name != 'Rüdiger' &
         #web_name != 'Agüero' &
         #web_name != 'Otamendi' &
         #web_name != 'Alonso' &
         #web_name != 'Christensen' &
         #web_name != 'Sterling' &
         web_name != 'Mahrez' &
         #web_name != 'Lundstram' &
         web_name != 'Kilman' &
         web_name != 'Saïss' &
         #(first_name != "Reece" & web_name != 'James') &
         #web_name != 'Watkins' &
         #web_name != 'Grealish' &
         #web_name != 'McBurnie' &
         #web_name != 'Vinagre' &
         #web_name != 'Vydra' &
         #web_name != 'Long' &
         #web_name != 'Wilson' &
         #web_name != 'Maupay' &
         minutes >= 100 &
         player_season_minutes >= 900 |
         #player_season_minutes >= 1000 |
         web_name == 'Nyland' |
         #(first_name == "Neco" & web_name == 'Williams') |
         #(first_name == "Robert" & web_name == 'Sánchez') |
         #web_name == 'Sánchez' |
         #web_name == 'Werner' |
         #web_name == 'Vinagre' |
         #web_name == 'Soucek' |
         #web_name == 'Justin' |
         web_name == 'Mitchell' |
         web_name == 'Forster' |
         web_name == 'Connolly' |
         web_name == 'Stones' |
         web_name == 'Antonio' |
         code == 215059 # Sanchez
         #code == 209289 # Smith Rowe
         ) %>%
  mutate(now_cost = now_cost / 10)

df$points_per_90 <- (df$total_points / df$minutes) * 90
df$bonus_per_90 <- ((df$bonus / df$minutes) * 90)

# # The vector to optimize on
# objective <- df$gw19.expected.points
# 
# # Fitting Constraints
# num_gk <- 2
# num_def <- 5
# num_mid <- 5
# num_fwd <- 3
# itb <- 1.3
# max_cost <- df %>% filter(current_team == 1) %>% { sum(.$now_cost) } + itb
# min_current_team <- 12
# keep_in_team <- 2
# discard_from_team <- 0


############## Best model team
# The vector to optimize on
objective <- df$expected.points.total

# Fitting Constraints
num_gk <- 2
num_def <- 5
num_mid <- 5
num_fwd <- 3
itb <- 0.6
max_cost <- df %>% filter(current_team == 1) %>% { sum(.$now_cost) } + itb
min_current_team <- 12
keep_in_team <- 2
discard_from_team <- 0

###############

print("max_cost")
print(max_cost)

# Create vectors to constrain by position
df$Goalkeeper <- ifelse(df$element_type == "1", 1, 0)
df$Defender <- ifelse(df$element_type == "2", 1, 0)
df$Midfielder <- ifelse(df$element_type == "3", 1, 0)
df$Forward <- ifelse(df$element_type == "4", 1, 0)

# Create constraint vectors to constrain by max number of players allowed per team
team_constraint <- unlist(lapply(unique(df$team_code), function(x, df){
  ifelse(df$team_code==x, 1, 0)
}, df=df))

# next we need the constraint directions
const_dir <- c("=", "=", "=", "=", "<=", ">=",
               "=",
               "=",
               rep("<=", 20))

# Now put the complete matrix together
const_mat <- matrix(c(df$Goalkeeper, df$Defender, df$Midfielder, df$Forward,
                      df$now_cost, df$current_team,
                      df$keep,
                      df$discard,
                      team_constraint),
                    nrow=(8 + length(unique(df$team_code))), byrow=TRUE)
const_rhs <- c(num_gk, num_def, num_mid, num_fwd, max_cost, min_current_team,
               keep_in_team,
               discard_from_team,
               rep(3, 20))

# then solve the matrix
x <- lp ("max", objective, const_mat, const_dir, const_rhs, all.bin=TRUE, all.int=TRUE)

#df$expected_points <- (df$expected_points_per_90 + 2) * data$total_matches_played
# And this is our team!
solution <- df %>%
  mutate(solution = x$solution) %>%
  filter(solution == 1) %>%
  select(web_name,
         points_per_90,
         #bonus.pts,
         #bonus_per_90,
         expected_points_per_90,
         expected.points.total,
         element_type,
         now_cost,
         total_points,
         gw21.expected.points,
         #expected_points,
         code,
         minutes,
         player_season_minutes,
         current_team,
         trend
         ) %>%
  arrange(desc(gw21.expected.points))

#print(solution$web_name)

print(solution)

solution %>% summarise(total_price = sum(now_cost)) %>% print
solution %>% summarise(expected.points.total = sum(expected.points.total)) %>% print
solution %>% summarise(total_points_scored = sum(total_points)) %>% print
solution %>% summarise(total_expected_points_per_90 = sum(expected_points_per_90)) %>% print
#solution %>% summarise(total_expected_points = sum(expected_points)) %>% print

colours <- c('Up' = '#26a80f', 'Down' = '#de1b0d')
ggplot(teamData %>% mutate(Direction=ifelse(xg.diff - xg.diff.prev>0,"Up","Down")),
       aes(y = reorder(team_name, xg.diff),
           x = xg.diff.prev,
           xend = xg.diff,
           colour = Direction)) +
  geom_dumbbell(size = 1.2,
                size_x = 1.8,
                size_xend = 3.4,
                #colour = Direction,
                #colour_x = Direction,
                #colour_xend = Direction
                ) +
  scale_color_manual(values = colours, guide = FALSE) +
  theme_minimal() +
  labs(title = "Change in xG difference since start of season",
       #subtitle = "Difference between xG for and against",
       x = "xG difference per game",
       y = "")

ggplot(teamData %>% mutate(Direction=ifelse(team_season_np_xg_pg - team_season_np_xg_pg_prev>0,"Up","Down")),
       aes(y = reorder(team_name, team_season_np_xg_pg),
           x = team_season_np_xg_pg_prev,
           xend = team_season_np_xg_pg,
           colour = Direction)) +
  geom_dumbbell(size = 1.2,
                size_x = 1.8,
                size_xend = 3.4,
                #colour = Direction,
                #colour_x = Direction,
                #colour_xend = Direction
  ) +
  scale_color_manual(values = colours, guide = FALSE) +
  theme_minimal() +
  labs(title = "Change in xG for since start of season",
       #subtitle = "Difference between xG for and against",
       x = "xG for per game",
       y = "")

ggplot(teamData %>% mutate(Direction=ifelse(team_season_np_xg_conceded_pg_prev - team_season_np_xg_conceded_pg>0,"Up","Down")),
       aes(y = reorder(team_name, -team_season_np_xg_conceded_pg),
           x = team_season_np_xg_conceded_pg_prev,
           xend = team_season_np_xg_conceded_pg,
           colour = Direction)) +
  geom_dumbbell(size = 1.2,
                size_x = 1.8,
                size_xend = 3.4,
                #colour = Direction,
                #colour_x = Direction,
                #colour_xend = Direction
  ) +
  scale_color_manual(values = colours, guide = FALSE) +
  theme_minimal() +
  labs(title = "Change in xG against since start of season",
       #subtitle = "Difference between xG for and against",
       x = "xG against per game",
       y = "")
# 
# def5 <- solution %>% filter(element_type == "2") %$% sum(gw03.expected.points) %>% first
# mid3 <- solution %>% filter(element_type == "3") %>% top_n(3, gw03.expected.points) %$% sum(gw03.expected.points) %>% first
# fwd2 <- solution %>% filter(element_type == "4") %>% top_n(2, gw03.expected.points) %$% sum(gw03.expected.points) %>% first
# 
# print("5-3-2:")
# print(def5 + mid3 + fwd2)
# 
# def4 <- solution %>% filter(element_type == "2") %>% top_n(4, gw03.expected.points) %$% sum(gw03.expected.points) %>% first
# mid4 <- solution %>% filter(element_type == "3") %>% top_n(4, gw03.expected.points) %$% sum(gw03.expected.points) %>% first
# fwd2 <- solution %>% filter(element_type == "4") %>% top_n(2, gw03.expected.points) %$% sum(gw03.expected.points) %>% first
# 
# print("4-4-2:")
# print(def4 + mid4 + fwd2)
# 
# def4 <- solution %>% filter(element_type == "2") %>% top_n(4, gw03.expected.points) %$% sum(gw03.expected.points) %>% first
# mid3 <- solution %>% filter(element_type == "3") %>% top_n(3, gw03.expected.points) %$% sum(gw03.expected.points) %>% first
# fwd3 <- solution %>% filter(element_type == "4") %>% top_n(3, gw03.expected.points) %$% sum(gw03.expected.points) %>% first
# 
# print("4-3-3:")
# print(def4 + mid3 + fwd3)
# 
# def3 <- solution %>% filter(element_type == "2") %>% top_n(3, gw03.expected.points) %$% sum(gw03.expected.points) %>% first
# mid5 <- solution %>% filter(element_type == "3") %>% top_n(5, gw03.expected.points) %$% sum(gw03.expected.points) %>% first
# fwd2 <- solution %>% filter(element_type == "4") %>% top_n(2, gw03.expected.points) %$% sum(gw03.expected.points) %>% first
# 
# print("3-5-2:")
# print(def3 + mid5 + fwd2)
# 
# def3 <- solution %>% filter(element_type == "2") %>% top_n(3, gw03.expected.points) %$% sum(gw03.expected.points) %>% first
# mid4 <- solution %>% filter(element_type == "3") %>% top_n(4, gw03.expected.points) %$% sum(gw03.expected.points) %>% first
# fwd3 <- solution %>% filter(element_type == "4") %>% top_n(3, gw03.expected.points) %$% sum(gw03.expected.points) %>% first
# 
# print("3-4-3:")
# print(def3 + mid4 + fwd3)
# 
# def5 <- solution %>% filter(element_type == "2") %>% top_n(5, gw03.expected.points) %$% sum(gw03.expected.points) %>% first
# mid4 <- solution %>% filter(element_type == "3") %>% top_n(4, gw03.expected.points) %$% sum(gw03.expected.points) %>% first
# fwd1 <- solution %>% filter(element_type == "4") %>% top_n(1, gw03.expected.points) %$% sum(gw03.expected.points) %>% first
# 
# print("5-4-1:")
# print(def5 + mid4 + fwd1)
# 
# def5 <- solution %>% filter(element_type == "2") %>% top_n(5, gw03.expected.points) %$% sum(gw03.expected.points) %>% first
# mid2 <- solution %>% filter(element_type == "3") %>% top_n(2, gw03.expected.points) %$% sum(gw03.expected.points) %>% first
# fwd3 <- solution %>% filter(element_type == "4") %>% top_n(3, gw03.expected.points) %$% sum(gw03.expected.points) %>% first
# 
# print("5-2-3:")
# print(def5 + mid2 + fwd3)

#linear_model = lm(df$total_points ~ df$expected_points)
#plot(df$expected_points, df$total_points)
#plot(linear_model, 1)
#plot(linear_model, 3)
#plot(linear_model, 2)
#plot(linear_model, 5)
#plot(linear_model, 4)
#summary(linear_model)
