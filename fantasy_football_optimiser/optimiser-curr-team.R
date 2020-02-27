library(lpSolve)
library(RCurl)
library(stringr)
library(jsonlite)
library(dplyr)

data <- "http://localhost:3000/" %>% 
  getURL %>% 
  fromJSON

df <- data %>% 
  .$elements %>% 
  data.frame %>% 
  filter(web_name != "Jesus" & 
           #web_name != "Barnes" & 
           web_name != "Bernardo Silva"  &
           web_name != "Lundstram" 
           #web_name != "Sterling" & 
           #web_name != "Abraham" &
           #web_name != "Wood"
           #web_name != "Maddison" &
           #web_name != "Otamendi" 
           #web_name != "El Ghazi" &
          # web_name != "Martial" &
          # web_name != "Mount"
         ) %>%
  mutate(now_cost = now_cost / 10) %>% 
  select(sort(names(.)))

# The vector to optimize on
objective <- df$expected.points.total

# Fitting Constraints
num_gk <- 2
num_def <- 5
num_mid <- 5
num_fwd <- 3
max_cost <- 100.7
min_current_team <- 12
keep_in_team <- 1
discard_from_team <- 0

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
const_dir <- c("=", "=", "=", "=", "<=", ">=", "=", "=", rep("<=", 20))

# Now put the complete matrix together
const_mat <- matrix(c(df$Goalkeeper, df$Defender, df$Midfielder, df$Forward, 
                      df$now_cost, df$current_team, df$keep, df$discard, team_constraint), 
                    nrow=(8 + length(unique(df$team_code))), byrow=TRUE)
const_rhs <- c(num_gk, num_def, num_mid, num_fwd, max_cost, min_current_team, keep_in_team, discard_from_team, rep(3, 20))

# then solve the matrix
x <- lp ("max", objective, const_mat, const_dir, const_rhs, all.bin=TRUE, all.int=TRUE)

df$expected_points <- (df$expected_points_per_90 + 2) * data$total_matches_played
# And this is our team!
solution <- df %>% 
  mutate(solution = x$solution) %>% 
  filter(solution == 1) %>% 
  select(web_name, element_type, now_cost, total_points, expected_points_per_90, expected.points.total, gw29.expected.points, expected_points, minutes, current_team, trend) %>% 
  arrange(desc(expected.points.total))

print(solution)

solution %>% summarise(total_price = sum(now_cost)) %>% print
solution %>% summarise(total_points = sum(total_points)) %>% print
solution %>% summarise(total_expected_points_per_90 = sum(expected_points_per_90)) %>% print
solution %>% summarise(total_expected_points = sum(expected_points)) %>% print

def5 <- solution %>% filter(element_type == "2") %$% sum(expected_points_per_90) %>% first
mid3 <- solution %>% filter(element_type == "3") %>% top_n(3, expected_points_per_90) %$% sum(expected_points_per_90) %>% first
fwd2 <- solution %>% filter(element_type == "4") %>% top_n(2, expected_points_per_90) %$% sum(expected_points_per_90) %>% first

print("5-3-2:")
print(def5 + mid3 + fwd2)

def4 <- solution %>% filter(element_type == "2") %>% top_n(4, expected_points_per_90) %$% sum(expected_points_per_90) %>% first
mid4 <- solution %>% filter(element_type == "3") %>% top_n(4, expected_points_per_90) %$% sum(expected_points_per_90) %>% first
fwd2 <- solution %>% filter(element_type == "4") %>% top_n(2, expected_points_per_90) %$% sum(expected_points_per_90) %>% first

print("4-4-2:")
print(def4 + mid4 + fwd2)

def4 <- solution %>% filter(element_type == "2") %>% top_n(4, expected_points_per_90) %$% sum(expected_points_per_90) %>% first
mid3 <- solution %>% filter(element_type == "3") %>% top_n(3, expected_points_per_90) %$% sum(expected_points_per_90) %>% first
fwd3 <- solution %>% filter(element_type == "4") %>% top_n(3, expected_points_per_90) %$% sum(expected_points_per_90) %>% first

print("4-3-3:")
print(def4 + mid3 + fwd3)

def3 <- solution %>% filter(element_type == "2") %>% top_n(3, expected_points_per_90) %$% sum(expected_points_per_90) %>% first
mid5 <- solution %>% filter(element_type == "3") %>% top_n(5, expected_points_per_90) %$% sum(expected_points_per_90) %>% first
fwd2 <- solution %>% filter(element_type == "4") %>% top_n(2, expected_points_per_90) %$% sum(expected_points_per_90) %>% first

print("3-5-2:")
print(def3 + mid5 + fwd2)

def3 <- solution %>% filter(element_type == "2") %>% top_n(3, expected_points_per_90) %$% sum(expected_points_per_90) %>% first
mid4 <- solution %>% filter(element_type == "3") %>% top_n(4, expected_points_per_90) %$% sum(expected_points_per_90) %>% first
fwd3 <- solution %>% filter(element_type == "4") %>% top_n(3, expected_points_per_90) %$% sum(expected_points_per_90) %>% first

print("3-4-3:")
print(def3 + mid4 + fwd3)

def5 <- solution %>% filter(element_type == "2") %>% top_n(5, expected_points_per_90) %$% sum(expected_points_per_90) %>% first
mid4 <- solution %>% filter(element_type == "3") %>% top_n(4, expected_points_per_90) %$% sum(expected_points_per_90) %>% first
fwd1 <- solution %>% filter(element_type == "4") %>% top_n(1, expected_points_per_90) %$% sum(expected_points_per_90) %>% first

print("5-4-1:")
print(def5 + mid4 + fwd1)

linear_model = lm(df$total_points ~ df$expected_points)
plot(df$expected_points, df$total_points)
plot(linear_model, 1)
plot(linear_model, 3)
plot(linear_model, 2)
plot(linear_model, 5)
plot(linear_model, 4)
summary(linear_model)
