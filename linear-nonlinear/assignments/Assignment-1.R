# Q1: Divvy
q1dat <- read.table("/Users/xnxk040/Library/CloudStorage/OneDrive-W.WGrainger,inc/Desktop/linear-nonlinear data/divvy.txt", header = TRUE)
distance <- q1dat$Distance
speed <- q1dat$Speed

# A. Fitted Line Plot of Data
fitted_line1 <- lm(distance ~ speed)
plot(x = speed, y = distance,
     xlab = "Speed, in miles per hour",
     ylab = "Avg. Distance, in yards",
     main = "Divvy Electric Scooter Speed versus Average Distance",
     panel.last = lines(sort(speed), fitted(fitted_line1)[order(speed)]))

# B. R2 Value
summary(fitted_line1) # 0.9533

## Q2. Soccer
q2dat <- read.table('soccer.txt', header = TRUE)
sprint <- q2dat$Sprint
score <- q2dat$Score

# A. Fitted Line Plot
fitted_line2 <- lm(score ~ sprint)
plot(x = sprint, y = score,
     xlab = "Sprint Yards", ylab = "Expected Goal Score",
     main = "Kante's Sprint Yards vs. Team's Final Goal Score",
     panel.last = lines(sort(sprint), fitted(fitted_line2)[order(sprint)]))
summary(fitted_line2)

# B. Remove data wheren sprint = 210 yds
q2dat_red <- q2dat[which(q2dat$Sprint != 210),]
fit3 <- lm(q2dat_red$Score ~ q2dat_red$Sprint)
plot(x = q2dat_red$Sprint, y = q2dat_red$Score,
     xlab = "Sprint Yards", ylab = "Expected Goal Score",
     main = "Kante's Sprint Yards vs. Team's Final Goal Score (Reduced)",
     panel.last = lines(sort(q2dat_red$Sprint), 
                        fitted(fit3)[order(q2dat_red$Sprint)]))
summary(fit3)

# Q3. Drug law Expenditures vs Drug-Induced Deaths
q3dat <- read.table("death.txt", header = TRUE)
budget <- q3dat$budget
deaths <- q3dat$deaths
year <- q3dat$year

# A. Response: deaths, Predictor: budget
fit4 <- lm(deaths ~ budget)
plot(x = budget, y = deaths,
     xlab = "D.E.A. Budget", ylab = "Drug Induced Deaths",
     main = "U.S. Drug Enforcement Ageny Budget vs. Number of Drug Induced Deaths",
     panel.last = lines(sort(budget), 
                        fitted(fit4)[order(budget)]))


# B. Response: budget, Predictor: deaths
fit5 <- lm(budget ~ deaths)
plot(x = deaths, y = budget,
     xlab = "Drug Induced Deaths", ylab = "D.E.A. Budget",
     main = "Number of Drug Induced Deaths vs. U.S. Drug Enforcement Ageny Budget",
     panel.last = lines(sort(deaths), 
                        fitted(fit5)[order(deaths)]))

# C. Response: budget, Predictor: year
fit6 <- lm(budget ~ year)
plot(x = year, y = budget,
     xlab = "Year", ylab = "D.E.A. Budget",
     main = "Year vs. U.S. Drug Enforcement Ageny Budget",
     panel.last = lines(sort(year), 
                        fitted(fit6)[order(year)]))

# D. Response: deaths, Predictor: year
fit7 <- lm(deaths ~ year)
plot(x = year, y = deaths,
     xlab = "Year", ylab = "Drug Induced Deaths",
     main = "Year vs. Number of Drug Induced Deaths",
     panel.last = lines(sort(year), 
                        fitted(fit7)[order(year)]))
