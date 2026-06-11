#######################
##World Cup Predictor##
#######################

##Import data##
setwd("~/Programming/R/World Cup 2026 Project")
wc1930_2022 = read.csv("matches_1930_2022.csv")
wc2026 = read.csv("future_match_probabilities_baseline.csv")


##Choose Rows & Columns##
names(wc1930_2022)
wc2006_2022 = wc1930_2022[wc1930_2022$Year %in% c(2006:2022),
                          c("Year", "Round", "home_team", "away_team", "home_score", "away_score")] 
str(wc2006_2022)


##Check Round##
table(wc2006_2022$Round)
wc2006_2022 = wc2006_2022[-(which(wc2006_2022$Round == "Third-place match")),]
wc2006_2022$Round = ifelse(wc2006_2022$Round == "Group stage", 1, 
                           ifelse(wc2006_2022$Round == "Round of 16", 2,
                                  ifelse(wc2006_2022$Round == "Quarter-finals", 3,
                                         ifelse(wc2006_2022$Round == "Semi-finals", 4,
                                                ifelse(wc2006_2022$Round == "Final", 5, 0)))))
head(wc2006_2022)

##Team Names##
table(wc2006_2022$home_team)
table(wc2006_2022$away_team)


##Ranking Difference##
home_minus_away.ranking = function(wc.year, country, rankings){
  fifa_rankings = data.frame(country, rankings)
  
  home_country_vec = wc.year$home_team
  away_country_vec = wc.year$away_team
  n = length(home_country_vec)
  
  home.rank = numeric(n)
  for (i in 1:n){
    idx = which(home_country_vec[i] == fifa_rankings[,1])
    home.rank[i] = fifa_rankings[idx,2]
  }
  
  away.rank = numeric(n)
  for (i in 1:n){
    idx = which(away_country_vec[i] == fifa_rankings[,1])
    away.rank[i] = fifa_rankings[idx,2]
  }
  
  return(-(home.rank - away.rank))
}



##Previous World Cups##
wc2006 = wc2006_2022[wc2006_2022$Year == 2006,]
wc2010 = wc2006_2022[wc2006_2022$Year == 2010,]
wc2014 = wc2006_2022[wc2006_2022$Year == 2014,]
wc2018 = wc2006_2022[wc2006_2022$Year == 2018,]
wc2022 = wc2006_2022[wc2006_2022$Year == 2022,]


##FIFA Rankings going into tournament##

country.2006 = c("IR Iran", "Japan", "Saudi Arabia", "Korea Republic",
                 "Angola", "Ghana", "Côte d'Ivoire", "Togo", "Tunisia",
                 "Costa Rica", "Mexico", "Trinidad and Tobago", "United States",
                 "Argentina", "Brazil", "Ecuador", "Paraguay",
                 "Australia",
                 "Croatia", "Czech Republic", "England", "France", "Germany", "Italy", "Netherlands",
                 "Poland", "Portugal", "Serbia and Montenegro", "Spain", "Sweden", "Switzerland", "Ukraine")
ranking.2006 = c(23,18,34,30,
                 57,48,32,61,21,
                 26,4,47,5,
                 9,1,39,33,
                 42,
                 24,2,10,8,19,13,3,
                 29,7,44,6,16,35,45)


country.2010 = c("Australia", "Japan", "Korea DPR", "Korea Republic",
                 "Algeria", "Cameroon", "Ghana", "Côte d'Ivoire", "Nigeria", "South Africa",
                 "Honduras", "Mexico", "United States",
                 "Argentina", "Brazil", "Chile", "Paraguay", "Uruguay",
                 "New Zealand",
                 "Denmark", "England", "France", "Germany", "Greece", "Italy", "Netherlands",
                 "Portugal", "Serbia", "Slovakia", "Slovenia", 'Spain', "Switzerland")
ranking.2010 = c(20,45,105,47,
                 30,19,32,27,21,83,
                 38,17,14,
                 7,1,18,31,16,
                 78,
                 36,8,9,6,13,5,4,3,15,34,25,2,24)


country.2014 = c("Australia", "IR Iran", "Japan", "Korea Republic",
                 "Algeria", "Cameroon", "Ghana", "Côte d'Ivoire", "Nigeria",
                 "Costa Rica", "Honduras", "Mexico", 'United States',
                 "Argentina", "Brazil", "Chile", "Colombia", "Ecuador", "Uruguay",
                 "Belgium", "Bosnia and Herzegovina", "Croatia", "England", 'France', "Germany",
                 "Greece", "Italy", "Netherlands", "Portugal", "Russia", "Spain", 'Switzerland')
ranking.2014 = c(62,43,46,57,
                 22,56,37,23,44,
                 28,33,20,13,
                 5,3,14,8,26,7,
                 11,21,18,10,17,2,12,9,15,4,19,1,6)


country.2018 = c("Australia", "IR Iran", "Japan", "Saudi Arabia","Korea Republic",
                 "Egypt", "Morocco", "Nigeria", "Senegal", "Tunisia",
                 "Costa Rica", "Mexico", 'Panama',
                 "Argentina", "Brazil", "Colombia", "Peru", "Uruguay",
                 "Belgium", "Croatia", "Denmark", "England", 'France', "Germany", "Iceland",
                 "Poland", "Portugal", "Russia", "Serbia", "Spain", "Sweden",'Switzerland')
ranking.2018 = c(36,37,61,67,57,
                 45,41,48,27,21,
                 23,15,55,
                 5,2,16,11,14,
                 3,20,13,12,7,1,22,8,4,70,34,10,24,6)


country.2022 = c("Australia", "IR Iran", "Japan", "Qatar", "Saudi Arabia","Korea Republic",
                 "Cameroon", "Ghana", "Morocco", "Senegal", "Tunisia",
                 "Canada", "Costa Rica", "Mexico", 'United States',
                 "Argentina", "Brazil", "Ecuador", "Uruguay",
                 "Belgium", "Croatia", "Denmark", "England", 'France', "Germany", "Netherlands",
                 "Poland", "Portugal", "Serbia", "Spain", 'Switzerland', "Wales")
ranking.2022 = c(38,20,24,50,51,28,
                 43,61,22,18,30,
                 41,31,13,16,
                 3,1,44,14,
                 2,12,10,5,4,11,8,26,9,21,7,15,19)


wc2006$rank_diff = home_minus_away.ranking(wc2006, country.2006, ranking.2006)
wc2010$rank_diff = home_minus_away.ranking(wc2010, country.2010, ranking.2010)
wc2014$rank_diff = home_minus_away.ranking(wc2014, country.2014, ranking.2014)
wc2018$rank_diff = home_minus_away.ranking(wc2018, country.2018, ranking.2018)
wc2022$rank_diff = home_minus_away.ranking(wc2022, country.2022, ranking.2022)
wc2006_2022 = data.frame(rbind(wc2022, wc2018, wc2014, wc2010, wc2006))
head(wc2006_2022)

##Add Result (wrt lower ranked aka larger ranking)##
wc2006_2022$GD = wc2006_2022$home_score - wc2006_2022$away_score
sign = wc2006_2022$rank_diff * wc2006_2022$GD

wc2006_2022$result = ifelse(sign == 0, "Draw", "-")
wc2006_2022$result = ifelse(sign > 0, "Lose", wc2006_2022$result)
wc2006_2022$result = ifelse(sign < 0, "Win", wc2006_2022$result)

wc2006_2022$Underdogs = ifelse(wc2006_2022$rank_diff < 0, wc2006_2022$home_team, wc2006_2022$away_team)
wc2006_2022$Favourites = ifelse(wc2006_2022$rank_diff > 0, wc2006_2022$home_team, wc2006_2022$away_team)


##Home Advantage##
Year = c(2022,2018,2014,2010,2006)
host = c("Qatar", "Russia", "Brazil", "South Africa", "Germany")
host_nations = data.frame(Year,host)

home_adv = numeric(nrow(wc2006_2022))
for (i in 1:nrow(wc2006_2022)){
  row = wc2006_2022[i,]
  country_hosting = host_nations$host[which(host_nations$Year %in% row[1])]
  if (row[7] > 0){
    lower_ranked = row[4]
    higher_ranked = row[3]
  } else {
    lower_ranked = row[3]
    higher_ranked = row[4]
  }
  advantage = ifelse(lower_ranked == country_hosting, 2, 
                     ifelse(higher_ranked == country_hosting, 1, 0))
  home_adv[i] = advantage
}

wc2006_2022$home_advantage = home_adv
wc2006_2022$GD = ifelse(wc2006_2022$result == "Draw", 0, 
                        ifelse(wc2006_2022$result == "Win", abs(wc2006_2022$GD), -abs(wc2006_2022$GD)))
wc2006_2022$rank_diff = abs(wc2006_2022$rank_diff)

##Removing unnecessary cols##
wanted_cols = c("Year", "Underdogs", "Favourites", "Round", "rank_diff", "GD", "home_advantage", "result")
wc2006_2022 = wc2006_2022[,wanted_cols]
wc2006_2022$home_advantage = as.factor(wc2006_2022$home_advantage)
wc2006_2022$Round = factor(wc2006_2022$Round, levels = c(1,2,3,4,5))
wc2006_2022$result = factor(wc2006_2022$result, levels = c("Win", "Draw", "Lose"))


##EDA##
ave.rankings = function(wc.year, country, rankings){
  fifa_rankings = data.frame(country, rankings)
  n = nrow(wc.year)
  GS = c()
  RO16 = c()
  QF = c()
  SF = c()
  Final = c()
  country.used = c()
  
  for (i in 1:n){
    game = wc.year[i,]
    round = game[2]
    team.A = game[3]
    team.B = game[4]
    
    for (team in c(team.A, team.B)){
      if (team %in% country.used){next}
      
      ranking = fifa_rankings[which(team == fifa_rankings[,1]),2]
      if (round == 1) {GS = c(GS, ranking)}
      else if (round == 2) {RO16 = c(RO16, ranking)}
      else if (round == 3) {QF = c(QF, ranking)}
      else if (round == 4) {SF = c(SF, ranking)}
      else {Final = c(Final, ranking)}
      
      country.used = c(country.used, team)
    }}
  return (c(mean(GS),mean(RO16),mean(QF),mean(SF),mean(Final)))
  }
ave.finishes = data.frame(rbind(WC2006 = ave.rankings(wc2006, country.2006, ranking.2006),
                                WC2010 = ave.rankings(wc2010, country.2010, ranking.2010),
                                WC2014 = ave.rankings(wc2014, country.2014, ranking.2014),
                                WC2018 = ave.rankings(wc2018, country.2018, ranking.2018),
                                WC2022 = ave.rankings(wc2022, country.2022, ranking.2022)))
names(ave.finishes) = c("GS", "RO16", "QF", "SF", "Final")
colMeans(ave.finishes)


seed = function(wc.year, country, rankings){
  ave.eli = numeric(10)
  for (seed in 1:10){
    ranked_n = country[which(rankings == seed)]
    if (length(ranked_n) == 0) { ave.eli[seed] = NA; next }

    for (i in 1:nrow(wc.year)){
      game = wc.year[i,]
      round = game[2]
      team.A = game[3]
      team.B = game[4]

      if (ranked_n %in% c(team.A, team.B)){
        ave.eli[seed] = round
        break
      }}}
  return (ave.eli)
  }
top10_ave = data.frame(rbind(WC2006 = seed(wc2006, country.2006, ranking.2006),
                             WC2010 = seed(wc2010, country.2010, ranking.2010),
                             WC2014 = seed(wc2014, country.2014, ranking.2014),
                             WC2018 = seed(wc2018, country.2018, ranking.2018),
                             WC2022 = seed(wc2022, country.2022, ranking.2022)))
names(top10_ave) = c(1:10) ; top10_ave


champ.rank = c(ranking.2006[which(country.2006 == "Italy")],
               ranking.2010[which(country.2010 == "Spain")],
               ranking.2014[which(country.2014 == "Germany")],
               ranking.2018[which(country.2018 == "France")],
               ranking.2022[which(country.2022 == "Argentina")])
mean.champ = mean(champ.rank) ; mean.champ


prop.table(table(wc2006_2022[wc2006_2022$Round == '1',]$result))
prop.table(table(wc2006_2022$result))
prop.table(table(wc2006_2022$home_advantage))
hist(wc2006_2022$rank_diff)
hist(wc2006_2022$GD)



##Logistic Regression##
library(nnet)
LR = multinom(result ~ Round + rank_diff + home_advantage, data = wc2006_2022)
summary(LR)
pred_lr = predict(LR, wc2006_2022, type = "class")
accuracy_lr = mean(pred_lr == wc2006_2022$result); accuracy_lr

##Threshold##
pred_wc2006.2022 = round(predict(LR, wc2006_2022, type = "prob")*100, 3)
max_lose = max(pred_wc2006.2022[,"Lose"])
min_lose = min(pred_wc2006.2022[,"Lose"])

actual_vals = wc2006_2022$result
actual_threshold_vals = ifelse(actual_vals == 'Lose', 'Lose', 'No Lose')
pred_vals = round(predict(LR, wc2006_2022, type = "prob")*100, 3)[,3]

delta_vals = seq(from = min_lose, to = max_lose, by = 0.001)
acc_vals = numeric(length(delta_vals))
for (i in 1:length(delta_vals)){
  pred_threshold_vals = ifelse(pred_vals > delta_vals[i], 'Lose', 'No Lose')
  accuracy_threshold = mean(actual_threshold_vals == pred_threshold_vals)
  acc_vals[i] = accuracy_threshold
}
threshold = round(mean(delta_vals[which(acc_vals == max(acc_vals))]),3); threshold; max(acc_vals)



##Biggest Upsets in History##
surprise_table = data.frame(Year = wc2006_2022$Year, Underdog_vs_Favourites = paste(wc2006_2022$Underdogs, "vs", wc2006_2022$Favourites), 
                            Win_Percentage = pred_wc2006.2022[,1], Draw_Percentage = pred_wc2006.2022[,2], Lose_Percentage = pred_wc2006.2022[,3],
                            Actual = wc2006_2022$result, rank_diff = wc2006_2022$rank_diff)
surprise_table = surprise_table[surprise_table$rank_diff > 20,]
surprise_table$prob_actual = ifelse(surprise_table$Actual == "Win", surprise_table$Win_Percentage,
                                    ifelse(surprise_table$Actual == "Draw", surprise_table$Draw_Percentage, surprise_table$Lose_Percentage))
surprise_table = surprise_table[order(surprise_table$prob_actual),]
surprise_table[1:10,c(1:6)]


##World Cup 2026##
wc2026 = wc2026[,1:3]

##Fix country names##
wc2026$away_team[which(wc2026$away_team == "UEFA_Playoff_A")] = "Bosnia and Herzegovina"
wc2026$away_team[which(wc2026$away_team == "UEFA_Playoff_B")] = "Sweden"
wc2026$away_team[which(wc2026$away_team == "UEFA_Playoff_C")] = "Turkey"
wc2026$away_team[which(wc2026$away_team == "UEFA_Playoff_D")] = "Czech Republic"
wc2026$away_team[which(wc2026$away_team == "Interconf_Playoff_1")] = "DR Congo"
wc2026$away_team[which(wc2026$away_team == "Interconf_Playoff_2")] = "Iraq"
wc2026$home_team[which(wc2026$home_team == "Iran")] = "IR Iran"
wc2026$away_team[which(wc2026$away_team == "Iran")] = "IR Iran"
wc2026$home_team[which(wc2026$home_team == "South Korea")] = "Korea Republic"
wc2026$away_team[which(wc2026$away_team == "South Korea")] = "Korea Republic"
wc2026$home_team[which(wc2026$home_team == "Côte d’Ivoire")] = "Cote d'Ivoire"
wc2026$away_team[which(wc2026$away_team == "Côte d’Ivoire")] = "Cote d'Ivoire"
wc2026$home_team[which(wc2026$home_team == "Curaçao")] = "Curacao"
wc2026$away_team[which(wc2026$away_team == "Curaçao")] = "Curacao"
wc2026$home_team[which(wc2026$home_team == "Cape_Verde")] = "Cape Verde"
wc2026$away_team[which(wc2026$away_team == "Cape_Verde")] = "Cape Verde"

wc2026$Round = rep("1",nrow(wc2026))

country.2026 = c("Australia", "IR Iran", "Iraq", "Japan", "Jordan", "Qatar", "Saudi Arabia","Korea Republic", "Uzbekistan",
                 "Algeria","Cape Verde","DR Congo","Egypt", "Ghana","Cote d'Ivoire", "Morocco", "Senegal", "South Africa", "Tunisia",
                 "Canada", "Curacao", "Haiti", "Mexico", "Panama", 'United States',
                 "Argentina", "Brazil", "Colombia", "Ecuador", "Paraguay", "Uruguay",
                 "New Zealand",
                 "Austria", "Belgium", "Bosnia and Herzegovina", "Croatia",
                 "Czech Republic", "England", 'France', "Germany", "Netherlands", "Norway",
                 "Portugal", "Scotland", "Spain", 'Sweden', "Turkey", "Switzerland")
ranking.2026 = c(27,21,56,18,63,57,61,25,50,
                 28,67,45,29,73,33,7,15,60,46,
                 30,82,83,14,34,17,
                 1,6,13,23,40,16,
                 85,
                 24,9,64,11,39,4,3,10,8,31,5,42,2,38,22,19)

wc2026$rank_diff = home_minus_away.ranking(wc2026,country.2026,ranking.2026)

home_adv.2026 = numeric(nrow(wc2026))
for (i in 1:nrow(wc2026)){
  row = wc2026[i,]
  if (row[5] > 0){
    lower_ranked = row[3]
    higher_ranked = row[2]
  } else {
    lower_ranked = row[2]
    higher_ranked = row[3]
  }
  advantage = ifelse(lower_ranked %in% c('United States', 'Canada', "Mexico"), 2,
                     ifelse(higher_ranked %in% c("United States", "Canada", "Mexico"), 1, 0))
  home_adv.2026[i] = advantage
}

wc2026$home_advantage = home_adv.2026
wc2026$Year = rep(2026, nrow(wc2026))
wc2026$Underdogs = ifelse(wc2026$rank_diff < 0, wc2026$home_team, wc2026$away_team)
wc2026$Favourites = ifelse(wc2026$rank_diff > 0, wc2026$home_team, wc2026$away_team)
wc2026$rank_diff = abs(wc2026$rank_diff)

wc2026 = wc2026[,c("Year", "group", "Underdogs", "Favourites", "Round", "rank_diff", "home_advantage")]
wc2026$home_advantage = as.factor(wc2026$home_advantage)
wc2026$Round = factor(wc2026$Round, levels = c(1,2,3,4,5))


##WC26 Watch List##
pred_wc26 = round(predict(LR, wc2026, type = "prob")*100, 3)
pred_wc26_class = ifelse(pred_wc26[,3] > threshold, "Expected to lose", "Potential Upset")
wc2026_watch.list = data.frame(Group = wc2026$group, Underdog_vs_Favourites = paste(wc2026$Underdogs, "vs", wc2026$Favourites),
                               Win_Percentage = pred_wc26[,1], Draw_Percentage = pred_wc26[,2], Lose_Percentage = pred_wc26[,3],
                               Predictions = pred_wc26_class)
wc2026_watch.list = wc2026_watch.list[order(wc2026_watch.list$Lose_Percentage),]
wc2026_watch.list = wc2026_watch.list[wc2026_watch.list$Predictions == "Potential Upset",]
wc2026_watch.list
nrow(wc2026_watch.list)

##Tournament Predictions##

##Group Stage LM##
group.stages = wc2006_2022[wc2006_2022$Round == '1',]
LM_grp = lm(GD ~ rank_diff + home_advantage, data = group.stages)
pred.gd_group = predict(LM_grp, group.stages)

agg_group = aggregate(pred.gd_group ~ group.stages$result, FUN = mean)
cut.off_win_grp = agg_group[agg_group[,1] == "Win", 2]
cut.off_draw_grp = agg_group[agg_group[,1] == "Draw", 2]
cut.off_lose_grp = agg_group[agg_group[,1] == "Lose", 2]

pred_group = ifelse(pred.gd_group > cut.off_win_grp, "Win",
                    ifelse(pred.gd_group < cut.off_lose_grp, "Lose", "Draw"))
accuracy_lm = mean(pred_group == group.stages$result); accuracy_lm

pred_wc2026 = predict(LM_grp, wc2026)
pred_wc2026_class = ifelse(pred_wc2026 > cut.off_win_grp, "Win",
                           ifelse(pred_wc2026 < cut.off_lose_grp, "Lose", "Draw"))

wc2026_predictions = data.frame(cbind(wc2026, pred_wc2026_class, pred_wc2026))
wc2026_predictions$pred_wc2026 = as.numeric(wc2026_predictions$pred_wc2026)

wc2026_predictions = wc2026_predictions[order(wc2026_predictions$group, 
                                              wc2026_predictions$pred_wc2026_class,
                                              -wc2026_predictions$pred_wc2026),]


##Tabulating Groups##
grp_standings = function(group.results){
  Team = c()
  Pts = numeric(4)
  GD = numeric(4)
  for (i in 1:6){
    match = group.results[i,]
    underdog = match$Underdogs
    favourite = match$Favourites
    goal.diff = match$pred_wc2026
    
    if (!underdog %in% Team) Team = c(Team, underdog)
    if (!favourite %in% Team) Team = c(Team, favourite)
    
    underdog.idx = which(Team == underdog)
    favourite.idx = which(Team == favourite)
    
    GD[underdog.idx] = GD[underdog.idx] + goal.diff
    GD[favourite.idx] = GD[favourite.idx] - goal.diff
    
    if (match$pred_wc2026_class == "Draw"){
      Pts[underdog.idx] = Pts[underdog.idx] + 1
      Pts[favourite.idx] = Pts[favourite.idx] + 1
    } else if (match$pred_wc2026_class == "Lose"){
      Pts[favourite.idx] = Pts[favourite.idx] + 3
    } else if (match$pred_wc2026_class == "Win"){
      Pts[underdog.idx] = Pts[underdog.idx] + 3
    }
  }
  tab = data.frame(Team, Pts, GD)
  tab = tab[order(-tab$Pts, -tab$GD),]
  return(tab)
}

##Groups##
A.res = wc2026_predictions[wc2026_predictions$group == 'A',]; group.A = grp_standings(A.res); group.A
B.res = wc2026_predictions[wc2026_predictions$group == 'B',]; group.B = grp_standings(B.res); group.B
C.res = wc2026_predictions[wc2026_predictions$group == 'C',]; group.C = grp_standings(C.res); group.C
D.res = wc2026_predictions[wc2026_predictions$group == 'D',]; group.D = grp_standings(D.res); group.D
E.res = wc2026_predictions[wc2026_predictions$group == 'E',]; group.E = grp_standings(E.res); group.E
F.res = wc2026_predictions[wc2026_predictions$group == 'F',]; group.F = grp_standings(F.res); group.F
G.res = wc2026_predictions[wc2026_predictions$group == 'G',]; group.G = grp_standings(G.res); group.G
H.res = wc2026_predictions[wc2026_predictions$group == 'H',]; group.H = grp_standings(H.res); group.H
I.res = wc2026_predictions[wc2026_predictions$group == 'I',]; group.I = grp_standings(I.res)[c(2,1,3,4),]; group.I
#Senegal wins France. Senegal tops the group, France finishes second (H2H)
J.res = wc2026_predictions[wc2026_predictions$group == 'J',]; group.J = grp_standings(J.res); group.J
K.res = wc2026_predictions[wc2026_predictions$group == 'K',]; group.K = grp_standings(K.res); group.K
L.res = wc2026_predictions[wc2026_predictions$group == 'L',]; group.L = grp_standings(L.res); group.L

##Best 3rd Place##
thirdplace = data.frame(rbind(group.A[3,], group.B[3,], group.C[3,], group.D[3,],
                              group.E[3,], group.F[3,], group.G[3,], group.H[3,],
                              group.I[3,], group.J[3,], group.K[3,], group.L[3,]))
thirdplace = thirdplace[order(-thirdplace$Pts, -thirdplace$GD),]
thirdplace


##Knockout Rounds##
ko.round = wc2006_2022[wc2006_2022$Round != "1",]
LM_ko = lm(GD ~ Round + rank_diff, data = ko.round)

pred.gd_ko = predict(LM_ko, ko.round)
agg = aggregate(pred.gd_ko ~ ko.round$result, FUN = mean)
cut.off_win = agg[agg[,1] == "Win", 2]
cut.off_lose = agg[agg[,1] == "Lose", 2]
cut.off_draw = (cut.off_win + cut.off_lose) / 2

ko.pred = function(match){
  match$rank_diff = home_minus_away.ranking(match, country.2026, ranking.2026)

  Underdog = ifelse(match$rank_diff < 0, match$home_team, match$away_team)
  Favourites = ifelse(match$rank_diff > 0, match$home_team, match$away_team)
  match$rank_diff = abs(match$rank_diff)
  
  pred_gd = predict(LM_ko, match)
  pred_class = ifelse(pred_gd > cut.off_draw, "Win", "Lose")
  AET = ifelse(pred_gd > cut.off_win, "Win",
               ifelse(pred_gd < cut.off_lose, "Lose", "Draw"))
  ET.PK = ifelse(AET == 'Draw', "in Penalties", "in Normal Time")
  
  res = data.frame(Underdog_vs_Favourites = paste(Underdog, "vs", Favourites),
                   Result = pred_class, AET = ET.PK)
  return(res)
}

##Manually input base on Brackets##
matchup = data.frame(home_team = "Mexico",
                     away_team = "Japan",
                     Round = factor("3", levels = c("1","2","3","4","5")))
ko.pred(matchup)
