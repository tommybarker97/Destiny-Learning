#Contents:

#Line 11   -  Loading Data
#Line 23   -  Preparing Data
#Line 127  -  Analysis
#Line 200  -  Further Analysis
#Line 979  -  Plots


#####
# 1.LOADING DATA

library(tidyverse)
library(ggpubr)
library(plotrix)
session_by_player <- read.csv(file = "../Data/rldat.csv", header = TRUE)
number_of_players <- length(unique(session_by_player[ ,1]))
average_session_length <- mean(session_by_player$secondsPlayed)/60 #in minutes



#####
# 2.PREPARING DATA


#orders rows by player and includes important columns from df + session count and cumulative activity duration
Individual <- session_by_player %>%
  
  arrange(destinyMembershipId, date) %>%
  
  select(destinyMembershipId, date, activitiesEntered, totalActivityDurationSeconds, kills, resurrectionsPerformed, 
         
         resurrectionsReceived, combatRating, killsDeathsRatio, assists, killsDeathsAssists) %>%
  
  group_by(destinyMembershipId) %>%
  
  mutate(SessionCount = 1:length(destinyMembershipId),
         
         CumulativeActivity = cumsum(totalActivityDurationSeconds / 3600), #cumulative activity in hours
         
         CRdiff = combatRating-lag(combatRating, 1), #calculating cumulative combat rating difference
         
         CumulativeCRdiff = cumsum(replace_na(CRdiff, 0))) #replacing NA values with 0



#correlation of revives given and received
Revives_given_vs_received <- cor(Individual$resurrectionsReceived,Individual$resurrectionsPerformed)  

#summary values for each player
overview <- Individual %>%
  
  group_by(destinyMembershipId) %>%
  
  summarise(Kills_per_game = sum(kills)/sum(activitiesEntered),  #kpg
            
            Max_kills_per_game = max(kills / activitiesEntered),  #max kpg for day
            
            Games = sum(activitiesEntered),  #number of games
            
            Sessions = max(SessionCount),  #number of sessions
            
            CR = mean(combatRating),  #mean combat rating
            
            Revives_given = sum(resurrectionsPerformed),
            
            Revives_received = sum(resurrectionsReceived),
            
            Total_revives_pg = sum(resurrectionsPerformed, resurrectionsReceived) / sum(activitiesEntered),
            
            Total_activity_duration = sum(totalActivityDurationSeconds / 3600), #in hours
            
            Log_revives = log1p(log1p(Total_revives_pg)), #Log(x+1) transformation to 'normalise' distribution
            
            logrevives = log(Total_revives_pg), #Log transformed just those with revives
            
            AbsdiffCR = last(combatRating) - first(combatRating), #Last combat rating minus first
            
            linearmodel_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2], #slopes of linear models for each player
            
            linearmodel_intercept = lm(combatRating ~ CumulativeActivity)$coefficients[1], #intercepts of linear models 
            
            Assists_pg = sum(assists) / sum(activitiesEntered), #Assists per game
            
            Initial_score = first(combatRating), #Initial combat rating
            
            log_Initial_score = log(Initial_score), #...log transformed
            
            final_CR = tail(combatRating, 1)
  )


overview <- overview[is.finite(overview$log_Initial_score), ] #removing players who do not have a recorded CR


#calculating z-scores to determine outliers and for regression model
overview <- overview %>%
  
  mutate(slope_z_score = scale(linearmodel_slope, center = TRUE, scale = TRUE),
         Initial_score_z_score = scale(log_Initial_score, center = TRUE, scale = TRUE),
         Log_revives_z_score = scale(Log_revives, center = TRUE, scale = TRUE),
         Assists_z_score = scale(Assists_pg, center = TRUE, scale = TRUE)
  )



#removing outliers (keeping those with a z-score between -3 and 3 and a minimum play time of 2 hours)
overview_no_outliers <- subset(overview, overview$slope_z_score < 3 & overview$slope_z_score > -3 & 
                                 overview$Initial_score_z_score < 3 & overview$Initial_score_z_score > -3 & 
                                 overview$Log_revives_z_score < 3 & overview$Log_revives_z_score > -3 & 
                                 overview$Assists_z_score < 3 & overview$Assists_z_score > -3,
                               Total_activity_duration >= 2)

number_of_players_no_outliers <- length(overview_no_outliers$destinyMembershipId) #new number of players



#including just players with some revives for 
solo_overview <- subset(overview_no_outliers, Log_revives_z_score <= quantile(Log_revives_z_score)[2])
social_overview <- subset(overview_no_outliers, Log_revives_z_score >= quantile(Log_revives_z_score)[4]) 

nonzerorevives <- subset(overview_no_outliers, overview_no_outliers$Total_revives_pg > 0)



#####
# 3.ANALYSIS

#correlation of combined revives (log) vs average combat rating
Revives_vs_CR <- cor(overview_no_outliers$Log_revives,overview_no_outliers$CR)

#correlation of log transformed revives vs combat rating difference
Revives_Learning_correlation <- cor(overview_no_outliers$AbsdiffCR,overview_no_outliers$Log_revives)
Revives_vs_lmslope <- cor(overview_no_outliers$Log_revives, replace_na(overview_no_outliers$linearmodel_slope,0))


#average slopes and intercepts
MeanlmSlope <-  mean(na.omit(overview_no_outliers$linearmodel_slope))
MeanlmIntercept <- mean(na.omit(overview_no_outliers$linearmodel_intercept))


#average slopes and intercepts for the 2 groups
solo_Meanlmslope <- mean(na.omit(solo_overview$linearmodel_slope))
solo_MeanlmIntercept <- mean(na.omit(solo_overview$linearmodel_intercept))
social_Meanlmslope <- mean(na.omit(social_overview$linearmodel_slope))
social_MeanlmIntercept <- mean(na.omit(social_overview$linearmodel_intercept))

 



#tables of game sessions for social and solo players (to be used for plotting learning curves)

Social_players <- subset(Individual, Individual$destinyMembershipId %in% social_overview$destinyMembershipId == TRUE)

Solo_players <- subset(Individual, Individual$destinyMembershipId %in% solo_overview$destinyMembershipId == TRUE)


Individual_no_outliers <- subset(Individual, Individual$destinyMembershipId %in% overview_no_outliers$destinyMembershipId == TRUE)


#colour schemes to visualise groups in graph
Social_players$Group <- rep('Social', length(Social_players$destinyMembershipId)) #social
Social_players$Colour_Code <- rep('forestgreen', length(Social_players$destinyMembershipId))
Solo_players$Group <- rep('Solo', length(Solo_players$destinyMembershipId)) #solo
Solo_players$Colour_Code <- rep('orchid4', length(Solo_players$destinyMembershipId))

Individual_no_outliers <- merge(Solo_players, Social_players, all = TRUE)




#regression analysis: revives, assists and initial score against learning rate


actual_regression <- lm(data = nonzerorevives,
                        formula = linearmodel_slope ~ Initial_score_z_score + Log_revives_z_score +
                          Assists_z_score)



#t-test for difference in linear model slopes and intercepts: zero revives vs nonzero revives

Social_vs_solo_slope <- t.test(y = solo_overview$linearmodel_slope, x = social_overview$linearmodel_slope,
                               alternative = "two.sided", var.equal = FALSE, conf.level = .95)

slope_d_value <- cohen.d(social_overview$linearmodel_slope, solo_overview$linearmodel_slope, 
                         hedges.correction = TRUE)

Social_vs_solo_inter <- t.test(y = solo_overview$linearmodel_intercept, x = social_overview$linearmodel_intercept, 
                               alternative = "two.sided", var.equal = FALSE, conf.level = .95)

intercept_d_value <- cohen.d(social_overview$linearmodel_intercept, solo_overview$linearmodel_intercept, 
                             hedges.correction = TRUE)




#####
# 4.FURTHER ANALYSiS

#Dividing time periods; new dataframe

######solo and social dataframes


#...first 10 hours; general layout for this section is to subset solo/social players in 10 hour periods, work out
#average learning curves, feed co-ordinates into a new dataframe for plotting. A for loop can be used; brute force
#was only used here to make sure everything was correct


TimePoint <- vector(mode = 'numeric', length = 42)
CRPoint <- vector(mode = 'numeric', length = 42)
Groups <- vector(mode = 'character', length = 42)
ColourCode <- vector(mode = 'character', length = 42)
CircleSize <- vector(mode = 'numeric', length = 42)
SEBar <- vector(mode = 'numeric', length = 42)
#initial points
TimePoint[1:21] <- seq(from = 0, to = 200, by = 10)
TimePoint[22:42] <- seq(from = 0, to =  200, by = 10)
Groups[1:21] <- 'Solo'
Groups[22:42] <- 'Social'
ColourCode[1:21] <- 'orchid4'
ColourCode[22:42] <- 'forestgreen'







solo_1_to_10 <- subset(Solo_players, CumulativeActivity < 10)
social_1_to_10 <- subset(Social_players, CumulativeActivity < 10)

solo_1_to_10_over <- solo_1_to_10 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            intercept = lm(combatRating ~ CumulativeActivity)$coefficients[1],
            CR = tail(combatRating, 1),
            firstCR = head(combatRating, 1))

social_1_to_10_over <- social_1_to_10 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            intercept = lm(combatRating ~ CumulativeActivity)$coefficients[1],
            CR = tail(combatRating, 1),
            firstCR = head(combatRating, 1))
CRPoint[1] <- mean(solo_1_to_10_over$firstCR)
CRPoint[22] <- mean(social_1_to_10_over$firstCR)
SEBar[1] <- std.error(solo_1_to_10_over$firstCR)
SEBar[22] <- std.error(social_1_to_10_over$firstCR)

SEBar[2] <- std.error(solo_1_to_10_over$CR)
SEBar[23] <- std.error(social_1_to_10_over$CR)

CRPoint[2] <- mean(solo_1_to_10_over$CR)
CRPoint[23] <- mean(social_1_to_10_over$CR)
CircleSize[1] <- length(solo_1_to_10_over$CR)
CircleSize[2] <- length(solo_1_to_10_over$CR)
CircleSize[22] <- length(social_1_to_10_over$CR)
CircleSize[23] <- length(social_1_to_10_over$CR)


x_solo <- seq(from = 0, to = 190, by = 10)
xend_solo <- seq(from = 10, to = 200, by = 10)
y_solo <- vector(mode = 'numeric', length = 20)
yend_solo <- vector(mode = 'numeric', length = 20)
lm_slope_solo <- vector(mode = 'numeric', length = 20)

x_social <- seq(from = 0, to = 190, by = 10)
xend_social <- seq(from = 10, to = 200, by = 10)
y_social <- vector(mode = 'numeric', length = 20)
yend_social <- vector(mode = 'numeric', length = 20)
lm_slope_social <- vector(mode = 'numeric', length = 20)


y_solo[1] = 66.32475
yend_solo[1] = y_solo[1] + 10*(mean(na.omit(solo_1_to_10_over$lm_slope)))


y_social[1] = 76.68724
yend_social[1] = y_social[1] + 10*mean(na.omit(social_1_to_10_over$lm_slope))




lm_slope_solo[1] <- mean(na.omit(solo_1_to_10_over$lm_slope))
lm_slope_social[1] <- mean(na.omit(social_1_to_10_over$lm_slope))



#10to20
solo_10_to_20 <- subset(Solo_players, CumulativeActivity >= 10 & CumulativeActivity < 20)
social_10_to_20 <- subset(Social_players,  CumulativeActivity >= 10 & CumulativeActivity < 20)

solo_10_to_20_over <- solo_10_to_20 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

social_10_to_20_over <- social_10_to_20 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

lm_slope_solo[2] <- mean(na.omit(solo_10_to_20_over$lm_slope))
lm_slope_social[2] <- mean(na.omit(social_10_to_20_over$lm_slope))

CRPoint[3] <- mean(solo_10_to_20_over$CR)
CRPoint[24] <- mean(social_10_to_20_over$CR)
CircleSize[3] <- length(solo_10_to_20_over$CR)
CircleSize[24] <- length(social_10_to_20_over$CR)

SEBar[3] <- std.error(solo_10_to_20_over$CR)
SEBar[24] <- std.error(social_10_to_20_over$CR)




#20to30
solo_20_to_30 <- subset(Solo_players, CumulativeActivity >= 20 & CumulativeActivity < 30)
social_20_to_30 <- subset(Social_players,  CumulativeActivity >= 20 & CumulativeActivity < 30)

solo_20_to_30_over <- solo_20_to_30 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

social_20_to_30_over <- social_20_to_30 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

lm_slope_solo[3] <- mean(na.omit(solo_20_to_30_over$lm_slope))
lm_slope_social[3] <- mean(na.omit(social_20_to_30_over$lm_slope))

CRPoint[4] <- mean(solo_20_to_30_over$CR)
CRPoint[25] <- mean(social_20_to_30_over$CR)
CircleSize[4] <- length(solo_20_to_30_over$CR)
CircleSize[25] <- length(social_20_to_30_over$CR)

SEBar[4] <- std.error(solo_20_to_30_over$CR)
SEBar[25] <- std.error(social_20_to_30_over$CR)



#30to40
solo_30_to_40 <- subset(Solo_players, CumulativeActivity >= 30 & CumulativeActivity < 40)
social_30_to_40 <- subset(Social_players,  CumulativeActivity >= 30 & CumulativeActivity < 40)

solo_30_to_40_over <- solo_30_to_40 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

social_30_to_40_over <- social_30_to_40 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))


lm_slope_solo[4] <- mean(na.omit(solo_30_to_40_over$lm_slope))
lm_slope_social[4] <- mean(na.omit(social_30_to_40_over$lm_slope))

CRPoint[5] <- mean(solo_30_to_40_over$CR)
CRPoint[26] <- mean(social_30_to_40_over$CR)
CircleSize[5] <- length(solo_30_to_40_over$CR)
CircleSize[26] <- length(social_30_to_40_over$CR)

SEBar[5] <- std.error(solo_30_to_40_over$CR)
SEBar[26] <- std.error(social_30_to_40_over$CR)




#40to50
solo_40_to_50 <- subset(Solo_players, CumulativeActivity >= 40 & CumulativeActivity < 50)
social_40_to_50 <- subset(Social_players,  CumulativeActivity >= 40 & CumulativeActivity < 50)

solo_40_to_50_over <- solo_40_to_50 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

social_40_to_50_over <- social_40_to_50 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

lm_slope_solo[5] <- mean(na.omit(solo_40_to_50_over$lm_slope))
lm_slope_social[5] <- mean(na.omit(social_40_to_50_over$lm_slope))

CRPoint[6] <- mean(solo_40_to_50_over$CR)
CRPoint[27] <- mean(social_40_to_50_over$CR)
CircleSize[6] <- length(solo_40_to_50_over$CR)
CircleSize[27] <- length(social_40_to_50_over$CR)
SEBar[6] <- std.error(solo_40_to_50_over$CR)
SEBar[27] <- std.error(social_40_to_50_over$CR)



#50to60

solo_50_to_60 <- subset(Solo_players, CumulativeActivity >= 50 & CumulativeActivity < 60)
social_50_to_60 <- subset(Social_players,  CumulativeActivity >= 50 & CumulativeActivity < 60)

solo_50_to_60_over <- solo_50_to_60 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

social_50_to_60_over <- social_50_to_60 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

lm_slope_solo[6] <- mean(na.omit(solo_50_to_60_over$lm_slope))
lm_slope_social[6] <- mean(na.omit(social_50_to_60_over$lm_slope))

CRPoint[7] <- mean(solo_50_to_60_over$CR)
CRPoint[28] <- mean(social_50_to_60_over$CR)
CircleSize[7] <- length(solo_50_to_60_over$CR)
CircleSize[28] <- length(social_50_to_60_over$CR)
SEBar[7] <- std.error(solo_50_to_60_over$CR)
SEBar[28] <- std.error(social_50_to_60_over$CR)



#60to70

solo_60_to_70 <- subset(Solo_players, CumulativeActivity >= 60 & CumulativeActivity < 70)
social_60_to_70 <- subset(Social_players,  CumulativeActivity >= 60 & CumulativeActivity < 70)

solo_60_to_70_over <- solo_60_to_70 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

social_60_to_70_over <- social_60_to_70 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

lm_slope_solo[7] <- mean(na.omit(solo_60_to_70_over$lm_slope))
lm_slope_social[7] <- mean(na.omit(social_60_to_70_over$lm_slope))

CRPoint[8] <- mean(solo_60_to_70_over$CR)
CRPoint[29] <- mean(social_60_to_70_over$CR)
CircleSize[8] <- length(solo_60_to_70_over$CR)
CircleSize[29] <- length(social_60_to_70_over$CR)
SEBar[8] <- std.error(solo_60_to_70_over$CR)
SEBar[29] <- std.error(social_60_to_70_over$CR)


#70to80
solo_70_to_80 <- subset(Solo_players, CumulativeActivity >= 70 & CumulativeActivity < 80)
social_70_to_80 <- subset(Social_players,  CumulativeActivity >= 70 & CumulativeActivity < 80)

solo_70_to_80_over <- solo_60_to_70 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

social_70_to_80_over <- social_70_to_80 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

lm_slope_solo[8] <- mean(na.omit(solo_70_to_80_over$lm_slope))
lm_slope_social[8] <- mean(na.omit(social_70_to_80_over$lm_slope))

CRPoint[9] <- mean(solo_70_to_80_over$CR)
CRPoint[30] <- mean(social_70_to_80_over$CR)
CircleSize[9] <- length(solo_70_to_80_over$CR)
CircleSize[30] <- length(social_70_to_80_over$CR)
SEBar[9] <- std.error(solo_70_to_80_over$CR)
SEBar[30] <- std.error(social_70_to_80_over$CR)



#80to90
solo_80_to_90 <- subset(Solo_players, CumulativeActivity >= 80 & CumulativeActivity < 90)
social_80_to_90 <- subset(Social_players,  CumulativeActivity >= 80 & CumulativeActivity < 90)

solo_80_to_90_over <- solo_80_to_90 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

social_80_to_90_over <- social_80_to_90 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

lm_slope_solo[9] <- mean(na.omit(solo_80_to_90_over$lm_slope))

lm_slope_social[9] <- mean(na.omit(social_80_to_90_over$lm_slope))

CRPoint[10] <- mean(solo_80_to_90_over$CR)
CRPoint[31] <- mean(social_80_to_90_over$CR)
CircleSize[10] <- length(solo_80_to_90_over$CR)
CircleSize[31] <- length(social_80_to_90_over$CR)
SEBar[10] <- std.error(solo_80_to_90_over$CR)
SEBar[31] <- std.error(social_80_to_90_over$CR)



#90to100
solo_90_to_100 <- subset(Solo_players, CumulativeActivity >= 90 & CumulativeActivity < 100)
social_90_to_100 <- subset(Social_players,  CumulativeActivity >= 90 & CumulativeActivity < 100)

solo_90_to_100_over <- solo_90_to_100 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

social_90_to_100_over <- social_90_to_100 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))


lm_slope_solo[10] <- mean(na.omit(solo_90_to_100_over$lm_slope))
lm_slope_social[10] <- mean(na.omit(social_90_to_100_over$lm_slope))

CRPoint[11] <- mean(solo_90_to_100_over$CR)
CRPoint[32] <- mean(social_90_to_100_over$CR)
CircleSize[11] <- length(solo_90_to_100_over$CR)
CircleSize[32] <- length(social_90_to_100_over$CR)
SEBar[11] <- std.error(solo_90_to_100_over$CR)
SEBar[32] <- std.error(social_90_to_100_over$CR)


#100to110
solo_100_to_110 <- subset(Solo_players, CumulativeActivity >= 100 & CumulativeActivity < 110)
social_100_to_110 <- subset(Social_players,  CumulativeActivity >= 100 & CumulativeActivity < 110)

solo_100_to_110_over <- solo_100_to_110 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

social_100_to_110_over <- social_100_to_110 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))


lm_slope_solo[11] <- mean(na.omit(solo_100_to_110_over$lm_slope))
lm_slope_social[11] <- mean(na.omit(social_100_to_110_over$lm_slope))

CRPoint[12] <- mean(solo_100_to_110_over$CR)
CRPoint[33] <- mean(social_100_to_110_over$CR)
CircleSize[12] <- length(solo_100_to_110_over$CR)
CircleSize[33] <- length(social_100_to_110_over$CR)
SEBar[12] <- std.error(solo_100_to_110_over$CR)
SEBar[33] <- std.error(social_100_to_110_over$CR)



#110to120
solo_110_to_120 <- subset(Solo_players, CumulativeActivity >= 110 & CumulativeActivity < 120)
social_110_to_120 <- subset(Social_players,  CumulativeActivity >= 110 & CumulativeActivity < 120)

solo_110_to_120_over <- solo_110_to_120 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

social_110_to_120_over <- social_110_to_120 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))


lm_slope_solo[12] <- mean(na.omit(solo_110_to_120_over$lm_slope))
lm_slope_social[12] <- mean(na.omit(social_110_to_120_over$lm_slope))

CRPoint[13] <- mean(solo_110_to_120_over$CR)
CRPoint[34] <- mean(social_110_to_120_over$CR)
CircleSize[13] <- length(solo_110_to_120_over$CR)
CircleSize[34] <- length(social_110_to_120_over$CR)
SEBar[13] <- std.error(solo_110_to_120_over$CR)
SEBar[34] <- std.error(social_110_to_120_over$CR)



#120to130
solo_120_to_130 <- subset(Solo_players, CumulativeActivity >= 120 & CumulativeActivity < 130)
social_120_to_130 <- subset(Social_players,  CumulativeActivity >= 120 & CumulativeActivity < 130)

solo_120_to_130_over <- solo_120_to_130 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

social_120_to_130_over <- social_120_to_130 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))


lm_slope_solo[13] <- mean(na.omit(solo_120_to_130_over$lm_slope))
lm_slope_social[13] <- mean(na.omit(social_120_to_130_over$lm_slope))

CRPoint[14] <- mean(solo_120_to_130_over$CR)
CRPoint[35] <- mean(social_120_to_130_over$CR)
CircleSize[14] <- length(solo_120_to_130_over$CR)
CircleSize[35] <- length(social_120_to_130_over$CR)
SEBar[14] <- std.error(solo_120_to_130_over$CR)
SEBar[35] <- std.error(social_120_to_130_over$CR)



#130to140
solo_130_to_140 <- subset(Solo_players, CumulativeActivity >= 130 & CumulativeActivity < 140)
social_130_to_140 <- subset(Social_players,  CumulativeActivity >= 130 & CumulativeActivity < 140)

solo_130_to_140_over <- solo_130_to_140 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

social_130_to_140_over <- social_130_to_140 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))


lm_slope_solo[14] <- mean(na.omit(solo_130_to_140_over$lm_slope))
lm_slope_social[14] <- mean(na.omit(social_130_to_140_over$lm_slope))

CRPoint[15] <- mean(solo_130_to_140_over$CR)
CRPoint[36] <- mean(social_130_to_140_over$CR)
CircleSize[15] <- length(solo_130_to_140_over$CR)
CircleSize[36] <- length(social_130_to_140_over$CR)
SEBar[15] <- std.error(solo_130_to_140_over$CR)
SEBar[36] <- std.error(social_130_to_140_over$CR)



#140to150
solo_140_to_150 <- subset(Solo_players, CumulativeActivity >= 140 & CumulativeActivity < 150)
social_140_to_150 <- subset(Social_players,  CumulativeActivity >= 140 & CumulativeActivity < 150)

solo_140_to_150_over <- solo_140_to_150 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

social_140_to_150_over <- social_140_to_150 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))


lm_slope_solo[15] <- mean(na.omit(solo_140_to_150_over$lm_slope))
lm_slope_social[15] <- mean(na.omit(social_140_to_150_over$lm_slope))

CRPoint[16] <- mean(solo_140_to_150_over$CR)
CRPoint[37] <- mean(social_140_to_150_over$CR)
CircleSize[16] <- length(solo_140_to_150_over$CR)
CircleSize[37] <- length(social_140_to_150_over$CR)
SEBar[16] <- std.error(solo_140_to_150_over$CR)
SEBar[37] <- std.error(social_140_to_150_over$CR)



#150to160
solo_150_to_160 <- subset(Solo_players, CumulativeActivity >= 150 & CumulativeActivity < 160)
social_150_to_160 <- subset(Social_players,  CumulativeActivity >= 150 & CumulativeActivity < 160)

solo_150_to_160_over <- solo_150_to_160 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

social_150_to_160_over <- social_150_to_160 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))


lm_slope_solo[16] <- mean(na.omit(solo_150_to_160_over$lm_slope))
lm_slope_social[16] <- mean(na.omit(social_150_to_160_over$lm_slope))

CRPoint[17] <- mean(solo_150_to_160_over$CR)
CRPoint[38] <- mean(social_150_to_160_over$CR)
CircleSize[17] <- length(solo_150_to_160_over$CR)
CircleSize[38] <- length(social_150_to_160_over$CR)
SEBar[17] <- std.error(solo_150_to_160_over$CR)
SEBar[38] <- std.error(social_150_to_160_over$CR)



#160to170
solo_160_to_170 <- subset(Solo_players, CumulativeActivity >= 160 & CumulativeActivity < 170)
social_160_to_170 <- subset(Social_players,  CumulativeActivity >= 160 & CumulativeActivity < 170)

solo_160_to_170_over <- solo_160_to_170 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

social_160_to_170_over <- social_160_to_170 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))


lm_slope_solo[17] <- mean(na.omit(solo_160_to_170_over$lm_slope))
lm_slope_social[17] <- mean(na.omit(social_160_to_170_over$lm_slope))

CRPoint[18] <- mean(solo_160_to_170_over$CR)
CRPoint[39] <- mean(social_160_to_170_over$CR)
CircleSize[18] <- length(solo_160_to_170_over$CR)
CircleSize[39] <- length(social_160_to_170_over$CR)
SEBar[18] <- std.error(solo_160_to_170_over$CR)
SEBar[39] <- std.error(social_160_to_170_over$CR)




#170to180
solo_170_to_180 <- subset(Solo_players, CumulativeActivity >= 170 & CumulativeActivity < 180)
social_170_to_180 <- subset(Social_players,  CumulativeActivity >= 170 & CumulativeActivity < 180)

solo_170_to_180_over <- solo_170_to_180 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

social_170_to_180_over <- social_170_to_180 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))


lm_slope_solo[18] <- mean(na.omit(solo_170_to_180_over$lm_slope))
lm_slope_social[18] <- mean(na.omit(social_170_to_180_over$lm_slope))

CRPoint[19] <- mean(solo_170_to_180_over$CR)
CRPoint[40] <- mean(social_170_to_180_over$CR)
CircleSize[19] <- length(solo_170_to_180_over$CR)
CircleSize[40] <- length(social_170_to_180_over$CR)
SEBar[19] <- std.error(solo_170_to_180_over$CR)
SEBar[40] <- std.error(social_170_to_180_over$CR)



#180to190
solo_180_to_190 <- subset(Solo_players, CumulativeActivity >= 180 & CumulativeActivity < 190)
social_180_to_190 <- subset(Social_players,  CumulativeActivity >= 180 & CumulativeActivity < 190)

solo_180_to_190_over <- solo_180_to_190 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

social_180_to_190_over <- social_180_to_190 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))


lm_slope_solo[19] <- mean(na.omit(solo_180_to_190_over$lm_slope))
lm_slope_social[19] <- mean(na.omit(social_180_to_190_over$lm_slope))

CRPoint[20] <- mean(solo_180_to_190_over$CR)
CRPoint[41] <- mean(social_180_to_190_over$CR)
CircleSize[20] <- length(solo_180_to_190_over$CR)
CircleSize[41] <- length(social_180_to_190_over$CR)
SEBar[20] <- std.error(solo_180_to_190_over$CR)
SEBar[41] <- std.error(social_180_to_190_over$CR)




#190to200
solo_190_to_200 <- subset(Solo_players, CumulativeActivity >= 190 & CumulativeActivity < 200)
social_190_to_200 <- subset(Social_players,  CumulativeActivity >= 190 & CumulativeActivity < 200)

solo_190_to_200_over <- solo_190_to_200 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))

social_190_to_200_over <- social_190_to_200 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            CR = tail(combatRating, 1))


lm_slope_solo[20] <- mean(na.omit(solo_190_to_200_over$lm_slope))
lm_slope_social[20] <- mean(na.omit(social_190_to_200_over$lm_slope))

CRPoint[21] <- mean(solo_190_to_200_over$CR)
CRPoint[42] <- mean(social_190_to_200_over$CR)
CircleSize[21] <- length(solo_190_to_200_over$CR)
CircleSize[42] <- length(social_190_to_200_over$CR)
SEBar[21] <- std.error(solo_190_to_200_over$CR)
SEBar[42] <- std.error(social_190_to_200_over$CR)


#combining vectors to make 2 new dataframes
sololine <- data.frame(x_solo, xend_solo, y_solo, yend_solo, lm_slope_solo)
socialline <- data.frame(x_social, xend_social, y_social, yend_social, lm_slope_social)
averagepoints <- data.frame(TimePoint, CRPoint, Groups, ColourCode, CircleSize,SEBar)




#working out y co-ordinates 
for(i in seq(from = 2, to = length(sololine$x_solo), by = 1)){
  y_solo[i] = y_solo[i-1] + 10*lm_slope_solo[i-1]
  
}

for(i in seq(from = 2, to = length(sololine$x_solo), by = 1)){
  yend_solo[i] = y_solo[i] + 10*lm_slope_solo[i]
}

for(i in seq(from = 2, to = length(sololine$x_solo), by = 1)){
  y_social[i] = y_social[i-1] + 10*lm_slope_social[i-1]
  
}

for(i in seq(from = 2, to = length(sololine$x_solo), by = 1)){
  yend_social[i] = y_social[i] + 10*lm_slope_social[i]
}





Individual_no_outliers2 <- subset(Individual_no_outliers, CumulativeActivity <= 200)

overview2 <- Individual_no_outliers2 %>%
  
  group_by(destinyMembershipId) %>%
  
  summarise(Kills_per_game = sum(kills)/sum(activitiesEntered),  #kpg
            
            Max_kills_per_game = max(kills / activitiesEntered),  #max kpg for day
            
            Games = sum(activitiesEntered),  #number of games
            
            Sessions = max(SessionCount),  #number of sessions
            
            CR = mean(combatRating),  #mean combat rating
            
            Revives_given = sum(resurrectionsPerformed),
            
            Revives_received = sum(resurrectionsReceived),
            
            Total_revives_pg = sum(resurrectionsPerformed, resurrectionsReceived) / sum(activitiesEntered),
            
            Total_activity_duration = sum(totalActivityDurationSeconds / 3600), #in hours
            
            Log_revives = log1p(log1p(Total_revives_pg)), #Log(x+1) transformation to 'normalise' distribution
            
            logrevives = log(Total_revives_pg), #Log transformed just those with revives
            
            AbsdiffCR = last(combatRating) - first(combatRating), #Last combat rating minus first
            
            linearmodel_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2], #slopes of linear models for each player
            
            linearmodel_intercept = lm(combatRating ~ CumulativeActivity)$coefficients[1], #intercepts of linear models 
            
            Assists_pg = sum(assists) / sum(activitiesEntered), #Assists per game
            
            Initial_score = first(combatRating), #Initial combat rating
            
            log_Initial_score = log(Initial_score), #...log transformed
            
            final_CR = tail(combatRating, 1)
  )


overview2 <- overview2[is.finite(overview2$log_Initial_score), ] #removing players who do not have a recorded CR
overview2 <- subset(overview2, Total_activity_duration >= 50)

#calculating z-scores to determine outliers and for regression model
overview2 <- overview2 %>%
  
  mutate(slope_z_score = scale(linearmodel_slope, center = TRUE, scale = TRUE),
         Initial_score_z_score = scale(log_Initial_score, center = TRUE, scale = TRUE),
         Log_revives_z_score = scale(Log_revives, center = TRUE, scale = TRUE),
         Assists_z_score = scale(Assists_pg, center = TRUE, scale = TRUE)
  )

new_regression <- lm(data = overview2, formula = linearmodel_slope ~ Initial_score_z_score + Assists_z_score +
                       Log_revives_z_score)


solo_new <- subset(overview2, Log_revives_z_score <= quantile(Log_revives_z_score)[2])
social_new <- subset(overview2, Log_revives_z_score >= quantile(Log_revives_z_score)[4])



Social_playersn <- subset(Individual_no_outliers, Individual$destinyMembershipId %in% social_new$destinyMembershipId == TRUE)
Solo_playersn <- subset(Individual_no_outliers, Individual$destinyMembershipId %in% solo_new$destinyMembershipId == TRUE)
solo200 <- subset(Solo_players, CumulativeActivity < 200)
social200 <- subset(Social_players, CumulativeActivity < 200)
solo200over <- solo200 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            lm_intercept = lm(combatRating ~ CumulativeActivity)$coefficients[1],
            Total_revives_pg = sum(resurrectionsPerformed, resurrectionsReceived) / sum(activitiesEntered),
            Log_revives = log1p(log1p(Total_revives_pg)),
            Assists_pg = sum(assists) / sum(activitiesEntered),
            Initial_score = first(combatRating))
social200over <- social200 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            lm_intercept = lm(combatRating ~ CumulativeActivity)$coefficients[1],
            Total_revives_pg = sum(resurrectionsPerformed, resurrectionsReceived) / sum(activitiesEntered),
            Log_revives = log1p(log1p(Total_revives_pg)),
            Assists_pg = sum(assists) / sum(activitiesEntered),
            Initial_score = first(combatRating))

indiv_200 <- subset(Individual_no_outliers, CumulativeActivity <= 200)
overview_200 <- indiv_200 %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            lm_intercept = lm(combatRating ~ CumulativeActivity)$coefficients[1],
            Total_revives_pg = sum(resurrectionsPerformed, resurrectionsReceived) / sum(activitiesEntered),
            Log_revives = log1p(log1p(Total_revives_pg)),
            Assists_pg = sum(assists) / sum(activitiesEntered),
            Initial_score = first(combatRating),
            log_Initial_score = log(Initial_score),
            TotalTime = sum(totalActivityDurationSeconds / 3600),
            TotalGames = sum(activitiesEntered),
            AverageCR = mean(combatRating))
overview_200_new <- subset(overview_200, TotalTime >= 50)
overview_200_new <- overview_200_new %>%
  mutate(slope_z_score = scale(lm_slope, center = TRUE, scale = TRUE),
         Initial_score_z_score = scale(log_Initial_score, center = TRUE, scale = TRUE),
         Log_revives_z_score = scale(Log_revives, center = TRUE, scale = TRUE),
         Assists_z_score = scale(Assists_pg, center = TRUE, scale = TRUE)
  )
regression_model_200 <- lm(data = overview_200_new,
                           formula = lm_slope ~ Initial_score_z_score + Log_revives_z_score + Assists_z_score)

overview_full <- Individual_no_outliers %>%
  group_by(destinyMembershipId) %>%
  summarise(lm_slope = lm(combatRating ~ CumulativeActivity)$coefficients[2],
            lm_intercept = lm(combatRating ~ CumulativeActivity)$coefficients[1],
            Total_revives_pg = sum(resurrectionsPerformed, resurrectionsReceived) / sum(activitiesEntered),
            Log_revives = log1p(log1p(Total_revives_pg)),
            Assists_pg = sum(assists) / sum(activitiesEntered),
            Initial_score = first(combatRating),
            log_Initial_score = log(Initial_score),
            TotalTime = sum(totalActivityDurationSeconds / 3600),
            TotalGames = sum(activitiesEntered),
            AverageCR = mean(combatRating))
overview_full <- subset(overview_full, TotalTime >= 50)
overview_full <- overview_full %>%
  mutate(slope_z_score = scale(lm_slope, center = TRUE, scale = TRUE),
         Initial_score_z_score = scale(log_Initial_score, center = TRUE, scale = TRUE),
         Log_revives_z_score = scale(Log_revives, center = TRUE, scale = TRUE),
         Assists_z_score = scale(Assists_pg, center = TRUE, scale = TRUE)
  )

overview_non_zero <- subset(overview_full, Total_revives_pg > 0)


regression_model_full <- lm(data = overview_non_zero,
                            formula = lm_slope ~ Initial_score_z_score + Log_revives_z_score + Assists_z_score)


solo_new <- subset(overview_200_new, Total_revives_pg < 0.5)
social_new <- subset(overview_200_new, Total_revives_pg >= 0.5)

solo_full <- subset(overview_full, Total_revives_pg < 0.5)
social_full <- subset(overview_full, Total_revives_pg >= 0.5)

#####
#5.PLOTS


# a) Distributions

#number of games
games_dist <- ggplot(data = overview, mapping = aes(x = Games)) + 
  geom_histogram(binwidth = 100) 

#pre-log distributions: Initial score, assists, revives

Initial_score_dist <- ggplot(data = overview, mapping = aes(x = Initial_score)) +
  geom_histogram(fill = 'blue2', colour = 'black', binwidth = 10) +
  labs(x = 'Initial Score (CR)', y = 'Frequency')

assists_dist <- ggplot(data = overview, mapping = aes(x = Assists_pg)) +
  geom_histogram(fill = 'red2', colour = 'black', binwidth = 0.2) +
  labs(x = 'Assists per Game', y = 'Frequency')

revives_dist <- ggplot(data = overview, mapping = aes(x = Total_revives_pg)) +
  geom_histogram(fill = 'forestgreen', colour = 'black', binwidth = 0.2) + 
  labs(x = 'Revives per Game', y = 'Frequency')

Predictor_Distributions <- ggarrange(Initial_score_dist, assists_dist, revives_dist, labels = c('A','B','C'),
                                     nrow = 1, ncol = 3) 
Predictor_Distributions <- annotate_figure(Predictor_Distributions, top = text_grob('Predictor Distributions', 
                                                                                    size = 12, face = 'bold'))


#post-log distributions: logInitial score, assists, logrevives

log_Initial_score_dist <- ggplot(data = overview, mapping = aes(x = log_Initial_score)) +
  geom_histogram(fill = 'blue2', colour = 'black', binwidth = 0.1) +
  labs(x = 'Log-transformed Initial Score (CR)', y = 'Frequency')


log_revives_dist <- ggplot(data = overview, mapping = aes(x = Log_revives)) +
  geom_histogram(fill = 'forestgreen', colour = 'black', binwidth = 0.02) + 
  labs(x = 'Log-transformed revives', y = 'Frequency')

Predictor_Distributions_log <- ggarrange(log_Initial_score_dist, assists_dist, log_revives_dist, 
                                         labels = c('A','B','C'),
                                         nrow = 1, ncol = 3) 
Predictor_Distributions_log <- annotate_figure(Predictor_Distributions_log, 
                                               top = text_grob('Predictor Distributions (post-transformation)', 
                                                               size = 12, face = 'bold'))


#predictor z-scores, post outlier exclusion

I_S_z_score_dist <- ggplot(data = overview_no_outliers, mapping = aes(x = Initial_score_z_score)) +
  geom_histogram(fill = 'blue2', colour = 'black', binwidth = 0.1) +
  labs(x = 'Initial score (z)', y = 'Frequency')


Ass_z_score_dist <- ggplot(data = overview_no_outliers, mapping = aes(x = Assists_z_score)) +
  geom_histogram(fill = 'red2', colour = 'black', binwidth = 0.1) +
  labs(x = 'Assists (z)', y = 'Frequency')


R_z_score_dist <- ggplot(data = overview_no_outliers, mapping = aes(x = Log_revives_z_score)) +
  geom_histogram(fill = 'forestgreen', colour = 'black', binwidth = 0.1) +
  labs(x = 'Revives (z)', y = 'Frequency')



z_score_distributions <- ggarrange(I_S_z_score_dist, Ass_z_score_dist, R_z_score_dist, 
                                   labels = c('A','B','C'),
                                   nrow = 1, ncol = 3) 
z_score_distributions <- annotate_figure(z_score_distributions, 
                                         top = text_grob('z-score Distributions (after outlier exclusion)', 
                                                         size = 12, face = 'bold'))

#all distribution figures combined

combined_predictor_distributions <- ggarrange(Initial_score_dist, assists_dist, revives_dist,
                                              log_Initial_score_dist, assists_dist, log_revives_dist,
                                              I_S_z_score_dist, Ass_z_score_dist, R_z_score_dist,
                                              labels = c('a(i)', 'a(ii)', 'a(iii)', 'b(i)', 'b(ii)', 'b(iii)',
                                                         'c(i)', 'c(ii)', 'c(iii)'),
                                              nrow = 3, ncol = 3)

combined_predictor_distributions <- annotate_figure(combined_predictor_distributions,
                                                    top = text_grob('Predictor Distributions',
                                                                    size = 12, face = 'bold'))


#revives correlation
Revs_given_vs_received_cor <- ggplot(data = overview, mapping = aes(x = Revives_given, y = Revives_received)) +
  geom_point(colour = 'forestgreen') + 
  geom_smooth(method = lm, se = FALSE, formula = 'y ~ x', colour = 'black') + 
  labs(x = 'Revives Performed', y = 'Revives Received') + 
  annotate('text', x = 400, y = 5500, label = 'r = .89', fontface = 'italic')

Revs_given_vs_received_cor <- annotate_figure(Revs_given_vs_received_cor,
                                              top = text_grob('Revives Performed and Received',
                                                              size = 12, face = 'bold'))

#revives against average CR
Revives_vs_average_CR <- ggplot(data = overview_no_outliers, mapping = aes(x = Log_revives, y = CR)) +
  geom_point(colour = 'forestgreen', alpha = 0.4) +
  geom_smooth(method = lm, se = FALSE, formula = 'y ~ x', colour = 'black') +
  scale_x_continuous(name = 'Log-transformed Revives', limits = c(0, 1)) +
  scale_y_continuous(name = 'Combat Rating', limits = c(0, 225)) +
  stat_density_2d(aes(fill = ..level..), binwidth = 0.01, geom = 'polygon', alpha = 0.4, contour = TRUE) +
  theme_minimal() +
  annotate('text', x = 0.1, y = 200, label = 'r = .38', fontface = 'italic')


#...just for those with revives
Revives_vs_average_CR_nonzero <- ggplot(data = nonzerorevives, mapping = aes(x = Log_revives, y = CR)) +
  geom_point(colour = 'forestgreen', alpha = 0.4) +
  geom_smooth(method = lm, se = FALSE, formula = 'y ~ x', colour = 'black') +
  scale_x_continuous(name = 'Log-transformed Revives', limits = c(0, 1)) +
  scale_y_continuous(name = 'Combat Rating', limits = c(0, 225)) +
  stat_density_2d(aes(fill = ..level..), binwidth = 0.01, geom = 'polygon', alpha = 0.4, contour = TRUE) +
  theme_minimal() +
  annotate('text', x = 0.1, y = 200, label = 'r = .34', fontface = 'italic')


Revives_vs_aveCR_comb <- ggarrange(Revives_vs_average_CR, Revives_vs_average_CR_nonzero, 
                                   labels = c('a', 'b'),
                                   nrow = 1, ncol = 2)
Revives_vs_aveCR_comb <- annotate_figure(Revives_vs_aveCR_comb,
                                         top = text_grob('Revives against mean Combat Rating',
                                                         size = 12, face = 'bold'))


#example of an individual player's learning curve
Example_learning_curve <- ggplot(data = Individual[681:769, ], mapping = aes(x = CumulativeActivity, y = combatRating)) + 
  geom_point() + geom_smooth(method = loess, formula = 'y ~ x', se = TRUE) +
  theme_minimal() +
  labs(x = 'Game Time (hours)', y = 'Combat Rating') +
  annotate('text', x = 20, y = 400, label = 'Absolute Difference = 36.71') +
  annotate('text', x = 20, y = 370, label = 'Learning Rate = 0.38')

Example_learning_curve <- annotate_figure(Example_learning_curve,
                                          top = text_grob('Individual Learning Curve',
                                                          size = 12, face = 'bold'))



#distribution of linear model slopes after outliers removed (those with a z-score of >3 or <-3 were removed)
lm_slope_distribution <- ggplot(data = overview_no_outliers, mapping = aes(x = linearmodel_slope)) + 
  geom_histogram(binwidth = 1) +
  theme_minimal() +
  labs(x = 'Learning Rate', y = 'Frequency (number of players)')

lm_slope_distribution <- annotate_figure(lm_slope_distribution,
                                         top = text_grob('Linear Model Slope Distribution',
                                                         size = 12, face = 'bold'))



#log-revives against learning rate

Revives_vs_learning_rate <- ggplot(data = overview_no_outliers, 
                                   mapping = aes(x = Log_revives, y = linearmodel_slope)) +
  geom_point(colour = 'forestgreen', alpha = 0.5) +
  geom_smooth(method = lm, formula = 'y ~ x', se = TRUE, colour = 'black') +
  theme_minimal() +
  annotate('text', x = 0.6, y = 105, label = 'r = -.02', fontface = 'italic') +
  labs(x = 'Revives (log-transformed)', y = 'Learning Rate')


#log-revives against absolute CR difference

Revives_vs_Abs_learning <- ggplot(data = overview_no_outliers, mapping = aes(x = Log_revives, y = AbsdiffCR)) +
  geom_point(colour = 'forestgreen', alpha = 0.5) +
  geom_smooth(method = lm, formula = 'y ~ x', se = TRUE, colour = 'black') +
  theme_minimal() +
  labs(x = 'Revives (log-transformed)', y = 'Absolute Combat Rating Difference') +
  annotate('text', x = 0.8, y = 620, label = 'r = .09', fontface = 'italic')

Revives_vs_learning_combined <- ggarrange(Revives_vs_Abs_learning, Revives_vs_learning_rate,
                                          labels = c('a', 'b'),
                                          nrow = 1, ncol = 2)
Revives_vs_learning_combined <- annotate_figure(Revives_vs_learning_combined,
                                                top = text_grob('Revives and Learning',
                                                                size = 12, face = 'bold'))


#linear model slope z-score distribution, post outlier exclusion

lm_z_score_dist <- ggplot(data = overview_no_outliers, mapping = aes(x = slope_z_score)) +
  geom_histogram(binwidth = 0.05) +
  labs(x = 'Learning Rate z-scores', y = 'Frequency (number of players)')


#total average learning curve

average_learning <- ggplot(data = Individual_no_outliers, mapping = aes(x = CumulativeActivity, y = combatRating)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 0.9324377, intercept = 74.77562, colour = 'blue2', size = 1) +
  theme_minimal() +
  labs(x = 'Cumulative Activity (hours)', y = 'Combat Rating') +
  annotate('text', x = 200, y = 2000, label = 'Slope = .93') +
  annotate('text', x = 200, y = 1850, label = 'Intercept = 74.78')

average_learning <- annotate_figure(average_learning,
                                    top = text_grob('Average Linear Learning curve',
                                                    size = 12, face = 'bold'))


#solo and social learning curves (purple = solo, green = social)

social_vs_solo_curves <- ggplot(data = Individual_no_outliers, mapping = aes(x = CumulativeActivity, 
                                                                             y = combatRating, colour = Colour_Code)) +
  geom_point(alpha = 0.2) +
  scale_colour_identity('Group', breaks = Individual_no_outliers$Colour_Code, 
                        labels = Individual_no_outliers$Group, guide = 'legend') +
  labs(x = 'Cumulative Activity (hours)', y = 'Combat Rating') +
  theme_minimal() +
  geom_abline(slope = 1.10136516030829, intercept = 70.1370942702567, colour = 'orchid4', size = 1) + #solo line
  geom_abline(slope = 0.63423554554028, intercept = 82.9638731501613, colour = 'forestgreen', size = 1) + #social line
  annotate('text', x = 100, y = 2000, label = 'Solo', fontface = 'bold') +
  annotate('text', x = 400, y = 2000, label = 'Social', fontface = 'bold') +
  annotate('text', x = 100, y = 1850, label = 'Slope = 1.10') +
  annotate('text', x = 100, y = 1750, label = 'Intercept = 70.14') +
  annotate('text', x = 400, y = 1850, label = 'Slope = 0.63') +
  annotate('text', x = 400, y = 1750, label = 'Intercept = 82.96') +
  guides(colour = guide_legend(override.aes = list(size=10)))

social_vs_solo_curves <- annotate_figure(social_vs_solo_curves,
                                         top = text_grob('Social vs Solo Players',
                                                         size = 12, face = 'bold'))



#restricted view of the above

restricted_slopes <- social_vs_solo_curves + 
  coord_cartesian(xlim = c(-10, 200), ylim = c(0, 300), expand = FALSE)


#time-divided linear models plot 0 to 100 hours solo and social
sololineplot <-  geom_segment(x = sololine$x_solo[1:length(sololine$x_solo)],
                              y = sololine$y_solo[1:length(sololine$x_solo)],
                              xend = sololine$xend_solo[1:length(sololine$x_solo)],
                              yend = sololine$yend_solo[1:length(sololine$x_solo)],
                              aes(colour = 'Solo'),
                              size = 1)
sociallineplot <- geom_segment(x = socialline$x_social[1:length(socialline$x_social)],
                               y = socialline$y_social[1:length(socialline$x_social)],
                               xend = socialline$xend_social[1:length(socialline$x_social)],
                               yend = socialline$yend_social[1:length(socialline$x_social)],
                               aes(colour = 'Social'),
                               size = 1)

solo_vs_social_first_200_hours <- ggplot(data = sololine, mapping = aes(x = xend_solo, y = yend_solo)) + 
  geom_blank() +
  xlim(0, length(sololine$x_solo)*10) +
  ylim(0, 200) +
  sololineplot +
  sociallineplot +
  theme_minimal() +
  labs(x = 'Cumulative Activity (hours)', y = 'Combat Rating') + 
  scale_colour_manual(name = 'Group', values = c(Solo = 'orchid4', Social = 'forestgreen')) +
  guides(colour = guide_legend(override.aes = list(size = 10)))

solo_vs_social_first_200_hours <- annotate_figure(solo_vs_social_first_200_hours,
                                                  top = text_grob('Solo vs Social Learning (10 hour intervals)',
                                                                  size = 12, face = 'bold'))

AverageCRPlot <- ggplot(data = averagepoints, mapping = aes(x = TimePoint, y = CRPoint, size = CircleSize,
                                                            colour = ColourCode)) +
  geom_point(alpha = 0.7, fill = ColourCode) +
  scale_size(range = c(.1, 24), name = 'Number of Players') +
  scale_colour_identity('ColourCode', labels = c('Social', 'Solo'), guide = 'legend') +
  geom_abline(slope = 1.10136516030829, intercept = 70.1370942702567, colour = 'orchid4', size = 1) +
  geom_abline(slope = 0.63423554554028, intercept = 82.9638731501613, colour = 'forestgreen', size = 1) +
  labs(x = 'Time (hours)', y = 'Mean Combat Rating') +
  guides(colour = guide_legend(override.aes = list(size=10)))
  

AverageCRPlotError <- ggplot(data = averagepoints, mapping = aes(x = TimePoint, y = CRPoint,
                                                            colour = ColourCode)) +
  geom_point(fill = ColourCode) +
  geom_errorbar(aes(ymin = CRPoint - SEBar, ymax = CRPoint + SEBar)) +
  scale_size(range = c(.1, 24), name = 'Number of Players') +
  scale_colour_identity('ColourCode', labels = c('Social', 'Solo'), guide = 'legend') +
  geom_abline(slope = 1.10136516030829, intercept = 70.1370942702567, colour = 'orchid4', size = 1) +
  geom_abline(slope = 0.63423554554028, intercept = 82.9638731501613, colour = 'forestgreen', size = 1) +
  guides(colour = guide_legend(override.aes = list(size=10)))
  

no_regline_Error_plot<-ggplot(data = averagepoints, mapping = aes(x = TimePoint, y = CRPoint,
                                           colour = ColourCode)) +
  geom_point(size = 2, fill = ColourCode) +
  geom_errorbar(aes(ymin = CRPoint - SEBar, ymax = CRPoint + SEBar)) +
  scale_size(range = c(.1, 24), name = 'Number of Players') +
  scale_colour_identity('ColourCode', labels = c('Social', 'Solo'), guide = 'legend') +
  geom_line() +
  labs(x = 'Time (hours)', y = 'Mean Combat Rating (SE bars)')
 


Regline_error_plot<-ggplot(data = averagepoints, mapping = aes(x = TimePoint, y = CRPoint,
                                           colour = ColourCode)) +
  geom_point(size = 2, fill = ColourCode) +
  scale_size(range = c(.1, 24), name = 'Number of Players') +
  scale_colour_identity('ColourCode', labels = c('Social', 'Solo'), guide = 'legend') + 
  geom_line() +
  geom_abline(slope = 1.10136516030829, intercept = 70.1370942702567, colour = 'orchid4', size = 1.5) +
  geom_abline(slope = 0.63423554554028, intercept = 82.9638731501613, colour = 'forestgreen', size = 1.5) +
  labs(x = 'Time (hours)', y = 'Mean Combat Rating')
