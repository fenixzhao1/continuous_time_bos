##### Individual analysis for each session #####
# generate random payment round
round(runif(1,4,15), digits = 0)

# load packages
library(ggplot2) # overwrite heatmap in latticeExtra
library(dplyr)

# load data
full_data = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_testing_10_5.csv", header = T)
full_data$round = as.double(substring(full_data$subsession_id, 2, 3))
#full_data$round = full_data$subsession_id
full_data = arrange(full_data, full_data$session_code, full_data$subsession_id, full_data$id_in_subsession, full_data$tick)

# create pair id
full_data$pair_id = paste(full_data$p1_code, full_data$p2_code, sep = "_")
full_data$round_pair_id = paste(full_data$round, full_data$pair_id,  sep = "_")
full_data$session_round_pair_id = paste(full_data$session_code, full_data$round_pair_id, sep = "_")

# drop first 6 seconds and first 3 games
#full_data = filter(full_data, tick > 3)
full_data = filter(full_data, round > 3)

# create unique ids and pairs
uniquepairs = unique(full_data$session_round_pair_id)
uniqueplayer = union(unique(full_data$p1_code), unique(full_data$p2_code))

# pair level dynamics
full_data = full_data %>% mutate(p2_strategy_jitter = p2_strategy + 0.01)

# loop over pairs
for (i in 1:length(uniquepairs)){
  pairdata = subset(full_data, session_round_pair_id == uniquepairs[i])
  
  title = paste(as.character(uniquepairs[i]))
  file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/data/figures_pair/10_5/", title, sep = "")
  file = paste(file, ".png", sep = "")
  
  png(file, width = 700, height = 400)
  par(mai=c(1.5, 1, 0.5, 0.5))
  xy=par("usr")
  plot(pairdata$tick, pairdata$p1_strategy, type='l', col="blue",
       xlab="time", ylab="strategy mixture", ylim=c(0:1.1), main=title)
  lines(pairdata$tick, pairdata$p2_strategy_jitter, type='l', col="red")
  legend(x=xy[2]-xinch(0.2), y=xy[3]-yinch(0.8), lty=1,
         c("Row player strategy","Column player strategy"), col=c("blue", "red"), xpd=TRUE)
  
  dev.off()
}


##### Power analysis #####
library(pwr)
pwr.t.test(n = NULL, d = 0.5, sig.level = 0.05, power = 0.95, type = "two.sample")


##### Data preparation #####
# load packages
library(ggplot2)
library(dplyr)
library(haven)
library(xtable)
library(dtw)

# load data
df_1 = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_testing_8_26.csv", header = T)
df_1 = df_1 %>% mutate(sequence = 1)
df_1$round = as.double(substring(df_1$subsession_id, 3, 4))
df_2 = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_testing_8_27.csv", header = T)
df_2 = df_2 %>% mutate(sequence = 1)
df_2$round = df_2$subsession_id
df_3 = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_testing_9_4.csv", header = T)
df_3 = df_3 %>% mutate(sequence = 1)
df_3$round = as.double(substring(df_3$subsession_id, 2, 3))
df_4 = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_testing_9_17_1.csv", header = T)
df_4 = df_4 %>% mutate(sequence = 2)
df_4$round = as.double(substring(df_4$subsession_id, 2, 3))
df_5 = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_testing_9_17_2.csv", header = T)
df_5 = df_5 %>% mutate(sequence = 2)
df_5$round = as.double(substring(df_5$subsession_id, 2, 3))
df_6 = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_testing_10_2_1.csv", header = T)
df_6 = df_6 %>% mutate(sequence = 2)
df_6$round = df_6$subsession_id
df_7 = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_testing_10_2_2.csv", header = T)
df_7 = df_7 %>% mutate(sequence = 1)
df_7$round = as.double(substring(df_7$subsession_id, 2, 3))
df_8 = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_testing_10_5.csv", header = T)
df_8 = df_8 %>% mutate(sequence = 2)
df_8$round = as.double(substring(df_8$subsession_id, 2, 3))

# combine all session data
df = rbind(df_1, df_2, df_3, df_4, df_5, df_6, df_7, df_8)
df = arrange(df, df$session_code, df$subsession_id, df$id_in_subsession, df$tick)
rm(df_1, df_2, df_3, df_4, df_5, df_6, df_7, df_8)

# create pair id
df$pair_id = paste(df$p1_code, df$p2_code, sep = "_")
df$round_pair_id = paste(df$round, df$pair_id,  sep = "_")
df$session_round_pair_id = paste(df$session_code, df$round_pair_id, sep = "_")

# drop first 1 second and first 3 games
df = filter(df, tick > 3 | num_subperiods != 0)
df = filter(df, round > 3)

# add treatment variables
df = df %>% mutate(game = ifelse(payoff1Bb == 280, "BoS1.4", ifelse(payoff1Bb == 160, "BoS2.5", "BoS10")))
df = df %>% mutate(time = ifelse(num_subperiods==0, 'Continuous', 'Discrete'))
df$treatment = paste(df$time, df$game, sep = '_')
df = df %>% mutate(BoS1.4 = ifelse(payoff1Bb == 280,1,0))
df = df %>% mutate(BoS2.5 = ifelse(payoff1Bb == 160,1,0))
df = df %>% mutate(BoS10 = ifelse(payoff1Bb == 40,1,0))
df = df %>% mutate(continuous = ifelse(num_subperiods==0, 1, 0))

# add payoff variables and NE variables
df = df %>% mutate(p1_payoff = payoff1Aa*p1_strategy*p2_strategy + payoff1Ab*p1_strategy*(1-p2_strategy) + payoff1Ba*(1-p1_strategy)*p2_strategy + payoff1Bb*(1-p1_strategy)*(1-p2_strategy))
df = df %>% mutate(p2_payoff = payoff2Aa*p1_strategy*p2_strategy + payoff2Ab*p1_strategy*(1-p2_strategy) + payoff2Ba*(1-p1_strategy)*p2_strategy + payoff2Bb*(1-p1_strategy)*(1-p2_strategy))
df = df %>% mutate(p1NEmix = ifelse(game=="BoS1.4", 0.59, ifelse(game=="BoS2.5", 0.71, 0.91)))  
df = df %>% mutate(p2NEmix = ifelse(game=="BoS1.4", 0.41, ifelse(game=="BoS2.5", 0.29, 0.09)))
df = df %>% mutate(p1NEdiff = (p1_strategy - p1NEmix)^2)
df = df %>% mutate(p2NEdiff = (p2_strategy - p2NEmix)^2)

# add behavior type and learning dummies
df = df %>% mutate(block = ifelse(round <= 9, 1, 2))
df = df %>% mutate(type = NA)
df = df %>% mutate(period = tick+1)
for(m in 1:length(df$tick)){
  if(df$p1_strategy[m]==1 & df$p2_strategy[m]==1){df$type[m]="Nash(A,a)"}
  if(df$p1_strategy[m]==0 & df$p2_strategy[m]==0){df$type[m]="Nash(B,b)"}
  if(df$p1_strategy[m]==0 & df$p2_strategy[m]==1){df$type[m]="Accommodate"}
  if(df$p1_strategy[m]==1 & df$p2_strategy[m]==0){df$type[m]="Aggressive"}
  if(df$num_subperiods[m]==0){df$period[m] = round((df$tick[m]+1)/12,digits=2)}
}
df = df %>% mutate(Nash_Aa = ifelse(type=='Nash(A,a)', 1, 0))
df = df %>% mutate(Nash_Bb = ifelse(type=='Nash(B,b)', 1, 0))
df = df %>% mutate(Mismatch_accommodate = ifelse(type=='Accommodate', 1, 0))
df = df %>% mutate(Mismatch_aggressive = ifelse(type=='Aggressive', 1, 0))
df = df %>% mutate(coordinate = Nash_Aa + Nash_Bb)
df = df %>% mutate(mismatch = Mismatch_accommodate + Mismatch_aggressive)

# numericalize the behavioral type
df = df %>% mutate(type_num = NA)
for(m in 1:length(df$tick)){
  if(df$p1_strategy[m]==1 & df$p2_strategy[m]==1){df$type_num[m]=2}
  if(df$p1_strategy[m]==0 & df$p2_strategy[m]==0){df$type_num[m]=3}
  if(df$p1_strategy[m]==0 & df$p2_strategy[m]==1){df$type_num[m]=4}
  if(df$p1_strategy[m]==1 & df$p2_strategy[m]==0){df$type_num[m]=1}
}

# create unique ids and pairs
uniquepairs = unique(df$session_round_pair_id)
gametype = unique(df$game)
treatmenttype = unique(df$treatment)
uniquePlayer = union(unique(df$p1_code), unique(df$p2_code))

# get state output file
df_stata = df
df_stata = df_stata %>% rename(BoS1_4 = BoS1.4, BoS2_5 = BoS2.5)
write_dta(df_stata, "D:/Dropbox/Working Papers/Continuous Time BOS/data/stata_bos.dta")


##### Pair analysis: subjects dynamics #####
# loop over pairs
for (i in 1:length(uniquepairs)){
  df_pair = filter(df, session_round_pair_id == uniquepairs[i])
  
  title = paste(as.character(uniquepairs[i]))
  file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/data/figures_pair/by_profile/", title, sep = "")
  file = paste(file, ".png", sep = "")
  png(file, width = 500, height = 300)
  
  pic = ggplot() +
    geom_line(data = df_pair, aes(x=period, y=type_num), colour = 'blue') + 
    geom_hline(aes(yintercept=3.5), colour = 'red', linetype = 'dotted') +
    geom_hline(aes(yintercept=1.5), colour = 'red', linetype = 'dotted') +
    scale_x_continuous(name='period', waiver(), limits=c(0,20), breaks = c(0,5,10,15,20)) +
    scale_y_continuous(name='action profile', limits=c(1,4),
                       breaks = c(1,2,3,4), labels = c('Aggressive','RPNE','CPNE','Accommodating')) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
          axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
          axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))
  
  print(pic)
  
  dev.off()
}

rm(df_pair, pic)


##### Treatment effect: Coordination rate summary table #####
# create summary table
summary = matrix(NA, nrow = 5, ncol = 3)
rownames(summary) = c('BoS1.4', 'BoS1.4-BoS2.5', 'BoS2.5', 'BoS2.5-BoS10', 'BoS10')
colnames(summary) = c('Continuous', 'Discrete', 'Continuous-Discrete')

# set up sub dataset
df_c1 = filter(df, treatment == 'Continuous_BoS1.4')
df_c2 = filter(df, treatment == 'Continuous_BoS2.5')
df_c10 = filter(df, treatment == 'Continuous_BoS10')
df_d1 = filter(df, treatment == 'Discrete_BoS1.4')
df_d2 = filter(df, treatment == 'Discrete_BoS2.5')
df_d10 = filter(df, treatment == 'Discrete_BoS10')

# fill out the table
summary[1,1] = mean(df_c1$coordinate)
summary[1,2] = mean(df_d1$coordinate)
summary[1,3] = mean(df_c1$coordinate) - mean(df_d1$coordinate)

summary[2,1] = mean(df_c1$coordinate) - mean(df_c2$coordinate)
summary[2,2] = mean(df_d1$coordinate) - mean(df_d2$coordinate)

summary[3,1] = mean(df_c2$coordinate)
summary[3,2] = mean(df_d2$coordinate)
summary[3,3] = mean(df_c2$coordinate) - mean(df_d2$coordinate)

summary[4,1] = mean(df_c2$coordinate) - mean(df_c10$coordinate)
summary[4,2] = mean(df_d2$coordinate) - mean(df_d10$coordinate)

summary[5,1] = mean(df_c10$coordinate)
summary[5,2] = mean(df_d10$coordinate)
summary[5,3] = mean(df_c10$coordinate) - mean(df_d10$coordinate)

# wilcox-test for each relations
test_c1_d1 = t.test(df_c1$coordinate, df_d1$coordinate, 
                         alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
test_c2_d2 = t.test(df_c2$coordinate, df_d2$coordinate, 
                         alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
test_c10_d10 = t.test(df_c10$coordinate, df_d10$coordinate, 
                           alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
test_c1_c2 = t.test(df_c1$coordinate, df_c2$coordinate, 
                         alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
test_d1_d2 = t.test(df_d1$coordinate, df_d2$coordinate, 
                         alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
test_c2_c10 = t.test(df_c2$coordinate, df_c10$coordinate, 
                          alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
test_d2_d10 = t.test(df_d2$coordinate, df_d10$coordinate, 
                          alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)

# update p value for each test
print(test_c1_d1$p.value)
print(test_c2_d2$p.value)
print(test_c10_d10$p.value)
print(test_c1_c2$p.value)
print(test_d1_d2$p.value)
print(test_c2_c10$p.value)
print(test_d2_d10$p.value)

# table output
xtable(summary, digits = 2, label = 'summary_table', align = 'lccc')
rm(df_c1, df_c2, df_c10, df_d1, df_d2, df_d10, summary)
rm(test_c1_d1, test_c2_d2, test_c10_d10, test_c1_c2, test_d1_d2, test_c2_c10, test_d2_d10)


##### Treatment effect: Coordination rate summary by pairs#####
length = rep(NA, length(uniquepairs))
pair_summary = data.frame(session_round_pair_id = length, treatment = length, coordinate = length,
                          game = length, time = length, sequence = length, block = length)

# loop over pairs for classification
for (i in 1:length(uniquepairs)){
  df_pair = filter(df, session_round_pair_id == uniquepairs[i])
  
  # update treatment info
  pair_summary$session_round_pair_id[i] = df_pair$session_round_pair_id[1]
  pair_summary$treatment[i] = df_pair$treatment[1]
  pair_summary$game[i] = df_pair$game[1]
  pair_summary$time[i] = df_pair$time[1]
  pair_summary$sequence[i] = df_pair$sequence[1]
  pair_summary$block[i] = df_pair$block[1]
  pair_summary$coordinate[i] = mean(df_pair$coordinate)
}

# export stata data
write_dta(pair_summary, "D:/Dropbox/Working Papers/Continuous Time BOS/data/stata_bos_pair3.dta")

# check distribution
df_c = filter(pair_summary, time == 'Continuous')
df_d = filter(pair_summary, time == 'Discrete')

# set up plot
pic = ggplot() +
  stat_density(geom = "line", data=df_c, aes(x=coordinate, colour='blue')) +
  stat_density(geom = "line", data=df_d, aes(x=coordinate, colour='red')) +
  scale_x_continuous(name='coordinate rate', waiver()) +
  scale_y_continuous(name='density') +
  theme_bw() + 
  scale_colour_manual(values=c('blue','red'), labels=c('continuous','discrete')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)


##### Treatment effect: Coordination learning within each super game #####
# build six treatments over time dataset
df_treat = list()
for (i in 1:length(treatmenttype)){
  df_temp = filter(df, treatment == treatmenttype[i])
  df_treat[[i]] = data.frame(
    coordinate_rate = tapply(df_temp$coordinate, df_temp$period, mean),
    period = tapply(df_temp$period, df_temp$period, mean)
  )
}

# DTW distance
d1 = dtw(df_treat[[1]]$coordinate_rate, df_treat[[4]]$coordinate_rate)
d2 = dtw(df_treat[[2]]$coordinate_rate, df_treat[[5]]$coordinate_rate)
d3 = dtw(df_treat[[3]]$coordinate_rate, df_treat[[6]]$coordinate_rate)
d1$normalizedDistance
d2$normalizedDistance
d3$normalizedDistance

# set up plot
title = 'coordination_within_supergames'
file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/writeup_ContinuousBOS/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 800, height = 500)
pic = ggplot() +
  geom_line(data=df_treat[[1]], aes(x=period, y=coordinate_rate, colour='blue', linetype='solid')) +
  geom_line(data=df_treat[[2]], aes(x=period, y=coordinate_rate, colour='blue', linetype='longdash')) +
  geom_line(data=df_treat[[3]], aes(x=period, y=coordinate_rate, colour='blue', linetype='dotted')) +
  geom_line(data=df_treat[[4]], aes(x=period, y=coordinate_rate, colour='red', linetype='solid')) +
  geom_line(data=df_treat[[5]], aes(x=period, y=coordinate_rate, colour='red', linetype='longdash')) +
  geom_line(data=df_treat[[6]], aes(x=period, y=coordinate_rate, colour='red', linetype='dotted')) +
  scale_x_discrete(name='period', waiver(), limits=c(0,20)) +
  scale_y_continuous(name='coordination rate', limits=c(0.3,1)) +
  theme_bw() + 
  scale_colour_manual(values=c('blue','red'), labels=c('continuous','discrete')) +
  scale_linetype_manual(values=c('solid', 'longdash', 'dotted'), labels=c('BoS1.4', 'BoS2.5', 'BoS10')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

rm(df_temp, df_treat, pic, d1, d2, d3)


##### Treatment effect: Coordination learning between super games #####
# create summary table
learning = matrix(NA, nrow = length(treatmenttype), ncol = 3)
rownames(learning) = treatmenttype
colnames(learning) = c('Game4-9', 'Game10-15', '(G10-15)-(G4-9)')

# loop over treatments for differences
for (i in 1:length(treatmenttype)){
  df_treat = filter(df, treatment == treatmenttype[i])
  df_treat_1 = filter(df_treat, round < 10)
  df_treat_2 = filter(df_treat, round >= 10)
  
  learning[i,1] = mean(df_treat_1$coordinate)
  learning[i,2] = mean(df_treat_2$coordinate)
  learning[i,3] = mean(df_treat_2$coordinate) - mean(df_treat_1$coordinate)
  test = t.test(df_treat_2$coordinate, df_treat_1$coordinate, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  print(test$p.value)
}

# table output
xtable(learning, digits = 2, label = 'learn_between', align = 'lccc')
rm(df_treat, df_treat_1, df_treat_2, test, learning)


##### Treatment effect: Order effect #####
# create summary table
ordereffect = matrix(NA, nrow = length(treatmenttype), ncol = 3)
rownames(ordereffect) = treatmenttype
colnames(ordereffect) = c('Sequence 1', 'Sequence 2', 'S1-S2')

# loop over treatments for differences
for (i in 1:length(treatmenttype)){
  df_treat = filter(df, treatment == treatmenttype[i])
  df_treat_1 = filter(df_treat, sequence==1)
  df_treat_2 = filter(df_treat, sequence==2)
  
  ordereffect[i,1] = mean(df_treat_1$coordinate)
  ordereffect[i,2] = mean(df_treat_2$coordinate)
  ordereffect[i,3] = mean(df_treat_1$coordinate) - mean(df_treat_2$coordinate)
  test = t.test(df_treat_1$coordinate, df_treat_2$coordinate, 
                alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
  print(test$p.value)
}

# table output
xtable(ordereffect, digits = 2, label = 'order_effect', align = 'lccc')
rm(df_treat, df_treat_1, df_treat_2, test, ordereffect)


##### Mechanisms: Pair-level classification #####
# set up dataset and parameters
#df_2 = filter(df, period > 10)
df_2 = df
length = rep(NA, length(uniquepairs))
pair_summary = data.frame(session_round_pair_id = length, treatment = length,
                          game = length, time = length, type = length,
                          sequence = length, block = length)

# loop over pairs for classification
for (i in 1:length(uniquepairs)){
  df_pair = filter(df_2, session_round_pair_id == uniquepairs[i])
  
  # update treatment info
  pair_summary$session_round_pair_id[i] = df_pair$session_round_pair_id[1]
  pair_summary$treatment[i] = df_pair$treatment[1]
  pair_summary$game[i] = df_pair$game[1]
  pair_summary$time[i] = df_pair$time[1]
  pair_summary$sequence[i] = df_pair$sequence[1]
  pair_summary$block[i] = df_pair$block[1]
  
  # classify turn taking and one NE dynamics
  if (mean(df_pair$coordinate) >= 0.8){
    if (abs(mean(df_pair$Nash_Aa) - mean(df_pair$Nash_Bb)) <= 0.2 )
       {pair_summary$type[i] = 'alternating'}
    else{pair_summary$type[i] = 'one NE'}
  }
  else{
    pair_summary$type[i] = 'others'
  }
}

# recreate frequency dataset
pair_summary = pair_summary %>% mutate(alternating = ifelse(type=='alternating', 1, 0))
pair_summary = pair_summary %>% mutate(one_ne = ifelse(type=='one NE', 1, 0))
pair_summary = pair_summary %>% mutate(others = ifelse(type=='others', 1, 0))

# export stata data
write_dta(pair_summary, "D:/Dropbox/Working Papers/Continuous Time BOS/data/stata_bos_pair2.dta")

# generate distribution table
sum_pair = matrix(NA, nrow = 6, ncol = 3)
rownames(sum_pair) = treatmenttype
colnames(sum_pair) = c('Alternating', 'One NE', 'Other types')
for (i in 1:length(treatmenttype)){
  df_treat = filter(pair_summary, treatment == treatmenttype[i])
  sum_pair[i,1] = mean(df_treat$alternating)
  sum_pair[i,2] = mean(df_treat$one_ne)
  sum_pair[i,3] = 1 - sum_pair[i,1] - sum_pair[i,2]
}

# add statistical testing result
df_c1 = filter(pair_summary, treatment == 'Continuous_BoS1.4')
df_c2 = filter(pair_summary, treatment == 'Continuous_BoS2.5')
df_c10 = filter(pair_summary, treatment == 'Continuous_BoS10')
df_d1 = filter(pair_summary, treatment == 'Discrete_BoS1.4')
df_d2 = filter(pair_summary, treatment == 'Discrete_BoS2.5')
df_d10 = filter(pair_summary, treatment == 'Discrete_BoS10')

sum_pair2 = matrix(NA, nrow = 3, ncol = 3)
rownames(sum_pair2) = c('R1-R4', 'R2-R5', 'R3-R6')
sum_pair2[1,1] = sum_pair[1,1] - sum_pair[4,1]
sum_pair2[1,2] = sum_pair[1,2] - sum_pair[4,2]
sum_pair2[1,3] = sum_pair[1,3] - sum_pair[4,3]
test = t.test(df_c10$alternating, df_d10$alternating, 
              alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
print(test$p.value)
test = t.test(df_c10$one_ne, df_d10$one_ne, 
              alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
print(test$p.value) 
test = t.test(df_c10$others, df_d10$others, 
              alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
print(test$p.value) 

sum_pair2[2,1] = sum_pair[2,1] - sum_pair[5,1]
sum_pair2[2,2] = sum_pair[2,2] - sum_pair[5,2]
sum_pair2[2,3] = sum_pair[2,3] - sum_pair[5,3]
test = t.test(df_c2$alternating, df_d2$alternating, 
              alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
print(test$p.value)
test = t.test(df_c2$one_ne, df_d2$one_ne, 
              alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
print(test$p.value) 
test = t.test(df_c2$others, df_d2$others, 
              alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
print(test$p.value) 

sum_pair2[3,1] = sum_pair[3,1] - sum_pair[6,1]
sum_pair2[3,2] = sum_pair[3,2] - sum_pair[6,2]
sum_pair2[3,3] = sum_pair[3,3] - sum_pair[6,3]
test = t.test(df_c1$alternating, df_d1$alternating, 
              alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
print(test$p.value)
test = t.test(df_c1$one_ne, df_d1$one_ne, 
              alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
print(test$p.value) 
test = t.test(df_c1$others, df_d1$others, 
              alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
print(test$p.value) 

sum_pair = rbind(sum_pair, sum_pair2)

# table output
xtable(sum_pair, digits = 2, label = 'pair_class', align = 'lccc')
rm(df_2, df_pair, pair_summary, sum_pair, sum_pair2, test)
rm(df_c1, df_c2, df_c10, df_d1, df_d2, df_d10, df_treat)


##### Mechanisms: Pair classification showcase #####
# set up dataset and parameters
df_2 = filter(df, period > 10)
length = rep(NA, length(uniquepairs))
pair_summary = data.frame(session_round_pair_id = length, treatment = length,
                          game = length, time = length, type = length,
                          nash_total = length, nash_diff = length)

# loop over pairs for classification
for (i in 1:length(uniquepairs)){
  df_pair = filter(df_2, session_round_pair_id == uniquepairs[i])
  
  # update treatment info
  pair_summary$session_round_pair_id[i] = df_pair$session_round_pair_id[1]
  pair_summary$treatment[i] = df_pair$treatment[1]
  pair_summary$game[i] = df_pair$game[1]
  pair_summary$time[i] = df_pair$time[1]
  pair_summary$nash_total[i] = round(mean(df_pair$coordinate), digits=2)
  pair_summary$nash_diff[i] = round(abs(mean(df_pair$Nash_Aa) - mean(df_pair$Nash_Bb)), digits=2)
  
  # classify turn taking and one NE dynamics
  if (mean(df_pair$coordinate) >= 0.8){
    if (abs(mean(df_pair$Nash_Aa) - mean(df_pair$Nash_Bb)) <= 0.2 )
    {pair_summary$type[i] = 'alternating'}
    else{pair_summary$type[i] = 'one NE'}
  }
  else{
    pair_summary$type[i] = 'others'
  }
}

# draw scatter plot
title = 'classification_scatter'
file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/writeup_ContinuousBOS/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 600, height = 500)
pic = ggplot(data = pair_summary) +
  geom_rect(aes(xmin = 0.8, ymin = 0, xmax = 1, ymax = 0.2, fill='green'), color='green') +
  geom_rect(aes(xmin = 0.8, ymin = 0.2, xmax = 1, ymax = 1, fill='yellow'), color='yellow') +
  geom_jitter(aes(x=nash_total, y=nash_diff, colour=time), width=0.05, height=0.05) +
  scale_x_continuous(name='total time at Nash', waiver(), limits=c(0,1), breaks=c(0,0.2,0.4,0.6,0.8,1)) +
  scale_y_continuous(name='time difference in two pure Nash', waiver(), limits=c(0,1), breaks=c(0,0.2,0.4,0.6,0.8,1)) +
  theme_bw() + 
  scale_colour_manual(values=c('blue','red'), 
                      labels=c('continuous','discrete')) +
  scale_fill_manual(values=c('green','yellow'), 
                    labels=c('Alternating zone','One NE zone')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()


##### Mechanisms: Length of stay distribution #####
## stay at NE
# create dataset
df = arrange(df, df$session_code, df$subsession_id, df$id_in_subsession, df$tick)
df_length = data.frame(matrix(0, nrow = 1, ncol = 1))
df_char = data.frame(matrix(NA, nrow = 1, ncol = 3))
colnames(df_length) = c('length')
colnames(df_char) = c('session_round_pair_id', 'treatment', 'time')

# loop over pairs to update the dataset
for (i in 1:length(uniquepairs)){
  df_pair = filter(df, session_round_pair_id == uniquepairs[i])
  
  # initialize the event observation and parameters
  pair_id = df_pair$session_round_pair_id[1]
  treatment = df_pair$treatment[1]
  treat_time = df_pair$time[1]
  time = 0
  
  # loop over observations
  for (j in 1:length(df_pair$period)){
    
    # j > 1
    if (j > 1){
      if (df_pair$type[j] == df_pair$type[j-1] & df_pair$coordinate[j] == 1){
        time = time + 1
      }
      else{
        if (time > 0){
          event = c(pair_id, treatment, treat_time)
          df_length = rbind(df_length, time)
          df_char = rbind(df_char, event)
          time = 0
        }
        if (df_pair$coordinate[j] == 1){
          time = time + 1
        }
      }
    }
    # j = 1
    else{
      if (df_pair$coordinate[j] == 1){
        time = time + 1
      }
    }
    
    # record the last event
    if (j == length(df_pair$period) & time > 0){
      event = c(pair_id, treatment, treat_time)
      df_length = rbind(df_length, time)
      df_char = rbind(df_char, event)
    }
  }
}

# organize dataset
df_stay = cbind(df_length, df_char)
df_stay = filter(df_stay, is.na(treatment) == FALSE)
df_stay = df_stay %>% mutate(length_period = ifelse(time=='Discrete', length, round(length/12, digits=1)))

# draw distribution plot
df_c1 = filter(df_stay, treatment == 'Continuous_BoS1.4')
df_c2 = filter(df_stay, treatment == 'Continuous_BoS2.5')
df_c10 = filter(df_stay, treatment == 'Continuous_BoS10')
df_d1 = filter(df_stay, treatment == 'Discrete_BoS1.4')
df_d2 = filter(df_stay, treatment == 'Discrete_BoS2.5')
df_d10 = filter(df_stay, treatment == 'Discrete_BoS10')

# set up plot
title = 'distribution_duration_nash'
file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/writeup_ContinuousBOS/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 700, height = 300)
pic = ggplot() +
  stat_ecdf(geom="step", data=df_c10, aes(x=length_period, colour='blue', linetype='solid')) +
  stat_ecdf(geom="step", data=df_c2, aes(x=length_period, colour='blue', linetype='longdash')) +
  stat_ecdf(geom="step", data=df_c1, aes(x=length_period, colour='blue', linetype='dotted')) +
  stat_ecdf(geom="step", data=df_d10, aes(x=length_period, colour='red', linetype='solid')) +
  stat_ecdf(geom="step", data=df_d2, aes(x=length_period, colour='red', linetype='longdash')) +
  stat_ecdf(geom="step", data=df_d1, aes(x=length_period, colour='red', linetype='dotted')) +
  scale_x_continuous(name='duration(period)', waiver(), limits=c(0,10), breaks = c(0,2,4,6,8,10)) +
  scale_y_continuous(name='density') +
  theme_bw() + 
  scale_colour_manual(values=c('blue','red'), labels=c('continuous','discrete')) +
  scale_linetype_manual(values=c('solid', 'longdash', 'dotted'), labels=c('BoS1.4', 'BoS2.5', 'BoS10')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

# K-S test
ks.test(df_c10$length_period, df_d10$length_period)
ks.test(df_c2$length_period, df_d2$length_period)
ks.test(df_c1$length_period, df_d1$length_period)

rm(df_c1, df_c2, df_c10, df_d1, df_d2, df_d10)
rm(df_length, df_char, df_pair, df_stay, pic)

## stay at Mismatch
# create dataset
df = arrange(df, df$session_code, df$subsession_id, df$id_in_subsession, df$tick)
df_length = data.frame(matrix(0, nrow = 1, ncol = 1))
df_char = data.frame(matrix(NA, nrow = 1, ncol = 3))
colnames(df_length) = c('length')
colnames(df_char) = c('session_round_pair_id', 'treatment', 'time')

# loop over pairs to update the dataset
for (i in 1:length(uniquepairs)){
  df_pair = filter(df, session_round_pair_id == uniquepairs[i])
  
  # initialize the event observation and parameters
  pair_id = df_pair$session_round_pair_id[1]
  treatment = df_pair$treatment[1]
  treat_time = df_pair$time[1]
  time = 0
  
  # loop over observations
  for (j in 1:length(df_pair$period)){
    
    # j > 1
    if (j > 1){
      if (df_pair$type[j] == df_pair$type[j-1] & df_pair$coordinate[j] == 0){
        time = time + 1
      }
      else{
        if (time > 0){
          event = c(pair_id, treatment, treat_time)
          df_length = rbind(df_length, time)
          df_char = rbind(df_char, event)
          time = 0
        }
        if (df_pair$coordinate[j] == 0){
          time = time + 1
        }
      }
    }
    # j = 1
    else{
      if (df_pair$coordinate[j] == 0){
        time = time + 1
      }
    }
    
    # record the last event
    if (j == length(df_pair$period) & time > 0){
      event = c(pair_id, treatment, treat_time)
      df_length = rbind(df_length, time)
      df_char = rbind(df_char, event)
    }
  }
}

# organize dataset
df_stay = cbind(df_length, df_char)
df_stay = filter(df_stay, is.na(treatment) == FALSE)
df_stay = df_stay %>% mutate(length_period = ifelse(time=='Discrete', length, round(length/12, digits=1)))

# draw distribution plot
df_c1 = filter(df_stay, treatment == 'Continuous_BoS1.4')
df_c2 = filter(df_stay, treatment == 'Continuous_BoS2.5')
df_c10 = filter(df_stay, treatment == 'Continuous_BoS10')
df_d1 = filter(df_stay, treatment == 'Discrete_BoS1.4')
df_d2 = filter(df_stay, treatment == 'Discrete_BoS2.5')
df_d10 = filter(df_stay, treatment == 'Discrete_BoS10')

# set up plot
title = 'distribution_duration_mismatch'
file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/writeup_ContinuousBOS/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 700, height = 300)
pic = ggplot() +
  stat_ecdf(geom="step", data=df_c10, aes(x=length_period, colour='blue', linetype='solid')) +
  stat_ecdf(geom="step", data=df_c2, aes(x=length_period, colour='blue', linetype='longdash')) +
  stat_ecdf(geom="step", data=df_c1, aes(x=length_period, colour='blue', linetype='dotted')) +
  stat_ecdf(geom="step", data=df_d10, aes(x=length_period, colour='red', linetype='solid')) +
  stat_ecdf(geom="step", data=df_d2, aes(x=length_period, colour='red', linetype='longdash')) +
  stat_ecdf(geom="step", data=df_d1, aes(x=length_period, colour='red', linetype='dotted')) +
  scale_x_continuous(name='duration(period)', waiver(), limits=c(0,10), breaks = c(0,2,4,6,8,10)) +
  scale_y_continuous(name='density') +
  theme_bw() + 
  scale_colour_manual(values=c('blue','red'), labels=c('continuous','discrete')) +
  scale_linetype_manual(values=c('solid', 'longdash', 'dotted'), labels=c('BoS1.4', 'BoS2.5', 'BoS10')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

# K-S test
ks.test(df_c10$length_period, df_d10$length_period)
ks.test(df_c2$length_period, df_d2$length_period)
ks.test(df_c1$length_period, df_d1$length_period)

rm(df_c1, df_c2, df_c10, df_d1, df_d2, df_d10)
rm(df_length, df_char, df_pair, df_stay, pic)


##### Mechanisms_ext: Whether the duration at Nash is squeezed out over time #####
# create dataset
df = arrange(df, df$session_code, df$subsession_id, df$id_in_subsession, df$tick)
df_length = data.frame(matrix(0, nrow = 1, ncol = 2))
df_char = data.frame(matrix(NA, nrow = 1, ncol = 3))
colnames(df_length) = c('length', 'start')
colnames(df_char) = c('session_round_pair_id', 'treatment', 'time')

# loop over pairs to update the dataset
for (i in 1:length(uniquepairs)){
  df_pair = filter(df, session_round_pair_id == uniquepairs[i])
  
  # initialize the event observation and parameters
  pair_id = df_pair$session_round_pair_id[1]
  treatment = df_pair$treatment[1]
  treat_time = df_pair$time[1]
  time = 0
  
  # loop over observations
  for (j in 1:length(df_pair$period)){
    
    # j > 1
    if (j > 1){
      if (df_pair$type[j] == df_pair$type[j-1] & df_pair$coordinate[j] == 1){
        time = time + 1
      }
      else{
        if (time > 0){
          event = c(pair_id, treatment, treat_time)
          record = c(time, start)
          df_length = rbind(df_length, record)
          df_char = rbind(df_char, event)
          time = 0
        }
        if (df_pair$coordinate[j] == 1){
          time = time + 1
          start = df_pair$period[j]
        }
      }
    }
    # j = 1
    else{
      if (df_pair$coordinate[j] == 1){
        time = time + 1
        start = df_pair$period[j]
      }
    }
    
    # record the last event
    if (j == length(df_pair$period) & time > 0){
      event = c(pair_id, treatment, treat_time)
      record = c(time, start)
      df_length = rbind(df_length, record)
      df_char = rbind(df_char, event)
    }
  }
}

# organize dataset
df_stay = cbind(df_length, df_char)
df_stay = filter(df_stay, is.na(treatment) == FALSE)
df_stay = df_stay %>% mutate(length_period = ifelse(time=='Discrete', length, round(length/12, digits=1)))
df_stay = df_stay %>% mutate(start_time = floor(start))

# further generate the average length at time tick in continuous time
df_stay_c = filter(df_stay, time == 'Continuous')
uniquetime = unique(df_stay_c$start_time)
length = rep(NA, length(uniquetime))
df_stay_cavg = data.frame(
  session_round_pair_id = length, treatment = length, time = length,
  length_period = length, start_time = length)
for (i in 1:length(uniquetime)){
  df_sub = filter(df_stay_c, start_time == uniquetime[i])
  df_stay_cavg$session_round_pair_id[i] = df_sub$session_round_pair_id[1]
  df_stay_cavg$treatment[i] = df_sub$treatment[1]
  df_stay_cavg$time[i] = df_sub$time[1]
  df_stay_cavg$length_period[i] = mean(df_sub$length_period)
  df_stay_cavg$start_time[i] = df_sub$start_time[1]
}
df_stay_cavg = arrange(df_stay_cavg, df_stay_cavg$time, df_stay_cavg$start_time)

# further generate the average length at time tick in discrete time
df_stay_d = filter(df_stay, time == 'Discrete')
uniquetime = unique(df_stay_d$start_time)
length = rep(NA, length(uniquetime))
df_stay_davg = data.frame(
  session_round_pair_id = length, treatment = length, time = length,
  length_period = length, start_time = length)
for (i in 1:length(uniquetime)){
  df_sub = filter(df_stay_d, start_time == uniquetime[i])
  df_stay_davg$session_round_pair_id[i] = df_sub$session_round_pair_id[1]
  df_stay_davg$treatment[i] = df_sub$treatment[1]
  df_stay_davg$time[i] = df_sub$time[1]
  df_stay_davg$length_period[i] = mean(df_sub$length_period)
  df_stay_davg$start_time[i] = df_sub$start_time[1]
}
df_stay_davg = arrange(df_stay_davg, df_stay_davg$time, df_stay_davg$start_time)

# plot the relation between start time and length of stay in two time environments
title = 'duration_nash_squeeze_over_time'
file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/writeup_ContinuousBOS/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 700, height = 300)
pic = ggplot() +
  geom_line(data = df_stay_cavg, aes(x=start_time, y=length_period, colour='blue')) + 
  geom_line(data = df_stay_davg, aes(x=start_time, y=length_period, colour='red')) + 
  scale_x_continuous(name='starting period', waiver(), limits=c(0,20), breaks = c(1,5,10,15,19,20)) +
  scale_y_continuous(name='average length of duration at Nash') +
  theme_bw() + 
  scale_colour_manual(values=c('blue','red'), labels=c('continuous','discrete')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

rm(df_char, df_length, df_pair, df_stay, df_stay_c, df_stay_cavg)
rm(df_stay_d, df_stay_davg, df_sub, pic)


##### Mechanisms_ext: Whether the interaction is more frequent over time #####
# add column see whether subjects switch between profiles
df_int = filter(df, floor(period)-period == 0)
df_int = df_int %>% group_by(session_round_pair_id) %>% mutate(
  switch = ifelse(type == lag(type), 0, 1))

# generate prob of switching over time in continuous time
df_c = filter(df_int, time == 'Continuous')
df_cswitch = data.frame(
  period = tapply(df_c$period, df_c$period, mean),
  prob_switch = tapply(df_c$switch, df_c$period, mean))

# generate prob of switching over time in continuous time
df_d = filter(df_int, time == 'Discrete')
df_dswitch = data.frame(
  period = tapply(df_d$period, df_d$period, mean),
  prob_switch = tapply(df_d$switch, df_d$period, mean))

# plot the relation between time and probability of switch
title = 'prob_transition_over_time'
file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/writeup_ContinuousBOS/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 700, height = 300)
pic = ggplot() +
  geom_line(data = df_cswitch, aes(x=period, y=prob_switch, colour='blue')) + 
  geom_line(data = df_dswitch, aes(x=period, y=prob_switch, colour='red')) + 
  scale_x_continuous(name='period', waiver(), limits=c(0,20), breaks = c(1,5,10,15,19,20)) +
  scale_y_continuous(name='probability of switch') +
  theme_bw() + 
  scale_colour_manual(values=c('blue','red'), labels=c('continuous','discrete')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

rm(df_int, df_c, df_cswitch, df_d, df_dswitch, pic)


##### Mechanisms: Transition probability matrix by time environments #####
## Continuous time
# create transition matrix
transition = matrix(0, nrow = 4, ncol = 5)
rownames(transition) = c('(A,b) at t', '(B,a) at t', '(A,a) at t', '(B,b) at t')
colnames(transition) = c('(A,b) at t+1', '(B,a) at t+1', '(A,a) at t+1', '(B,b) at t+1', '# of obs')

# select treatment
treatment_data = subset(df, time == 'Continuous')
pairs = unique(treatment_data$session_round_pair_id)

# loop over pairs
for (i in 1:length(pairs)){
  round_data = subset(treatment_data, session_round_pair_id == pairs[i])
  
  # loop over observations
  for (j in 2:length(round_data$tick)){
    
    # when the original observation is (1,1)
    if (round_data$type[j-1] == 'Nash(A,a)'){
      if (round_data$type[j] == 'Nash(A,a)'){transition[3,3] = transition[3,3] + 1}
      if (round_data$type[j] == 'Nash(B,b)'){transition[3,4] = transition[3,4] + 1}
      if (round_data$type[j] == 'Accommodate'){transition[3,2] = transition[3,2] + 1}
      if (round_data$type[j] == 'Aggressive'){transition[3,1] = transition[3,1] + 1}
    }
    
    # when the original observation is (0,0)
    if (round_data$type[j-1] == 'Nash(B,b)'){
      if (round_data$type[j] == 'Nash(A,a)'){transition[4,3] = transition[4,3] + 1}
      if (round_data$type[j] == 'Nash(B,b)'){transition[4,4] = transition[4,4] + 1}
      if (round_data$type[j] == 'Accommodate'){transition[4,2] = transition[4,2] + 1}
      if (round_data$type[j] == 'Aggressive'){transition[4,1] = transition[4,1] + 1}
    }
    
    # when the original observation is (1,0)
    if (round_data$type[j-1] == 'Aggressive'){
      if (round_data$type[j] == 'Nash(A,a)'){transition[1,3] = transition[1,3] + 1}
      if (round_data$type[j] == 'Nash(B,b)'){transition[1,4] = transition[1,4] + 1}
      if (round_data$type[j] == 'Accommodate'){transition[1,2] = transition[1,2] + 1}
      if (round_data$type[j] == 'Aggressive'){transition[1,1] = transition[1,1] + 1}
    }
    
    # when the original observation is (0,1)
    if (round_data$type[j-1] == 'Accommodate'){
      if (round_data$type[j] == 'Nash(A,a)'){transition[2,3] = transition[2,3] + 1}
      if (round_data$type[j] == 'Nash(B,b)'){transition[2,4] = transition[2,4] + 1}
      if (round_data$type[j] == 'Accommodate'){transition[2,2] = transition[2,2] + 1}
      if (round_data$type[j] == 'Aggressive'){transition[2,1] = transition[2,1] + 1}
    }
  }
}

# calculate transition probability
transition_prob = transition
for (k in 1:4){
  transition_prob[k,1] = round(transition[k,1] / sum(transition[k,1:4]), 2)
  transition_prob[k,2] = round(transition[k,2] / sum(transition[k,1:4]), 2)
  transition_prob[k,3] = round(transition[k,3] / sum(transition[k,1:4]), 2)
  transition_prob[k,4] = round(transition[k,4] / sum(transition[k,1:4]), 2)
  transition_prob[k,5] = sum(transition[k,1:4])
}
transition_c = transition_prob

# calculate the transition probability without diagonal
transition_prob = transition
transition[1,1] = 0
transition[2,2] = 0
transition[3,3] = 0
transition[4,4] = 0
for (k in 1:4){
  transition_prob[k,1] = round(transition[k,1] / sum(transition[k,1:4]), 2)
  transition_prob[k,2] = round(transition[k,2] / sum(transition[k,1:4]), 2)
  transition_prob[k,3] = round(transition[k,3] / sum(transition[k,1:4]), 2)
  transition_prob[k,4] = round(transition[k,4] / sum(transition[k,1:4]), 2)
  transition_prob[k,5] = sum(transition[k,1:4])
}
transition_c2 = transition_prob

## Discrete time
# create transition matrix
transition = matrix(0, nrow = 4, ncol = 5)
rownames(transition) = c('(A,b) at t', '(B,a) at t', '(A,a) at t', '(B,b) at t')
colnames(transition) = c('(A,b) at t+1', '(B,a) at t+1', '(A,a) at t+1', '(B,b) at t+1', '# of obs')

# select treatment
treatment_data = subset(df, time == 'Discrete')
pairs = unique(treatment_data$session_round_pair_id)

# loop over pairs
for (i in 1:length(pairs)){
  round_data = subset(treatment_data, session_round_pair_id == pairs[i])
  
  # loop over observations
  for (j in 2:length(round_data$tick)){
    
    # when the original observation is (1,1)
    if (round_data$type[j-1] == 'Nash(A,a)'){
      if (round_data$type[j] == 'Nash(A,a)'){transition[3,3] = transition[3,3] + 1}
      if (round_data$type[j] == 'Nash(B,b)'){transition[3,4] = transition[3,4] + 1}
      if (round_data$type[j] == 'Accommodate'){transition[3,2] = transition[3,2] + 1}
      if (round_data$type[j] == 'Aggressive'){transition[3,1] = transition[3,1] + 1}
    }
    
    # when the original observation is (0,0)
    if (round_data$type[j-1] == 'Nash(B,b)'){
      if (round_data$type[j] == 'Nash(A,a)'){transition[4,3] = transition[4,3] + 1}
      if (round_data$type[j] == 'Nash(B,b)'){transition[4,4] = transition[4,4] + 1}
      if (round_data$type[j] == 'Accommodate'){transition[4,2] = transition[4,2] + 1}
      if (round_data$type[j] == 'Aggressive'){transition[4,1] = transition[4,1] + 1}
    }
    
    # when the original observation is (1,0)
    if (round_data$type[j-1] == 'Aggressive'){
      if (round_data$type[j] == 'Nash(A,a)'){transition[1,3] = transition[1,3] + 1}
      if (round_data$type[j] == 'Nash(B,b)'){transition[1,4] = transition[1,4] + 1}
      if (round_data$type[j] == 'Accommodate'){transition[1,2] = transition[1,2] + 1}
      if (round_data$type[j] == 'Aggressive'){transition[1,1] = transition[1,1] + 1}
    }
    
    # when the original observation is (0,1)
    if (round_data$type[j-1] == 'Accommodate'){
      if (round_data$type[j] == 'Nash(A,a)'){transition[2,3] = transition[2,3] + 1}
      if (round_data$type[j] == 'Nash(B,b)'){transition[2,4] = transition[2,4] + 1}
      if (round_data$type[j] == 'Accommodate'){transition[2,2] = transition[2,2] + 1}
      if (round_data$type[j] == 'Aggressive'){transition[2,1] = transition[2,1] + 1}
    }
  }
}

# calculate transition probability
transition_prob = transition
for (k in 1:4){
  transition_prob[k,1] = round(transition[k,1] / sum(transition[k,1:4]), 2)
  transition_prob[k,2] = round(transition[k,2] / sum(transition[k,1:4]), 2)
  transition_prob[k,3] = round(transition[k,3] / sum(transition[k,1:4]), 2)
  transition_prob[k,4] = round(transition[k,4] / sum(transition[k,1:4]), 2)
  transition_prob[k,5] = sum(transition[k,1:4])
}
transition_d = transition_prob

# calculate the transition probability without diagonal
transition_prob = transition
transition[1,1] = 0
transition[2,2] = 0
transition[3,3] = 0
transition[4,4] = 0
for (k in 1:4){
  transition_prob[k,1] = round(transition[k,1] / sum(transition[k,1:4]), 2)
  transition_prob[k,2] = round(transition[k,2] / sum(transition[k,1:4]), 2)
  transition_prob[k,3] = round(transition[k,3] / sum(transition[k,1:4]), 2)
  transition_prob[k,4] = round(transition[k,4] / sum(transition[k,1:4]), 2)
  transition_prob[k,5] = sum(transition[k,1:4])
}
transition_d2 = transition_prob

# data export
xtable(transition_c, digits = 2, label = 'transition_c', align = 'lccccc')
xtable(transition_d, digits = 2, label = 'transition_d', align = 'lccccc')
xtable(transition_c2, digits = 2, label = 'transition_c_nod', align = 'lccccc')
xtable(transition_d2, digits = 2, label = 'transition_d_nod', align = 'lccccc')
rm(treatment_data, round_data, transition, transition_prob)
rm(transition_c, transition_d, transition_c2, transition_d2)


##### Mechanisms_ext: Transition probability matrix by all treatments #####
# set up transition matrix container
transition_all = list()

# loop over treatments
for (m in 1:length(treatmenttype)){
  
  # create transition matrix
  transition = matrix(0, nrow = 4, ncol = 5)
  rownames(transition) = c('(A,b) at t', '(B,a) at t', '(A,a) at t', '(B,b) at t')
  colnames(transition) = c('(A,b) at t+1', '(B,a) at t+1', '(A,a) at t+1', '(B,b) at t+1', 'num of transitions')
  
  # select sub dataset
  treatment_data = filter(df, treatment == treatmenttype[m])
  pairs = unique(treatment_data$session_round_pair_id)
  
  # loop over pairs
  for (i in 1:length(pairs)){
    round_data = filter(treatment_data, session_round_pair_id == pairs[i])
    
    # loop over observations
    for (j in 2:length(round_data$tick)){
      
      # when the original observation is (1,1)
      if (round_data$type[j-1] == 'Nash(A,a)'){
        if (round_data$type[j] == 'Nash(A,a)'){transition[3,3] = transition[3,3] + 1}
        if (round_data$type[j] == 'Nash(B,b)'){transition[3,4] = transition[3,4] + 1}
        if (round_data$type[j] == 'Accommodate'){transition[3,2] = transition[3,2] + 1}
        if (round_data$type[j] == 'Aggressive'){transition[3,1] = transition[3,1] + 1}
      }
      
      # when the original observation is (0,0)
      if (round_data$type[j-1] == 'Nash(B,b)'){
        if (round_data$type[j] == 'Nash(A,a)'){transition[4,3] = transition[4,3] + 1}
        if (round_data$type[j] == 'Nash(B,b)'){transition[4,4] = transition[4,4] + 1}
        if (round_data$type[j] == 'Accommodate'){transition[4,2] = transition[4,2] + 1}
        if (round_data$type[j] == 'Aggressive'){transition[4,1] = transition[4,1] + 1}
      }
      
      # when the original observation is (1,0)
      if (round_data$type[j-1] == 'Aggressive'){
        if (round_data$type[j] == 'Nash(A,a)'){transition[1,3] = transition[1,3] + 1}
        if (round_data$type[j] == 'Nash(B,b)'){transition[1,4] = transition[1,4] + 1}
        if (round_data$type[j] == 'Accommodate'){transition[1,2] = transition[1,2] + 1}
        if (round_data$type[j] == 'Aggressive'){transition[1,1] = transition[1,1] + 1}
      }
      
      # when the original observation is (0,1)
      if (round_data$type[j-1] == 'Accommodate'){
        if (round_data$type[j] == 'Nash(A,a)'){transition[2,3] = transition[2,3] + 1}
        if (round_data$type[j] == 'Nash(B,b)'){transition[2,4] = transition[2,4] + 1}
        if (round_data$type[j] == 'Accommodate'){transition[2,2] = transition[2,2] + 1}
        if (round_data$type[j] == 'Aggressive'){transition[2,1] = transition[2,1] + 1}
      }
    }
  }
  
  # calculate the transition probability without diagonal
  transition_prob = transition
  transition[1,1] = 0
  transition[2,2] = 0
  transition[3,3] = 0
  transition[4,4] = 0
  for (k in 1:4){
    transition_prob[k,1] = round(transition[k,1] / sum(transition[k,1:4]), 2)
    transition_prob[k,2] = round(transition[k,2] / sum(transition[k,1:4]), 2)
    transition_prob[k,3] = round(transition[k,3] / sum(transition[k,1:4]), 2)
    transition_prob[k,4] = round(transition[k,4] / sum(transition[k,1:4]), 2)
    transition_prob[k,5] = sum(transition[k,1:4])
  }
  transition_all[[m]] = transition_prob
}

# data export
xtable(transition_all[[1]], digits = 2, caption = as.character(treatmenttype[1]), align = 'lccccc')
xtable(transition_all[[2]], digits = 2, caption = as.character(treatmenttype[2]), align = 'lccccc')
xtable(transition_all[[3]], digits = 2, caption = as.character(treatmenttype[3]), align = 'lccccc')
xtable(transition_all[[4]], digits = 2, caption = as.character(treatmenttype[4]), align = 'lccccc')
xtable(transition_all[[5]], digits = 2, caption = as.character(treatmenttype[5]), align = 'lccccc')
xtable(transition_all[[6]], digits = 2, caption = as.character(treatmenttype[6]), align = 'lccccc')

rm(transition, transition_prob, treatment_data, round_data, transitional_all)


##### Mechanisms_ext: Transition probability by pairs and scatter plot #####
# set up transition matrix container
length = rep(NA, length(uniquepairs))
transition_pair = data.frame(session_round_pair_id = length, treatment = length,
                             game = length, time = length, sequence = length, block = length,
                             Nash_Ab = length, Nash_Ba = length, Nash_Nash = length)

# loop over pairs
for (i in 1:length(uniquepairs)){
  
  # get the pair data
  round_data = filter(df, session_round_pair_id == uniquepairs[i])
  
  # set up record
  Nash_Ab = 0
  Nash_Ba = 0
  Nash_Nash = 0
  
  # loop over observations
  for (j in 2:length(round_data$tick)){
    
    # when the original observation is (1,1)
    if (round_data$type[j-1] == 'Nash(A,a)'){
      if (round_data$type[j] == 'Nash(A,a)'){next}
      if (round_data$type[j] == 'Nash(B,b)'){Nash_Nash = Nash_Nash + 1}
      if (round_data$type[j] == 'Accommodate'){Nash_Ba = Nash_Ba + 1}
      if (round_data$type[j] == 'Aggressive'){Nash_Ab = Nash_Ab + 1}
    }
    
    # when the original observation is (0,0)
    if (round_data$type[j-1] == 'Nash(B,b)'){
      if (round_data$type[j] == 'Nash(A,a)'){Nash_Nash = Nash_Nash + 1}
      if (round_data$type[j] == 'Nash(B,b)'){next}
      if (round_data$type[j] == 'Accommodate'){Nash_Ba = Nash_Ba + 1}
      if (round_data$type[j] == 'Aggressive'){Nash_Ab = Nash_Ab + 1}
    }
    
    # when the original observation is (1,0)
    if (round_data$type[j-1] == 'Aggressive'){
      if (round_data$type[j] == 'Nash(A,a)'){Nash_Ab = Nash_Ab + 1}
      if (round_data$type[j] == 'Nash(B,b)'){Nash_Ab = Nash_Ab + 1}
      if (round_data$type[j] == 'Accommodate'){next}
      if (round_data$type[j] == 'Aggressive'){next}
    }
    
    # when the original observation is (0,1)
    if (round_data$type[j-1] == 'Accommodate'){
      if (round_data$type[j] == 'Nash(A,a)'){Nash_Ba = Nash_Ba + 1}
      if (round_data$type[j] == 'Nash(B,b)'){Nash_Ba = Nash_Ba + 1}
      if (round_data$type[j] == 'Accommodate'){next}
      if (round_data$type[j] == 'Aggressive'){next}
    }
  }
  
  # update treatment info
  transition_pair$session_round_pair_id[i] = round_data$session_round_pair_id[1]
  transition_pair$treatment[i] = round_data$treatment[1]
  transition_pair$game[i] = round_data$game[1]
  transition_pair$time[i] = round_data$time[1]
  transition_pair$sequence[i] = round_data$sequence[1]
  transition_pair$block[i] = round_data$block[1]
  
  # fill out the transition related columns
  total = Nash_Nash + Nash_Ab + Nash_Ba
  transition_pair$Nash_Nash[i] = round(Nash_Nash / total, digits = 3)
  transition_pair$Nash_Ab[i] = round(Nash_Ab / total, digits = 3)
  transition_pair$Nash_Ba[i] = round(Nash_Ba /total, digits = 3)
}

# further count the size of each group
transition_pair$group = paste(transition_pair$time, transition_pair$Nash_Ab, transition_pair$Nash_Ba, sep = '_')
uniquegroup = unique(transition_pair$group)
length = rep(NA, length(uniquegroup))
df_group = data.frame(time = length, count = length, Nash_Ab = length, Nash_Ba = length)
for (i in 1:length(uniquegroup)){
  pair_group = filter(transition_pair, group == uniquegroup[i])
  df_group$time[i] = pair_group$time[1]
  df_group$Nash_Ab[i] = pair_group$Nash_Ab[1]
  df_group$Nash_Ba[i] = pair_group$Nash_Ba[1]
  df_group$count[i] = length(pair_group$group)
}

# draw scatter plot
title = 'transition_scatter'
file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/writeup_ContinuousBOS/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 600, height = 500)
pic = ggplot(data = df_group) +
  geom_point(aes(x=Nash_Ab, y=Nash_Ba, colour=time, size=count)) +
  geom_abline(aes(intercept = 0, slope = 1), color = 'black') +
  scale_x_continuous(name='Transition prob between Nash and Aggressive', waiver(), limits=c(0,1), breaks=c(0,0.2,0.4,0.6,0.8,1)) +
  scale_y_continuous(name='Transition prob between Nash and Accommodating', waiver(), limits=c(0,1), breaks=c(0,0.2,0.4,0.6,0.8,1)) +
  theme_bw() + 
  scale_colour_manual(values=c('blue','red'), 
                      labels=c('continuous','discrete')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()


##### Mechanisms: First time reach NE #####
length = rep(NA, length(uniquepairs))
pair_summary = data.frame(session_round_pair_id = length, treatment = length, 
                          game = length, time = length, timing = length)
  
# loop over pairs for classification
for (i in 1:length(uniquepairs)){
  df_pair = filter(df, session_round_pair_id == uniquepairs[i])
  
  # update treatment info
  pair_summary$session_round_pair_id[i] = df_pair$session_round_pair_id[1]
  pair_summary$treatment[i] = df_pair$treatment[1]
  pair_summary$game[i] = df_pair$game[1]
  pair_summary$time[i] = df_pair$time[1]
  
  # update first Nash timing
  for (j in 1:length(df_pair$tick)){
    if (df_pair$coordinate[j] == 1){
      pair_summary$timing[i] = df_pair$period[j]
      break
    }
    else{next}
  }
}

# draw distribution plot
df_c1 = filter(pair_summary, treatment == 'Continuous_BoS1.4')
df_c2 = filter(pair_summary, treatment == 'Continuous_BoS2.5')
df_c10 = filter(pair_summary, treatment == 'Continuous_BoS10')
df_d1 = filter(pair_summary, treatment == 'Discrete_BoS1.4')
df_d2 = filter(pair_summary, treatment == 'Discrete_BoS2.5')
df_d10 = filter(pair_summary, treatment == 'Discrete_BoS10')

# set up plot
title = 'timing_first_ne'
file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/writeup_ContinuousBOS/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 700, height = 300)
pic = ggplot() +
  stat_ecdf(geom="step", data=df_c10, aes(x=timing, colour='blue', linetype='solid')) + 
  stat_ecdf(geom="step", data=df_c10, aes(x=timing, colour='blue', linetype='solid')) +
  stat_ecdf(geom="step", data=df_c2, aes(x=timing, colour='blue', linetype='longdash')) +
  stat_ecdf(geom="step", data=df_c1, aes(x=timing, colour='blue', linetype='dotted')) +
  stat_ecdf(geom="step", data=df_d10, aes(x=timing, colour='red', linetype='solid')) +
  stat_ecdf(geom="step", data=df_d2, aes(x=timing, colour='red', linetype='longdash')) +
  stat_ecdf(geom="step", data=df_d1, aes(x=timing, colour='red', linetype='dotted')) +
  scale_x_continuous(name='timing(period)', waiver(), limits=c(0,10), breaks = c(0,2,4,6,8,10)) +
  scale_y_continuous(name='density') +
  theme_bw() + 
  scale_colour_manual(values=c('blue','red'), labels=c('continuous','discrete')) +
  scale_linetype_manual(values=c('solid', 'longdash', 'dotted'), labels=c('BoS1.4', 'BoS2.5', 'BoS10')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

# K-S test
ks.test(df_c10$timing, df_d10$timing)
ks.test(df_c2$timing, df_d2$timing)
ks.test(df_c1$timing, df_d1$timing)

rm(df_c1, df_c2, df_c10, df_d1, df_d2, df_d10)
rm(df_pair, pair_summary, pic)


##### Mechanisms: Payoff inequality #####
# set up new dataset for cumulative payoff
df = df %>% mutate(p1_payoff_cumu = 0)
df = df %>% mutate(p2_payoff_cumu = 0)
df = arrange(df, df$session_code, df$subsession_id, df$id_in_subsession, df$tick)
df_new = filter(df, is.na(tick))

# loop over pairs to update the cumulative payoff
for (i in 1:length(uniquepairs)){
  df_pair = filter(df, session_round_pair_id == uniquepairs[i])
  
  # loop over observations
  for (j in 1:length(df_pair$tick)){
    df_pair$p1_payoff_cumu[j] = sum(df_pair$p1_payoff[1:j]) / j
    df_pair$p2_payoff_cumu[j] = sum(df_pair$p2_payoff[1:j]) / j
  }
  
  # combine pair_data to new dataset
  df_new = rbind(df_new, df_pair)
}

df_new = df_new %>% mutate(payoff_diff = abs(p1_payoff_cumu - p2_payoff_cumu))

# build six treatments over time dataset
df_treat = list()
for (i in 1:length(treatmenttype)){
  df_temp = filter(df_new, treatment == treatmenttype[i])
  df_treat[[i]] = data.frame(
    payoff_diff = tapply(df_temp$payoff_diff, df_temp$period, mean),
    period = tapply(df_temp$period, df_temp$period, mean)
  )
}

# DTW distance
d1 = dtw(df_treat[[1]]$payoff_diff, df_treat[[4]]$payoff_diff)
d2 = dtw(df_treat[[2]]$payoff_diff, df_treat[[5]]$payoff_diff)
d3 = dtw(df_treat[[3]]$payoff_diff, df_treat[[6]]$payoff_diff)
d1$normalizedDistance
d2$normalizedDistance
d3$normalizedDistance

# set up plot
title = 'inequality_within_supergames'
file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/writeup_ContinuousBOS/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 800, height = 500)
pic = ggplot() +
  geom_line(data=df_treat[[1]], aes(x=period, y=payoff_diff, colour='blue', linetype='solid')) +
  geom_line(data=df_treat[[2]], aes(x=period, y=payoff_diff, colour='blue', linetype='longdash')) +
  geom_line(data=df_treat[[3]], aes(x=period, y=payoff_diff, colour='blue', linetype='dotted')) +
  geom_line(data=df_treat[[4]], aes(x=period, y=payoff_diff, colour='red', linetype='solid')) +
  geom_line(data=df_treat[[5]], aes(x=period, y=payoff_diff, colour='red', linetype='longdash')) +
  geom_line(data=df_treat[[6]], aes(x=period, y=payoff_diff, colour='red', linetype='dotted')) +
  scale_x_discrete(name='period', waiver(), limits=c(0,20)) +
  scale_y_continuous(name='difference of cumulative payoff') +
  theme_bw() + 
  scale_colour_manual(values=c('blue','red'), labels=c('continuous','discrete')) +
  scale_linetype_manual(values=c('solid', 'longdash', 'dotted'), labels=c('BoS1.4', 'BoS2.5', 'BoS10')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

rm(df_temp, df_treat, df_pair, pic, d1, d2, d3)


##### Extension: two-step transition #####
# filter to data when the adjacent choices are different
df_slim = df %>% group_by(session_round_pair_id) %>% mutate(lag_type = lag(type))
df_slim = df_slim %>% filter(type!=lag_type | is.na(lag_type))

# build two-step transition
df_slim = df_slim %>% group_by(session_round_pair_id) %>% mutate(
  trans1 = lead(type),
  trans2 = lead(trans1),
  period1 = lead(period),
  period2 = lead(period1))
df_slim = df_slim %>% filter(is.na(trans1)==FALSE & is.na(trans2)==FALSE)
df_slim = df_slim %>% mutate(trans_type = paste(type, trans1, trans2, sep = '_'))

# collapse the data for continuous time
df_slim_c = df_slim %>% filter(time == 'Continuous')
uniquetrans = unique(df_slim_c$trans_type)
length = rep(NA, length(uniquetrans))
trans_data_c = data.frame(
  trans0 = length, trans1 = length, trans2 = length,
  duration1 = length, duration2 = length,
  trans_type = length, frequency = length)

for (i in 1:length(uniquetrans)){
  df_trans = df_slim_c %>% filter(trans_type == uniquetrans[i])
  trans_data_c$trans0[i] = df_trans$type[1]
  trans_data_c$trans1[i] = df_trans$trans1[1]
  trans_data_c$trans2[i] = df_trans$trans2[1]
  trans_data_c$duration1[i] = mean(df_trans$period1 - df_trans$period)
  trans_data_c$duration2[i] = mean(df_trans$period2 - df_trans$period1)
  trans_data_c$trans_type[i] = uniquetrans[i]
  trans_data_c$frequency[i] = length(df_trans$tick)
}
trans_data_c = trans_data_c %>% mutate(prob = round(frequency/sum(trans_data_c$frequency), digits=3))
trans_data_c = trans_data_c %>% arrange(desc(trans_data_c$prob))

# select the transitions between Nash
trans_data_c = trans_data_c %>% filter(trans0 == 'Nash(A,a)' | trans0 == 'Nash(B,b)')
trans_data_c = trans_data_c %>% mutate(prob1 = round(frequency/sum(trans_data_c$frequency), digits=3))
trans_data_c = trans_data_c %>% filter(trans2 == 'Nash(A,a)' | trans2 == 'Nash(B,b)')
trans_data_c = trans_data_c %>% mutate(prob2 = round(frequency/sum(trans_data_c$frequency), digits=3))

# collapse the data for discrete time
df_slim_d = df_slim %>% filter(time == 'Discrete')
uniquetrans = unique(df_slim_d$trans_type)
length = rep(NA, length(uniquetrans))
trans_data_d = data.frame(
  trans0 = length, trans1 = length, trans2 = length,
  duration1 = length, duration2 = length,
  trans_type = length, frequency = length)

for (i in 1:length(uniquetrans)){
  df_trans = df_slim_d %>% filter(trans_type == uniquetrans[i])
  trans_data_d$trans0[i] = df_trans$type[1]
  trans_data_d$trans1[i] = df_trans$trans1[1]
  trans_data_d$trans2[i] = df_trans$trans2[1]
  trans_data_d$duration1[i] = mean(df_trans$period1 - df_trans$period)
  trans_data_d$duration2[i] = mean(df_trans$period2 - df_trans$period1)
  trans_data_d$trans_type[i] = uniquetrans[i]
  trans_data_d$frequency[i] = length(df_trans$tick)
}
trans_data_d = trans_data_d %>% mutate(prob = round(frequency/sum(trans_data_d$frequency), digits=3))
trans_data_d = trans_data_d %>% arrange(desc(trans_data_d$prob))

# select the transitions between Nash
trans_data_d = trans_data_d %>% filter(trans0 == 'Nash(A,a)' | trans0 == 'Nash(B,b)')
trans_data_d = trans_data_d %>% mutate(prob1 = round(frequency/sum(trans_data_d$frequency), digits=3))
trans_data_d = trans_data_d %>% filter(trans2 == 'Nash(A,a)' | trans2 == 'Nash(B,b)')
trans_data_d = trans_data_d %>% mutate(prob2 = round(frequency/sum(trans_data_d$frequency), digits=3))

# collect all the data and make the final table for comparing three transitional dynamics
transition_matrix = matrix(0, nrow = 2, ncol = 4)
rownames(transition_matrix) = c('Continuous', 'Discrete')
colnames(transition_matrix) = c('Disadvantaged', 'Advantaged', 'Direct', 'Fail')

# fill out the table for continuous time row 1
subset = filter(trans_data_c, trans0!=trans2 & trans1=='Aggressive')
transition_matrix[1,1] = sum(subset$prob2)
subset = filter(trans_data_c, trans0!=trans2 & trans1=='Accommodate')
transition_matrix[1,2] = sum(subset$prob2)
subset = filter(trans_data_c, trans1=='Nash(A,a)' | trans1=='Nash(B,b)')
transition_matrix[1,3] = sum(subset$prob2)
transition_matrix[1,4] = 1 - sum(transition_matrix[1,1:3])

# fill out the table for discrete time row 2
subset = filter(trans_data_d, trans0!=trans2 & trans1=='Aggressive')
transition_matrix[2,1] = sum(subset$prob2)
subset = filter(trans_data_d, trans0!=trans2 & trans1=='Accommodate')
transition_matrix[2,2] = sum(subset$prob2)
subset = filter(trans_data_d, trans1=='Nash(A,a)' | trans1=='Nash(B,b)')
transition_matrix[2,3] = sum(subset$prob2)
transition_matrix[2,4] = 1 - sum(transition_matrix[2,1:3])

# get table output and remove table
xtable(transition_matrix, digits = 2, label = 'two_step_transition', align = 'lcccc')
rm(df_slim, df_slim_c, df_slim_d, df_trans, subset)
rm(trans_data_c, trans_data_d, transition_matrix)