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

# make up added columns
df = df %>% mutate(
  communication = 0,
  signal_exist = FALSE,
  signal_freq = 10,
  signaltwo_exist = FALSE,
  signaltwo_freq = 10,
  signalthree_exist = FALSE
)

# add new sessions after revision
df_9 = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_bar_4_8.csv", header = T)
df_9 = df_9 %>% mutate(sequence = 1)
df_9$round = df_9$subsession_id
df_10 = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_bar_4_16_1.csv", header = T)
df_10 = df_10 %>% mutate(sequence = 2)
df_10$round = as.double(substring(df_10$subsession_id, 3, 4))
df_11 = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_bar_4_16_2.csv", header = T)
df_11 = df_11 %>% mutate(sequence = 2)
df_11$round = as.double(substring(df_11$subsession_id, 3, 4))
df_12 = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_bar_4_20.csv", header = T)
df_12 = df_12 %>% mutate(sequence = 1)
df_12$round = df_12$subsession_id

df = rbind(df, df_9, df_10, df_11, df_12)
df = arrange(df, df$session_code, df$subsession_id, df$id_in_subsession, df$tick)
rm(df_9, df_10, df_11, df_12)

# create pair id
df$pair_id = paste(df$p1_code, df$p2_code, sep = "_")
df$round_pair_id = paste(df$round, df$pair_id,  sep = "_")
df$session_round_pair_id = paste(df$session_code, df$round_pair_id, sep = "_")

# drop first 1 second and first 3 games
df = filter(df, tick > 3 | num_subperiods != 0)
df = filter(df, round > 3)

# add treatment variables
df = df %>% mutate(game = ifelse(payoff1Bb == 280, "BoS1.4", ifelse(payoff1Bb == 160, "BoS2.5", "BoS10")))
df = df %>% mutate(time = ifelse(num_subperiods!=0, 'Discrete', ifelse(signalthree_exist==TRUE, 'Hybrid', 'Continuous')))
df$treatment = paste(df$time, df$game, sep = '_')
df = df %>% mutate(BoS1.4 = ifelse(payoff1Bb == 280,1,0))
df = df %>% mutate(BoS2.5 = ifelse(payoff1Bb == 160,1,0))
df = df %>% mutate(BoS10 = ifelse(payoff1Bb == 40,1,0))
df = df %>% mutate(continuous = ifelse(time == 'Continuous', 1, 0))
df = df %>% mutate(hybrid = ifelse(time == 'Hybrid', 1, 0))

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

# create unique ids and pairs
uniquepairs = unique(df$session_round_pair_id)
uniquetime = unique(df$time)
gametype = unique(df$game)
treatmenttype = unique(df$treatment)
uniquePlayer = union(unique(df$p1_code), unique(df$p2_code))

# get state output file
df_stata = df
df_stata = df_stata %>% rename(BoS1_4 = BoS1.4, BoS2_5 = BoS2.5)
write_dta(df_stata, "D:/Dropbox/Working Papers/Continuous Time BOS/data/stata_bos.dta")

# generate pair level dataset
df_2 = df
#df_2 = filter(df, period > 10)
length = rep(NA, length(uniquepairs))
pair_summary = data.frame(session_round_pair_id = length, treatment = length, coordinate = length,
                          game = length, time = length, sequence = length, block = length, session = length,
                          nash_total = length, nash_diff = length, type = length, round = length)

# loop over pairs for classification
for (i in 1:length(uniquepairs)){
  df_pair = filter(df_2, session_round_pair_id == uniquepairs[i])
  
  # update treatment info
  pair_summary$session[i] = df_pair$session_code[1]
  pair_summary$session_round_pair_id[i] = df_pair$session_round_pair_id[1]
  pair_summary$treatment[i] = df_pair$treatment[1]
  pair_summary$game[i] = df_pair$game[1]
  pair_summary$time[i] = df_pair$time[1]
  pair_summary$sequence[i] = df_pair$sequence[1]
  pair_summary$block[i] = df_pair$block[1]
  pair_summary$round[i] = df_pair$round[1]
  pair_summary$coordinate[i] = mean(df_pair$coordinate)
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

# recreate frequency dataset
pair_summary = pair_summary %>% mutate(alternating = ifelse(type=='alternating', 1, 0))
pair_summary = pair_summary %>% mutate(one_ne = ifelse(type=='one NE', 1, 0))
pair_summary = pair_summary %>% mutate(others = ifelse(type=='others', 1, 0))

# export stata data
write_dta(pair_summary, "D:/Dropbox/Working Papers/Continuous Time BOS/data/stata_bos_pair_full.dta")
rm(df_pair, df_2)


##### Treatment effect: Order effect at pair level #####
# create summary table
ordereffect = matrix(NA, nrow = length(uniquetime), ncol = 3)
rownames(ordereffect) = uniquetime
colnames(ordereffect) = c('Sequence 1', 'Sequence 2', 'S1-S2')

# loop over treatments for differences
for (i in 1:length(uniquetime)){
  df_treat = filter(pair_summary, time == uniquetime[i])
  df_treat_1 = filter(df_treat, sequence==1)
  df_treat_2 = filter(df_treat, sequence==2)
  
  ordereffect[i,1] = mean(df_treat_1$coordinate)
  ordereffect[i,2] = mean(df_treat_2$coordinate)
  ordereffect[i,3] = mean(df_treat_1$coordinate) - mean(df_treat_2$coordinate)
  
  # wilcox-test for the comparison
  test = wilcox.test(df_treat_1$coordinate, df_treat_2$coordinate, alternative = 'two.sided', 
                     mu = 0, conf.level = 0.95, paired = FALSE)
  print(test$p.value)
}

# table output
xtable(ordereffect, digits = 3, label = 'order_effect', align = 'lccc')
rm(df_treat, df_treat_1, df_treat_2, ordereffect, test)


##### Treatment effect: Coordination learning within each super game #####
# build six treatments over time dataset
df_treat = list()
for (i in 1:length(uniquetime)){
  df_temp = filter(df, time == uniquetime[i])
  df_treat[[i]] = data.frame(
    coordinate_rate = tapply(df_temp$coordinate, df_temp$period, mean),
    period = tapply(df_temp$period, df_temp$period, mean)
  )
}

# DTW distance
d = dtw(df_treat[[1]]$coordinate_rate, df_treat[[2]]$coordinate_rate)
d$normalizedDistance
d = dtw(df_treat[[1]]$coordinate_rate, df_treat[[3]]$coordinate_rate)
d$normalizedDistance
d = dtw(df_treat[[2]]$coordinate_rate, df_treat[[3]]$coordinate_rate)
d$normalizedDistance

# set up plot
title = 'coordination_within_supergames_short'
file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/writeup_ContinuousBOS/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 800, height = 500)
pic = ggplot() +
  geom_line(data=df_treat[[1]], aes(x=period, y=coordinate_rate, colour='blue')) +
  geom_line(data=df_treat[[2]], aes(x=period, y=coordinate_rate, colour='red')) +
  geom_line(data=df_treat[[3]], aes(x=period, y=coordinate_rate, colour='black')) +
  scale_x_discrete(name='period', waiver(), limits=c(0,20)) +
  scale_y_continuous(name='coordination rate', limits=c(0.3,1)) +
  theme_bw() + 
  scale_colour_manual(values=c('black','blue','red'), labels=c('hybrid','continuous','discrete')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

rm(df_temp, df_treat, pic, d)


##### Treatment effect: learning between super games #####
# create summary table
learning = matrix(NA, nrow = length(uniquetime), ncol = 3)
rownames(learning) = uniquetime
colnames(learning) = c('Game4-9', 'Game10-15', '(G10-15)-(G4-9)')

# loop over treatments for differences
for (i in 1:length(uniquetime)){
  df_treat = filter(pair_summary, time == uniquetime[i])
  df_treat_1 = filter(df_treat, round < 10)
  df_treat_2 = filter(df_treat, round >= 10)
  
  learning[i,1] = mean(df_treat_1$coordinate)
  learning[i,2] = mean(df_treat_2$coordinate)
  learning[i,3] = mean(df_treat_2$coordinate) - mean(df_treat_1$coordinate)
  
  # wilcox-test for the comparison
  test = wilcox.test(df_treat_1$coordinate, df_treat_2$coordinate, alternative = 'two.sided', 
                     mu = 0, conf.level = 0.95, paired = FALSE)
  print(test$p.value)
}

# table output
xtable(learning, digits = 3, label = 'learn_between', align = 'lccc')
rm(df_treat, df_treat_1, df_treat_2, test, learning)


##### Mechanisms: Pair-level classification #####
# generate distribution table
sum_pair = matrix(NA, nrow = 3, ncol = 2)
rownames(sum_pair) = uniquetime
colnames(sum_pair) = c('Alternating', 'One NE')
for (i in 1:length(uniquetime)){
  df_treat = filter(pair_summary, time == uniquetime[i])
  sum_pair[i,1] = mean(df_treat$alternating)
  sum_pair[i,2] = mean(df_treat$one_ne)
}

# add comparison and statistical testing result
df_c = filter(pair_summary, time == 'Continuous')
df_d = filter(pair_summary, time == 'Discrete')
df_h = filter(pair_summary, time == 'Hybrid')

sum_pair2 = matrix(NA, nrow = 3, ncol = 2)
rownames(sum_pair2) = c('C-D', 'C-H', 'D-H')

sum_pair2[1,1] = sum_pair[1,1] - sum_pair[2,1]
sum_pair2[1,2] = sum_pair[1,2] - sum_pair[2,2]
test = wilcox.test(df_c$alternating, df_d$alternating, alternative = 'two.sided', 
                   mu = 0, conf.level = 0.95, paired = FALSE)
print(test$p.value)
test = wilcox.test(df_c$one_ne, df_d$one_ne, alternative = 'two.sided', 
                   mu = 0, conf.level = 0.95, paired = FALSE)
print(test$p.value)

sum_pair2[2,1] = sum_pair[1,1] - sum_pair[3,1]
sum_pair2[2,2] = sum_pair[1,2] - sum_pair[3,2]
test = wilcox.test(df_c$alternating, df_h$alternating, alternative = 'two.sided', 
                   mu = 0, conf.level = 0.95, paired = FALSE)
print(test$p.value)
test = wilcox.test(df_c$one_ne, df_h$one_ne, alternative = 'two.sided', 
                   mu = 0, conf.level = 0.95, paired = FALSE)
print(test$p.value)

sum_pair2[3,1] = sum_pair[2,1] - sum_pair[3,1]
sum_pair2[3,2] = sum_pair[2,2] - sum_pair[3,2]
test = wilcox.test(df_d$alternating, df_h$alternating, alternative = 'two.sided', 
                   mu = 0, conf.level = 0.95, paired = FALSE)
print(test$p.value)
test = wilcox.test(df_d$one_ne, df_h$one_ne, alternative = 'two.sided', 
                   mu = 0, conf.level = 0.95, paired = FALSE)
print(test$p.value)

sum_pair = rbind(sum_pair, sum_pair2)

# table output
xtable(sum_pair, digits = 3, label = 'pair_class', align = 'lcc')
rm(sum_pair, sum_pair2, test, df_c, df_d, df_h, df_treat)


##### Mechanisms: Pair classification showcase #####
# further count the size of each group
pair_summary$group = paste(pair_summary$time, pair_summary$nash_total, pair_summary$nash_diff, sep = '_')
uniquegroup = unique(pair_summary$group)
length = rep(NA, length(uniquegroup))
df_group = data.frame(time = length, count = length, nash_total = length, nash_diff = length)
for (i in 1:length(uniquegroup)){
  pair_group = filter(pair_summary, group == uniquegroup[i])
  df_group$time[i] = pair_group$time[1]
  df_group$nash_total[i] = pair_group$nash_total[1]
  df_group$nash_diff[i] = pair_group$nash_diff[1]
  df_group$count[i] = length(pair_group$group)
}

# draw scatter plot 1
title = 'classification_scatter_cluster'
file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/writeup_ContinuousBOS/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 600, height = 500)
pic = ggplot(data = df_group) +
  geom_rect(aes(xmin = 0.8, ymin = 0, xmax = 1, ymax = 0.2, fill='green'), color='green') +
  geom_rect(aes(xmin = 0.8, ymin = 0.2, xmax = 1, ymax = 1, fill='yellow'), color='yellow') +
  geom_point(aes(x=nash_total, y=nash_diff, colour=time, size=count)) +
  scale_x_continuous(name='Efficiency: total time at Nash', waiver(), limits=c(-0.001,1.001), breaks=c(0,0.2,0.4,0.6,0.8,1)) +
  scale_y_continuous(name='Unfairness: time difference in two pure Nash', waiver(), limits=c(-0.001,1.001), breaks=c(0,0.2,0.4,0.6,0.8,1)) +
  theme_bw() + 
  scale_colour_manual(values=c('blue','red','black'), 
                      labels=c('continuous','discrete','hybrid')) +
  scale_fill_manual(values=c('green','yellow'), 
                    labels=c('Alternating zone','One NE zone')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

# draw scatter plot 2
title = 'classification_scatter_cluster_1'
file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/writeup_ContinuousBOS/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 600, height = 500)
pic = ggplot(data = df_group) +
  geom_rect(aes(xmin = 0.8, ymin = 0, xmax = 1, ymax = 0.2, fill='green'), color='green') +
  geom_rect(aes(xmin = 0.8, ymin = 0.2, xmax = 1, ymax = 1, fill='yellow'), color='yellow') +
  scale_x_continuous(name='Efficiency: total time at Nash', waiver(), limits=c(-0.001,1.001), breaks=c(0,0.2,0.4,0.6,0.8,1)) +
  scale_y_continuous(name='Unfairness: time difference in two pure Nash', waiver(), limits=c(-0.001,1.001), breaks=c(0,0.2,0.4,0.6,0.8,1)) +
  theme_bw() + 
  scale_colour_manual(values=c('blue','red'), 
                      labels=c('continuous','discrete')) +
  scale_fill_manual(values=c('green','yellow'), 
                    labels=c('Alternating zone','One NE zone')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

# remove temporary df
rm(df_2, df_group, df_pair, pair_group, pair_summary, pic)


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
df_c = filter(df_stay, time == 'Continuous')
df_d = filter(df_stay, time == 'Discrete')
df_h = filter(df_stay, time == 'Hybrid')

# set up plot
title = 'distribution_duration_nash_short'
file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/writeup_ContinuousBOS/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 700, height = 300)
pic = ggplot() +
  stat_ecdf(geom="step", data=df_c, aes(x=length_period, colour='blue')) +
  stat_ecdf(geom="step", data=df_d, aes(x=length_period, colour='red')) +
  stat_ecdf(geom="step", data=df_h, aes(x=length_period, colour='black')) +
  scale_x_continuous(name='duration(period)', waiver(), limits=c(0,10), breaks = c(0,2,4,6,8,10)) +
  scale_y_continuous(name='density') +
  theme_bw() + 
  scale_colour_manual(values=c('black','blue','red'), labels=c('hybrid','continuous','discrete')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

# K-S test
ks.test(df_c$length_period, df_d$length_period)
ks.test(df_c$length_period, df_h$length_period)
ks.test(df_d$length_period, df_h$length_period)

rm(df_c, df_d, df_h)
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
df_c = filter(df_stay, time == 'Continuous')
df_d = filter(df_stay, time == 'Discrete')
df_h = filter(df_stay, time == 'Hybrid')

# set up plot
title = 'distribution_duration_mismatch_short'
file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/writeup_ContinuousBOS/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 700, height = 300)
pic = ggplot() +
  stat_ecdf(geom="step", data=df_c, aes(x=length_period, colour='blue')) +
  stat_ecdf(geom="step", data=df_d, aes(x=length_period, colour='red')) +
  stat_ecdf(geom="step", data=df_h, aes(x=length_period, colour='black')) +
  scale_x_continuous(name='duration(period)', waiver(), limits=c(0,10), breaks = c(0,2,4,6,8,10)) +
  scale_y_continuous(name='density') +
  theme_bw() + 
  scale_colour_manual(values=c('black','blue','red'), labels=c('hybrid','continuous','discrete')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

# K-S test
ks.test(df_c$length_period, df_d$length_period)
ks.test(df_c$length_period, df_h$length_period)
ks.test(df_d$length_period, df_h$length_period)

rm(df_c, df_d, df_h)
rm(df_length, df_char, df_pair, df_stay, pic)


##### Mechanisms: First time reach NE #####
length = rep(NA, length(uniquepairs))
pair_summary1 = data.frame(session_round_pair_id = length, treatment = length, 
                           game = length, time = length, timing = length)
  
# loop over pairs for classification
for (i in 1:length(uniquepairs)){
  df_pair = filter(df, session_round_pair_id == uniquepairs[i])
  
  # update treatment info
  pair_summary1$session_round_pair_id[i] = df_pair$session_round_pair_id[1]
  pair_summary1$treatment[i] = df_pair$treatment[1]
  pair_summary1$game[i] = df_pair$game[1]
  pair_summary1$time[i] = df_pair$time[1]
  
  # update first Nash timing
  for (j in 1:length(df_pair$tick)){
    if (df_pair$coordinate[j] == 1){
      pair_summary1$timing[i] = df_pair$period[j]
      break
    }
    else{next}
  }
}

# draw distribution plot
df_c = filter(pair_summary1, time == 'Continuous')
df_d = filter(pair_summary1, time == 'Discrete')
df_h = filter(pair_summary1, time == 'Hybrid')

# set up plot
title = 'timing_first_ne_short'
file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/writeup_ContinuousBOS/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 700, height = 300)
pic = ggplot() +
  stat_ecdf(geom="step", data=df_c, aes(x=timing, colour='blue')) + 
  stat_ecdf(geom="step", data=df_d, aes(x=timing, colour='red')) +
  stat_ecdf(geom="step", data=df_h, aes(x=timing, colour='black')) +
  scale_x_continuous(name='timing(period)', waiver(), limits=c(0,10), breaks = c(0,2,4,6,8,10)) +
  scale_y_continuous(name='density') +
  theme_bw() + 
  scale_colour_manual(values=c('black','blue','red'), labels=c('hybrid','continuous','discrete')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

# K-S test
ks.test(df_c$timing, df_d$timing)
ks.test(df_c$timing, df_h$timing)
ks.test(df_d$timing, df_h$timing)

rm(df_c, df_d, df_h, df_pair, pair_summary1, pic)


##### (not use) Mechanisms: Payoff inequality #####
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
for (i in 1:length(uniquetime)){
  df_temp = filter(df_new, time == uniquetime[i])
  df_treat[[i]] = data.frame(
    payoff_diff = tapply(df_temp$payoff_diff, df_temp$period, mean),
    period = tapply(df_temp$period, df_temp$period, mean)
  )
}

# DTW distance
d = dtw(df_treat[[1]]$payoff_diff, df_treat[[2]]$payoff_diff)
d$normalizedDistance

# set up plot
title = 'inequality_within_supergames_short'
file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/writeup_ContinuousBOS/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 800, height = 500)
pic = ggplot() +
  geom_line(data=df_treat[[1]], aes(x=period, y=payoff_diff, colour='blue')) +
  geom_line(data=df_treat[[2]], aes(x=period, y=payoff_diff, colour='red')) +
  scale_x_discrete(name='period', waiver(), limits=c(0,20)) +
  scale_y_continuous(name='difference of cumulative payoff') +
  theme_bw() + 
  scale_colour_manual(values=c('blue','red'), labels=c('continuous','discrete')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

rm(df_temp, df_treat, df_pair, pic, d)
