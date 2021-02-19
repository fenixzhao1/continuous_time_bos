##### Individual analysis for each session #####
# load packages
library(ggplot2) # overwrite heatmap in latticeExtra
library(dplyr)

# load data
full_data = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_new_2_17_2.csv", header = T)
full_data$round = as.double(substring(full_data$subsession_id, 2, 3))
#full_data$round = full_data$subsession_id
full_data = arrange(full_data, full_data$session_code, full_data$subsession_id, full_data$id_in_subsession, full_data$tick)

# create pair id
full_data$pair_id = paste(full_data$p1_code, full_data$p2_code, sep = "_")
full_data$round_pair_id = paste(full_data$round, full_data$pair_id,  sep = "_")
full_data$session_round_pair_id = paste(full_data$session_code, full_data$round_pair_id, sep = "_")

# drop first 2 seconds and first 3 games
full_data = filter(full_data, tick > 3)
full_data = filter(full_data, round > 2)

# create unique ids and pairs
uniquepairs = unique(full_data$session_round_pair_id)
uniqueplayer = union(unique(full_data$p1_code), unique(full_data$p2_code))

# pair level dynamics
full_data = full_data %>% mutate(p2_strategy_jitter = p2_strategy + 0.01)

# loop over pairs
for (i in 1:length(uniquepairs)){
  pairdata = subset(full_data, session_round_pair_id == uniquepairs[i])
  
  title = paste(as.character(uniquepairs[i]))
  file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/data/figures_pair/2_17_2/", title, sep = "")
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


##### Data preparation #####
# load packages
library(ggplot2)
library(dplyr)
library(haven)
library(xtable)
library(dtw)

# load session data
df_1 = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_new_2_17_1.csv", header = T)
df_1$round = df_1$subsession_id
df_2 = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_new_2_17_2.csv", header = T)
df_2$round = as.double(substring(df_2$subsession_id, 2, 3))

# load chat data
df_chat = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_new_2_17_2_chat.csv", header = T)

# combine all session data
df = rbind(df_1, df_2)
df = arrange(df, df$session_code, df$subsession_id, df$id_in_subsession, df$tick)
rm(df_1, df_2)

# create pair id
df$pair_id = paste(df$p1_code, df$p2_code, sep = "_")
df$round_pair_id = paste(df$round, df$pair_id,  sep = "_")
df$session_round_pair_id = paste(df$session_code, df$round_pair_id, sep = "_")

# drop first 2 seconds and first 2 practice games
df = filter(df, tick > 3 | num_subperiods != 0)
df = filter(df, round > 2)

# add payoff variables
df = df %>% mutate(p1_payoff = payoff1Aa*p1_strategy*p2_strategy + payoff1Ab*p1_strategy*(1-p2_strategy) + payoff1Ba*(1-p1_strategy)*p2_strategy + payoff1Bb*(1-p1_strategy)*(1-p2_strategy),
                   p2_payoff = payoff2Aa*p1_strategy*p2_strategy + payoff2Ab*p1_strategy*(1-p2_strategy) + payoff2Ba*(1-p1_strategy)*p2_strategy + payoff2Bb*(1-p1_strategy)*(1-p2_strategy))

# add treatment variables
df = df %>% mutate(time = ifelse(num_subperiods==0, 'Continuous', 'Discrete'),
                   continuous = ifelse(num_subperiods==0, 1, 0),
                   tool_type = ifelse(communication==2, 'Freetext', ifelse(communication==1, 'Messaging', ifelse(signal_exist==TRUE, 'Signal', 'None'))),
                   freetext = ifelse(communication==2, 1, 0),
                   messaging = ifelse(communication==1, 1, 0),
                   signal = ifelse(signal_exist==TRUE, 1, 0))
df$treatment = paste(df$time, df$tool_type, sep = '_')

# add learning dummies
df = df %>% mutate(block = ifelse(round <= 7, 1, 2))
df = df %>% mutate(period = tick+1)

# add behaivoral type dummies
df = df %>% mutate(type = NA)
df = df %>% mutate(type_num = NA)
for(m in 1:length(df$tick)){
  if(df$p1_strategy[m]==1 & df$p2_strategy[m]==1){
    df$type[m]="RPNE"
    df$type_num[m]=2}
  if(df$p1_strategy[m]==0 & df$p2_strategy[m]==0){
    df$type[m]="CPNE"
    df$type_num[m]=3}
  if(df$p1_strategy[m]==0 & df$p2_strategy[m]==1){
    df$type[m]="Accommodate"
    df$type_num[m]=4}
  if(df$p1_strategy[m]==1 & df$p2_strategy[m]==0){
    df$type[m]="Aggressive"
    df$type_num[m]=1}
  #if(df$num_subperiods[m]==0){df$period[m] = round((df$tick[m]+1)/12,digits=2)}
}
df = df %>% mutate(Nash_rpne = ifelse(type=='RPNE', 1, 0))
df = df %>% mutate(Nash_cpne = ifelse(type=='CPNE', 1, 0))
df = df %>% mutate(Mismatch_accommodate = ifelse(type=='Accommodate', 1, 0))
df = df %>% mutate(Mismatch_aggressive = ifelse(type=='Aggressive', 1, 0))
df = df %>% mutate(coordinate = Nash_rpne + Nash_cpne)
df = df %>% mutate(mismatch = Mismatch_accommodate + Mismatch_aggressive)

# create unique ids and pairs
uniquepairs = unique(df$session_round_pair_id)
treatmenttype = unique(df$treatment)
uniquePlayer = union(unique(df$p1_code), unique(df$p2_code))

# get state output file
write_dta(df, "D:/Dropbox/Working Papers/Continuous Time BOS/data/stata_bos_new.dta")


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
summary = matrix(NA, nrow = 2, ncol = 4)
rownames(summary) = c('coor_rate', 'diff')
colnames(summary) = c('Control', 'Free Text', 'Messaging', 'Signal')

# set up sub dataset
df_control = filter(df, treatment == 'Continuous_None')
df_freetext = filter(df, treatment == 'Continuous_Freetext')
df_messaging = filter(df, treatment == 'Continuous_Messaging')
df_signal = filter(df, treatment == 'Continuous_Signal')

# fill out the table
summary[1,1] = mean(df_control$coordinate)
summary[1,2] = mean(df_freetext$coordinate)
summary[1,3] = mean(df_messaging$coordinate)
summary[1,4] = mean(df_signal$coordinate)

summary[2,1] = summary[1,1] - summary[1,1]
summary[2,2] = summary[2,1] - summary[1,1]
summary[2,3] = summary[3,1] - summary[1,1]
summary[2,4] = summary[4,1] - summary[1,1]

# wilcox-test for each relations
test_freetext = t.test(df_freetext$coordinate, df_control$coordinate, 
                         alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
test_messaging = t.test(df_messaging$coordinate, df_control$coordinate, 
                         alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
test_signal = t.test(df_signal$coordinate, df_control$coordinate, 
                           alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)

# update p value for each test
print(test_freetext$p.value)
print(test_messaging$p.value)
print(test_signal$p.value)

# table output
xtable(summary, digits = 2, label = 'summary_table', align = 'lccc')
rm(df_control, df_freetext, df_messaging, df_signal)
rm(summary, test_freetext, test_messaging, test_signal)


##### Treatment effect: Coordination learning within each super game #####
# build treatments over time dataset
df_treat = list()
for (i in 1:length(treatmenttype)){
  df_temp = filter(df, treatment == treatmenttype[i])
  df_treat[[i]] = data.frame(
    coordinate_rate = tapply(df_temp$coordinate, df_temp$period, mean),
    period = tapply(df_temp$period, df_temp$period, mean)
  )
}

# DTW distance
d1 = dtw(df_treat[[2]]$coordinate_rate, df_treat[[1]]$coordinate_rate)
d2 = dtw(df_treat[[3]]$coordinate_rate, df_treat[[1]]$coordinate_rate)
d3 = dtw(df_treat[[4]]$coordinate_rate, df_treat[[1]]$coordinate_rate)
d1$normalizedDistance
d2$normalizedDistance
d3$normalizedDistance

# set up plot
title = 'stage2_coordination_within_supergames'
file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/writeup_ContinuousBOS/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 800, height = 500)
pic = ggplot() +
  geom_line(data=df_treat[[1]], aes(x=period, y=coordinate_rate, colour='blue')) +
  geom_line(data=df_treat[[2]], aes(x=period, y=coordinate_rate, colour='red')) +
  #geom_line(data=df_treat[[3]], aes(x=period, y=coordinate_rate, colour='green')) +
  #geom_line(data=df_treat[[4]], aes(x=period, y=coordinate_rate, colour='red')) +
  scale_x_discrete(name='period', waiver(), limits=c(5,50,100,150,200,240)) +
  scale_y_continuous(name='coordination rate', limits=c(0.3,1)) +
  theme_bw() + 
  #scale_colour_manual(values=c('blue','red'), labels=c('continuous','discrete')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

rm(df_temp, df_treat, pic, d1, d2, d3)


##### Treatment effect: Coordination learning between super games #####
# build treatments over time dataset
df_treat = list()
for (i in 1:length(treatmenttype)){
  df_temp = filter(df, treatment == treatmenttype[i])
  df_treat[[i]] = data.frame(
    coordinate_rate = tapply(df_temp$coordinate, df_temp$round, mean),
    round = tapply(df_temp$round, df_temp$round, mean)
  )
}

# DTW distance
d1 = dtw(df_treat[[2]]$coordinate_rate, df_treat[[1]]$coordinate_rate)
d2 = dtw(df_treat[[3]]$coordinate_rate, df_treat[[1]]$coordinate_rate)
d3 = dtw(df_treat[[4]]$coordinate_rate, df_treat[[1]]$coordinate_rate)
d1$normalizedDistance
d2$normalizedDistance
d3$normalizedDistance

# set up plot
title = 'stage2_coordination_between_supergames'
file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/writeup_ContinuousBOS/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 800, height = 500)
pic = ggplot() +
  geom_line(data=df_treat[[1]], aes(x=round, y=coordinate_rate, colour='blue')) +
  geom_line(data=df_treat[[2]], aes(x=round, y=coordinate_rate, colour='red')) +
  #geom_line(data=df_treat[[3]], aes(x=round, y=coordinate_rate, colour='green')) +
  #geom_line(data=df_treat[[4]], aes(x=round, y=coordinate_rate, colour='red')) +
  scale_x_discrete(name='period', waiver(), limits=c(3,6,9,12)) +
  scale_y_continuous(name='coordination rate', limits=c(0.3,1)) +
  theme_bw() + 
  #scale_colour_manual(values=c('blue','red'), labels=c('continuous','discrete')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

rm(df_temp, df_treat, pic, d1, d2, d3)


##### Mechanisms: Pair-level classification #####
# set up dataset and parameters
df_2 = filter(df, period > 120)
df_2 = df
length = rep(NA, length(uniquepairs))
pair_summary = data.frame(session_round_pair_id = length, treatment = length,
                          time = length, tool_type = length, type = length, block = length)

# loop over pairs for classification
for (i in 1:length(uniquepairs)){
  df_pair = filter(df_2, session_round_pair_id == uniquepairs[i])
  
  # update treatment info
  pair_summary$session_round_pair_id[i] = df_pair$session_round_pair_id[1]
  pair_summary$treatment[i] = df_pair$treatment[1]
  pair_summary$time[i] = df_pair$time[1]
  pair_summary$tool_type[i] = df_pair$tool_type[1]
  pair_summary$block[i] = df_pair$block[1]
  
  # classify turn taking and one NE dynamics
  if (mean(df_pair$coordinate) >= 0.8){
    if (abs(mean(df_pair$Nash_rpne) - mean(df_pair$Nash_cpne)) <= 0.2 )
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
write_dta(pair_summary, "D:/Dropbox/Working Papers/Continuous Time BOS/data/stata_bos_new_pair.dta")

# generate distribution table
sum_pair = matrix(NA, nrow = 4, ncol = 2)
rownames(sum_pair) = c('Control', 'Free Text', 'Messaging', 'Signal')
colnames(sum_pair) = c('Alternating', 'One NE')
for (i in 1:length(treatmenttype)){
  df_treat = filter(pair_summary, treatment == treatmenttype[i])
  sum_pair[i,1] = mean(df_treat$alternating)
  sum_pair[i,2] = mean(df_treat$one_ne)
}

# add statistical testing result
df_control = filter(pair_summary, treatment == 'Continuous_None')
df_freetext = filter(pair_summary, treatment == 'Continuous_Freetext')
df_messaging = filter(pair_summary, treatment == 'Continuous_Messaging')
df_signal = filter(pair_summary, treatment == 'Discrete_Signal')

sum_pair2 = matrix(NA, nrow = 3, ncol = 2)
rownames(sum_pair2) = c('R2-R1', 'R3-R1', 'R4-R1')

sum_pair2[1,1] = sum_pair[2,1] - sum_pair[1,1]
sum_pair2[1,2] = sum_pair[2,2] - sum_pair[1,2]
test = t.test(df_freetext$alternating, df_control$alternating, 
              alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
print(test$p.value)
test = t.test(df_freetext$one_ne, df_control$one_ne, 
              alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
print(test$p.value) 

sum_pair2[2,1] = sum_pair[3,1] - sum_pair[1,1]
sum_pair2[2,2] = sum_pair[3,2] - sum_pair[1,2]
test = t.test(df_messaging$alternating, df_control$alternating, 
              alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
print(test$p.value)
test = t.test(df_messaging$one_ne, df_control$one_ne, 
              alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
print(test$p.value) 

sum_pair2[3,1] = sum_pair[4,1] - sum_pair[1,1]
sum_pair2[3,2] = sum_pair[4,2] - sum_pair[1,2]
test = t.test(df_signal$alternating, df_control$alternating, 
              alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
print(test$p.value)
test = t.test(df_signal$one_ne, df_control$one_ne, 
              alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
print(test$p.value) 

# combine two tables
sum_pair = rbind(sum_pair, sum_pair2)

# table output
xtable(sum_pair, digits = 2, label = 'pair_class', align = 'lcc')
rm(df_2, df_pair, pair_summary, sum_pair, sum_pair2, test)
rm(df_control, df_freetext, df_messaging, df_signal, df_treat)


##### Mechanisms: Duration of staying at NE #####
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

# draw distribution plot
df_control = filter(df_stay, treatment == 'Continuous_None')
df_freetext = filter(df_stay, treatment == 'Continuous_Freetext')

# set up plot
title = 'stage2_distribution_duration_nash'
file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/writeup_ContinuousBOS/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 700, height = 300)
pic = ggplot() +
  stat_ecdf(geom="step", data=df_control, aes(x=length, colour='blue')) +
  stat_ecdf(geom="step", data=df_freetext, aes(x=length, colour='red')) +
  scale_x_continuous(name='duration(period)', waiver(), limits=c(0,150), breaks = c(0,10,20,50,100,150)) +
  scale_y_continuous(name='density') +
  theme_bw() + 
  #scale_colour_manual(values=c('blue','red'), labels=c('continuous','discrete')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

# K-S test
ks.test(df_freetext$length, df_control$length)

rm(df_control, df_freetext, pair_summary)
rm(df_length, df_char, df_pair, df_stay, pic)


##### (Not use) Mechanisms: Transition probability matrix by time environments #####
# create transition matrix
transition = matrix(0, nrow = 4, ncol = 5)
rownames(transition) = c('Aggressive at t', 'Accommodate at t', 'RPNE at t', 'CPNE at t')
colnames(transition) = c('Aggressive at t+1', 'Accommodate at t+1', 'RPNE at t+1', 'CPNE at t+1', '# of obs')

# select treatment
treatment_data = subset(df, treatment == 'Continuous_Freetext')
pairs = unique(treatment_data$session_round_pair_id)

# loop over pairs
for (i in 1:length(pairs)){
  round_data = subset(treatment_data, session_round_pair_id == pairs[i])
  
  # loop over observations
  for (j in 2:length(round_data$tick)){
    
    # when the original observation is (1,1)
    if (round_data$type[j-1] == 'RPNE'){
      if (round_data$type[j] == 'RPNE'){transition[3,3] = transition[3,3] + 1}
      if (round_data$type[j] == 'CPNE'){transition[3,4] = transition[3,4] + 1}
      if (round_data$type[j] == 'Accommodate'){transition[3,2] = transition[3,2] + 1}
      if (round_data$type[j] == 'Aggressive'){transition[3,1] = transition[3,1] + 1}
    }
    
    # when the original observation is (0,0)
    if (round_data$type[j-1] == 'CPNE'){
      if (round_data$type[j] == 'RPNE'){transition[4,3] = transition[4,3] + 1}
      if (round_data$type[j] == 'CPNE'){transition[4,4] = transition[4,4] + 1}
      if (round_data$type[j] == 'Accommodate'){transition[4,2] = transition[4,2] + 1}
      if (round_data$type[j] == 'Aggressive'){transition[4,1] = transition[4,1] + 1}
    }
    
    # when the original observation is (1,0)
    if (round_data$type[j-1] == 'Aggressive'){
      if (round_data$type[j] == 'RPNE'){transition[1,3] = transition[1,3] + 1}
      if (round_data$type[j] == 'CPNE'){transition[1,4] = transition[1,4] + 1}
      if (round_data$type[j] == 'Accommodate'){transition[1,2] = transition[1,2] + 1}
      if (round_data$type[j] == 'Aggressive'){transition[1,1] = transition[1,1] + 1}
    }
    
    # when the original observation is (0,1)
    if (round_data$type[j-1] == 'Accommodate'){
      if (round_data$type[j] == 'RPNE'){transition[2,3] = transition[2,3] + 1}
      if (round_data$type[j] == 'CPNE'){transition[2,4] = transition[2,4] + 1}
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


##### (Not use) Extension: two-step transition #####
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
df_slim_c = df_slim %>% filter(treatment == 'Continuous_Freetext')
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
trans_data_c = trans_data_c %>% filter(trans0 == 'RPNE' | trans0 == 'CPNE')
trans_data_c = trans_data_c %>% mutate(prob1 = round(frequency/sum(trans_data_c$frequency), digits=3))
trans_data_c = trans_data_c %>% filter(trans2 == 'RPNE' | trans2 == 'CPNE')
trans_data_c = trans_data_c %>% mutate(prob2 = round(frequency/sum(trans_data_c$frequency), digits=3))

# collect all the data and make the final table for comparing three transitional dynamics
transition_matrix = matrix(0, nrow = 1, ncol = 4)
rownames(transition_matrix) = c('Continuous')
colnames(transition_matrix) = c('Disadvantaged', 'Advantaged', 'Direct', 'Fail')

# fill out the table for continuous time row 1
subset = filter(trans_data_c, trans0!=trans2 & trans1=='Aggressive')
transition_matrix[1,1] = sum(subset$prob2)
subset = filter(trans_data_c, trans0!=trans2 & trans1=='Accommodate')
transition_matrix[1,2] = sum(subset$prob2)
subset = filter(trans_data_c, trans1=='RPNE' | trans1=='CPNE')
transition_matrix[1,3] = sum(subset$prob2)
transition_matrix[1,4] = 1 - sum(transition_matrix[1,1:3])

# get table output and remove table
xtable(transition_matrix, digits = 2, label = 'two_step_transition', align = 'lcccc')
rm(df_slim, df_slim_c, df_slim_d, df_trans, subset)
rm(trans_data_c, trans_data_d, transition_matrix)