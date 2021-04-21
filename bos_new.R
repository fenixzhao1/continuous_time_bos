##### Individual analysis for each session #####
# load packages
library(ggplot2) # overwrite heatmap in latticeExtra
library(dplyr)

# load data
full_data = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_bar_4_16_2.csv", header = T)
full_data$round = as.double(substring(full_data$subsession_id, 3, 4))
#full_data$round = full_data$subsession_id
full_data = arrange(full_data, full_data$session_code, full_data$subsession_id, full_data$id_in_subsession, full_data$tick)

# create pair id
full_data$pair_id = paste(full_data$p1_code, full_data$p2_code, sep = "_")
full_data$round_pair_id = paste(full_data$round, full_data$pair_id,  sep = "_")
full_data$session_round_pair_id = paste(full_data$session_code, full_data$round_pair_id, sep = "_")

# drop first 2 seconds and first 3 games
full_data = filter(full_data, tick > 3)
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
  file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/data/figures_pair/4_16_2/", title, sep = "")
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

rm(full_data, pairdata)


##### Data preparation #####
# load packages
library(ggplot2)
library(dplyr)
library(haven)
library(xtable)
library(dtw)

# load session data
df_1 = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_new_2_17_1.csv", header = T, stringsAsFactors = FALSE)
df_1$round = df_1$subsession_id
df_2 = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_new_2_17_2.csv", header = T, stringsAsFactors = FALSE)
df_2$round = as.double(substring(df_2$subsession_id, 2, 3))
df_3 = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_new_2_25_1.csv", header = T, stringsAsFactors = FALSE)
df_3$round = as.double(substring(df_3$subsession_id, 3, 4))
df_4 = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_new_2_25_2.csv", header = T, stringsAsFactors = FALSE)
df_4$round = as.double(substring(df_4$subsession_id, 3, 4))
df_5 = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_new_3_9.csv", header = T, stringsAsFactors = FALSE)
df_5$round = as.double(substring(df_5$subsession_id, 2, 3))

# load chat data
df_chat = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_new_2_17_2_chat.csv", header = T, stringsAsFactors = FALSE)
df_message_1 = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_new_2_25_2_message.csv", header = T, stringsAsFactors = FALSE)
df_message_2 = read.csv("D:/Dropbox/Working Papers/Continuous Time BOS/data/bos_new_3_9_message.csv", header = T, stringsAsFactors = FALSE)

# combine all session data
df = rbind(df_1, df_2, df_3, df_4, df_5)
df = arrange(df, df$session_code, df$subsession_id, df$id_in_subsession, df$tick)
rm(df_1, df_2, df_3, df_4, df_5)

# create pair id
df$pair_id = paste(df$p1_code, df$p2_code, sep = "_")
df$round_pair_id = paste(df$round, df$pair_id,  sep = "_")
df$session_round_pair_id = paste(df$session_code, df$round_pair_id, sep = "_")

# drop first 2 seconds and first 2 practice games
df = filter(df, tick > 3 | num_subperiods != 0)
df = filter(df, round > 2)

# drop missing data caused by the server error
df = filter(df, is.nan(p1_strategy) == FALSE & is.nan(p2_strategy) == FALSE)

# add payoff variables
df = df %>% mutate(p1_payoff = payoff1Aa*p1_strategy*p2_strategy + payoff1Ab*p1_strategy*(1-p2_strategy) + payoff1Ba*(1-p1_strategy)*p2_strategy + payoff1Bb*(1-p1_strategy)*(1-p2_strategy),
                   p2_payoff = payoff2Aa*p1_strategy*p2_strategy + payoff2Ab*p1_strategy*(1-p2_strategy) + payoff2Ba*(1-p1_strategy)*p2_strategy + payoff2Bb*(1-p1_strategy)*(1-p2_strategy))

# add treatment variables
df = df %>% mutate(time = ifelse(num_subperiods==0, 'Continuous', 'Discrete'),
                   continuous = ifelse(num_subperiods==0, 1, 0),
                   tool_type = ifelse(communication==2, 'Chatbox', ifelse(communication==1&signal_exist==TRUE, 'Message_Signal', ifelse(communication==1, 'Message', ifelse(signal_exist==TRUE, 'Signal', 'Control')))),
                   chatbox = ifelse(communication==2, 1, 0),
                   message = ifelse(communication==1, 1, 0),
                   signal = ifelse(signal_exist==TRUE, 1, 0))
df$treatment = paste(df$time, df$tool_type, sep = '_')

# add learning dummies
df = df %>% mutate(block = ifelse(round <= 7, 1, 2))
df = df %>% mutate(period = tick+1)
df = df %>% mutate(tick_second = round(period/2, digits = 1))

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
summary = matrix(NA, nrow = 2, ncol = 5)
rownames(summary) = c('coor_rate', 'diff')
colnames(summary) = c('Control', 'Chatbox', 'Message', 'Signal', 'Message_Signal')

# set up sub dataset
df_control = filter(df, treatment == 'Continuous_Control')
df_freetext = filter(df, treatment == 'Continuous_Chatbox')
df_messaging = filter(df, treatment == 'Continuous_Message')
df_signal = filter(df, treatment == 'Continuous_Signal')
df_message_signal = filter(df, treatment == 'Continuous_Message_Signal')

# fill out the table
summary[1,1] = mean(df_control$coordinate, na.rm = TRUE)
summary[1,2] = mean(df_freetext$coordinate, na.rm = TRUE)
summary[1,3] = mean(df_messaging$coordinate, na.rm = TRUE)
summary[1,4] = mean(df_signal$coordinate, na.rm = TRUE)
summary[1,5] = mean(df_message_signal$coordinate, na.rm = TRUE)

summary[2,1] = summary[1,1] - summary[1,1]
summary[2,2] = summary[1,2] - summary[1,1]
summary[2,3] = summary[1,3] - summary[1,1]
summary[2,4] = summary[1,4] - summary[1,1]
summary[2,5] = summary[1,5] - summary[1,1]

# wilcox-test for each relations
test_freetext = t.test(df_freetext$coordinate, df_control$coordinate, 
                         alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
test_messaging = t.test(df_messaging$coordinate, df_control$coordinate, 
                         alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
test_signal = t.test(df_signal$coordinate, df_control$coordinate, 
                     alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
test_message_signal = t.test(df_message_signal$coordinate, df_control$coordinate, 
                             alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)

# update p value for each test
print(test_freetext$p.value)
print(test_messaging$p.value)
print(test_signal$p.value)
print(test_message_signal$p.value)

# table output
xtable(summary, digits = 2, label = 'summary_table', align = 'lcccc')
rm(df_control, df_freetext, df_messaging, df_signal, df_message_signal)
rm(summary, test_freetext, test_messaging, test_signal, test_message_signal)


##### Treatment effect: Coordination learning within each super game #####
# build treatments over time dataset
df_treat = list()
for (i in 1:length(treatmenttype)){
  df_temp = filter(df, treatment == treatmenttype[i])
  df_treat[[i]] = data.frame(
    coordinate_rate = tapply(df_temp$coordinate, df_temp$period, mean, na.rm = TRUE),
    period = tapply(df_temp$period, df_temp$period, mean)
  )
}

# DTW distance
d1 = dtw(df_treat[[1]]$coordinate_rate, df_treat[[2]]$coordinate_rate)
d2 = dtw(df_treat[[3]]$coordinate_rate, df_treat[[2]]$coordinate_rate)
d3 = dtw(df_treat[[4]]$coordinate_rate, df_treat[[2]]$coordinate_rate)
d4 = dtw(df_treat[[5]]$coordinate_rate, df_treat[[2]]$coordinate_rate)
d1$normalizedDistance
d2$normalizedDistance
d3$normalizedDistance
d4$normalizedDistance

# set up plot
title = 'stage2_coordination_within_supergames'
file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/writeup_ContinuousBOS/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 800, height = 500)
pic = ggplot() +
  geom_line(data=df_treat[[1]], aes(x=period, y=coordinate_rate, colour='blue')) +
  geom_line(data=df_treat[[2]], aes(x=period, y=coordinate_rate, colour='red')) +
  geom_line(data=df_treat[[3]], aes(x=period, y=coordinate_rate, colour='green')) +
  geom_line(data=df_treat[[4]], aes(x=period, y=coordinate_rate, colour='purple')) +
  geom_line(data=df_treat[[5]], aes(x=period, y=coordinate_rate, colour='orange')) +
  scale_x_discrete(name='time tick', waiver(), limits=c(5,50,100,150,200,240)) +
  scale_y_continuous(name='coordination rate', limits=c(0.3,1)) +
  theme_bw() + 
  scale_colour_manual(values=c('blue', 'green', 'purple', 'red'), labels=c('control', 'signal', 'message', 'chatbox')) +
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
d1 = dtw(df_treat[[1]]$coordinate_rate, df_treat[[2]]$coordinate_rate)
d2 = dtw(df_treat[[3]]$coordinate_rate, df_treat[[2]]$coordinate_rate)
d3 = dtw(df_treat[[4]]$coordinate_rate, df_treat[[2]]$coordinate_rate)
d4 = dtw(df_treat[[5]]$coordinate_rate, df_treat[[2]]$coordinate_rate)
d1$normalizedDistance
d2$normalizedDistance
d3$normalizedDistance
d4$normalizedDistance

# set up plot
title = 'stage2_coordination_between_supergames'
file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/writeup_ContinuousBOS/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 800, height = 500)
pic = ggplot() +
  geom_line(data=df_treat[[1]], aes(x=round, y=coordinate_rate, colour='blue')) +
  geom_line(data=df_treat[[2]], aes(x=round, y=coordinate_rate, colour='red')) +
  geom_line(data=df_treat[[3]], aes(x=round, y=coordinate_rate, colour='green')) +
  geom_line(data=df_treat[[4]], aes(x=round, y=coordinate_rate, colour='purple')) +
  scale_x_discrete(name='supergame', waiver(), limits=c(3,6,9,12)) +
  scale_y_continuous(name='coordination rate', limits=c(0.3,1)) +
  theme_bw() + 
  scale_colour_manual(values=c('blue','red','green','purple'), labels=c('control', 'signal', 'message', 'chatbox')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

rm(df_temp, df_treat, pic, d1, d2, d3)


##### Mechanisms: Pair-level classification #####
# set up dataset and parameters
#df_2 = filter(df, period > 120)
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
sum_pair = matrix(NA, nrow = 5, ncol = 2)
rownames(sum_pair) = treatmenttype
colnames(sum_pair) = c('Alternating', 'One NE')
for (i in 1:length(treatmenttype)){
  df_treat = filter(pair_summary, treatment == treatmenttype[i])
  sum_pair[i,1] = mean(df_treat$alternating)
  sum_pair[i,2] = mean(df_treat$one_ne)
}

# add statistical testing result
df_control = filter(pair_summary, treatment == 'Continuous_Control')
df_freetext = filter(pair_summary, treatment == 'Continuous_Chatbox')
df_messaging = filter(pair_summary, treatment == 'Continuous_Message')
df_signal = filter(pair_summary, treatment == 'Continuous_Signal')
df_message_signal = filter(pair_summary, treatment == 'Continuous_Message_Signal')

sum_pair2 = matrix(NA, nrow = 4, ncol = 2)
rownames(sum_pair2) = c('R2-R1', 'R3-R1', 'R4-R1', 'R5-R1')

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
test = t.test(df_signal$alternating, df_control$alternating, 
              alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
print(test$p.value)
test = t.test(df_signal$one_ne, df_control$one_ne, 
              alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
print(test$p.value) 

sum_pair2[3,1] = sum_pair[4,1] - sum_pair[1,1]
sum_pair2[3,2] = sum_pair[4,2] - sum_pair[1,2]
test = t.test(df_messaging$alternating, df_control$alternating, 
              alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
print(test$p.value)
test = t.test(df_messaging$one_ne, df_control$one_ne, 
              alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
print(test$p.value)

sum_pair2[4,1] = sum_pair[4,1] - sum_pair[1,1]
sum_pair2[4,2] = sum_pair[4,2] - sum_pair[1,2]
test = t.test(df_message_signal$alternating, df_control$alternating, 
              alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)
print(test$p.value)
test = t.test(df_message_signal$one_ne, df_control$one_ne, 
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
df_control = filter(df_stay, treatment == 'Continuous_Control')
df_freetext = filter(df_stay, treatment == 'Continuous_Chatbox')
df_messaging = filter(df_stay, treatment == 'Continuous_Message')
df_signal = filter(df_stay, treatment == 'Continuous_Signal')
df_signal = filter(df_signal, length > 2)
df_message_signal = filter(df_stay, treatment == 'Continuous_Message_Signal')

# set up plot
title = 'stage2_distribution_duration_nash'
file = paste("D:/Dropbox/Working Papers/Continuous Time BOS/writeup_ContinuousBOS/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 700, height = 300)
pic = ggplot() +
  stat_ecdf(geom="step", data=df_control, aes(x=length, colour='blue')) +
  stat_ecdf(geom="step", data=df_freetext, aes(x=length, colour='red')) +
  stat_ecdf(geom="step", data=df_signal, aes(x=length, colour='green')) +
  stat_ecdf(geom="step", data=df_messaging, aes(x=length, colour='purple')) +
  stat_ecdf(geom="step", data=df_message_signal, aes(x=length, colour='orange')) +
  scale_x_continuous(name='duration(time tick)', waiver(), limits=c(0,150), breaks = c(0,10,20,50,100,150)) +
  scale_y_continuous(name='density') +
  theme_bw() + 
  scale_colour_manual(values=c('blue', 'green', 'orange', 'purple', 'red'), labels=c('control', 'signal', 'message_signal', 'message', 'chatbox')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

# K-S test
ks.test(df_freetext$length, df_control$length)
ks.test(df_signal$length, df_control$length)
ks.test(df_messaging$length, df_control$length)

rm(df_control, df_freetext, df_messaging, df_signal)
rm(df_length, df_char, df_pair, df_stay, pic)


##### (Not use) Mechanisms: Transition probability matrix by time environments #####
# create transition matrix
transition = matrix(0, nrow = 4, ncol = 5)
rownames(transition) = c('Aggressive at t', 'Accommodate at t', 'RPNE at t', 'CPNE at t')
colnames(transition) = c('Aggressive at t+1', 'Accommodate at t+1', 'RPNE at t+1', 'CPNE at t+1', '# of obs')

# select treatment
treatment_data = subset(df, treatment == 'Continuous_Chatbox')
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


##### Mechanisms: two-step transition #####
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

# collapse the data for each treatment
trans_data = list()

for (j in 1:length(treatmenttype)){
  
  df_treat = df_slim %>% filter(treatment == treatmenttype[j])
  
  # fix the outlier for signal treatment
  if (treatmenttype[j] == 'Continuous_Signal'){
    df_treat = filter(df_treat, period2 - period > 4)
  }
  
  uniquetrans = unique(df_treat$trans_type)
  length = rep(NA, length(uniquetrans))
  trans_data[[j]] = data.frame(
    trans0 = length, trans1 = length, trans2 = length,
    duration1 = length, duration2 = length,
    trans_type = length, frequency = length)
  
  for (i in 1:length(uniquetrans)){
    df_trans = df_treat %>% filter(trans_type == uniquetrans[i])
    trans_data[[j]]$trans0[i] = df_trans$type[1]
    trans_data[[j]]$trans1[i] = df_trans$trans1[1]
    trans_data[[j]]$trans2[i] = df_trans$trans2[1]
    trans_data[[j]]$duration1[i] = mean(df_trans$period1 - df_trans$period)
    trans_data[[j]]$duration2[i] = mean(df_trans$period2 - df_trans$period1)
    trans_data[[j]]$trans_type[i] = uniquetrans[i]
    trans_data[[j]]$frequency[i] = length(df_trans$tick)
  }
  trans_data[[j]] = trans_data[[j]] %>% mutate(prob = round(frequency/sum(trans_data[[j]]$frequency), digits=3))
  trans_data[[j]] = trans_data[[j]] %>% arrange(desc(trans_data[[j]]$prob))
  
  # select the transitions between Nash
  trans_data[[j]] = trans_data[[j]] %>% filter(trans0 == 'RPNE' | trans0 == 'CPNE')
  trans_data[[j]] = trans_data[[j]] %>% mutate(prob1 = round(frequency/sum(trans_data[[j]]$frequency), digits=3))
  trans_data[[j]] = trans_data[[j]] %>% filter(trans2 == 'RPNE' | trans2 == 'CPNE')
  trans_data[[j]] = trans_data[[j]] %>% mutate(prob2 = round(frequency/sum(trans_data[[j]]$frequency), digits=3))
}

# collect all the data and make the final table for comparing three transitional dynamics
transition_matrix = matrix(0, nrow = 5, ncol = 4)
rownames(transition_matrix) = treatmenttype
colnames(transition_matrix) = c('Disadvantaged', 'Advantaged', 'Direct', 'Fail')

# fill out the table
for (k in 1:length(treatmenttype)){
  subset = filter(trans_data[[k]], trans0!=trans2 & trans1=='Aggressive')
  transition_matrix[k,1] = sum(subset$prob2)
  subset = filter(trans_data[[k]], trans0!=trans2 & trans1=='Accommodate')
  transition_matrix[k,2] = sum(subset$prob2)
  subset = filter(trans_data[[k]], trans1=='RPNE' | trans1=='CPNE')
  transition_matrix[k,3] = sum(subset$prob2)
  transition_matrix[k,4] = 1 - sum(transition_matrix[k,1:3])
}


# get table output and remove table
xtable(transition_matrix, digits = 2, label = 'two_step_transition', align = 'lcccc')
rm(df_slim, df_treat, df_trans, subset, transition_matrix, trans_data)


##### Extension: Chat data overview #####
# reconstruct the message data
df_message = df_message_2
df_message = df_message %>% select(-c(participant.label, participant._is_bot, participant._index_in_pages,
                                      participant._max_page_index, participant._current_app_name, participant._current_page_name,
                                      participant.time_started, participant.visited, participant.mturk_worker_id,
                                      participant.mturk_assignment_id, player.silo_num, player._initial_decision,
                                      group.ran_ready_function, session.label, session.mturk_HITGroupId, session.mturk_HITId,
                                      session.comment, session.is_demo, player.final_payoff, group.group_decisions,
                                      group.subperiod_group_decisions, group._group_decisions_updated,
                                      participant.id_in_session, participant.payoff, player.payoff))
df_message$session_round_id = paste(df_message$session.code, df_message$subsession.round_number, sep = '_')
df_message$merge_identifier = paste(df_message$session_round_id, df_message$group.id_in_subsession, sep = '_')

df_p1 = filter(df_message, player.id_in_group == 1)
names(df_p1)[names(df_p1)=='participant.code']='p1_code'
names(df_p1)[names(df_p1)=='player._message']='p1_message'
df_p1 = df_p1 %>% select(-c(player.id_in_group))

df_p2 = filter(df_message, player.id_in_group == 2)
names(df_p2)[names(df_p2)=='participant.code']='p2_code'
names(df_p2)[names(df_p2)=='player._message']='p2_message'
df_p2 = df_p2 %>% select(-c(player.id_in_group))

df_message = merge(df_p1, df_p2, by = c('merge_identifier', 'group.id_in_subsession', 'subsession.round_number', 'session.code', 'session_round_id'))
rm(df_p1, df_p2)

# reconstruct the chat data
df_chat = df_chat %>% mutate(
  round = as.numeric(substr(channel, 31, 32)),
  group_id = as.numeric(substr(channel, 34, 34)),
)
df_chat$session_round_id = paste(df_chat$participant__session__code, df_chat$round, sep = '_')
df_chat$session_round_group_id = paste(df_chat$session_round_id, df_chat$group_id, sep = '_')
df_chat = df_chat %>% select(-c(participant__session_id, participant__id_in_session, channel, nickname))

# drop the practice games
df_message = filter(df_message, subsession.round_number > 2)
df_chat = filter(df_chat, round > 2)



