library(tidyverse)
library(rpart)
library(rpart.plot)

d2 <- read.csv('../exp2/data/disconnect_green_temp.csv')
d1 <- read.csv('../exp1/data/disconnect.csv')
N1 <- d1 %>% 
  group_by(subject) %>% 
  summarize(n()) %>% 
  nrow()

N2 <- d2 %>% 
  group_by(subject) %>% 
  summarize(n()) %>% 
  nrow()

d1 <- d1 %>%
  select(-sd_rt, -leftpoints, -rightpoints, -responselocation) %>% 
  mutate(color = NA)

d2 <- d2 %>% 
  select(-rt_mean, -rt_sd)

d1$experiment <- 'Experiment 1'
d2$experiment <- 'Experiment 2'
d <- rbind(d1,d2)

### EXPERIMENT 1
m1 <- rpart(transcode ~ difference, data = d[d$experiment == 'Experiment 1',], control = rpart.control(cp=.0008))
test_data <- data.frame(difference = -9:9)
test_data$proba <- predict(m1, newdata = test_data)

subject_means <- d1 %>% 
  group_by(subject, difference) %>% 
  summarize(transcode = mean(transcode))


subject_means %>% 
  group_by(difference) %>% 
  summarize(pswitch = mean(transcode), se = sd(transcode) / sqrt(N2)) %>% 
  ggplot(aes(x = difference, y = pswitch)) + 
  geom_line(data = subject_means, aes(x = difference, y = transcode, group = subject), alpha = .3) + 
  geom_line(size = 2) + 
  geom_errorbar(aes(ymin = pswitch - se, ymax = pswitch + se), width = .5) + 
  geom_line(data = test_data, aes(x = difference, y = proba), size = 2, color = 'blue') +
  scale_x_continuous(breaks = -9:9, labels = -9:9) + 
  labs(
    x = 'Point Difference (Points for Switching)',
    y = 'Proportion of Switching'
  ) +
  theme_bw() +
  theme(panel.grid = element_blank())


ggsave('exp1_fullresults.png', width = 7.5, height = 4.5, units = 'in')



### EXPERIMENT 2
m2 <- rpart(transcode ~ difference, data = d[d$experiment == 'Experiment 2',], control = rpart.control(cp=.0008))
test_data <- data.frame(difference = -9:9)
test_data$proba <- predict(m2, newdata = test_data)

subject_means <- d2 %>% 
  group_by(subject, difference) %>% 
  summarize(transcode = mean(transcode))

color_ranges <- data.frame(xmin = c(-3.5, 3.5, 6.5), xmax = c(3.5, 6.5, 9.5), ymin = c(0, 0, 0), ymax = c(1, 1,1), val = c('Low Difference Range', 'Medium Difference Range', 'High Difference Range'))
color_ranges$val <- factor(color_ranges$val)
color_ranges$val <- factor(color_ranges$val, levels(color_ranges$val)[c(2,3,1)])

subject_means %>% 
  group_by(difference) %>% 
  summarize(pswitch = mean(transcode), se = sd(transcode) / sqrt(N2)) %>% 
  ggplot(aes(x = difference, y = pswitch)) + 
  geom_rect(data = color_ranges, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = val, color = val), alpha = .3) +
  geom_line(data = subject_means, aes(x = difference, y = transcode, group = subject), alpha = .3) + 
  geom_line(size = 2) + 
  geom_errorbar(aes(ymin = pswitch - se, ymax = pswitch + se), width = .5) + 
  geom_line(data = test_data, aes(x = difference, y = proba), size = 2, color = 'blue') +
  scale_x_continuous(breaks = -9:9, labels = -9:9) + 
  scale_fill_manual(values = c(NA, 'Green', 'Dark Green')) + 
  scale_color_manual(values = c('Black',NA, NA)) +
  labs(
    x = 'Point Difference (Points for Switching)',
    y = 'Proportion of Switching'
  ) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(.15,.75),
        panel.grid = element_blank())


ggsave('exp2_fullresults.png', width = 7.5, height = 4.5, units = 'in')

## tree plot
prp(m2, digits = 4, extra = 1)
ggsave('exp2_treeresults.png', width = 5.5, height = 3.5, units = 'in')
rpart.plot(m2, box.col = 'white')









