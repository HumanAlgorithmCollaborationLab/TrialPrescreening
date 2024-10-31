library(tidyverse)
library(ggpubr)

###### Read and Process Responses ######
human_chart_level = readRDS("Data/First Round/human_chart_level_round_1.RDS")
aug_chart_level = readRDS("Data/First Round/aug_chart_level_round_1.RDS")

################### Power Calculations ###################
vard = var(aug_chart_level$chart_accuracy - human_chart_level$chart_accuracy)
mu_a = mean(aug_chart_level$chart_accuracy)
mu_h = mean(human_chart_level$chart_accuracy)

power_n = function(n, mu_a, mu_h, vard, margin = 0.05, a = 0.05){
  # Calculates power given 
    # n: sample size
    # mu_a: mean accuracy in the Human+AI arm
    # mu_h: mean accuracy in the Human Alone arm
    # vard: the variance of the difference between arms across charts
    # margin: default is for inferiority design w/ 0.05 margin, superiority 0 margin
    # a: alpha level
  return(pnorm(sqrt(n*((mu_a-mu_h + margin)^2)/(vard)) - qnorm(1-a)))
}

plot_power = function(n_seq, mu_a, mu_h, vard, margin = 0.05, a = 0.05){
  # Plots power graph along sample size range with specified parameters
  power_data = data.frame(n = n_seq, vard = rep(vard, each = length(n_seq))) %>% 
                            mutate(Power = power_n(n, mu_a, mu_h, vard, margin, a)) %>%
    mutate(var_type = as.factor(round(vard,2))) 
    
  pp = ggplot(power_data) + geom_line(aes(x = n, y = Power, group = var_type, color = var_type), linewidth = 0.8) + 
    xlab("Sample Size") + theme_bw() + 
    geom_hline(aes(yintercept = 0.8, linetype = "80% power"), 
               color = "red", size = 0.8) +
    scale_linetype_manual(name = "", values = "dotted") +
    scale_color_discrete(name = "Variance")
  return(pp)
}

# Inferiority
# Power for Observed difference and no difference as true difference, varied variance
ip1 = plot_power(seq(0, 400, by = 1), mu_a, mu_h, vard = c(vard, 2*vard, 5*vard)) + 
    ggtitle("Noninferiority Power Graph at Observed Effect Size")
ip2 = plot_power(seq(0, 400, by = 1), mu_a, mu_a, vard = c(vard, 2*vard, 5*vard)) + 
  ggtitle("Noninferiority Power Graph at Null Effect Size")

#Superiority
sp1 = plot_power(seq(0, 400, by = 1), mu_a, mu_h, vard = c(vard, 2*vard, 5*vard), margin = 0) + 
  ggtitle("Superiority Power Graph at Observed Effect Size")
sp2 = plot_power(seq(0, 400, by = 1), mu_a, mu_h, vard = c(vard, 2*vard, 5*vard), margin = 0.02) +
  ggtitle("Superiority Power Graph at Final Observed Effect Size")

ggarrange(plotlist = list(ip1, ip2, sp1, sp2))
