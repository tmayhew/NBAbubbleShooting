library(tidyverse)
library(rvest)
library(knitr)
options(stringsAsFactors = F)
bub = read.csv("bubbleshooting.csv")[,-1]
byYr = read.csv("NBA3PbyYear.csv")[,-1] %>% filter(Yr != 2020 & Yr >= 2000)
tot = bub %>% group_by(Bubble) %>% summarise(.groups = "drop", X3P = sum(X3P), X3PA = sum(X3PA))

pre.bubble = c(2020.0, tot$X3P[1], tot$X3PA[1])
pos.bubble = c(2020.5, tot$X3P[2], tot$X3PA[2])
byYr = byYr %>% rbind.data.frame(pre.bubble, pos.bubble)
byYr[,2:3] = sapply(byYr[,2:3], as.numeric)
byYr = byYr %>% mutate(X3P. = (X3P/X3PA)*100)
byYr$bub = ifelse(byYr$Yr > 2019, ifelse(byYr$Yr > 2020, "2020 NBA Bubble", "2019-20 Regular Season"), 'previousyrs')
byYr %>% ggplot(aes(x = Yr, y = X3P.)) + geom_line(linetype = "dashed") + geom_point(data = filter(byYr, bub != "previousyrs"), aes(color = bub)) + theme_bw() + ggtitle("League-Wide NBA 3-Point Percentage", "Since 2000") + scale_x_continuous(breaks = seq(2000,2020,2)) + scale_color_manual("", values = c("black", "red2")) + scale_y_continuous("3-Point Percentage")
