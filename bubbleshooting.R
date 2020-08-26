library(tidyverse)
library(rvest)
library(formattable)
options(stringsAsFactors = F)
bub = read.csv("bubbleshooting.csv")[,-1]
bub2 = bub %>% mutate(X2P = XFG - X3P, X2PA = XFGA - X3PA, X3P. = X3P/X3PA, XFG. = XFG/XFGA, X2P. = X2P/X2PA)
for (i in 1:nrow(bub2)){
  if (i %% 2 == 0){
    bub2$diff3P[i] = 100*(bub2$X3P.[i] - bub2$X3P.[i-1])
  } else{
    bub2$diff3P[i] = 0
  }
}
bub2 = bub2 %>% filter(Bubble == "Yes")
bub2 = bub2 %>% arrange(desc(diff3P))
bub2$Tm = factor(bub2$Tm, levels = rev(bub2$Tm))
bub2$col = ifelse(bub2$diff3P > 0, "Better", "Worse")
bub2 %>% ggplot(aes(x = Tm, y = diff3P, fill = col)) + geom_bar(color = "black", stat = "identity") + coord_flip() + scale_y_continuous("Difference in 3P Percentage") + scale_x_discrete("") + scale_fill_manual("3P Shooting", values = c("#FED976", "#800026")) + theme_bw() + ggtitle("Bubble 3-Point Shooting", "Regular Season vs. Seeding Games")
# 11 teams are shooting better, 10 teams are shooting worse, 1 team is shooting almost exactly the same

tot = bub %>% group_by(Bubble) %>% summarise(.groups = "drop", X3P = round(sum(X3P)), X3PA = round(sum(X3PA)))
tot$prop = 100*(tot$X3P/tot$X3PA)
names(tot)[2:4] = c("3-Pointers", "3-Point Attempts", "Percentage")
formattable(tot)

# Hypothesis Test
# Null Hypothesis: p1 = p2
# Alt Hypothesis: p1 != p2

# pooled sample proportion
p1 = tot$prop[1]
p2 = tot$prop[2]
n1 = tot$X3PA[1]
n2 = tot$X3PA[2]
p = (p1*n1 + p2*n2)/(n1 + n2) # 0.3622345
se = sqrt(p*(1-p)*((1/n1)+(1/n2))) # 0.006418743

Z = (p1-p2)/se # -0.1323965
pnorm(q = Z)
# The p-value (0.4473353) indicates that the difference between these
#   two proportions is insignificant at the given sample size, which
#   means that there is no difference between regular season 3-point
#   percentage and seeding game 3-point percentage. This is 







