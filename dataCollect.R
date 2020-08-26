library(tidyverse)
library(rvest)
library(formattable)
options(stringsAsFactors = F)
tms = read.csv("C:/Users/Admin/Documents/teams.csv")[,-1]
tms = tms %>% select(Team, abb) %>% mutate(link = paste0("https://www.basketball-reference.com/teams/", abb, "/2020/splits/"))
links = tms$link
df = NULL
for (i in 1:length(links)){
  link = links[i]
  team = tms$abb[i]
  html = read_html(link)
  data = html_table(html)[[1]]
  names(data) = data[3,]
  data = data[,1:19]
  data = data %>% filter(Value %in% c("October", "November", "December", "January", "February", "March", "July", "August"))
  data[,3:ncol(data)] = sapply(data[,3:ncol(data)], as.numeric)
  data = data %>% mutate(FGTot = FG*G, FGATot = FGA*G, X3PTot = `3P`*G, X3PATot = `3PA`*G)
  data$Bubble = ifelse(data$Value %in% c("October", "November", "December", "January", "February", "March"), "No", "Yes")
  data = data %>% group_by(Bubble) %>% summarise(.groups = "drop", X3P = sum(X3PTot), X3PA = sum(X3PATot), XFG = sum(FGTot), XFGA = sum(FGATot)) %>% mutate(Tm = team) %>% select(Tm, Bubble, everything())
  df = rbind.data.frame(df, data)
}