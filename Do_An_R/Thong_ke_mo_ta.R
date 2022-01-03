#set v??? thu m???c làm vi???c
setwd("C:\\Do_An_R")

#D???c 2 files csv ch???a d??? li???u dã cào v??? s???n ph???m và ngu???i dùng

df_item = read.csv(file = 'output.csv',header = TRUE)
df_review = read.csv(file = 'output2.csv',header = TRUE)


#import các thu vi???n c???n thi???t
library(tidyverse)
library(dplyr)
library(psych)
library(ggplot2)
library(RColorBrewer)
library(plotly)


#Tính các giá tr??? th???ng kê miêu t???
describe(df_item)
describe(df_review)
summary(df_item)
summary(df_review)



# V??? bi???u d???

#plot 1
remove <- c(0)
data <- df_item[!df_item$Total.Reviews %in% remove, ]
p <- data %>%  ggplot( aes(x = Rate)) +
  geom_bar(stat="count", color="blue",fill = "#C77CFF") + 
  labs(title = "Histogram of product's rate", 
       x = "score (star)",
       y = "Counts") 
ggplotly(p)

#plot2: 

p <- df_review %>%  ggplot( aes(x = rate)) +
  geom_bar(stat="count", width=0.5, color="black",fill = "#98EBDD")+
  labs(title = "Histogram of all reviewer's rate", 
       x = "score (star)",
       y = "Counts") 
ggplotly(p)


#plot3
apple = 0
sam = 0
opo =0
xiao = 0
real = 0
n = length(df_item$Rate)
for (i in 1:n){
  ifelse(df_item$Brand.Name[i-1] == "Apple",apple <- apple + 1,apple <- apple + 0)
  ifelse(df_item$Brand.Name[i-1] == "Samsung",sam <- sam + 1,sam <- sam + 0)
  ifelse(df_item$Brand.Name[i-1] == "OPPO",opo <- opo + 1,opo <- opo + 0)
  ifelse(df_item$Brand.Name[i-1] == "Xiaomi",xiao <- xiao + 1,xiao <- xiao + 0)
  ifelse(df_item$Brand.Name[i-1] == "Realme",real <- real + 1,real <- real + 0)
}
other = n - apple - sam - opo - xiao - real
count <- c(apple, sam, opo, xiao, real, other)
color <- brewer.pal(length(count), "Set2") 
pie(count, paste0(round(100 * count/sum(count), 2), "%"),
    col = color, edges=10000, lwd=10, clockwise=TRUE, main = "Pie plot about brand's product")
legend("topleft", legend = c("Apple", "Samsung", "OPPO", "Xiaomi", "Realme", "Other"),
       fill =  color)

#plot 4
apple = 0
sam = 0
opo =0
xiao = 0
real = 0
n = length(df_review$brand.of.Item)
for (i in 1:n){
  ifelse(df_review$brand.of.Item[i-1] == "Apple",apple <- apple + 1,apple <- apple + 0)
  ifelse(df_review$brand.of.Item[i-1] == "Samsung",sam <- sam + 1,sam <- sam + 0)
  ifelse(df_review$brand.of.Item[i-1] == "OPPO",opo <- opo + 1,opo <- opo + 0)
  ifelse(df_review$brand.of.Item[i-1] == "Xiaomi",xiao <- xiao + 1,xiao <- xiao + 0)
  ifelse(df_review$brand.of.Item[i-1] == "Realme",real <- real + 1,real <- real + 0)
}
other = n - apple - sam - opo - xiao - real
count <- c(apple, sam, opo, xiao, real, other)
color <- brewer.pal(length(count), "Set2") 
pie(count, paste0(round(100 * count/sum(count), 2), "%"),
    col = color, edges=10000, lwd=10, clockwise=TRUE, main = "Pie plot about brand's reviewer")

legend("topleft", legend = c("Apple", "Samsung", "OPPO", "Xiaomi", "Realme", "Other"),
       fill =  color)

#plot 5
pl <-  data %>% 
  ggplot(aes(Item.Category, Rate, fill = Item.Category)) + 
  geom_boxplot() +
  labs(title = "Overview of df_item Rate by Category of Item", 
       x = "",
       y = "Rate (star)") +
  scale_y_continuous(breaks = seq(0,30, by = 0.5)) 
ggplotly(pl)

#plot 6
p1 <- df_item %>% 
  ggplot(aes(Price)) +
  geom_density(aes(fill = Item.Category), alpha = 0.7) +
  labs(title = "Bi???u d??? m???t d??? th??? hi???n giá c???a các m???t hàng di???n t???")
ggplotly(p1)
 
#plot 7
p2 <- df_item %>% 
  ggplot(aes(Rate, Total.Reviews)) +
  geom_point(aes(col = Item.Category)) +
  theme_bw() +
  labs(title = "Interactive scatter plot") +
  facet_wrap(~Item.Category)
ggplotly(p2)

#plot 8
color <- brewer.pal(n = 5, "Set2") 

p <- ggplot(data, aes(x = Rate, y = Total.Reviews)) + 
  geom_point(aes(col = Item.Category)) + 
  labs(title= "Bubble plot between rate and count of reviewer",y="All reviewer", x = "score") +
  theme(legend.position="right")
ggplotly(p)



#plot 9
newdata <- df_item[order(-df_item$Total.Reviews),]
p <- ggplot(newdata[1:20,1:9], aes(x = Rate, y = Total.Reviews, size = Price)) + 
  geom_point(aes(col = Item.Category)) + 
  labs(title= "Bubble plot between count of reviews and rate of top 10 favorite products",y="Total reviewer", x = "Score") +
  theme(legend.position="right")
ggplotly(p)



