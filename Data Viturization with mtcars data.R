library(tidyverse)

#comment load library
head(mtcars)
mtcars%>%
rownames_to_column(var="model")

ggplot(data = mtcars,
       mapping = aes(x = mpg))+
  geom_histogram()

ggplot(data = mtcars,
       mapping = aes(x = mpg))+
  geom_boxplot()

## base layer 
base<- ggplot(data = mtcars,
              mapping = aes(x = mpg))
base + 
  geom_histogram(bins=15,fill="deeppink") # bin = จำนวนแท่งกราฟ

## on - varieable,dicrete
student <- data.frame(id=1:5
  ,gender=c("M","M","M","F","F"))

ggplot(data = student,
       mapping = aes(x = gender))+
  geom_bar()

## two variable

base2<-ggplot(mtcars,aes(y=mpg,x=hp))
base2 + 
  geom_point(size = 5,col="red",alpha=0.4)+ 
  geom_smooth() + 
  geom_rug()



base2 + 
  #linear model lm
  geom_smooth(method = "lm",
              col= "black",
              fill = "gold") + 
  geom_point()

## two variables, one discrete 
base3 <- ggplot(diamonds,aes(cut,price))
ggplot(diamonds,aes(cut,price)) + 
  geom_boxplot()
P1<-diamonds%>%
  group_by(cut)%>%
  summarise(
    med_price = median(price)
  )%>%
  ggplot(aes(cut,med_price))+
  geom_col() + 
  theme_minimal()

#qpolt qucik plot
## ggplot2
#Independent variable => x
#dependent variable => y

p1<-qplot(x=price,data = diamonds,geom = "histogram",bins = 10)
qplot(y=price,x=carat,data=diamonds,geom ="point")

P2 <- qplot(x=carat,y=price,
      data = diamonds,
      geom ="point")

p3 <- qplot(x=cut,data=diamonds,geom="bar")

## ggplot()
## overplotting
mini_diamonds <- sample_frac(diamonds,0.02)
ggplot(mini_diamonds,
       aes(carat,price,col=cut))+
  geom_point(alpha= 1)+
  theme_minimal()+
  scale_color_brewer(type="qual",palette=1)

ggplot(mini_diamonds,
       aes(carat,price,col=price))+
  geom_point(alpha= 0.7)+
  theme_minimal() + 
  scale_color_gradient(low="gold",high="blue")
#facet
#segment our charts into groups
ggplot(mini_diamonds,
       aes(carat,price,col=price))+
  geom_point(alpha= 0.7)+
  theme_minimal() + 
  scale_color_gradient(low="gold",high="blue")+
  facet_wrap(~cut,ncol = 2)

ggplot(mini_diamonds,
       aes(carat,price,depth,col=price))+
  geom_point(alpha= 0.7)+
  geom_smooth() +
  theme_minimal() + 
  scale_color_gradient(low="gold",high="blue")+
  facet_grid(cut ~ clarity)
#labels
ggplot(mtcars,aes(hp,mpg))+
  geom_point()+
  theme_minimal()+
  labs(
    titles = "Mt first scatter plot",
    subtitle = "Awesome work!",
    x="Horse Power",
    y="Miles per Gallon",
    caption="Source : R studio"
  )

## simple bar chart
ggplot(diamonds,aes(cut,fill=color))+
  geom_bar(position="dodge")+
  theme_wsj()
## install more theme for ggplot charts
ggplot(mini_diamonds,
       aes(carat,price,col=price))+ 
  geom_point(alpha = 0.8)+
  theme_minimal()+
  scale_color_gradient(low ="gold",
                       high="blue")+
  facet_wrap(~cut,ncol=2)























