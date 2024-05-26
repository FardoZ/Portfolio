library(tidyverse)
library(RSQLite)
library(glue)
library(dplyr)
library(tidyr)
# string template
my_name<- "Dew"
age <- 34
city <- "bangkok"
text <- glue("Hello my name is {my_name} and I'm {age}
     years old and I live in {city}")
print(text)

#working with date
D<- "2023-06-17"
D<- ymd(D)
wday(D,label = T,abbr=F)
D + 1

#tidyverse - dplyr
#1.select
#2.filter
#3.arrange
#4.mutate(Create new column)
#5.sumarise + group_by(aggregate in SQL)

select(mtcars,hp,wt,am)
mtcars %>%
  select(hp,wt,am)
mtcars %>%
  select(1,2:5,10)
mtcars %>%
  select(11,everything())
mtcars %>%
  select(starts_with("a"))

arrange(mtcars,-mpg)

View(mtcars)
select(mtcars,mile_per_gallon_wight = mpg,cyl,disp,hp,am)
select(mtcars,starts_with("m"))
select(mtcars,ends_with("p"))
select(mtcars,contains("p"))
mtcars<-(rownames_to_column(mtcars, var="model"))

summarize(mtcars)
#filter by condition
filter(mtcars,hp>100|disp>300|mpg>15)
filter(mtcars,model== "Mazda RX4")

#grep 
state.name
grep("^A",state.name)
grepl("^M",state.name)

filter(mtcars,grepl(("C$"),model))

##dplye the right way %>% (PIPE)
mtcars %>%
  select(model,mpg,am,hp) %>%
  filter(hp > 100,mpg > 15) 

#sort data
dT1<-mtcars%>%
  select(am,mpg) %>%
  arrange(am,desc(mpg))

dT1
## mutate - create new column
mtcars %>%
  select(model,mpg) %>%
  filter(mpg > 20 )%>%
  mutate (mpg_double= mpg*2,
         mpg_add_five= mpg+5,
         mpg_log = log(mpg),
         ,model = tolower(model))

## summarise + group_by
mtcars%>%
  group_by(am)%>%
  summarise(avg_mpg=mean(mpg),
            sum_mpg= sum(mpg),n=n())

#mutate am
mtcars <-mtcars %>%
  mutate(am= ifelse(am==0,"Auto","Manual"))


##simple pipeline 
mtcars %>%
  select(mpg,hp,am,wt,am)%>%
  filter(mpg>=15)%>%
  group_by(am)%>%
  summarise(n=n())
DFN<-read_csv("https://gist.githubusercontent.com/seankross/a412dfbd88b3db70b74b/raw/5f23f993cd87c283ce766e7ac6b329ee7cc2e1d1/mtcars.csv")

View(DFN)

DFN %>% head()

##sampling (random sample)
set.seed(16)
DFN%>%
  select(model,hp)%>%
  filter(hp > 100)%>%
  sample_n(3)

DFN %>%
  sample_frac(0.1) #10%

## count => create frequency table 
mtcars<-DFN %>%
  mutate(am = ifelse(am==0,"Auto","manaul"))%>%
  count(am)%>%
  mutate(pct=n/sum(n))
## bind_rows()

df1<- mtcars%>%
  filter(hp<=80)
df2<-mtcars%>%
  filter(hp>300)
View(bind_rows(df1,df2))
df3 <-mtcars%>%
  filter(am =="Auto")

df4<-mtcars%>%
  filter(mpg>=20)

#stack a lot of DF
list_DF<- list(df1,df2,df3,df4)
full_DF<-bind_rows(list_DF)
full_DF<-full_DF%>%
  select(model,mpg,hp,am)%>%
  mutate(n=n())
view(full_DF)

##bind_cols() vs join()
df1<-data.frame(id=1:4,
                name=c("A","B","C","D"))
df2<-data.frame(id=c(3,2,1,5),
    country=c("US","UK","TH","JP"))
df3<-data.frame(id= 1:5,
                classes =c("data","data","ux","business",NA),
                score = c(2.5,2.8,2.9,3,NA))

bind_cols(df1,df2)

inner_join(df1,df2,by="id")
right_join(df1,df2,by="id")
full_join(df1,df2,by="id")

#delete missing value
df1%>%
  full_join(df2, by=c("id"="student_id"))%>%
  drop_na()

## joins mutiple tables
df1 %>%
  inner_join(df2,by="id")%>%
  inner_join(df3,by="id")
##clean missing values

df3[5,2]<- "business"

replace_na(df3$classes,"data engi")

df3%>% 
  mutate(classes = replace_na(classes,"FooFoo"),
         #mean imputation
         score = replace_na(score,mean(score,na.rm=T)))

apply(mtcars,2,mean)
library(rvest)








