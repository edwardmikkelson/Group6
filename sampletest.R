library(tidyverse)
a<- sample(0:100, 100)
b<- sample(0:250, 100)
c<- sample(0:120, 100)
x<- sample(0:100, 100)
y<- sample(0:250, 100)
z<- sample(0:120, 100)

df_abc<-tibble(a,b,c)
df_xyz<-tibble(a,x,y,z)

df_abcxyz<- full_join(df_abc, df_xyz, by= "a")
lm(a~b, data = df_abcxyz)
scatter.smooth(x = x, y=y)

by_request<- requests_by_date$dailyrequests
by_crash<- frequency_by_date$dailycrashes
date<-frequency_by_date$FROMDATE
full_df<- tibble(date, by_request, by_crash)