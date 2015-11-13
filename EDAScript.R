library(dplyr)
library(tidyr)
library(car)
library(ggplot2)
library(codemog)
library(grid)

sya=readr::read_csv("co_pop_sya_1900_2050.csv")%>%
  gather(year, population, -AGE)%>%
  rename(age=AGE)%>%
  filter(!is.na(age))%>%
  mutate(age=recode(age, "90:101=90"),
         year=levels(year)[year])%>%
  group_by(year, age)%>%
  summarise(population=sum(population))%>%
  ungroup()

e0=readxl::read_excel("Life Expectancy at Birth (Total).xlsx", skip=2)%>%
  gather(year, lifeExpectancy, -Code)%>%
  select(year, lifeExpectancy)


p=sya%>%
  filter(age<90)%>%
  ggplot(aes(x=age, y=population, color=year))+
  geom_line()
p