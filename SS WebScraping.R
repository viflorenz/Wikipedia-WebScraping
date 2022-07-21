#SS WebScarping
rm(list = ls()) 
#https://en.wikipedia.org/wiki/School_shooting
#https://en.wikipedia.org/wiki/List_of_school_shootings_in_the_United_States_(2000%E2%80%93present)
#https://en.wikipedia.org/wiki/List_of_school_shootings_in_the_United_States_(before_2000)
library(rvest)
library(stringr)
library(rio)
library(tidyverse)
library(purrr)
library(dplyr)
library(plotly)

urlb2 <- "https://en.wikipedia.org/wiki/List_of_school_shootings_in_the_United_States_(before_2000)"
urla2 <- "https://en.wikipedia.org/wiki/List_of_school_shootings_in_the_United_States_(2000%E2%80%93present)"
htmlb2 <- read_html(urlb2)
htmla2 <- read_html(urla2)

tablasb2 <- htmlb2 |>  #*todas* las tablas se pasan a una sola lista
  html_elements("table.wikitable") |>  html_table()

tablasa2 <- htmla2 |> 
  html_elements("table.wikitable") |>  html_table()

tablasb2 |> str(1)#Hay 16 tablas en wiki-SS antes de los 2000: desde 1840, incrementos en décadas.
tablasa2 |> str(1)#Hay 3 tablas en wiki-SS después de los 2000: 2000s, 2010s,2020s

#df_tablas[[X]] |> head(Y)#elección de alguna tabla en particular

sapply(tablasb2 [[1]], class)
sapply(tablasb2 [[3]], class)
#la columna "death" e "injuries" van difiriendo de tipo entre tibbles.

#after 2000
tablasa2_df <- tibble("Date"="chr","Location"="chr","Deaths"="chr",
               "Injuries"="chr","Description"="chr")

for (i in 1:3) {
print (tablasa2_df <- bind_rows(tablasa2_df,tablasa2[[i]])
)}
tablasa2_df <- tablasa2_df[-1,]

#before 2000
tablasb2_df <- map_df(lapply(tablasb2, function(x) data.frame(lapply(x, as.character))),
               bind_rows)
#o 
#tablasa2_df <- data.table::rbindlist(tablasa2)
#y
#tablasb2_df <- data.table::rbindlist(tablasb2)
                      
#todos
all_ss <- bind_rows(tablasb2_df,tablasa2_df)

#limpieza tibble agregada
all_ss$Deaths <-  str_remove_all(all_ss$Deaths, "\\[n [:digit:]]")|> 
  str_replace_all("[:alpha:]", " ") |> 
  str_replace_all("[:punct:]", " ")
all_ss$Injuries <-str_remove_all(all_ss$Injuries, "\\[n [:digit:]]") |> 
  str_replace_all("\\+", " ") 
View(all_ss)  

all_ss_anos <- all_ss |> 
  separate(Date,sep = ",",into = c("Date","Year"))
all_ss_anos$Year <- as.numeric(all_ss_anos$Year)
all_ss_anos$Deaths <- as.numeric(all_ss_anos$Deaths)
all_ss_anos$Injuries <- as.numeric(all_ss_anos$Injuries)
plot <- all_ss_anos |>
  group_by(Year) |> 
  summarise("Deaths" = sum(Deaths))

plot2 <- all_ss_anos |> 
  group_by(Year) |> 
  summarise("Injuries" = sum(Injuries))

plot3 <- bind_cols(plot,plot2) |> 
  select(Year...1, Deaths, Injuries) |> 
  plyr::rename(c("Year...1"="Year"))


#plots
vis_1<-ggplot (plot,
        aes (x = Year, y = Deaths)) + 
  geom_point()+
  labs (y="Deaths by year")

vis_2<-ggplot (plot2, aes (x = Year, y = Injuries)) + 
  geom_point()+
  labs (y="Injuries by year")

vis_3 <- ggplot(data=plot, aes(x = Year, y = Deaths)) +
  geom_point(aes(color=Deaths), alpha=0.5) + 
  labs(title="School Shootings Deaths by Year",
       subtitle="From Wikipedia dataset",
       y="Death by year", 
       color = "Deaths count")

vis_4 <- ggplot(plot2, aes (x = Year, y = Injuries)) + 
  geom_area()+
  labs (y="Injuries by year")


vis_5 <- ggplot() +  
  geom_point(data = plot3, aes (x = Year, y = Deaths, color = Deaths), col = "purple") +
  geom_point(data = plot3, aes (x = Year, y = Injuries, color = Injuries),col = "darkblue")+
  labs(title="School Shootings Deaths and Injuries by Year",
       subtitle="From Wikipedia dataset",
       y=" ", 
       color = "Deaths count")


plotly::ggplotly(vis_5)
