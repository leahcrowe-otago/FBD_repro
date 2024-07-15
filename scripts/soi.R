library(dplyr);library(ggplot2)

#noaa SOI
download.file(url= "https://www.cpc.ncep.noaa.gov/data/indices/soi",
              destfile='./data/soi.txt', method='curl')

soi<-read.delim('./data/soi.txt')

soi<-readLines("./data/soi.txt")

soi_stand<-data.frame(do.call(rbind, strsplit(soi[88:161], split="  ")), stringsAsFactors=FALSE)
names(soi_stand)<-soi_stand[1,]
soi_stand<-soi_stand[1:13]%>%
  filter(YEAR != "YEAR")

soi_SLP<-soi_stand%>%
  tidyr::pivot_longer(!YEAR, names_to = "month", values_to = "SLP_index") #sea level pressure index (diff between Tahiti and Darwin)

soi_SLP$SLP_index<-as.numeric(soi_SLP$SLP_index)
soi_SLP$YEAR<-as.numeric(soi_SLP$YEAR)

soi_SLP<-soi_SLP%>%mutate(month_num = rep(1:12, nrow(soi_stand)))

soi_mean<-soi_SLP%>%
  mutate(mo_12 = zoo::rollmeanr(SLP_index, 12, fill = NA))%>%
  mutate(year_mo = as.Date(paste0(YEAR,"-",month_num,"-01")))

#####
max_year = 2023
min_year = 1990

source('~/git-otago/Fiordland_reporting/scripts/connect to MySQL.R', local = TRUE)$value
source('~/git-otago/Fiordland_reporting/scripts/life_history_ageclass update.R', local = TRUE)$value
lifehist<-lifehist

calves<-lifehist%>%
  #mutate(BIRTH_YEAR = FIRST_YEAR)%>%
  filter(BIRTH_YEAR >= min_year)%>%
  #filter(AGECLASS == "C" | AGECLASS == "D")%>%
  distinct(POD, BIRTH_YEAR, NAME)%>%
  group_by(POD, BIRTH_YEAR)%>%
  tally()%>%
  ungroup()%>%
  tidyr::complete(POD, BIRTH_YEAR)%>%
  replace(is.na(.), 0)%>%
  mutate(year_mo = as.Date(paste0(BIRTH_YEAR,"-01-01")),
         lag_year_mo = as.Date(paste0(as.numeric(BIRTH_YEAR)-2,"-01-01")))


ggplot(soi_mean%>%
         filter(YEAR > 1992))+
  geom_col(aes(x = year_mo, y = SLP_index))+
  geom_line(aes(x = year_mo, y = mo_12))+
  scale_x_date()+
  geom_col(data = calves, mapping = aes(x = year_mo, y = n/5, fill = POD), size = 1, alpha = 0.5)+
  #geom_line(data = calves, mapping = aes(x = year_mo, y = n/5, color = POD, linetype = POD))+
  geom_point(data = calves, mapping = aes(x = lag_year_mo, y = n/5, color = POD), size = 1, alpha = 0.5)+
  facet_wrap(~POD)
  
  
  
