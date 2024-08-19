library(dplyr);library(ggplot2)

#noaa SOI
download.file(url= "https://www.cpc.ncep.noaa.gov/data/indices/soi",
              destfile='./data/soi.txt', method='curl')

#noaa Antarctic Oscillation = Southern annular mode
download.file(url= "https://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/aao/monthly.aao.index.b79.current.ascii.table",
              destfile='./data/ao_sam.txt', method='curl')

#soi
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

#ao_sam
ao_sam<-read.table("./data/ao_sam.txt", sep="", fill = TRUE, header=T) #manually added "YEAR" to txt header

ao_sam_long<-ao_sam%>%
  tidyr::pivot_longer(!YEAR, names_to = "month", values_to = "SAM_index")

ao_sam_long$SAM_index<-as.numeric(ao_sam_long$SAM_index)

ao_sam_mo<-ao_sam_long%>%
  mutate(month_num = rep(1:12, nrow(ao_sam)))

ao_sam_mo$SAM_index<-as.numeric(ao_sam_mo$SAM_index)

ao_sam_mean<-ao_sam_mo%>%
  mutate(mo_12 = zoo::rollmeanr(SAM_index, 12, fill = NA))%>%
  mutate(year_mo = as.Date(paste0(YEAR,"-",month_num,"-01")))

#####
max_year = 2023
min_year = 1990

source('~/git-otago/Fiordland_reporting/scripts/connect to MySQL.R', local = TRUE)$value
source('~/git-otago/Fiordland_reporting/scripts/life_history_ageclass update.R', local = TRUE)$value
lifehist<-lifehist

calves<-lifehist%>%
  filter(BIRTH_YEAR >= min_year)%>%
  mutate(suv = case_when(
    as.numeric(LAST_YEAR) - as.numeric(BIRTH_YEAR) >= 3 ~ "3+",
    as.numeric(LAST_YEAR) - as.numeric(BIRTH_YEAR) < 3 ~ "<3"))%>%
  distinct(POD, BIRTH_YEAR, NAME, suv)%>%
  group_by(POD, BIRTH_YEAR, suv)%>%
  tally()%>%
  ungroup()%>%
  tidyr::complete(POD, BIRTH_YEAR)%>%
  filter(!is.na(suv))%>%
  replace(is.na(.), 0)%>%
  group_by(POD, BIRTH_YEAR)%>%
  mutate(year_mo = as.Date(paste0(BIRTH_YEAR,"-01-01")),
         lag_year_mo = as.Date(paste0(as.numeric(BIRTH_YEAR)-1,"-01-01")),
         total = sum(n))%>%
  ungroup()%>%
  mutate(prop = n/total)

soi_plot<-ggplot(soi_mean%>%
         filter(YEAR > 1992))+
  geom_col(aes(x = year_mo, y = SLP_index))+
  geom_line(aes(x = year_mo, y = mo_12))+
  scale_x_date(limits = c(ymd("2008-01-01"),ymd("2023-12-31")))+
  geom_col(data = calves, mapping = aes(x = lag_year_mo, y = n/5, fill = POD), size = 1, alpha = 0.5)+
  theme_bw()+
  ylab("SOI index")+
  facet_wrap(~POD)

sam_plot<-ggplot(ao_sam_mean%>%
                   filter(YEAR > 1992))+
  #geom_col(aes(x = year_mo, y = SAM_index))+
  geom_line(aes(x = year_mo, y = mo_12))+
  scale_x_date(limits = c(ymd("2008-01-01"),ymd("2023-12-31")))+
  geom_col(data = calves, mapping = aes(x = lag_year_mo, y = n/5, fill = POD), size = 1, alpha = 0.5)+
  facet_wrap(~POD)

ggplot()+
  geom_line(data = calves%>%filter(!(POD == "DUSKY" & as.numeric(BIRTH_YEAR) < 2008)), 
            mapping = aes(x = year_mo, y = n, color = POD, linetype = POD), linewidth = 1)+
  xlab("Year")+
  ylab("Numnber of calves observed born")+
  theme_bw()

doubtful<-calves%>%distinct(POD, BIRTH_YEAR, total)%>%filter(POD == "DOUBTFUL")
dusky<-calves%>%distinct(POD, BIRTH_YEAR, total)%>%filter(POD == "DUSKY")

calves_wide<-doubtful%>%
  dplyr::rename("doubtful_calves" = total)%>%
  left_join(dusky, by = "BIRTH_YEAR")%>%
  dplyr::rename("dusky_calves" = total)%>%
  filter(BIRTH_YEAR >= 2008)

ggplot(calves_wide,aes(x = dusky_calves, y = doubtful_calves))+
  geom_point(size = 2)+
  theme_bw()+
  xlim(c(0,15))+
  ylim(c(0,15))


doubtful_suv<-calves%>%distinct(POD, BIRTH_YEAR, suv, prop)%>%filter(POD == "DOUBTFUL" & suv == "3+")
dusky_suv<-calves%>%distinct(POD, BIRTH_YEAR, suv, prop)%>%filter(POD == "DUSKY" & suv == "3+")

calves_wide_suv<-doubtful_suv%>%
  dplyr::rename("doubtful_calves" = prop)%>%
  left_join(dusky_suv, by = "BIRTH_YEAR")%>%
  dplyr::rename("dusky_calves" = prop)%>%
  filter(BIRTH_YEAR >= 2008)

ggplot(calves_wide_suv,aes(x = dusky_calves, y = doubtful_calves))+
  geom_point(size = 2)+
  theme_bw()+
  xlim(c(0,15))+
  ylim(c(0,15))
