library(odbc);library(dplyr);library(DBI);library(ggplot2)

#reproductive assignment for available females

source('~/git-otago/Fiordland_reporting/scripts/connect to MySQL.R', local = TRUE)$value
photo_analysis_calfyear_sql<-dbReadTable(con, "photo_analysis_calfyear")%>%filter(ID_NAME != "CULL")
source('~/git-otago/Fiordland_reporting/scripts/life_history_ageclass update.R', local = TRUE)$value
lifehist<-lifehist

calves<-lifehist%>%
  filter(!is.na(MOM) & MOM != "")%>%
  dplyr::select(calf = NAME, SEX, MOM, BIRTH_YEAR, POD)

birth_calves<-calves%>%
  dplyr::select(-SEX, -calf)%>%
  tidyr::pivot_longer(cols = c(2), names_to = "year_type", values_to = "Year")%>%
  mutate(year_type = "CALF_BIRTH")%>%
  dplyr::rename("NAME" = "MOM")

head(birth_calves)

females<-lifehist%>%
  filter(SEX == "F")%>%
  filter(NAME != "COMMON")%>%
  dplyr::select(-SEX)%>%
  dplyr::select(POD, NAME, FIRST_CALF, BIRTH_YEAR, FIRST_YEAR, LAST_YEAR)%>%
  filter(LAST_YEAR != "<NA>")

females_long<-females%>%
  tidyr::pivot_longer(cols = c(3:6), names_to = "year_type", values_to = "Year")%>%
  filter(Year != "" & !is.na(Year))

females_tl<-females_long%>%
  filter(year_type != "LAST_YEAR")%>%
  filter(year_type != "FIRST_YEAR")%>%
  bind_rows(birth_calves)%>%
  arrange(NAME, Year)

head(females_tl)
female_last<-females_long%>%
  filter(year_type == "LAST_YEAR")%>%
  dplyr::select(NAME, "last_year" = Year)

lh_long<-lifehist%>%
  filter(SEX == "F")%>%
  tidyr::pivot_longer(cols = 14:ncol(lifehist), names_to = "YEAR", values_to = "life_stage")%>%
  dplyr::select(NAME, POD, YEAR, life_stage)

lh_ls_tl<-lh_long%>%
  left_join(females_tl, by = c("NAME" = "NAME", "YEAR" = "Year", "POD" = "POD"))%>%
  mutate(life_stage = case_when(
    year_type == "FIRST_CALF" ~ "F",
    year_type == "BIRTH_YEAR" ~ "C",
    year_type == "CALF_BIRTH" ~ "M",
    TRUE ~ life_stage
  ))%>%
  group_by(NAME, YEAR)%>%
  mutate(n = n())%>%
  #filter(NAME == "2-SCALLOPS")%>%
  ungroup()%>%
  arrange(NAME, YEAR, year_type)%>%
  filter(!(n == 2 & year_type == "CALF_BIRTH"))%>%
  mutate(life_stage_lag_lead = case_when(
    lag(year_type) == "CALF_BIRTH" | lag(year_type) == "FIRST_CALF" ~ "W",
    lead(year_type) == "CALF_BIRTH" | lead(year_type) == "FIRST_CALF" ~ "P",
    TRUE ~ NA
  ))%>%
  mutate(life_stage = case_when(
    !is.na(life_stage_lag_lead) ~ life_stage_lag_lead,
    TRUE ~ life_stage))%>%
  as.data.frame()


###

calves_mom_list<-split(calves, calves$MOM)
calves_mom_list$`2-SCALLOPS`
length(calves_mom_list)

for (j in 1:length(calves_mom_list)){
  x = as.data.frame(calves_mom_list[[j]])
  print(x)
  print(nrow(x))
  print("skip")

  for (i in 1:nrow(x)){
    print(i)
      wean_years<-photo_analysis_calfyear_sql%>%
        filter(ID_NAME == x$calf[i] | ID_NAME == x$MOM[i])%>%
        distinct(SURVEY_AREA, TRIP, DATETIME, ID_NAME, PHOTOGRAPHER, CALFYEAR)%>%
        group_by(TRIP, DATETIME, PHOTOGRAPHER)%>%
        mutate(n_time = n())%>%
        #filter(CALFYEAR == 2018)%>%
        arrange(DATETIME)%>%
        filter(n_time > 1)%>%
        ungroup()%>%
        distinct(CALFYEAR)%>%
        mutate(mom = x$MOM[i],
               wean_year = "W")
  
  if (i != nrow(x)){
    wean_years<-wean_years%>%
      filter(CALFYEAR < x$BIRTH_YEAR[i+1])
  }

  print(wean_years)
  lh_ls_tl$YEAR<-as.numeric(lh_ls_tl$YEAR)
  
  lh_ls_tl<-lh_ls_tl%>%
    left_join(wean_years, by = c("NAME" = "mom", "YEAR" = "CALFYEAR"))%>%
    mutate(life_stage = case_when(
      wean_year == "W" & life_stage == "A" ~ "W",
      TRUE ~ life_stage
    ))%>%
    dplyr::select(-wean_year)
  
  print(lh_ls_tl%>%group_by(life_stage)%>%tally())
  
}
}

lh_ls_tl%>%
  filter(NAME == "PAUA")

females_lifestage<-lh_ls_tl%>%
  left_join(female_last, by = "NAME")%>%
  filter(!is.na(last_year))%>%
  filter(YEAR <= last_year)%>%
  filter(life_stage != "<NA>")%>%
  filter(life_stage != "NA")%>%
  arrange(NAME, YEAR)

ls_tally<-females_lifestage%>%
  group_by(YEAR, POD, life_stage)%>%
  tally()%>%
  as.data.frame()


available_f<-ls_tally%>%filter(life_stage == "A" | life_stage == "U")%>%filter(YEAR != 2024)


calves_plot<-calves%>%
  group_by(POD,BIRTH_YEAR)%>%
  tally%>%
  ungroup()%>%
  add_row(POD = "DOUBTFUL",
              BIRTH_YEAR = "2015",
              n = 0)

ggplot(available_f)+
  geom_col(aes(x = YEAR, y = n, fill = life_stage))+
  geom_point(data = calves_plot, mapping = aes(x = as.numeric(BIRTH_YEAR), y = n), size = 4, alpha = 0.5)+
  geom_line(data = calves_plot, mapping = aes(x = as.numeric(BIRTH_YEAR), y = n), alpha = 0.5)+
  facet_wrap(~POD)


females_lifestage%>%
  filter(life_stage == "M" & YEAR == 2010)
