
# results from phi_all.R & phi_SA.R

## all data vs all data from only doubtful and dusky complexes

results_phi<-results_phi_all%>%
  bind_rows(results_phi_SA)

head(results_phi)

results_p<-results_p_all%>%
  bind_rows(results_p_SA)%>%
  filter(!(pod == "DUSKY" & calfyr_season == 2007.00))

head(results_p)

results_phi$season<-as.factor(results_phi$season)

phi_together<-ggplot(results_phi%>%filter(eff != "no effort"), aes(x = as.numeric(calfyr_season), y = median))+
  geom_errorbar(aes(ymin = q5, ymax = q95, color = area), alpha = 0.8)+
  geom_point(aes(shape = Season, color = area), alpha = 0.8)+
  #geom_errorbar(results_phi%>%filter(eff == "no effort"), mapping = aes(ymin = q5, ymax = q95), color = "grey", size = 1)+
  #geom_point(results_phi%>%filter(eff == "no effort"), mapping = aes(x = as.numeric(calfyr_season), y = median, shape = as.factor(season)), color = "grey", size = 3)+
  #ylim(c(0.0,1.0))+
  facet_wrap(~pod)+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),
        legend.box.background = element_rect(fill='white'),
        panel.grid.minor.x = element_blank(),
        legend.background = element_rect(color = "white"))+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab(expression('Survival probability,' *phi))+
  scale_color_manual("Data", values = c("All areas" = "red", "Complexes only" = "black"))

phi_together

ggsave('./figures/phi_together.png', phi_together, dpi = 300, width = 200, height = 100, units = "mm")

p_together<-ggplot(results_p%>%filter(eff != "no effort"), aes(x = as.numeric(calfyr_season), y = median))+
  geom_errorbar(aes(ymin = q5, ymax = q95, color = area), alpha = 0.8)+
  geom_point(aes(shape = Season, color = area), alpha = 0.8)+
  #geom_errorbar(results_p%>%filter(eff == "no effort"), mapping = aes(ymin = q5, ymax = q95), color = "grey", size = 1)+
  #geom_point(results_p%>%filter(eff == "no effort"), mapping = aes(x = as.numeric(calfyr_season), y = median, shape = as.factor(season)), color = "grey", size = 3)+
  #ylim(c(0.0,1.0))+
  facet_wrap(~pod)+
  theme_bw()+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  theme(legend.position = "bottom",
        legend.box.background = element_rect(fill='white'),
        panel.grid.minor.x = element_blank(),
        legend.background = element_rect(color = "white"))+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab(expression('Capture probability, p'))+
  scale_color_manual("Data",values = c("All areas" = "red", "Complexes only" = "black"))

p_together

ggsave('./figures/p_together.png', p_together, dpi = 300, width = 200, height = 100, units = "mm")

phi_p_together<-ggpubr::ggarrange(p_together,phi_together, common.legend = T, labels = "auto", ncol = 1, legend = "bottom")

ggsave('./figures/phi_p_together.png', phi_p_together, dpi = 300, width = 250, height = 200, units = "mm")

####
p_box<-ggplot(results_p%>%filter(eff != "no effort"), aes(x = as.factor(Season), y = median))+
  geom_boxplot(aes(fill = area), alpha = 0.5)+
  #geom_jitter(aes(color = Season))+
  facet_wrap(~pod)+
  ylab(expression('Capture probability, p'))+
  xlab("Season")+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_fill_manual("Data",values = c("All areas" = "red", "Complexes only" = "black"))

#ggsave('./figures/p_box.png', p_box, dpi = 300, width = 200, height = 100, units = "mm")

###
p_all<-results_p_all%>%filter(eff != "no effort")

# complexes only for capture days
library(ggridges)
season_point<-ggplot(results_p_SA%>%filter(eff != "no effort"), aes(y = median, x = n.x, fill = Season))+
  geom_point(shape = 21, size = 3, alpha = 0.5)+
  theme_bw()+
  theme(legend.position = "none")+
  xlab("# capture days")+
  ylab(expression('Capture probability, p'))+
  scale_fill_manual(values = c("Spring" = "orange", "Summer" = "lightgrey", "Winter" = "blue"))+
  facet_wrap(~pod+Season)
  
#ggsave('./figures/season_point.png', season_point, dpi = 300, width = 200, height = 100, units = "mm")

season_p<-ggpubr::ggarrange(p_box, season_point, labels = "auto", ncol = 1)
ggsave('./figures/season_p.png', season_p, dpi = 300, width = 200, height = 200, units = "mm")

##
nrow(results_p_all)

results_p_all%>%
  #filter(median >= 0.975)%>%
  group_by(pod)%>%
  dplyr::summarise(quantile(median, c(0.10)))
40/56 
