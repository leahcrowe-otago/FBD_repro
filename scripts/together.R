
# results from phi_all.R & phi_SA.R

## all data vs all data from only doubtful and dusky complexes

results_phi<-results_phi_all%>%
  bind_rows(results_phi_SA)

head(results_phi)

results_p<-results_p_all%>%
  bind_rows(results_p_SA)

head(results_p)

results_phi$season<-as.factor(results_phi$season)

phi_together<-ggplot(results_phi%>%filter(eff != "no effort"), aes(x = as.numeric(calfyr_season), y = median))+
  geom_errorbar(aes(ymin = q5, ymax = q95, color = area), size = 1, alpha = 0.8)+
  geom_point(aes(shape = Season, color = area), size = 3, alpha = 0.8)+
  #geom_errorbar(results_phi%>%filter(eff == "no effort"), mapping = aes(ymin = q5, ymax = q95), color = "grey", size = 1)+
  #geom_point(results_phi%>%filter(eff == "no effort"), mapping = aes(x = as.numeric(calfyr_season), y = median, shape = as.factor(season)), color = "grey", size = 3)+
  #ylim(c(0.0,1.0))+
  facet_wrap(~pod)+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab(expression('Survival probability,' *phi))+
  scale_color_manual("Data", values = c("All areas" = "red", "Complexes only" = "black"))

phi_together

ggsave('./figures/phi_together.png', phi_together, dpi = 300, width = 350, height = 150, units = "mm")

p_together<-ggplot(results_p%>%filter(eff != "no effort"), aes(x = as.numeric(calfyr_season), y = median))+
  geom_errorbar(aes(ymin = q5, ymax = q95, color = area), size = 1, alpha = 0.8)+
  geom_point(aes(shape = Season, color = area),size = 3, alpha = 0.8)+
  #geom_errorbar(results_p%>%filter(eff == "no effort"), mapping = aes(ymin = q5, ymax = q95), color = "grey", size = 1)+
  #geom_point(results_p%>%filter(eff == "no effort"), mapping = aes(x = as.numeric(calfyr_season), y = median, shape = as.factor(season)), color = "grey", size = 3)+
  #ylim(c(0.0,1.0))+
  facet_wrap(~pod)+
  theme_bw()+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  theme(legend.position = "bottom")+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab(expression('Capture probability, p'))+
  scale_color_manual("Data",values = c("All areas" = "red", "Complexes only" = "black"))

p_together
ggsave('./figures/p_together.png', p_together, dpi = 300, width = 350, height = 150, units = "mm")

phi_p_together<-ggpubr::ggarrange(p_together,phi_together, common.legend = T, labels = "auto", ncol = 1, legend = "bottom")

ggsave('./figures/phi_p_together.png', phi_p_together, dpi = 300, width = 300, height = 250, units = "mm")
