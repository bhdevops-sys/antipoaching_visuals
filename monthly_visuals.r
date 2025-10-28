#patrols monthly
antp <- atp5 %>%
  drop_na(Number_of_Patrols)%>%
  filter (month=='Sep' & year=='24')%>%
  group_by (month) %>%
  summarise(tot= sum((Number_of_Patrols), na.rm = TRUE))%>%
  ggplot(aes(x=month, y=tot,group = tot)) +
  geom_bar(stat='identity',fill = '#D4B068')+
  geom_text(aes(label=tot,), size = 10, hjust = .5, vjust = 1.5,colour='white')+
  ggtitle('Patrols for the last month') + 
  xlab('Month') + 
  ylab('Sum of Patrols') +
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 0, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "#808080", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "#808080", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.title= element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = .5, face = "italic"),
        strip.text = element_text(size=18))

 antp

#patrols for year

antp_y <- atp5 %>%
  drop_na(Number_of_Patrols)%>%
  filter(year=='24')%>%
  group_by(year)%>%
  summarise(tot= sum((Number_of_Patrols), na.rm = TRUE))%>%
  ggplot(aes(x=year, y=tot)) +
  geom_bar(stat='identity',fill = '#D4B068')+
  geom_text(aes(label=tot,), size = 10, hjust = .5, vjust = 1.5,colour='white')+
  ggtitle('Patrols for the year') + 
  xlab('Year') + 
  ylab('Sum of Patrols') +
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 0, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "#808080", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "#808080", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.title= element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = .5, face = "italic"),
        strip.text = element_text(size=18))
 antp_y

#Poached turtle monthly

rh_cols <- c('#D4B068',"#454B1B", "#808000")

antp_t <- atp2 %>%
  drop_na(Sum) %>%
  filter (Observation_Category_0 == 'Poached turtles sum' & month == 'Sep' & year == '24') %>%
  group_by(Age, month, year) %>%
  summarise(tot = sum((Sum), na.rm = TRUE)) %>%
  ggplot(aes(x = 2, tot, fill = Age)) +
  scale_x_discrete(drop=FALSE)+
  geom_bar(stat='identity',width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label=tot,), size = 12,colour='white',position = position_stack(vjust = .5))+
  scale_fill_manual(values = rh_cols) +
  theme_void() +
  xlim(0.5, 2.5)+
  ggtitle('Mortalities for the last month')+
  theme(title= element_text(color = "grey20", size = 14, angle = 0, hjust = 0.5,face = "italic"),
        strip.text = element_text(size=18))
 antp_t
 #Poached turtle yearly
antp_y1<-atp2 %>%
  drop_na(Sum)%>%
  filter(Observation_Category_0=='Poached turtles sum' & year=='24')%>%
  group_by(Age, year)%>%
  summarise(tot = sum((Sum), na.rm = TRUE)) %>%
  ggplot(aes(x=2,tot,fill=Age)) +
  geom_bar(stat='identity',width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label=tot,), size = 12,colour='white',position = position_stack(vjust = .5))+
  scale_fill_manual(values = rh_cols) +
  theme_void() +
  xlim(0.5, 2.5)+
  ggtitle('Mortalities from year start to Date')+
  theme(title= element_text(color = "grey20", size = 14, angle = 0, hjust = 0.5,face = "italic"),
        strip.text = element_text(size=18))
antp_y1

# mangroves
rh_col2 <- c('#D4B068',"#454B1B", "#808000","#808059",'#820059','#980900')
antp_m<-atp2 %>%
  drop_na(Sum)%>%
  filter(Observation_Category_0=='Mangrove logging' & month=='Sep'& year=='24' )%>%
  group_by(Evidence,year)%>%
  summarise(tot = sum((Sum), na.rm = TRUE)) %>%
  ggplot(aes(x=2,tot,fill=Evidence)) +
  geom_bar(stat='identity',width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label=tot,), size = 10,colour='white',position = position_stack(vjust = .5))+
  scale_fill_manual(values = rh_col2) +
  theme_void() +
  xlim(0.5, 2.5)+
  ggtitle('Mangrove Poaching for last month')+
  theme(title= element_text(color = "grey20", size = 14, angle = 0, hjust = 0.5,face = "italic"),
        strip.text = element_text(size=18))

antp_m


# mangroves year
rh_col <- c('#D4B068',"#454B1B", "#808000","#808059",'#820059','#980900')
antp_my<-atp2 %>%
  drop_na(Evidence)%>%
  filter(Observation_Category_0=='Mangrove logging' & year=='24')%>%
  group_by(Evidence,year)%>%
  summarise(tot = sum((Sum), na.rm = TRUE)) %>%
  ggplot(aes(x=2,tot,fill=Evidence)) +
  geom_bar(stat='identity',width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label=tot,), size = 10,colour='white',position = position_stack(vjust = .5))+
  scale_fill_manual(values = rh_col) +
  theme_void() +
  xlim(0.5, 2.5)+
  ggtitle('Mangroves Poaching from year start to Date')+
  theme(title= element_text(color = "grey20", size = 14, angle = 0, hjust = 0.5,face = "italic"),
        strip.text = element_text(size=18))

antp_my

# illegal gear bar

antp_ig1<-atp2 %>%
  drop_na(Observation)%>%
  filter(Observation_Category_0=='Resource use'& Observation %in% c('Undersized net', 'Monofilament',
                                                                    'Spear gun','Mosquito net') & month=='Sep' & year=='24')%>%
  group_by(Observation,year)%>%
  summarise(tot = sum((Sum_of_Evidence), na.rm = TRUE)) %>%
  ggplot(aes(x=Observation, y=tot,group = tot)) +
  geom_bar(stat='identity',fill = "#808000")+
  geom_text(aes(label=tot,), size = 10, hjust = .5, vjust = 1.5,colour='white')+
  ggtitle('Illegal gear for the last month') + 
  xlab('Sep') + 
  ylab('Sum of Evidence') +
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"),  
        axis.title.x = element_text(color = "#808080", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "#808080", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.title= element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = .5, face = "italic"),
        strip.text = element_text(size=18))
antp_ig2<-antp_ig1 + scale_y_continuous(breaks = seq(-6, 6, by = 1))
antp_ig2

#illegal gear by year
rh_col2 <- c('#D4B068',"#454B1B", "#808000",'#820059')

antp_igy<-atp2 %>%
  drop_na(Observation)%>%
  filter(Observation_Category_0=='Resource use' & Observation %in% c('Undersized net', 'Monofilament',
                                                                     'Spear gun','Mosquito net') & year=='24' )%>%
  group_by(Observation,year)%>%
  summarise(tot = sum((Sum_of_Evidence), na.rm = TRUE)) %>%
  ggplot(aes(x=Observation, y=tot,group = tot)) +
  geom_bar(stat='identity',fill = "#808000")+
  scale_x_discrete(guide=guide_axis(n.dodge=2)) +
  geom_text(aes(label=tot,), size = 10, hjust = .5, vjust = 1.5,colour='white')+
  ggtitle('Illegal gear from year start to Date') + 
  xlab('2024') + 
  ylab('Sum of Evidence') +
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "#808080", size = 12, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "#808080", size = 12, angle = 90, hjust = .5, vjust = 0, face = "plain"),
        plot.title= element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = .5, face = "italic"),
        strip.text = element_text(size=18))
antp_igy
