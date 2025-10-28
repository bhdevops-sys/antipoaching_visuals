
#foot patrols for year 
patrl_ft<- atp5 %>%
  drop_na(Number_of_Patrols)%>%
  filter(type=='Foot')%>%
  group_by(month,year)%>%
  summarise(tot= sum((Number_of_Patrols), na.rm = TRUE))%>%
  ggplot(aes(x=month,y=tot,color=as.factor(year), group = year))+
  geom_point(size=2.5)+
  geom_line(linewidth = 1)+ 
  scale_color_manual(values = c('red','blue','#028a0f','#b53389'),limits = c("24", "23","22", "21"))+
  ggtitle('Fig 1: Number of Monthly Foot Patrols') + 
  geom_text(aes(label = sprintf("%1.0f", tot)),show.legend=F, vjust = 0.5, hjust = 2, size = 5) +
  xlab('Month') + 
  ylab('Number of Patrols') +
  guides(col= guide_legend(title= "year"))+
  theme(legend.title = element_text(color = "grey20", size = 16),legend.text = element_text(color = "grey20", size = 14))+
  theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 0, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "#808080", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "#808080", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.title= element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "italic"),
        strip.text = element_text(size=18))
 patrl_ft

#boat patrols for year 
patrl_bt<- atp5 %>%
  drop_na(Number_of_Patrols)%>%
  filter(type=='Boat')%>%
  group_by(month,year)%>%
  summarise(tot= sum((Number_of_Patrols), na.rm = TRUE))%>%
  ggplot(aes(x=month,y=tot,color = as.factor(year), group = year))+
  geom_point(size=2.5)+
  geom_line(linewidth = 1)+ 
  scale_color_manual(values = c('red','blue','#028a0f','#b53389'),limits = c("24", "23","22", "21"))+
  ggtitle('Number of Monthly Boat Patrols') + 
  geom_text(aes(label = tot),show.legend=F, vjust = 0.5, hjust = 2, size = 5) +
  xlab('Month') + 
  ylab('Number of Patrols') +
  guides(col= guide_legend(title= "year"))+
  theme(legend.title = element_text(color = "grey20", size = 16),legend.text = element_text(color = "grey20", size = 14))+
  theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 0, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "#808080", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "#808080", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.title= element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "italic"),
        strip.text = element_text(size=18))
 patrl_bt

#Patrol hrs foot

patrl_hrs_ft<- atp5 %>%
  drop_na(Number_of_Patrols)%>%
  filter(type=='Foot')%>%
  group_by(month,year)%>%
  summarise(tot= sum((Number_of_Patrol_Hours), na.rm = TRUE))%>%
  ggplot(aes(x=month,y=tot,color = as.factor(year), group = year))+
  geom_point(size=2.5)+
  geom_line(linewidth = 1)+ 
  scale_color_manual(values = c('red','blue','#028a0f','#b53389'),limits = c("24", "23","22", "21"))+
  ggtitle('Number of Monthly Foot Patrol Hours') + 
  geom_text(aes(label = sprintf("%1.0f", tot)),show.legend=F, vjust = 0.5, hjust = 2, size = 5) +
  xlab('Month') + 
  ylab('Number of Patrol Hours') +
  guides(col= guide_legend(title= "year"))+
  theme(legend.title = element_text(color = "grey20", size = 16),legend.text = element_text(color = "grey20", size = 14))+
  theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 0, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "#808080", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "#808080", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.title= element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "italic"),
        strip.text = element_text(size=18))
 patrl_hrs_ft



#avg 

patrl_hrs_avg_ft<- atp5 %>%
  drop_na(Number_of_Patrols)%>%
  filter(type=='Foot')%>%
  group_by(month,year)%>%
  summarise(avg_phrs=mean((Number_of_Patrol_Hours),na.rm = T)) %>%
  ggplot(aes(x=month,y=avg_phrs,color =as.factor(year), group = year))+
  geom_point(size=2.5)+
  geom_line(linewidth = 1)+ 
  scale_color_manual(values = c('red','blue','#028a0f','#b53389'),limits = c("24", "23","22", "21"))+
  labs(colour = "year")+
  ggtitle('Mean Yearly Foot Patrol Hours') + 
  geom_text(aes(label = sprintf("%1.0f", avg_phrs)),show.legend=F, vjust = 0.5, hjust = 2, size = 5) +
  xlab('Month') + 
  ylab('Sum') +
  theme(legend.title = element_text(color = "grey20", size = 16),legend.text = element_text(color = "grey20", size = 14))+
  theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 0, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "#808080", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "#808080", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.title= element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "italic"),
        strip.text = element_text(size=18))
 patrl_hrs_avg_ft

#Patrol hrs boat

 patrl_hrs_bt<- atp5 %>%
  drop_na(Number_of_Patrols)%>%
  filter(type=='Boat')%>%
  group_by(month,year)%>%
  summarise(tot= sum((Number_of_Patrol_Hours), na.rm = TRUE))%>%
  ggplot(aes(x=month,y=tot, color = as.factor(year), group = year))+
  geom_point(size=2.5)+
  geom_line(linewidth = 1)+ 
  scale_color_manual(values = c('red','blue','#028a0f','#b53389'),limits = c("24", "23","22", "21"))+
  ggtitle('Number of Monthly Boat Patrol Hours') + 
  geom_text(aes(label = sprintf("%1.0f", tot)),show.legend=F, vjust = 0.5, hjust = 2, size = 5) +
  xlab('Month') + 
  ylab('Number of Patrol Hours') +
  guides(col= guide_legend(title= "year"))+
  theme(legend.title = element_text(color = "grey20", size = 16),legend.text = element_text(color = "grey20", size = 14))+
  theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 0, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "#808080", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "#808080", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.title= element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "italic"),
        strip.text = element_text(size=18))
 patrl_hrs_bt


#patrol dist ft
patrl_dist_ft<- atp5 %>%
  drop_na(Number_of_Patrols)%>%
  filter(type=='Foot')%>%
  group_by(month,year)%>%
  summarise(tot= sum((Distance_km), na.rm = TRUE))%>%
  ggplot(aes(x=month,y=tot,color = as.factor(year), group = year))+
  geom_point(size=2.5)+
  geom_line(linewidth = 1)+ 
  scale_color_manual(values = c('red','blue','#028a0f','#b53389'),limits = c("24", "23","22", "21"))+
  ggtitle('Monthly Foot Patrol Distance(Km)') + 
  geom_text(aes(label = sprintf("%1.0f", tot)),show.legend=F, vjust = 0.5, hjust = 2, size = 5) +
  xlab('Month') + 
  ylab('Patrol Distance (km)') +
  guides(col= guide_legend(title= "year"))+
  theme(legend.title = element_text(color = "grey20", size = 16),legend.text = element_text(color = "grey20", size = 14))+
  theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 0, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "#808080", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "#808080", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.title= element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "italic"),
        strip.text = element_text(size=18))
 patrl_dist_ft

#patrol dist bt
patrl_dist_bt<- atp5 %>%
  drop_na(Number_of_Patrols)%>%
  filter(type=='Boat')%>%
  group_by(month,year)%>%
  summarise(tot= sum((Distance_km), na.rm = TRUE))%>%
  ggplot(aes(x=month,y=tot,color = as.factor(year), group = year))+
  geom_point(size=2.5)+
  geom_line(linewidth = 1)+ 
  scale_color_manual(values = c('red','blue','#028a0f','#b53389'),limits = c("24", "23","22", "21"))+
  ggtitle('Number Boat Patrol Distance(Km)') + 
  geom_text(aes(label = sprintf("%1.0f", tot)),show.legend=F, vjust = 0.5, hjust = 2, size = 5) +
  xlab('Month') + 
  ylab('Patrol Distance(Km)') +
  guides(col= guide_legend(title= "year"))+
  theme(legend.title = element_text(color = "grey20", size = 16),legend.text = element_text(color = "grey20", size = 14))+
   theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 0, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "#808080", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "#808080", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.title= element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "italic"),
        strip.text = element_text(size=18))
 patrl_dist_bt


# avg patrol dist ft
patrl_dist_avg_ft<- atp5 %>%
  drop_na(Number_of_Patrols)%>%
  filter(type=='Foot')%>%
  group_by(month,year)%>%
  summarise(avg_pdt=mean((Distance_km),na.rm = T)) %>%
  ggplot(aes(x=month,y=avg_pdt,color = as.factor(year), group = year))+
  geom_point(size=2.5)+
  geom_line(linewidth = 1)+ 
  scale_color_manual(values = c('red','blue','#028a0f','#b53389'),limits = c("24", "23","22", "21"))+
  ggtitle(' Mean Yearly Foot Patrol Distance(Km)') + 
  geom_text(aes(label = sprintf("%1.0f", avg_pdt)),show.legend=F, vjust = 0.5, hjust = 2, size = 5) +
  xlab('Month') + 
  ylab('Sum') +
  theme(legend.title = element_text(color = "grey20", size = 16),legend.text = element_text(color = "grey20", size = 14))+
  theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 0, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "#808080", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "#808080", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.title= element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "italic"),
        strip.text = element_text(size=18))
 patrl_dist_avg_ft


#turtle poaching
 antp_l<-atp2 %>%
   drop_na(Sum)%>%
   filter(Observation_Category_0=='Poached turtles sum')|>
   group_by(month,year)%>%
   summarise(tot = sum((Sum), na.rm = TRUE)) %>%
   ggplot(aes(x=month,y=tot,color =as.factor(year), group = year))+
   geom_point(size=2.5)+
   geom_line(linewidth = 1)+ 
   scale_color_manual(values = c('red','blue','#028a0f','#b53389'),limits = c("24", "23","22", "21"))+
   #scale_color_discrete(limits = c("2024", "2023","2022", "2021"))+
   ggtitle('Number of Monthly Poached Turtle') + 
   geom_text(aes(label = tot),show.legend=F, vjust = 0.5, hjust = 2, size = 5) +
   xlab('Month') + 
   ylab('Number of Poached Turtles') +
   labs(colour = "year")+
   theme(legend.title = element_text(color = "grey20", size = 16),legend.text = element_text(color = "grey20", size = 14))+
   theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
         axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 0, vjust = 0, face = "plain"),  
         axis.title.x = element_text(color = "#808080", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
         axis.title.y = element_text(color = "#808080", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
         plot.title= element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "italic"),
         strip.text = element_text(size=18))
  antp_l
 

#mangrove logging based on stumps Mangrove stumps

mang_l<-atp2 %>%
  filter(Evidence=='Mangrove stumps')|>
  group_by(month,year)%>%
  summarise(tot = sum((Sum), na.rm = TRUE)) %>%
  ggplot(aes(x=month,y=tot,color = as.factor(year), group = year))+
  geom_point(size=2.5)+
  geom_line(linewidth = 1)+ 
  scale_color_manual(values = c('red','blue','#028a0f','#b53389'),limits = c("24", "23","22", "21"))+
  ggtitle('Number of Monthly Illegal Mangrove Logging') + 
  geom_text(aes(label = tot),show.legend=F, vjust = 0.5, hjust = 2, size = 5) +
  xlab('Month') + 
  ylab('Number of Illegal Logging') +
  labs(colour = "year")+
  theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 0, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "#808080", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "#808080", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.title= element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "italic"),
        strip.text = element_text(size=18))
mang_l


#mangrove logging based on count of logging cases Mangrove stumps

mang_lc<-atp2 %>%
  count(Observation_Category_0,month,year)|>
  filter(Observation_Category_0=='Mangrove logging')%>%
  group_by(month,year)%>%
  summarise(tot = sum((n), na.rm = TRUE))%>%
  ggplot(aes(x=month,y=tot,color = as.factor(year), group = year))+
  geom_point(size=2.5)+
  geom_line(linewidth = 1)+ 
  scale_color_manual(values = c('red','blue','#028a0f','#b53389'),limits = c("24", "23","22", "21"))+
  ggtitle('Number of Monthly Illegal Mangrove Logging') + 
  geom_text(aes(label = tot),show.legend=F, vjust = 0.5, hjust = 2, size = 5) +
  xlab('Month') + 
  ylab('Number of Illegal Logging') +
  labs(colour = "year")+
  theme(legend.title = element_text(color = "grey20", size = 16),legend.text = element_text(color = "grey20", size = 14))+
  theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 0, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "#808080", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "#808080", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.title= element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "italic"),
        strip.text = element_text(size=18))

 mang_lc

#illegal fishing gears
ilg_fish_g<-atp2 %>%
  filter(Observation %in% c('Undersized net', 'Monofilament','Spear gun','Mosquito net'))|>
  group_by(month,year)%>%
  summarise(tot = sum((Sum_of_Evidence), na.rm = TRUE)) %>%
  ggplot(aes(x=month,y=tot,color = as.factor(year), group = year))+
  geom_point(size=2.5)+
  geom_line(linewidth = 1)+ 
  scale_color_manual(values = c('red','blue','#028a0f','#b53389'),limits = c("24", "23","22", "21"))+
  ggtitle('Number of Monthly Illegal Fishing Gears') + 
  geom_text(aes(label = tot),show.legend=F, vjust = 0.5, hjust = 2, size = 5) +
  xlab('Month') + 
  ylab('Number of Illegal Fishing Gears') +
  labs(colour = "year")+
  theme(legend.title = element_text(color = "grey20", size = 16),legend.text = element_text(color = "grey20", size = 14))+
  theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 0, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "#808080", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "#808080", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.title= element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "italic"),
        strip.text = element_text(size=18))
 ilg_fish_g

# resource users 

usersl<-atp2 %>%
  drop_na(User_Type)%>%
  filter(Observation_Category_0=='Resource use')%>%
  group_by(month,year)%>%
  summarise(tot = sum((Sum), na.rm = TRUE))%>%
  ggplot(aes(x=month,y=tot,color = as.factor(year), group = year))+
  geom_point(size=2.5)+
  geom_line(linewidth = 1)+ 
  scale_color_manual(values = c('red','blue','#028a0f','#b53389'),limits = c("24", "23","22", "21"))+
  geom_text(aes(label = tot),show.legend=F, vjust = 0.5, hjust = 2, size = 5) +
  ggtitle('Number of Monthly Resource Users') + 
  xlab('Month') + 
  ylab('Number of Resource Users') +
  labs(colour = "year")+
  theme((legend.position="right"), size = 5)+
  theme(legend.title = element_text(color = "grey20", size = 16),legend.text = element_text(color = "grey20", size = 14))+
  theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 0, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "#808080", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "#808080", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.title= element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "italic"),
        strip.text = element_text(size=18))
  usersl
