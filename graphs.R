
type_level


regional_1999_2021<-KESIS_elec_consumption_raw %>%
  mutate(year = as.numeric(year),
         type = fct_relevel(type, type_level),
         TWh = GWh/1000) %>% 
  ggplot(aes(x = year, y = TWh, color = type))+
  scale_color_manual(values = c("#ea1117", "#1f5f97", '#b3b09e','#b3b09e','#b3b09e','#b3b09e'))+
  geom_line()+
  theme_minimal()+
  theme(text= element_text(size= 14, family= 'nanumgothic'),
        plot.title = element_text(size = 20),
        plot.subtitle = element_markdown(),
        strip.text = element_textbox(
          size = 12,
          color = "white", fill = "#5D729D", box.color = "#4A618C",
          halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
          padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)),
        plot.title.position = "plot",
        legend.position ="none",
        panel.grid.minor = element_blank(),
        panel.grid.major.y =element_blank(),
        axis.text.x = element_text(size =12))+
  facet_wrap(~region, ncol = 4)+
  labs(x = "",
       y = "",
       title ="1990-2021년 대한민국 지역별 산업분류별 전력소비량",
       subtitle ="<span style = 'color:red'>제조업</span>, <span style = 'color:blue'>서비스업</span> 전력사용량을 나타내며, 회색은 가정용, 공공용, 농림어업, 광업",
       caption = "Souce :KESIS, KEPCO, Graphic :Jiseok")


ggsave(regional_1999_2021, file =  "./img/regional_1999_2021.png", width = 800, height =900, units = "px", dpi = 100)






type_img<- KESIS_elec_consumption_2021 %>% 
  group_by(type) %>% 
  summarise(TWh = sum(GWh)/1000) %>%
  # mutate(color_label =ifelse(type =="제조업", "1", "2")) %>% 
  mutate(color_label =case_when(type =="제조업" ~ "1",
                                type =="서비스업" ~"2",
                                TRUE ~ "3")) %>%
  ggplot(aes(x = reorder(type, TWh), y = TWh, fill = color_label))+
  geom_col()+
  scale_fill_manual(values = c("#ea1117", "#1f5f97", '#b3b09e'))+
  geom_text(aes(label = round(TWh, 1)),size=4, family= 'nanumgothic', hjust = -.2)+
  scale_y_continuous(limits =c(0, 300))+
  coord_flip()+
  
  theme_bw()+
  theme_minimal()+
  theme(text = element_text(size =16, family= 'nanumgothic'),
        plot.title =element_text(size = 20),
        plot.title.position = "plot",
        legend.position ="none")+
  labs(y ="TWh",
       x = "산업분류",
       title = "2021년 대한민국 산업분류별 전력소비량",
       caption = "Source : KESIS, KEPCO, Grahphic : Jiseok")



type_img

ggsave(type_img, file =  "./img/type_img.png", width = 600, height =400, units = "px", dpi = 100)


region_img<- KESIS_elec_consumption_2021 %>% 
  group_by(region) %>% 
  summarise(TWh = sum(GWh)/1000) %>%
  # mutate(color_label =ifelse(type =="제조업", "1", "2")) %>% 
  ggplot(aes(x = reorder(region, TWh), y = TWh))+
  geom_col()+
  geom_text(aes(label = round(TWh, 1)),size=4, family= 'nanumgothic', hjust = -.2)+
  scale_y_continuous(limits =c(0, 300))+
  coord_flip()+
  
  theme_bw()+
  theme_minimal()+
  theme(text = element_text(size =16, family= 'nanumgothic'),
        plot.title =element_text(size = 20),
        plot.title.position = "plot",
        legend.position ="none")+
  labs(y ="TWh",
       x = "지역",
       title = "")


region_img

ggsave(file =  "./img/region_img.png", width = 600, height =600, units = "px", dpi = 100)



region_facet_img<- KESIS_elec_consumption_2021 %>% 
  group_by(region, type) %>% 
  summarise(GWh = sum(GWh),
            TWh = GWh/1000) %>% 
  #mutate(color_label =ifelse(type =="제조업", "1", "2")) %>%
  mutate(color_label =case_when(type =="제조업" ~ "1",
                                type =="서비스업" ~"2",
                                TRUE ~ "3")) %>%
  ggplot(aes(x = fct_relevel(type, rev(type_level)), y = TWh, fill = color_label))+
  geom_col()+
  scale_y_continuous(limits =c(0, 90))+
  scale_fill_manual(values = c("#ea1117", "#1f5f97", '#b3b09e'))+
  coord_flip()+
  theme_bw()+
  theme_minimal()+
  geom_text(aes(label = round(TWh, 1)),size = 3, family= 'nanumgothic', hjust = -.2)+
  facet_wrap(~fct_relevel(region, region_level),ncol = 4)+
  theme(text= element_text(size= 14, family= 'nanumgothic'),
        plot.title = element_text(size = 20),
        strip.text = element_textbox(
          size = 12,
          color = "white", fill = "#5D729D", box.color = "#4A618C",
          halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
          padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)),
        plot.title.position = "plot",
        legend.position ="none",
        panel.grid.minor = element_blank(),
        panel.grid.major.y =element_blank(),
        axis.text.x = element_text(size =12))+
  labs(x = "",
       y = "",
       title ="2021년 대한민국 지역별 산업분류별 전력소비량",
       subtitle ="",
       caption = "Souce :KESIS, KEPCO, Graphic :Jiseok")


ggsave(region_facet_img, file =  "./img/region_facet_img.png", width = 800, height =900, units = "px", dpi = 100)

