# read data
kwl_seed <- readxl::read_excel(here::here("analysis","data","raw_data","KWL-seed-P-H.xlsx"), sheet = 1,
                                col_types = "text")

kwl_seed_tidy <-
  kwl_seed %>%
  filter(`文化層` == "上文化層") %>%
  filter(!種類或部位 %in% c("待判J", "待判", "殘件")) %>%
  mutate(area = str_extract(`坑內位置`, "^.{1}")) %>%
  left_join(kwl_chro_tidy_6,
            by = c("Pit" = "Pit", "Layer" = "layer", "area" = "area")) %>%
  mutate(`重量(g)` = as.numeric(str_remove(`重量(g)`, "以下")))

rice_weight <-
  kwl_seed_tidy %>%
  filter(種類或部位 == "稻穀") %>%
  group_by(period) %>%
  summarize(`重量(g)` = sum(`重量(g)`)) %>%
  filter(!is.na(period))

rice_count <-
  kwl_seed_tidy %>%
  filter(種類或部位 == "稻穀") %>%
  filter(!`件數`== "碎") %>%
  mutate(`件數`= as.numeric(`件數`)) %>%
  group_by(period) %>%
  summarize(`件數` = sum(`件數`)) %>%
  filter(!is.na(period))

rice_weight_count <-
  rice_weight %>%
  left_join(rice_count) %>%
  pivot_longer(-period, names_to = "稻穀", values_to = "value")

# rice plot
rice_num_weight_plot <-
  rice_weight_count %>%
  mutate(period = factor(period, levels = c("CL1","CL2","CL3","CL4","CL5","CL6"), order = T)) %>%
  ggplot(aes(x= period, y= value, fill = `稻穀`)) +
  geom_bar(stat="identity", position = "dodge2") +
  labs(x= NULL, y= NULL) +
  #scale_y_continuous(breaks = seq(0, 90, by = 10)) +
  theme_minimal()
ggsave(here::here("analysis", "figures", "talk-seed-rice.png"), h = 3, w =4, units = "in")

# other seed
seed_number_period <-
  kwl_seed_tidy %>%
  filter(!is.na(period)) %>%
  filter(!種類或部位 == "稻穀") %>%
  count(period) %>%
  rename(piece = n)

seed_type_period <-
  kwl_seed_tidy %>%
  filter(!is.na(period)) %>%
  filter(!種類或部位 == "稻穀") %>%
  count(period, 種類或部位) %>%
  count(period) %>%
  rename(taxanomy = n)

# join
seed_num_type <-
  seed_number_period %>%
  left_join(seed_type_period)

# plot
seed_num_type_plot <-
  seed_num_type %>%
  mutate(period = factor(period, levels = c("CL1","CL2","CL3","CL4","CL5","CL6"), order = T)) %>%
  ggplot(aes(x= period, y=piece,  width=0.7)) +
  geom_bar(stat="identity")+
  geom_line(aes(x= period, y= taxanomy),  size=2, group = 1, color = "red")+
  labs(x= NULL, y= "植物種子數量 (紅線: 種類)") +
  #scale_y_continuous(breaks = seq(0, 90, by = 10)) +
  theme_minimal()

##########specific taxa########
seed_specific_period <-
  kwl_seed_tidy %>%
  filter(!is.na(period)) %>%
  rename("種類" = "種類或部位") %>%
  filter(`種類` %in% c("林投子","桃","花生", "西瓜")) %>%
  count(period, `種類`)

seed_group_plot <-
  seed_specific_period  %>%
  mutate(period = factor(period, levels = c("CL1","CL2","CL3","CL4","CL5","CL6"), order = T)) %>%
  ggplot(aes(x= period, y= n, color = `種類`, group =`種類`)) +
  geom_point() +
  geom_line()+
  labs(x= NULL, y= "常見植物種子") +
  theme_minimal()

plot_grid(rice_num_weight_plot,
          seed_num_type_plot ,
          seed_group_plot,
          ncol =3,
          labels = c('A', 'B', 'C'),
          rel_widths = c(1.2, 0.9, 1.2), label_size = 12)

ggsave(here::here("analysis", "figures", "talk-seed-1.png"), h = 3, w =9, units = "in")
