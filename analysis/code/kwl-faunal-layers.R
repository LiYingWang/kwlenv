library(tidyverse)
# read data
kwl_fauna <- readxl::read_excel(here::here("analysis","data","raw_data","KWL_faunal_P.xlsx"), sheet = 1,
                                col_types = "text")

kwl_fauna_HM <- readxl::read_excel(here::here("analysis","data","raw_data","KWL_faunal_HM.xlsx"), sheet = 1,
                                   col_types = "text")

kwl_fauna_broken <- readxl::read_excel(here::here("analysis","data","raw_data","broken_fauna.xlsx"), sheet = 1,
                                   col_types = "text")

sample_p <- readr::read_csv(here::here("analysis", "data", "raw_data", "kwl-list-of-sampling-squares.csv"))

kwl_chro_6 <- readxl::read_excel(here::here("analysis", "data", "raw_data", "KWL_chronology.xlsx"), sheet = 5)

# tidy chronological data
kwl_chro_tidy_6 <-
  kwl_chro_6 %>%
  mutate(area = str_sub(Pit, 5, 7), Pit = str_sub(Pit, 1, 4)) %>%
  mutate(area = ifelse(area == "", "ABCD", area)) %>%
  mutate(area = sapply(strsplit(area, ""), paste, collapse= ",")) %>%
  separate_rows(area) %>%
  pivot_longer(cols = starts_with("L"), names_to = "layer", values_to = "period") %>%
  mutate(layer = str_sub(layer, 2, 3)) %>%
  mutate(layer = ifelse(nchar(layer) == 1, paste0("0", layer), layer))

# combine faunal data from cultural layers and temporal sequences
fauna_sam <-
  kwl_fauna %>%
  mutate(Pit = paste(context, pit, sep = "")) %>%
  filter(Pit %in% sample_p$the_sq) %>%
  filter(componant == "上文化層" &!layer == "00") %>% #remove sc
  filter(!str_detect(`總報告類別`, "[[:punct:]]+|[0-9]+")) %>%
  mutate(area = ifelse(is.na(area), "C", area)) %>% # assign an area to the blanks, which won't change the results
  left_join(kwl_chro_tidy_6, by = c("Pit" = "Pit", "layer" = "layer", "area" = "area")) %>%
  select(-section)

broken <- kwl_fauna %>% filter(`部位/名稱` == "碎骨")

# combine faunal data from middens and temporal sequences
fauna_H_sam <-
  kwl_fauna_HM %>%
  mutate(Pit = paste("P", pit, sep = "")) %>%
  filter(Pit %in% sample_p$the_sq) %>%
  filter(componant == "上文化層") %>%
  filter(!str_detect(`總報告類別`, "[[:punct:]]+|[0-9]+")) %>%
  left_join(kwl_chro_tidy_6, by = c("Pit" = "Pit", "layer" = "layer", "area" = "area"))

# tidy data
fauna_combined_context <-
  rbind(fauna_sam, fauna_H_sam) %>% # combine the two datasets
  mutate(`重量(g)` = as.numeric(`重量(g)`)) %>%
  mutate(`種屬/屬類` = ifelse(is.na(`種屬/屬類`), "none", `種屬/屬類`)) %>%
  mutate(period = factor(period, levels = c("CL1","CL2","CL3","CL4","CL5","CL6"), order = T)) %>%
  mutate(taxa = case_when(`總報告類別` == "鹿" ~ "Deer",
                          `總報告類別` == "豬" ~ "Pig",
                          `總報告類別` == "牛" ~ "Cattle",
                          `總報告類別` == "魚" ~ "Fish",
                          `總報告類別` == "鳥" ~ "Bird",
                          `總報告類別` == "囓齒" ~ "Rodent")) %>%
  mutate(taxa = ifelse(str_detect(`種屬/屬類`, "羌"), "Muntjac", taxa)) %>%
  mutate(class = case_when(taxa == "Fish" ~ "Fish",
                           taxa == "Bird" ~ "Bird",
                           TRUE ~ "Mammal")) %>%
  mutate(size = case_when(taxa == "Deer" ~ "Large animals",
                          taxa == "Cattle" ~ "Large animals",
                          taxa == "Pig" ~ "Medium animals",
                          taxa == "Muntjac" ~ "Medium animals",
                          TRUE ~ "Small animals")) %>%
  mutate(cutmarks = ifelse(`人為痕跡` == "", "no", "yes"))

# counts
fauna_combined_taxa <-
  fauna_combined_context %>% count(taxa) %>%
  mutate(percentage = n/sum(n))

# calculate MNI
fauna_combined_MNI <-
  fauna_combined_context %>%
  mutate(`部位/左右` = case_when(`部位/左右` == "Ｌ" ~ "L",
                                  `部位/左右` == "Ｒ" ~ "R", TRUE ~ `部位/左右`)) %>%
  filter(!is.na(`部位/左右`)&!`部位/左右` == "可") %>%
  count(`部位/左右`, taxa, `部位/名稱`)

################### ubiquity ###################
# calculate the number of units for each cultural layers
kwl_unit_per_period <-
  kwl_chro_6 %>%
  mutate(all = pmap_chr(select(., -Pit, -Grid), ~toString(unique(na.omit(c(...)))))) %>%  # unite and remove duplicates
  mutate(unit = str_extract(Pit, "[A-Z]+[0-9]{3}")) %>%
  distinct(unit, .keep_all = TRUE) %>%
  mutate(CL1 = ifelse(str_detect(all, "CL1"), unit, NA)) %>%
  mutate(CL2 = ifelse(str_detect(all, "CL2"), unit, NA)) %>%
  mutate(CL3 = ifelse(str_detect(all, "CL3"), unit, NA)) %>%
  mutate(CL4 = ifelse(str_detect(all, "CL4"), unit, NA)) %>%
  mutate(CL5 = ifelse(str_detect(all, "CL5"), unit, NA)) %>%
  mutate(CL6 = ifelse(str_detect(all, "CL6"), unit, NA))

# count the units by cultural layers for later join
unit_by_period <-
  data.frame(unit_count = colSums(!is.na(kwl_unit_per_period))) %>%
  slice_tail(n = 6) %>%
  tibble::rownames_to_column("period")

# ubiquity of taxa over the 40 units by temporal sequences
fauna_ubiquity <-
  fauna_combined_context %>%
  select(Pit, layer, taxa, `重量(g)`, period, size) %>%
  count(period, Pit, taxa) %>%
  count(period, taxa) %>%
  filter(!is.na(period)) %>%
  left_join(unit_by_period) %>% # join the unit counts for each period
  mutate(ubiquity = paste(round(n/unit_count *100, 2), "%")) %>%
  select(-n, -unit_count) %>%
  pivot_wider(names_from = period, values_from = ubiquity) %>%
  mutate(taxa = fct_relevel(taxa, "Deer", "Pig", "Muntjac", "Cattle", "Rodent", "Bird", "Fish")) %>%
  arrange(taxa)
  #replace_na(list(`pre-European` = "-", `post-European` = "-", `Chinese` = "-"))

################### Taxonomic abundance ###################
# barplot I: classes of vertebrates by period
fauna_taxa_barplot <-
  fauna_combined_context %>%
  drop_na(period, class) %>%
  mutate(class = factor(class, levels = c("Mammal", "Bird", "Fish"), ordered = TRUE)) %>%
  ggplot(aes(x = period, fill = class))+
  geom_bar(position = position_dodge2(preserve = "single")) +
  labs(y = "NISP", x = NULL) +
  theme_minimal() +
  theme(legend.title=element_blank())

# barplot II: relative abundance of mammals by period
fauna_mammal_barplot <-
  fauna_combined_context %>%
  filter(class == "Mammal") %>%
  drop_na(period) %>%
  mutate(taxa = factor(taxa, levels = c("Deer", "Pig", "Muntjac", "Cattle", "Rodent"), ordered = TRUE)) %>%
  ggplot(aes(x = period, fill = taxa))+
  geom_bar(position = "fill", width = 0.6) +
  labs(y = NULL, x = NULL) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  guides(fill= guide_legend(title="Mammals"))

ggsave(here::here("analysis", "figures", ".png"), h = 3, w =5, units = "in")

################### Skeletal part ###################
fauna_exclud <-
  fauna_combined_context %>%
  filter(taxa %in% c("Deer")) %>%
  filter(!is.na(`部位/名稱`)) %>%
  distinct(`部位/名稱`)

fauna_deer_portion <-
  fauna_combined_context %>%
  filter(taxa %in% c("Deer")) %>%
  mutate(portion = case_when(
    `部位/名稱` %in% c("肩胛骨","肩頰骨","肩胛骨.","胸椎","腰椎","肋骨","肩頰,肋骨","薦骨","胸骨") ~ "mid-meaty\n(body)",
    `部位/名稱` %in% c("肱骨", "尺骨", "radius", "肱骨humerus", "左肱骨","橈骨", "Humerus", "骺",
                   "脛骨", "tibia", "脛關節", "髖骨", "股骨", "肢骨") ~ "meaty\n(upper limb)",
    `部位/名稱` %in% c("掌骨","掌骨或蹠骨","蹠骨","踝骨","腕骨","metatarsal","metacarpal","腕或踝骨","腕骨或踝骨",
                   "蹠骨(或掌骨)","掌或蹠骨","蹠骨或掌骨","跟骨","astragalus","astragulas","calcanous",
                   "第1趾骨","第2趾骨","第3趾骨","左第2趾骨","右第2趾骨","carpal附骨？","附骨","趾骨,附骨") ~ "non-meaty\n(lower limb)",
    `部位/名稱` %in% c("下顎及齒","上或下顎","上下顎齒","下顎骨","顱骨","第二頸椎","枕骨",
                   "前臼齒","碎齒","齒","臼齒","上顎臼齒") ~ "non-meaty\n(cranial parts)")) #角基部, 犄角, 鹿角

weight_head <-
fauna_deer_portion %>%
  mutate(`重量(g)` = as.numeric(`重量(g)`)) %>%
  filter(portion == "non-meaty\n(cranial parts)") %>%
  group_by(period) %>%
  summarise(weight = sum(`重量(g)`)) %>%
  filter(!is.na(period)) %>%
  ggplot(aes(x= period, y=weight,  width=0.5)) +
  geom_bar(stat="identity")+
  labs(x= NULL, y= "weight of cranial parts(g)") +
  theme_minimal()

deer_portion_plot <-
  fauna_deer_portion %>%
  select(period, portion, taxa) %>%
  count(period, portion, taxa) %>%
  drop_na() %>%
  mutate(period = factor(period, levels = c("CL1","CL2","CL3","CL4","CL5","CL6"))) %>% #ordered = TRUE
  ggplot(aes(period, n, fill = portion)) +
  geom_bar(stat = "identity", position = "fill", width = 0.6) +
  #facet_wrap(~taxa) +
  labs(x = NULL, y = NULL) +
  guides(fill= guide_legend(title="portion"))+
  scale_fill_viridis_d(labels=c('upper limb', 'body', 'cranial parts','lower limb'))+
  #scale_fill_discrete(breaks=c("body\n(mid-meaty)", "upper limb\n(meaty)", "lower limb\n(meaty)","cranial elements(none-meaty)")) +
  theme_minimal()

ggsave(here::here("analysis", "figures", "talk-fauna-2.png"), h = 4, w =4, units = "in")

library(cowplot)
plot_grid(fauna_mammal_barplot, deer_portion_plot,
          labels = c('A', 'B'), rel_widths = c(1.3, 1), label_size = 12)

ggsave(here::here("analysis", "figures", "talk-fauna.png"), h = 4, w =9, units = "in")

################### Cutmarks ###################
# count cutmarks throughout all taxa
fauna_total_cut <-
  fauna_combined_context %>%
  group_by(taxa, period, cutmarks) %>%
  count() %>%
  rename(`NISP with cutmarks` = n) %>%
  drop_na(period, taxa, cutmarks)

# cutmarks on deer bones
fauna_deer_joints <-
  fauna_deer_portion  %>%
  mutate(joint = case_when(`部位/名稱` %in% c( "肩胛骨", "肩頰骨", "肩胛骨.") ~ "shoulder",
                             `部位/名稱` %in% c("橈骨", "尺骨", "radius", "肱骨humerus", "左肱骨","肱骨", "Humerus", "骺") ~ "elbow",
                             `部位/名稱` %in% c("掌骨", "腕骨", "metacarpal", "carpal附骨？", "附骨", "趾骨,附骨") ~ "wrist", #"掌骨或蹠骨"
                             `部位/名稱` %in% c("髖骨", "股骨", "薦骨") ~ "hip",
                             `部位/名稱` %in% c("脛骨", "tibia", "脛關節") ~ "knee",
                             `部位/名稱` %in% c("蹠骨", "跟骨", "踝骨", "astragalus","astragulas", "metatarsal","calcanous") ~ "ankle")) #"蹠骨(或掌骨)"

# deer joints with cutmarks
fauna_deer_cut <-
  fauna_deer_joints %>%
  group_by(period, joint, cutmarks) %>%
  count() %>%
  rename(`NISP with cutmarks` = n) %>%
  drop_na()

# deer bones by portion in total
fauna_deer_NNISP <-
  fauna_deer_joints %>%
  group_by(period, joint) %>%
  count() %>%
  left_join(fauna_deer_cut) %>%
  rename(NISP = n) %>%
  select(-cutmarks) %>%
  drop_na(period, joint) %>%
  #filter(!period =="Chinese") %>%
  mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
  mutate(proportion = round(`NISP with cutmarks`/NISP, 2),
         NNISP = NISP/2) %>%
  group_by(period) %>%
  mutate(`%NNISP` = round(NNISP/max(NNISP), 2)) %>%
  mutate(cut_percent = `%NNISP` * proportion)

# plot
deer_cut <-
  ggplot(fauna_deer_NNISP,
         aes(x = period, y = cut_percent,
             fill = factor(joint, levels = (c("shoulder","elbow","wrist","hip","knee","ankle"))))) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  scale_color_discrete(breaks=c("CL1", "CL2", "CL3", "CL4", "CL5", "CL6")) +
  labs(x = NULL, y = "joints with cutmarks (%NNISP)") +
  guides(fill= guide_legend(title="joints"))+
  scale_fill_viridis_d(labels=c("shoulder","elbow","wrist","hip","knee","ankle")) +
  theme_minimal()
  #theme(legend.position = "none")

ggsave(here::here("analysis", "figures", "talk-fauna-3.png"), h = 4, w =4.5, units = "in")

# t-test (for two groups only)
res <- t.test(cut_percent ~ period, data = fauna_deer_NNISP)

################### Cutmarks related to skinning ###################
# skinning portions
fauna_deer_skin <-
  fauna_combined_context %>%
  mutate(skinning_cut = case_when(str_detect(.$`部位/名稱`, "掌骨")|
                               str_detect(.$`部位/名稱`, "蹠骨")|
                               str_detect(.$`部位/名稱`, "meta")|
                               str_detect(.$`部位/名稱`, "tarsal") ~ "metacarpal/\ntarsal",
                             str_detect(.$`部位/名稱`, "趾骨") ~ "phalanges",
                             str_detect(.$`新標本名`, "跟骨")|
                               str_detect(.$`新標本名`, "cubon")|
                               str_detect(.$`新標本名`, "calcanous") ~ "calcaneus/\ncubonavicular",
                             str_detect(.$`部位/名稱`, "脛骨")|
                             str_detect(.$`部位/名稱`, "tibia") ~ "tibia")) %>%
           filter(!`人為痕跡` %in% c("火燒"))

c("掌骨","掌骨或蹠骨","蹠骨","踝骨","腕骨","metatarsal","metacarpal","腕或踝骨","腕骨或踝骨",
  "蹠骨(或掌骨)","掌或蹠骨","蹠骨或掌骨","跟骨","astragalus","astragulas","calcanous",
  "第1趾骨","第2趾骨","第3趾骨","左第2趾骨","右第2趾骨","carpal附骨？","附骨","趾骨,附骨")


# skinning bones with cutmarks
fauna_deer_skin_cut <-
  fauna_deer_skin %>%
  group_by(period, skinning_cut, cutmarks) %>%
  count() %>%
  rename(`NISP with cutmarks` = n) %>%
  drop_na()

# deer bones related to skinning by portion in total
fauna_deer_skin_NNISP <-
  fauna_deer_skin %>%
  group_by(period, skinning_cut) %>%
  count() %>%
  left_join(fauna_deer_skin_cut) %>%
  rename(NISP = n) %>%
  select(-cutmarks) %>%
  drop_na(period) %>%
  #filter(!period =="Chinese") %>%
  mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
  mutate(proportion = round(`NISP with cutmarks`/NISP, 2),
         NNISP = NISP/2) %>%
  group_by(period) %>%
  mutate(`%NNISP` = round(NNISP/max(NNISP), 2)) %>%
  mutate(cut_percent = `%NNISP` * proportion) # filter(portion %in% c("tibia", "metacarpal/\ntarsal"))

# plot
deer_cut_skinning <-
  ggplot(fauna_deer_skin_NNISP, aes(x = period, y = cut_percent, fill = skinning_cut)) +
  scale_color_discrete(breaks=c("CL1", "CL2", "CL3", "CL4", "CL5", "CL6")) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  labs(x = NULL, y = "具有切痕的骨頭百分比 (%)") +
  guides(fill= guide_legend(title="剝皮相關骨骼"))+
  scale_fill_viridis_d(labels=c('跟骨calcaneus', '掌/蹠骨\nmetacarpal/tarsal', '指骨phalanges','脛骨tibia')) +
  theme_minimal()

ggsave(here::here("analysis", "figures", "talk-fauna-5.png"), h = 3, w =5, units = "in")

# combine
plot_grid(deer_cut, deer_cut_skinning,
          labels = c('A', 'B'), rel_widths = c(1, 1.55), label_size = 12)

ggsave(here::here("analysis", "figures", "deer-cut.png"), h = 4, w =8, units = "in")

# t-test
res <- t.test(cut_percent ~ period, data = fauna_deer_skin_NNISP)

#broken bones
fauna_broken <-
  kwl_fauna_broken %>%
  mutate(Pit = paste(編號別, 空間號, sep = "")) %>%
  filter(Pit %in% sample_p$the_sq) %>%
  filter(!層位 == "00") %>% #remove sc
  mutate(area = ifelse(is.na(區位), "A", 區位)) %>%
  mutate(`重量(g)` = as.numeric(`重量(g)`)) %>%
  left_join(kwl_chro_tidy_6,
            by = c("Pit" = "Pit", "層位" = "layer", "area" = "area")) %>%
  group_by(period) %>%
  summarise(weight = sum(`重量(g)`)) %>%
  filter(!is.na(period))

fauna_broken_plot <-
  fauna_broken %>%
  mutate(period = factor(period, levels = c("CL1","CL2","CL3","CL4","CL5","CL6"), order = T)) %>%
  ggplot(aes(x= period, y=weight,  width=0.5)) +
  geom_bar(stat="identity")+
  labs(x= NULL, y = NULL, title= "碎骨重量(g)") +
  #scale_y_continuous(breaks = seq(0, 90, by = 10)) +
  theme_minimal()

ggsave(here::here("analysis", "figures", "talk-fauna-6.png"), h = 2, w =4, units = "in")
