library(tidyverse)
# read data
kwl_fauna <- readxl::read_excel(here::here("analysis","data","raw_data","KWL_faunal_P.xlsx"), sheet = 1,
                                col_types = "text")

kwl_fauna_HM <- readxl::read_excel(here::here("analysis","data","raw_data","KWL_faunal_HM.xlsx"), sheet = 1,
                                   col_types = "text")

kwl_fauna_broken <- readxl::read_excel(here::here("analysis","data","raw_data","broken_fauna.xlsx"), sheet = 1,
                                   col_types = "text")

sample_p <- readr::read_csv(here::here("analysis", "data", "raw_data",
                                       "kwl-list-of-sampling-squares.csv"))

# run "kwl-pots.R" first, and then tidy data and join chronology
fauna_sam <-
  kwl_fauna %>%
  mutate(Pit = paste(context, pit, sep = "")) %>%
  filter(Pit %in% sample_p$the_sq) %>%
  filter(componant == "上文化層" &!layer == "00") %>% #remove sc
  filter(!str_detect(`總報告類別`, "[[:punct:]]+|[0-9]+")) %>%
  #mutate(area = ifelse(is.na(area), "A", area)) %>%
  left_join(kwl_chro_tidy_6,
            by = c("Pit" = "Pit", "layer" = "layer", "area" = "area")) %>%
  #mutate(period = ifelse(Pit == "P071"& layer == "02", "ch-con", period)) %>%
  select(-section)

broken <- kwl_fauna %>% filter(`部位/名稱` == "碎骨")

# tidy data for H context and join chronology
fauna_H_sam <-
  kwl_fauna_HM %>%
  mutate(Pit = paste("P", pit, sep = "")) %>%
  filter(Pit %in% sample_p$the_sq) %>%
  filter(componant == "上文化層") %>%
  filter(!str_detect(`總報告類別`, "[[:punct:]]+|[0-9]+")) %>%
  #mutate(area = ifelse(is.na(area), "A", area)) %>%
  left_join(kwl_chro_tidy_6, by = c("Pit" = "Pit", "layer" = "layer", "area" = "area"))

# ubiquity of taxa over the 40 pits by period
fauna_combined_context <-
  rbind(fauna_sam, fauna_H_sam) %>%
  mutate(`重量(g)` = as.numeric(`重量(g)`)) %>%
  mutate(`種屬/屬類` = ifelse(is.na(`種屬/屬類`), "none", `種屬/屬類`)) %>%
  mutate(taxa = case_when(`總報告類別` == "鹿" ~ "Deer",
                          `總報告類別` == "豬" ~ "Pig",
                          `總報告類別` == "牛" ~ "Cow",
                          `總報告類別` == "魚" ~ "Fish",
                          `總報告類別` == "鳥" ~ "Bird",
                          `總報告類別` == "囓齒" ~ "Rodent")) %>%
  mutate(taxa = ifelse(`種屬/屬類` == "羌", "Muntjac", taxa))

fauna_combined_taxa <-
  fauna_combined_context %>% count(taxa) %>%
  mutate(percentage = n/sum(n))

fauna_combined_MNI <-
  fauna_combined_context %>%
  mutate(`部位/左右` = case_when(`部位/左右` == "Ｌ" ~ "L",
                                  `部位/左右` == "Ｒ" ~ "R", TRUE ~ `部位/左右`)) %>%
  filter(!is.na(`部位/左右`)&!`部位/左右` == "可") %>%
  count(`部位/左右`, taxa, `部位/名稱`)

fauna_ubiquity <-
  fauna_combined_context %>%
  select(Pit, layer, taxa, `重量(g)`, period) %>%
  count(period, Pit, taxa) %>%
  count(period, taxa) %>%
  filter(!is.na(period)) %>%
  mutate(ubiquity = paste(n/40 *100, "%")) %>%
  select(-n) %>%
  pivot_wider(names_from = period, values_from = ubiquity) %>%
  mutate(taxa = fct_relevel(taxa, "Deer", "Pig", "Muntjac", "Cow", "Bird", "Fish", "Rodent")) %>%
  arrange(taxa) %>%
  #replace_na(list(`pre-European` = "-", `post-European` = "-", `Chinese` = "-")) %>%
  mutate(group = ifelse(taxa %in% c("Deer", "Pig", "Muntjac", "Cow"), "Large/Middle animals", "Small animals"))
  #select(group, taxa, `pre-European`, `post-European`, `Chinese`)

############################# for NISP ###################
#combine P and H
fauna_total <-
  fauna_combined_context %>%
  mutate(taxa = case_when(taxa == "Fish" ~ "Small animals",
                          taxa == "Bird" ~ "Small animals",
                          taxa == "Rodent" ~ "Small animals",
                          TRUE ~ taxa)) %>%
  mutate(cutmarks = ifelse(`人為痕跡` == "", "no", "yes"))

# count total taxa
fauna_total_taxa <-
  fauna_total %>%
  group_by(taxa, period) %>%
  count() %>%
  drop_na(period, taxa) %>%
  rename(`NISP` = n)

# plot different species
fauna_taxa_barplot <-
  fauna_total_taxa %>%
  mutate(taxa = factor(taxa, levels = c("Deer", "Pig", "Cow", "Muntjac", "Small animals"), ordered = TRUE)) %>%
  mutate(period = factor(period, levels = c("CL1","CL2","CL3","CL4","CL5","CL6"), order = T)) %>%
  ggplot(aes(x = period, y = NISP, fill = taxa))+
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  #scale_x_discrete(limits = rev) +
  labs(x = NULL) +
  guides(fill= guide_legend(title="類別"))+
  scale_fill_viridis_d(labels=c('鹿', '豬', '牛', '山羌','小型\n動物')) +
  theme_minimal()

ggsave(here::here("analysis", "figures", "talk-fauna-1.png"), h = 3, w =5, units = "in")

fauna_deer_portion <-
  fauna_total %>%
  filter(taxa %in% c("Deer")) %>%
  filter(`部位/名稱` %in% c("肩胛骨","肩頰骨","肩胛骨.","胸椎","腰椎","肋骨","肩頰,肋骨","薦骨",
                        "橈骨", "尺骨", "radius", "肱骨humerus", "左肱骨","肱骨", "Humerus", "骺",
                        "脛骨", "tibia", "脛關節","髖骨", "股骨", "肢骨",
                        "掌骨", "腕骨", "metacarpal", "carpal附骨？","腕或踝骨","掌骨或蹠骨","蹠骨(或掌骨)","掌或蹠骨",
                        "蹠骨或掌骨","蹠骨", "跟骨", "astragalus", "metatarsal","第1趾骨","第2趾骨","第3趾骨",
                        "下顎及齒","下顎骨","上下顎齒","顱骨","第二頸椎","枕骨","齒","下顎齒","臼齒","前臼齒",
                        "上顎齒?","上顎臼齒","鹿角","角","角基部")) %>%
  mutate(portion = case_when(`部位/名稱` %in% c("肩胛骨","肩頰骨","肩胛骨.","胸椎","腰椎","肋骨","肩頰,肋骨","薦骨") ~ "mid-meaty\n(body)",
                             `部位/名稱` %in% c("橈骨", "尺骨", "radius", "肱骨humerus", "左肱骨","肱骨", "Humerus", "骺",
                                            "脛骨", "tibia", "脛關節", "髖骨", "股骨", "肢骨") ~ "meaty\n(upper limb)",
                             `部位/名稱` %in% c("掌骨", "腕骨", "metacarpal", "carpal附骨？","腕或踝骨","掌骨或蹠骨","蹠骨(或掌骨)","掌或蹠骨",
                                            "蹠骨或掌骨","蹠骨", "跟骨", "astragalus", "metatarsal","第1趾骨","第2趾骨","第3趾骨") ~ "non-meaty\n(lower limb)",
                             `部位/名稱` %in% c("下顎及齒","上下顎齒","下顎骨","顱骨","第二頸椎","枕骨") ~ "non-meaty\n(cranial parts)"))

weight_head <-
fauna_deer_portion %>%
  mutate(`重量(g)` = as.numeric(`重量(g)`)) %>%
  filter(portion == "non-meaty\n(cranial parts)") %>%
  group_by(period) %>%
  summarise(weight = sum(`重量(g)`)) %>%
  filter(!is.na(period)) %>%
  mutate(period = factor(period, levels = c("CL3","CL4","CL5"), order = T)) %>%
  ggplot(aes(x= period, y=weight,  width=0.5)) +
  geom_bar(stat="identity")+
  labs(x= NULL, y= "碎頭骨重量(g)") +
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
  labs(x = NULL, y = "百分比") +
  guides(fill= guide_legend(title="部位"))+
  scale_fill_discrete(labels=c('上肢骨(多肉)', '軀幹(中等)', '頭骨(少肉)','下肢骨(少肉)')) +
  #scale_fill_discrete(breaks=c("body\n(mid-meaty)", "upper limb\n(meaty)", "lower limb\n(meaty)","cranial elements(none-meaty)")) +
  theme_minimal()

ggsave(here::here("analysis", "figures", "talk-fauna-2.png"), h = 4, w =4, units = "in")

library(cowplot)
plot_grid(fauna_taxa_barplot, deer_portion_plot,
          labels = c('A', 'B'), rel_widths = c(1.3, 1), label_size = 12)

ggsave(here::here("analysis", "figures", "talk-fauna.png"), h = 4, w =9, units = "in")

################## cutmarks #################
# count cutmarks
fauna_total_cut <-
  fauna_total %>%
  group_by(taxa, period, cutmarks) %>%
  count() %>%
  rename(`NISP with cutmarks` = n) %>%
  drop_na(period, taxa, cutmarks)

# only look at cutmarks on deer bones
fauna_deer_joints <-
  fauna_total %>%
  filter(taxa == "Deer") %>%
  filter(`部位/名稱` %in% c("肩胛骨", "肩頰骨", "肩胛骨.",
                        "橈骨", "尺骨", "radius", "肱骨humerus", "左肱骨","肱骨", "Humerus", "骺",
                        "掌骨", "腕骨", "metacarpal", "carpal附骨？",
                        "髖骨", "股骨",
                        "脛骨", "tibia", "脛關節",
                        "蹠骨", "跟骨", "astragalus", "metatarsal")) %>%
  mutate(portion = case_when(`部位/名稱` %in% c( "肩胛骨", "肩頰骨", "肩胛骨.") ~ "shoulder",
                             `部位/名稱` %in% c("橈骨", "尺骨", "radius", "肱骨humerus", "左肱骨","肱骨", "Humerus", "骺") ~ "elbow",
                             `部位/名稱` %in% c("掌骨", "腕骨", "metacarpal", "carpal附骨？") ~ "wrist",
                             `部位/名稱` %in% c("髖骨", "股骨") ~ "hip",
                             `部位/名稱` %in% c("脛骨", "tibia", "脛關節") ~ "knee",
                             `部位/名稱` %in% c("蹠骨", "跟骨", "astragalus", "metatarsal") ~ "ankle"))

# deer bones with cutmarks
fauna_deer_cut <-
  fauna_deer_joints %>%
  group_by(period, portion, cutmarks) %>%
  count() %>%
  rename(`NISP with cutmarks` = n) %>%
  drop_na()

# deer bones by portion in total
fauna_deer_NNISP <-
  fauna_deer_joints %>%
  group_by(period, portion) %>%
  count() %>%
  left_join(fauna_deer_cut) %>%
  rename(NISP = n) %>%
  select(-cutmarks) %>%
  drop_na(period, portion) %>%
  filter(!period =="Chinese") %>%
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
             fill = factor(portion, levels = (c("shoulder","elbow","wrist","hip","knee","ankle"))))) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  scale_color_discrete(breaks=c("CL1", "CL2", "CL3", "CL4", "CL5", "CL6")) +
  labs(x = NULL, y = NULL, title = "具有切痕的鹿骨百分比 (%NNISP)", fill = "joints") +
  guides(fill= guide_legend(title="關節"))+
  scale_fill_viridis_d(labels=c('肩', '肘', '腕','髖', '膝','踝')) +
  theme_minimal()
  #theme(legend.position = "none")

ggsave(here::here("analysis", "figures", "talk-fauna-3.png"), h = 4, w =4.5, units = "in")

# t-test (for two groups only)
res <- t.test(cut_percent ~ period, data = fauna_deer_NNISP)

# related to deer skinning
fauna_deer_skin <-
  fauna_total %>%
  filter(taxa == "Deer") %>%
  filter(str_detect(.$`部位/名稱`, "掌骨")|
           str_detect(.$`部位/名稱`, "蹠骨")|
           str_detect(.$`部位/名稱`, "meta")|
           str_detect(.$`部位/名稱`, "tarsal")|
           str_detect(.$`部位/名稱`, "趾骨")|
           str_detect(.$`新標本名`, "cubon")|
           str_detect(.$`新標本名`, "calcanous")|
           str_detect(.$`部位/名稱`, "脛骨")) %>%
  mutate(portion = case_when(str_detect(.$`部位/名稱`, "掌骨")|
                               str_detect(.$`部位/名稱`, "蹠骨")|
                               str_detect(.$`部位/名稱`, "meta")|
                               str_detect(.$`部位/名稱`, "tarsal")  ~ "metacarpal/\ntarsal",
                             str_detect(.$`部位/名稱`, "趾骨") ~ "phalanges",
                             str_detect(.$`新標本名`, "cubon")|
                               str_detect(.$`新標本名`, "calcanous") ~ "calcaneus/\ncubonavicular",
                             str_detect(.$`部位/名稱`, "脛骨") ~ "tibia")) %>%
  filter(!`人為痕跡` %in% c("火燒"))

# skinning bones with cutmarks
fauna_deer_skin_cut <-
  fauna_deer_skin %>%
  group_by(period, portion, cutmarks) %>%
  count() %>%
  rename(`NISP with cutmarks` = n) %>%
  drop_na()

# deer bones related to skinning by portion in total
fauna_deer_skin_NNISP <-
  fauna_deer_skin %>%
  group_by(period, portion) %>%
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
  ggplot(fauna_deer_skin_NNISP, aes(x = period, y = cut_percent, fill = portion)) +
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
