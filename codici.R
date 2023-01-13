library(tidyverse)
library(here)
library(readxl)
library(openxlsx)
library(FactoMineR)
library(factoextra)
library(janitor)

dt <- read_excel("data/Tabella campioni.xlsx")




dt <- dt %>% 
  mutate(origin = ifelse(source == "environmental", "environmental", 
                         ifelse(source == "clinical", "clinical", origin)))  



# association ST----
ST <- dt %>% mutate(st = paste0("ST",st)) %>% 
  filter(st %in% c("ST9", "ST121", "ST8", "ST224",
                   "ST325", "ST1", "ST155", "ST5", "ST3", "ST2")) %>% #, 
         #!origin %in% c("environmental", "clinical")) %>%
  #filter(!is.na(source)) %>% 
  group_by(st, origin) %>% 
  summarise(n = n()) %>%  
  pivot_wider(names_from = "origin", values_from = "n", values_fill = 0) %>%  # %>% ungroup() %>% gt()
  # column_to_rownames("st") %>% 
  adorn_totals(where = "row") %>% 
  adorn_totals(where = "col") %>%
  gt()

#gt(rowname_col = "st")


ST %>% 
  group_by(origin) %>% 
  summarize(s = sum(n))

ST %>% ungroup() %>% summarize(s = sum(sum(n)))

res.ca <- CA(ST, graph = FALSE)
fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 50))   
fviz_screeplot(res.ca) +
  geom_hline(yintercept=25, linetype=2, color="red")

fviz_ca_biplot(res.ca, repel = TRUE )

fviz_ca_col(res.ca, col.col = "cos2", 
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            repel = TRUE)


fviz_ca_biplot(res.ca, repel = TRUE)


fviz_ca_biplot(res.ca, 
               map ="rowprincipal", arrow = c(TRUE, TRUE),
               repel = TRUE)


fviz_ca_biplot(res.ca, map ="colgreen", arrow = c(TRUE, FALSE),
               repel = TRUE)

#associaton serotype----   

Syt <- dt %>% 
  filter(!is.na(serotype)) %>% 
  group_by(serotype, source) %>% 
  summarise(n = n()) %>%  
  pivot_wider(names_from = "source", values_from = "n", values_fill = 0) %>% 
  column_to_rownames("serotype")

res.ca <- CA(Syt, graph = FALSE)
fviz_ca_biplot(res.ca, 
               map ="rowprincipal", arrow = c(TRUE, TRUE),
               repel = TRUE)


Syt2 <- dt %>% 
  filter(!is.na(serotype)) %>% 
  group_by(serotype, origin) %>%  
  summarise(n = n()) %>%  
  pivot_wider(names_from = "origin", values_from = "n", values_fill = 0) %>% 
  column_to_rownames("serotype")

res.ca <- CA(Syt2, graph = FALSE)
fviz_ca_biplot(res.ca, 
               map ="rowprincipal", arrow = c(TRUE, TRUE),
               repel = TRUE)


# grafici per articolo

library(RColorBrewer)
library(stringr)

#fig.1
dt %>% mutate(st = paste0("ST",st)) %>%
  group_by(st, source,lineage) %>%
  count() %>%
arrange(desc(n)) %>%
 
  ggplot()+
  aes(x = reorder(st,+n), y = n, fill = source)+
  geom_bar( color = "black", stat='identity', position = "stack")+
    facet_wrap(~lineage, ncol = 2, scales = "free", labeller = label_both)+
    coord_flip()+
  scale_fill_brewer(direction = +1, palette = "Blues")+
  theme_bw()+
  labs(y = "N. of isolates", x = "sequence types (ST)")

#fig.2
dt %>% mutate(st = paste0("ST",st)) %>%
  filter(!is.na(origin)) %>%
  mutate(origin = str_to_title(origin) ) %>% 
  group_by(origin) %>%
  count() %>%
  arrange(desc(n)) %>% 
  ggplot()+
  aes(x = reorder(origin, +n), y = n)+
  geom_bar(color="black", stat = "identity", fill = "deepskyblue4", width = 0.5)+
  
  coord_flip()+
  theme_bw()+
  labs(y = "N. of isolates", x = "Source")

#fig.3

dt %>% mutate(st = paste0("ST",st)) %>%
  group_by( serotype,source) %>%
  count() %>%
  arrange(n, .by_group = T) %>% 
  mutate(x = factor(interaction(source,serotype,  drop=TRUE))) -> my_df
  

ggplot(my_df)+
  aes(x = reorder(x, +n), y = n)+
  geom_bar(color="black", stat = "identity", fill = "deepskyblue4", width = 0.5)+
  facet_wrap(~ source, scales = "free")+
  theme_bw()+
  coord_flip()+
  labs(y = "N. of isolates", x = "Serotype")+
   scale_x_discrete(breaks = my_df$x, labels=gsub("^.*\\.", "", my_df$x))












