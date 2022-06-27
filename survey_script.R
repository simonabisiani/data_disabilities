
library(tidyverse)
library(readxl)

dat <- read_excel("sondaggio.xlsx")
View(dat)

dat_adj <- dat %>% 
  rename(topic = `Su quali di queste tematiche riguardanti la disabilità vorresti vedere e esplorare i dati esistenti? (Seleziona al massimo 5 opzioni)`) %>% 
  select(topic) 



###### EXPLORING TOPICS ######################################


topic <- as_tibble(unlist(str_split(dat_adj$topic, ";"))) %>% 
  filter(value != "") %>% 
  count(value) %>% 
  arrange(n)

topic$release <- c("Not included",
                   "Not included",
                   "Not included",
                   "Not included",
                   "Not included",
                   "Not included",
                   "Not included",
                   "Not included",
                   "Not included",
                   "Not included",
                   "Not included",
                   "Not included",
                   "November",
                   "November",
                   "November",
                   "Not included",
                   "November",
                   "November",
                   "November",
                   "November",
                   "November",
                   "July",
                   "September",
                   "September",
                   "September",
                   "September",
                   "July")
                   
jpeg("Topic.jpeg", width = 8, height = 4, units = 'in', res = 300)

topic %>% 
  filter(n != 1) %>% 
  mutate(release = factor(release, levels = c("July", "September", "November", "Not included"))) %>% 
ggplot(aes(x = reorder(value, +n), y = n, fill = release)) +
  geom_bar(stat = "identity") +
  labs(title = "Aree tematiche", 
       subtitle = "e quando verrano esplorate nella dashboard", fill = "Periodo di rilascio") +
  xlab("") +
  ylab("Number of respondents") +
  scale_fill_manual(values=c(
    "#7fc97f",
    "#beaed4",
    "#fdc086",
    "#ffff99",
    "#386cb0")) +
  theme_minimal() +
  theme(legend.title = element_text(face = "bold")) +
  coord_flip() 
  
dev.off()

###### PREPPING DATA FOR SINGLE THEME EXPLORATION ###########################

spec <- dat %>% 
  select(c(11:18))

try <- lapply(spec, function(x) str_split(x, ";"))


###### EXPLORING VITA E PARTECIPAZIONE SOCIALE ###########################


vita_part_soc <- as.data.frame(unlist(try[5]))
vita_part_soc <- vita_part_soc %>% 
  rename(value = "unlist(try[5])") %>% 
  filter(value != "") %>% 
  count(value) %>% 
  arrange(n) %>% 
  filter(n != 1)
  

ggplot(vita_part_soc, aes(reorder(value, +n), n)) +
  geom_bar(stat = "identity") +
  labs(title = "Vita e partecipazione sociale", 
       subtitle = "Priorità") +
  xlab("") +
  ylab("Number of respondents") +
  scale_fill_manual(values=c(
    "#7fc97f",
    "#beaed4",
    "#fdc086",
    "#ffff99",
    "#386cb0")) +
  theme_minimal() +
  theme(legend.title = element_text(face = "bold")) +
  coord_flip() 



###### EXPLORING LAVORO E OCCUPAZIONE ###########################


lavoro <- as.data.frame(unlist(try[4]))
lavoro <- lavoro %>% 
  rename(value = "unlist(try[4])") %>% 
  filter(value != "") %>% 
  count(value) %>% 
  arrange(n) %>% 
  filter(n != 1)
  

ggplot(lavoro, aes(reorder(value, +n), n)) +
  geom_bar(stat = "identity") +
  labs(title = "Lavoro e occupazione", 
       subtitle = "Priorità") +
  xlab("") +
  ylab("Number of respondents") +
  scale_fill_manual(values=c(
    "#7fc97f",
    "#beaed4",
    "#fdc086",
    "#ffff99",
    "#386cb0")) +
  theme_minimal() +
  theme(legend.title = element_text(face = "bold")) +
  coord_flip() 




###### EXPLORING ISTRUZIONE  ###########################

edu <- as.data.frame(unlist(try[3]))
edu <- edu %>% 
  rename(value = "unlist(try[3])") %>% 
  filter(value != "") %>% 
  count(value) %>% 
  arrange(n) %>% 
  filter(n != 1)

ggplot(edu, aes(reorder(value, +n), n)) +
  geom_bar(stat = "identity") +
  labs(title = "Educazione e istruzione", 
       subtitle = "Priorità") +
  xlab("") +
  ylab("Number of respondents") +
  scale_fill_manual(values=c(
    "#7fc97f",
    "#beaed4",
    "#fdc086",
    "#ffff99",
    "#386cb0")) +
  theme_minimal() +
  theme(legend.title = element_text(face = "bold")) +
  coord_flip() 



###### EXPLORING ASSISTENZA SANITARIA E SOCIALE ###########################

assist <- as.data.frame(unlist(try[1]))
assist <- assist %>% 
  rename(value = "unlist(try[1])") %>% 
  filter(value != "") %>% 
  count(value) %>% 
  arrange(n) %>% 
  filter(n != 1)

ggplot(assist, aes(reorder(value, +n), n)) +
  geom_bar(stat = "identity") +
  labs(title = "Assistenza sanitaria e sociale", 
       subtitle = "Priorità") +
  xlab("") +
  ylab("Number of respondents") +
  scale_fill_manual(values=c(
    "#7fc97f",
    "#beaed4",
    "#fdc086",
    "#ffff99",
    "#386cb0")) +
  theme_minimal() +
  theme(legend.title = element_text(face = "bold")) +
  coord_flip() 



###### EXPLORING SALUTE  ###########################

salute <- as.data.frame(unlist(try[2]))
salute <- salute %>% 
  rename(value = "unlist(try[2])") %>% 
  filter(value != "") %>% 
  count(value) %>% 
  arrange(n) %>% 
  filter(n != 1)

ggplot(salute, aes(reorder(value, +n), n)) +
  geom_bar(stat = "identity") +
  labs(title = "Salute", 
       subtitle = "Priorità") +
  xlab("") +
  ylab("Number of respondents") +
  scale_fill_manual(values=c(
    "#7fc97f",
    "#beaed4",
    "#fdc086",
    "#ffff99",
    "#386cb0")) +
  theme_minimal() +
  theme(legend.title = element_text(face = "bold")) +
  coord_flip() 


###### EXPLORING PROTEZIONE SOCIALE  ###########################

protez_soc <- as.data.frame(unlist(try[7]))
protez_soc <- protez_soc %>% 
  rename(value = "unlist(try[7])") %>% 
  filter(value != "") %>% 
  count(value) %>% 
  arrange(n) %>% 
  filter(n != 1)

ggplot(protez_soc, aes(reorder(value, +n), n)) +
  geom_bar(stat = "identity") +
  labs(title = "Protezione sociale", 
       subtitle = "Priorità") +
  xlab("") +
  ylab("Number of respondents") +
  scale_fill_manual(values=c(
    "#7fc97f",
    "#beaed4",
    "#fdc086",
    "#ffff99",
    "#386cb0")) +
  theme_minimal() +
  theme(legend.title = element_text(face = "bold")) +
  coord_flip() 


###### EXPLORING CONDIZIONI ABITATIVE  ###########################

cond_abit <- as.data.frame(unlist(try[8]))
cond_abit <- cond_abit %>% 
  rename(value = "unlist(try[8])") %>% 
  filter(value != "") %>% 
  count(value) %>% 
  arrange(n) %>% 
  filter(n != 1)

ggplot(cond_abit, aes(reorder(value, +n), n)) +
  geom_bar(stat = "identity") +
  labs(title = "Condizioni abitative", 
       subtitle = "Priorità") +
  xlab("") +
  ylab("Number of respondents") +
  scale_fill_manual(values=c(
    "#7fc97f",
    "#beaed4",
    "#fdc086",
    "#ffff99",
    "#386cb0")) +
  theme_minimal() +
  theme(legend.title = element_text(face = "bold")) +
  coord_flip() 





#### WITH DEMOGRAPHICS ####

library(tidyverse)
library(readxl)

dat <- read_excel("sondaggio.xlsx")
View(dat)

dat_adj <- dat %>% 
  rename(topic = `Su quali di queste tematiche riguardanti la disabilità vorresti vedere e esplorare i dati esistenti? (Seleziona al massimo 5 opzioni)`) %>% 
  select(topic) 

# 1. TOPICS

topic_dem <- dat %>% 
  select(Età, Sesso, Disabilità, `Dove vivi?`, `Su quali di queste tematiche riguardanti la disabilità vorresti vedere e esplorare i dati esistenti? (Seleziona al massimo 5 opzioni)`) %>% 
  rename(topic = `Su quali di queste tematiche riguardanti la disabilità vorresti vedere e esplorare i dati esistenti? (Seleziona al massimo 5 opzioni)`)
  

ggplot(topic_dem, aes(Età, fill = Sesso)) +
  geom_bar(stat = "count") +
  facet_grid(topic_dem$`Dove vivi?`~ topic_dem$Disabilità)
  

library(splitstackshape) 
topic_demm <- cSplit(topic_dem, 'topic', ';')

topic_demm_long <- topic_demm %>% 
  rowid_to_column() %>% 
  pivot_longer(cols = c(6:20)) %>% 
  filter(!is.na(value))
  

write.csv(topic_demm_long, "sondaggio_topic_long.csv")



# 2. VITA E PARTECIPAZIONE SOCIALE

vit_dem <- dat %>% 
  select(Età, Sesso, Disabilità, `Dove vivi?`, 15) %>% 
  rename(vit = 5)


library(splitstackshape) 
vit_demm <- cSplit(vit_dem, 'vit', ';')

vit_demm_long <- vit_demm %>% 
  rowid_to_column() %>% 
  pivot_longer(cols = c(6:14)) %>% 
  filter(!is.na(value))


write.csv(vit_demm_long, "sondaggio_vit_long.csv")



# 3. LAVORO E OCCUPAZIONE

lav_dem <- dat %>% 
  select(Età, Sesso, Disabilità, `Dove vivi?`, 14) %>% 
  rename(lav = 5)


library(splitstackshape) 
lav_demm <- cSplit(lav_dem, 'lav', ';')

lav_demm_long <- lav_demm %>% 
  rowid_to_column() %>% 
  pivot_longer(cols = c(6:13)) %>% 
  filter(!is.na(value))

write.csv(lav_demm_long, "sondaggio_lav_long.csv")



# 4. SALUTE

sal_dem <- dat %>% 
  select(Età, Sesso, Disabilità, `Dove vivi?`, 12) %>% 
  rename(sal = 5)


library(splitstackshape) 
sal_demm <- cSplit(sal_dem, 'sal', ';')

sal_demm_long <- sal_demm %>% 
  rowid_to_column() %>% 
  pivot_longer(cols = c(6:10)) %>% 
  filter(!is.na(value))


write.csv(sal_demm_long, "sondaggio_sal_long.csv")



# 5. CONDIZIONI ABITATIVE

abit_dem <- dat %>% 
  select(Età, Sesso, Disabilità, `Dove vivi?`, 16) %>% 
  rename(abit = 5)


library(splitstackshape) 
abit_demm <- cSplit(abit_dem, 'abit', ';')

abit_demm_long <- abit_demm %>% 
  rowid_to_column() %>% 
  pivot_longer(cols = c(6:9)) %>% 
  filter(!is.na(value))


write.csv(abit_demm_long, "sondaggio_abit_long.csv")



