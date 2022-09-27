library(tidyverse)
library(readr)
likert_dat <- read_csv("~/GitHub/classifying_news/disabled-data/data/disabilita-in-cifre/processing/6-luglio.csv")
View(X6_luglio)


one_example <- likert_dat %>% 
  mutate(my_ranks = as.integer(factor(descrizione))) %>% 
  filter(my_ranks == 3) %>% 
  filter(anno == 2009) %>% 
  select(-1) %>% 
  mutate(scelta = factor(scelta, levels = c("Molto soddisfatto",
                                            "Abbastanza soddisfatto",
                                            "Poco soddisfatto",     
                                            "Per niente soddisfatto",
                                            "Non indicato"))) %>% 
  select(-c(anno, my_ranks, `Non indicato`))


inverted <- one_example %>% 
  pivot_longer(cols = 2:5) %>% 
  pivot_wider(id_cols = c(descrizione, gerarchia, name), names_from = scelta, values_from = value) %>% 
  select(-c(`Non indicato`, descrizione, gerarchia))

HH::likert(name~., inverted, positive.order=TRUE,as.percent = TRUE,
           main="Soddisfazione per il tempo libero, anno 2009",
           xlab="percentage",ylab="")



library(reshape2)
library(RColorBrewer)

mb = melt(one_example)

ggplot()+
  geom_bar(data = mb, aes(x = reorder(variable,value), y=value, fill=scelta), position="stack", stat="identity")+
  coord_flip() + 
  ggtitle("Soddisfazione per il tempo libero, 2009")+
  ylab("Percentage")+
  xlab("")+
  scale_fill_brewer(palette="PRGn")+
  theme(legend.position="bottom")
#scale_x_discrete(limits=c("StronglyAgree", "Agree", "DontKnow","Disagree","StronglyDisagree"))

# 
# 
# 
# # plot for only the North slope
# merging %>% 
#   ggplot(aes(x=option, y = sppInv,  fill=id))+
#   geom_bar(stat="identity",position="identity")+
#   scale_fill_manual(name="Response type",values = c("#FFA373","#50486D"))+
#   coord_flip()+
#   ggtitle("Which elements would you like to remove | keep ?")+
#   geom_hline(yintercept=0)+
#   ylab("% of respondents selecting the option")+
#   xlab("")+
#   scale_y_continuous(limits = c(-75,75), breaks = pretty(merging$sppInv), labels = abs(pretty(merging$sppInv))) +
#   theme_scientific()



# partecipazione culturale


part_cult <- likert_dat %>% 
  mutate(my_ranks = as.integer(factor(descrizione))) %>% 
  filter(my_ranks == 1) %>% 
  pivot_longer(cols = 3:7)

ggplot(part_cult, aes(x = anno, y = value, fill = c(scelta))) +
  geom_bar(stat = "identity", position = "dodge")
