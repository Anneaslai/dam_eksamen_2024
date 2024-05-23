################################################################################

#DATASÆT FRA 1801 

library(tidyverse)

civilstand_1801 <- read_csv ("/Users/anneaslai-sorensen/Documents/Historie AU/2. Semester/Digitale arkiver og metoder/Eksamen/Csv data filer/datasæt_20_til_31_fra_1801.txt")

#######################

#FORHOLDET MELLEM GIFT OG UGIFT I 1801

civilstand_1801 %>%
  ggplot(aes(x=civilstand)) +
  geom_bar(aes(fill=civilstand))

colnames(civilstand_1801)
names(civilstand_1801)[names(civilstand_1801)=="ft"]<- "år"


civilstand_1801 %>% 
  ggplot(aes(x=alder, y=`år`))+
  geom_line()


#########################

#FORDELT PÅ ALDER 

civilstand_1801 %>% 
  drop_na(alder) %>% 
  mutate(alder_str=paste0(alder, " år")) %>% 
  ggplot(aes(x=civilstand, fill=civilstand))+
  geom_bar(stat = 'count', position="dodge")+ 
  facet_wrap(~alder_str, ncol = 3)+ 
  theme_minimal()+
  ggtitle("Civilstand fordelt på alder i 1801")


################################################################################

#DATASÆT FRA 2000 TIL 2020 

danmarksstatistik <- read_csv2("/Users/anneaslai-sorensen/Documents/Historie AU/2. Semester/Digitale arkiver og metoder/Eksamen/Csv data filer/datasæt_2000_til_2020_3.csv")


danmarksstatistik %>% 
  ggplot(aes(x=Alder, y=`2000`))+
  geom_line()

civilstand_2000_2020 <- danmarksstatistik %>% 
  pivot_longer(cols=`2000`:`2020`,
               names_to = "år",
               values_to = "count")

civilstand_2000_2020 %>% 
  ggplot(aes(x=år,y=count, fill=civilstand))+
  geom_bar(stat = "identity", position="dodge")+
  facet_wrap(~Alder, ncol = 3)+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+ 
  ggtitle("Civilstand fordelt på alder fra 2000 til 2020")


################################################################################



