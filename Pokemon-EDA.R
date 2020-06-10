library(tidyverse)
library(dplyr)
library(ggdark)

pokemon = read.csv('dataset/Pokemon/datasets_121_280_Pokemon.csv')

glimpse(pokemon)
summary(pokemon)

pokemon = pokemon %>% 
  rename(Power = Total)
pokemon$Type.1 = as.factor(pokemon$Type.1)
pokemon$Type.2 = as.factor(pokemon$Type.2)
pokemon$Generation = as.factor(pokemon$Generation)
pokemon$Legendary = as.factor(pokemon$Legendary)

####VARIATION####


###Categorical Variable
##Pokemon tipe apa yang memiliki jenis paling banyak dan paling sedikit?
TypeCount = plyr::count(pokemon, 'Type.1')
TypePlot = ggplot(pokemon)+
  geom_bar(mapping = aes(Type.1), fill="orange")+
  ggtitle("Jumlah Pokemon Berdasarkan Tipe")+
  xlab('Tipe')+ylab('Jumlah')+
  coord_flip()+
  dark_theme_light()

TypeCount
TypePlot
#Interpretasi :   #1. Pokemon dengan tipe Air (Water) memiliki jenis paling banyak
                  #2. Pokemon dengan tipe tebang (Flying) memuliki jenis paling sedikit

#==========||===========##==========||===========##==========||===========##==========||===========#
#==========||===========##==========||===========##==========||===========##==========||===========#


###Categorical Variable
##Pokemon generasi berapa yang memiliki jenis paling banyak dan paling sedikit?
#Visualisasi
GenCount = plyr::count(pokemon, 'Generation')
GenPlot = ggplot(pokemon)+
  geom_bar(mapping = aes(Generation), fill="orange")+ #steelblue
  ggtitle("Jumlah Pokemon Berdasarkan Generasi")+
  xlab('Generasi')+ylab('Jumlah')+
  scale_x_discrete(breaks=c('1', '2', '3', '4', '5', '6'), 
                   labels = c('Gen 1', 'Gen 2', 'Gen 3', 'Gen 4', 'Gen 5', 'Gen 6'))+
  coord_flip()+
  dark_theme_light()

GenCount
GenPlot
#Interpretasi :   #1. Pokemon dengan generasi 1 dan 5 memiliki jumlah yang paling banyak
                  #2. Pokemon dengan generasi 6 memiliki jumlah yang paling Sedikit


#==========||===========##==========||===========##==========||===========##==========||===========#
#==========||===========##==========||===========##==========||===========##==========||===========#


###Continous Variable
##Berapa HP yang dimiliki setiap pokemon?
HpPlot = ggplot(pokemon)+
  geom_histogram(mapping = aes(x=HP, fill = Type.1), binwidth = 3)+
  xlab('HP')+ylab('Jumlah')+
  ggtitle("Pokemon HP")+ labs(fill = "Tipe")+
  dark_theme_light()

HpPlot
#Interpretasi :   #1. Banyak pokemon memiliki HP dibawah 100
                  #2. Hanya ada 2 tipe pokemon yang memiliki HP diatas 200


#==========||===========##==========||===========##==========||===========##==========||===========#
#==========||===========##==========||===========##==========||===========##==========||===========#


###Continous Variable
##Berapa Attack yang dimiliki setiap pokemon?
AttPlot = ggplot(pokemon)+
  geom_histogram(mapping = aes(x=Attack, fill = Type.1), binwidth = 3)+
  xlab('Attack')+ylab('Jumlah')+
  ggtitle("Pokemon Attack")+ labs(fill = "Tipe")+
  dark_theme_light()

AttPlot
#Interpretasi :   #1. Mayoritas pokemon memiliki Attack dibawah 150


#==========||===========##==========||===========##==========||===========##==========||===========#
#==========||===========##==========||===========##==========||===========##==========||===========#


####COVARIATION####


###Categorical vs Categorical
##Pokemon Legendary paling banyak pada tipe apa?
LegendCount = pokemon %>%
  filter(Legendary == "True")%>%
  count(Type.1, Legendary) %>%
  arrange(desc(n))
LegendPlot = pokemon %>%
  filter(Legendary == "True")%>%
  count(Type.1, Legendary) %>%
  arrange(desc(n))%>%
  ggplot(mapping = aes(x=Legendary, y=Type.1))+
  geom_tile(mapping = aes(fill = n))+
  ggtitle("Pokemon Legendary")+ labs(fill = "Freq")+
  ylab('Tipe')+
  dark_theme_light()

LegendCount
LegendPlot
#Interpretasi :   #1. Pokemon Legendary paling banyak bertipe Psychic dan Dragon


#==========||===========##==========||===========##==========||===========##==========||===========#
#==========||===========##==========||===========##==========||===========##==========||===========#


###Categorical vs Categorical
##Pokemon generasi 1-6 banyak bertipe apa?
GenComposition = pokemon %>%
  count(Type.1, Generation) %>%
  arrange(Type.1)

GenComPlot = pokemon %>%
  count(Type.1, Generation) %>%
  arrange(desc(n))%>%
  ggplot(mapping = aes(x=Generation, y=Type.1))+
  geom_tile(mapping = aes(fill = n))+
  ggtitle("Komposisi Generation Setiap Tipe")+ labs(fill = "Freq")+
  ylab('Tipe')+
  scale_fill_viridis_c()+
  dark_theme_light()

GenComposition
GenComPlot
#Interpretasi :   #1. Pokemon generasi 1 banyak bertipe Water
                  #2. Pokemon generasi 2 banyak bertipe Water dan Normal
                  #3. Pokemon generasi 3 banyak bertipe Water
                  #4. Pokemon generasi 4 banyak bertipe Normal
                  #5. Pokemon generasi 5 banyak bertipe Normal dan Bug
                  #6. Pokemon generasi 6 banyak bertipe Ghost


#==========||===========##==========||===========##==========||===========##==========||===========#
#==========||===========##==========||===========##==========||===========##==========||===========#


###Continous vs Continous
##Apakah semakin tinggi Attack, semakin tinggi juga Special Attack?

AttRelPlot = ggplot(pokemon, aes(Attack, Sp..Atk))+
  geom_point() + 
  scale_x_continuous(breaks = seq(0,200,20))+
  scale_y_continuous(breaks = seq(0,200,20))+
  ggtitle('Hubungan Antara Attack dan Special Attack')+
  dark_theme_light()
r_att = cor.test(pokemon$Attack, pokemon$Sp..Atk, 
             method = "pearson")

AttRelPlot
r_att

#Interpretasi :   #1. Variabel Attack dan Sp..Atk memiliki relasi positif sedang (Moderate Positive Relationship) 
                  #   +0.3963618
  

#==========||===========##==========||===========##==========||===========##==========||===========#
#==========||===========##==========||===========##==========||===========##==========||===========#


###Continous vs Continous
##Apakah semakin tinggi Defense, semakin tinggi juga Special Deffense?

DefRelPlot = ggplot(pokemon, aes(Defense, Sp..Def))+
  geom_point() + 
  ggtitle('Hubungan Antara Defense dan Special Defense')+
  scale_x_continuous(breaks = seq(0,240,20))+
  scale_y_continuous(breaks = seq(0,240,20))+
  dark_theme_light()
r_def = cor.test(pokemon$Defense, pokemon$Sp..Def, 
         method = "pearson")

DefRelPlot
r_def
#Interpretasi :   #1. Variabel Defense dan Sp..Def memiliki relasi positif sedang (Moderate Positive Relationship) 
                  #   +0.5107466


#==========||===========##==========||===========##==========||===========##==========||===========#
#==========||===========##==========||===========##==========||===========##==========||===========#


###Categorical vs Continous
##Bagaimana distribusi dari Power setiap tipe pokemon?


DisType = ggplot(pokemon) +
  geom_boxplot(mapping = aes(x=Type.1, y=Power))+
  scale_y_continuous(breaks = seq(0,800,100))+
  ggtitle('Distribusi Power Setiap Tipe Pokemon')+
  xlab('Type')+
  dark_theme_light()

DisType

#Interpretasi :   #1. Distribusi pokemon bertipe Bug cenderung rendah dibandingkan pokemon Tipe lainnya.
                  #   Median Power dari Pokemon Tipe Bug adalah 395, dengan IQR Power sekitar 205, yang berarti 
                  #   sekitar 50% Pokemon Tipe Bug memiliki power antara 270 - 475


#==========||===========##==========||===========##==========||===========##==========||===========#
#==========||===========##==========||===========##==========||===========##==========||===========#


###Categorical vs Continous
##Bagaimana distribusi dari Power pada pokemon Legendary?
DisLegend = ggplot(pokemon) +
  geom_boxplot(mapping = aes(x=Legendary, y=Power))+
  scale_y_continuous(breaks = seq(0,800,100))+
  ggtitle('Distribusi Power Pada Pokemon Legendary')+
  xlab('Legendary')+
  dark_theme_light()

DisLegend

#Interpretasi :   #1. Distribusi pokemon legendary cenderung lebih tinggi dibandingkan pokemon non legendary
                  #   Median Power dari Legendary Pokemon adalah 600, dengan IQR Power sekitar 105, yang berarti 
                  #   sekitar 50% Legendary pokemon memiliki power antara 575 - 685