### Analizar Whatsapp ###

### Instalar los paquetes necesarios

install.packages("rwhatsapp")
library(rwhatsapp)

### leer .txt con el chat, lo generan en whatsapp
data<-rwa_read("industrial.txt")

library("dplyr")
library("ggplot2")
library("lubridate")

### Cantidad de mensajes Diarios en el grupo ###
data<-mutate(data,yr=year(time))
data<-filter(data,yr>2017)
data %>%
  mutate(day = date(time)) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  ggtitle("Mensajes Diarios") +
  theme_bw()



#Codigo Opcional para tomar los que mas escriben
prueba<-data %>%
  mutate(day = date(time)) %>%
  count(author)
prueba<-filter(prueba,n>100)
prueba %>%
  ggplot(aes(x=reorder(author,n),y=n))+
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Numero de Mensajes") +
  theme_bw()
# fin codigo Opcional

# Numero de mensajes por persona del grupo
data<-filter(data,!is.na(author))
data %>%
  mutate(day = date(time)) %>%
  count(author) %>%
  ggplot(aes(x = reorder(author, n), y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Numero de Mensajes") +
  theme_bw()


### Emojis mas utilizados por persona del grupo
data<-filter(data,!is.na(author))
library("tidyr")
data %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y")  +
  ggtitle("Emojis mas utilizados") +
  theme_bw()


# Palabras mas utilizadas por persona del grupo

library("tidytext")
data %>%
  unnest_tokens(input = text,
                output = word) %>%
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  ggtitle("Most often used words") +
  theme_bw()


library("stopwords")
to_remove <- c(stopwords(language = "es"),
               "q",
               "media",
               "omitted",
               "dass",
               "schon",
               "mal",
               "android.s.wt")

data %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 5, n) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  ggtitle("Palabras mas utilizadas") +
  theme_bw()


# Cantidad de palabras diferentes (Diversidad de Lexico)

data %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  group_by(author) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
 filter(lex_diversity>200) %>%
  arrange(desc(lex_diversity)) %>%
  ggplot(aes(x = reorder(author, lex_diversity),
             y = lex_diversity,
             fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(expand = (mult = c(0, 0, 0, 500))) +
  geom_text(aes(label = scales::comma(lex_diversity)), hjust = -0.1) +
  ylab("Palabras Unicas") +
  xlab("") +
  ggtitle("Diversidad de Lexico") +
  theme_bw() +
  coord_flip()


#word cloud

library(tm)
library(wordcloud2)
library(SnowballC)
library(qdapRegex)     # Removing URLs 
library(wordcloud2)
library(tidytext)
library(stringr)

AllData=data.frame(text=data$text)

allchats<-str_c(AllData$text, collapse = "")


allchats <- 
  allchats %>%
  str_remove("\\n") %>%                   # remove linebreaks
  rm_twitter_url() %>%                    # Remove URLS
  rm_url() %>%
  str_remove_all("#\\S+") %>%             # Remove any hashtags
  str_remove_all("@\\S+") %>%             # Remove any @ mentions
  removeWords(stopwords("spanish")) %>%   # Remove common words (a, the, it etc.)
  removeWords(stopwords("english")) %>%
  removeNumbers() %>%
  stripWhitespace() %>% # ajustar palabras dependiendo del caso
  removeWords(c("jaja","jajaja","omitted>","< media","<media","message","que","muchas")) 

Nc=3

CChat=Corpus(VectorSource(allchats))

tdmChat=TermDocumentMatrix(CChat)
MChat=as.matrix(tdmChat)
VChat=sort(rowSums(MChat),decreasing=TRUE)
VChat=VChat[-1]
AChat=data.frame(word=names(VChat),freq=VChat)
SChat= max((AChat[,2])/Nc)
#AChat <- filter(AChat, freq > SChat)

IMA.ALL=AChat
IMA.ALL=IMA.ALL[1:300,]
wordcloud2(IMA.ALL, size=0.2,shape = 'star')
