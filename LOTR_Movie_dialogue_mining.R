##### LOTR - MOVIE DIALOGUE TEXT MINING #####

#### LIBRARY NEEDED
library(tm)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(reshape2)
library(pals)
library(colorRamps)
library(wordcloud)
library(stringr)
library(dplyr)

#### LOAD AND PREPARE DATA
fellowship <- read.csv("01 Fellowship.txt", header = F, sep = "\n")
fellowship$movie <- "The Fellowship of the Ring"
twotowers <- read.csv("02 Two Towers.txt", header = F, sep = "\n")
twotowers$movie <- "The Two Towers"
returnking <- read.csv("03 Return of the King.txt", header = F, sep = "\n")
returnking$movie <- "The Return of the King"
trilogy <- rbind(fellowship, twotowers, returnking)

colnames(trilogy) <- c("dialogue", "movie")
trilogy$dialogue <- as.character(trilogy$dialogue)
trilogy$movie <- as.factor(trilogy$movie)
                        

### DATA PROCESSING

# Get each dialogue's character
characters <- vector()
for(i in 1:nrow(trilogy)){
  who <- gsub( ":.*$", "", trilogy[i,1])
  who <- gsub(" ", "", who)
  characters <- append(characters, who )
}

trilogy$character <- as.factor(characters)

# Remove character from dialogue
sentence <- vector()
for(i in 1:nrow(trilogy)){
  what <- gsub( ".*:", "", trilogy[i,1])
  what <- gsub("^ ", "", what)
  sentence <- append(sentence, what)
}

trilogy$dialogue <- as.character(sentence)


# Next we will add another column with each characters race
# For this task we'll use the file data "races" that I've prepared
# beforehand

races <- read.csv("races.csv", sep = ";")

get_race <- function(x, y){
  race <- vector()
  for(i in 1:nrow(x)){
    if(x[i,"character"] %in% y$Character){
      race <- append(race, as.character(y$Race[match(x[i,"character"], y$Character)]))
    }
  }
  return(race)
}

char_race <- get_race(trilogy, races)
trilogy$race <- as.factor(char_race)

### Clean the text
cleaning <- function(x) {
  x = gsub("[[:punct:]]", " ", x)
  x = tolower(x)
  x = gsub("^ ", "", x)
  x = gsub(" $", "", x)
  x = gsub(" +", " ", x)
}

text <- cleaning(trilogy$dialogue)
trilogy$clean_dialogue <- as.character(text)

# To make things easier let's attach the trilogy dataset
attach(trilogy)

# We will also get another column with the amount of words by each line
by_words <- strsplit(clean_dialogue, " ")

amount_words <- vector()
for(i in 1:length(by_words)){
  amount_words <- append(amount_words, length(by_words[[i]]))  
}

trilogy$amount_words <- amount_words


### DATA MINING

### A) Global movie comparison

# Let's create a table with the amount of lines, words and characters so
# we can compare the three movies. For better perspective, we'll also
# include the duration of the movies, and get the variables refferring
# to text per minute.

Duration <- c(208, 223, 251) # in minutes
Characters_count <- c(length(unique(trilogy[movie=="The Fellowship of the Ring", "character"])),
                     length(unique(trilogy[movie=="The Two Towers", "character"])),
                     length(unique(trilogy[movie=="The Return of the King", "character"])))

Lines <- c(nrow(fellowship), nrow(twotowers), nrow(returnking))

Words <- c(sum(trilogy[movie=="The Fellowship of the Ring", "amount_words"]),
           sum(trilogy[movie=="The Two Towers", "amount_words"]),
           sum(trilogy[movie=="The Return of the King", "amount_words"]))

Lines_min <- round(Lines/Duration)
Words_min <- round(Words/Duration)

Movies <- as.data.frame(rbind("Duration (min)"= Duration, "Amount of characters"= Characters_count,
                Lines, Words, "Lines/min"= Lines_min, "Words/min"= Words_min))


colnames(Movies) <- c("The Fellowship of the Ring", "The Two Towers",
                      "The Return of the King")

grid.table(Movies)

# As you can see even though The Fellowship of the Ring is the shorter
# and the one with less characters, it has he largest amount of dialogue


### B) Characters comparison
# Let's create a dataset with amount of lines, words and lines length by
# movie, race and character

lines_char <- as.data.frame.matrix(table(trilogy$character, trilogy$movie))
lines_char$character <- row.names(lines_char)
lines_char <- melt(lines_char, value.name = "lines",
                   variable.name = "movie",
                   varnames="character")
char_race <- get_race(lines_char, races)
lines_char$race <- as.factor(char_race)

words_char <- trilogy %>% group_by(movie, character, race) %>% 
  summarise(words = sum(amount_words))

full_info <- merge(lines_char, words_char)

# reorder columns
full_info <- full_info[, c(1,3,2,4,5)]

# add a column with the length of sentences (words/line)
full_info$line_length <- round(full_info$words/full_info$lines, 2)

full_info$character <- as.factor(character)
full_info$race <- as.factor(full_info$race)
full_info$movie <- factor(full_info$movie, 
                        levels= c("The Fellowship of the Ring",
                                  "The Two Towers",
                                  "The Return of the King"))
attach(full_info)

## Visualize data

# In order to create a color palette to viasualize 
# characters, I'll create a new column with the 
# names of the main characters (or somewhat important),
# and "Other" to secondary characters

main <- c("Frodo", "Sam", "Merry", "Pippin", "Gollum",
          "Sméagol", "Bilbo", "Gimli", "Gríma",
          "Aragorn", "Boromir", "Gandalf", "Saruman",
          "Arwen", "Celeborn", "Galadriel", "Legolas",
          "Elrond", "Haldir", "Éomer", "Éowyn", "Faramir",
          "KingoftheDead", "MouthofSauron", "Nazgûl",
          "Ring", "Sauron", "Théoden", "Treebeard", "Tree",
          "WitchKing", "Uglúk", "Uruk")
maindt <- as.data.frame(main)

get_importance <- function(x, y){
  imp <- vector()
  for(i in 1:nrow(x)){
    if(x[i,"character"] %in% y$main){
      imp <- append(imp, as.character(y$main[match(x[i,"character"], y$main)]))
    }else{
      imp <- append(imp, "Other")
    }
  }
  return(imp)
}

importance <- get_importance(full_info, maindt)

full_info$importance <- as.factor(importance)

pal_1 <- c(Aragorn = "royalblue1", 
           Arwen = "springgreen2", 
           Bilbo = "tomato3",
           Boromir = "royalblue4",
           Celeborn = "olivedrab1",
           Elrond  = "seagreen3",
           Éomer = "aquamarine",
           Éowyn  = "aquamarine4",
           Faramir = "skyblue3",
           Frodo  = "chocolate3",
           Galadriel  = "olivedrab3",
           Gandalf  = "deeppink3",
           Gimli = "firebrick2",
           Gollum  = "salmon",
           Gríma = "darkcyan",
           Haldir  = "darkkhaki",
           KingoftheDead  = "mediumslateblue",
           Legolas = "green2",
           Merry = "lightgoldenrod",
           MouthofSauron   = "midnightblue",
           Nazgûl  = "mediumslateblue", 
           Pippin  = "peru",
           Ring = "gold",
           Sam  = "darkorange",    
           Saruman = "orchid4",
           Sauron  = "darkred",
           Sméagol = "tan1",
           Théoden  = "slateblue1",
           Tree  = "darkgreen",   
           Treebeard  = "forestgreen",
           Uglúk  = "gray64",
           Uruk   = "gray50",
           Other = "gray28",     
           WitchKing = "slateblue4")


lines_plot <- ggplot(full_info, aes(x= reorder(race, lines), y = lines, fill=importance)) +
  geom_bar(stat="identity", show.legend = F) +
  labs(title= "Amount of lines") +
  coord_flip() +
  facet_wrap(~movie) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, vjust = 5, face = "bold"), 
        axis.title = element_blank(),
        axis.text = element_text(size = 10, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10, face = "bold")) +
  scale_fill_manual(values = pal_1) +
  scale_x_discrete(limits = rev(levels(full_info$race)))

words_plot <- ggplot(full_info, aes(x=reorder(race, words), y = words, fill=importance)) +
  geom_bar(stat="identity", show.legend = F) +
  labs(title= "Amount of words") +
  coord_flip() +
  facet_wrap(~movie) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, vjust = 5, face = "bold"), 
        axis.title = element_blank(),
        axis.text = element_text(size = 10, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold")) +
  scale_fill_manual(values = pal_1) +
  scale_x_discrete(limits = rev(levels(full_info$race)))

linelength_plot <- ggplot(full_info, aes(x= reorder(race, line_length), y = line_length, fill=importance)) +
  geom_bar(stat="identity", show.legend = F) +
  labs(title= "Lines length") +
  coord_flip() +
  facet_wrap(~movie) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, vjust = 1, face = "bold"), 
        axis.title = element_blank(),
        axis.text = element_text(size = 10, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold")) +
  scale_fill_manual(values = pal_1) +
  scale_x_discrete(limits = rev(levels(full_info$race)))


#### *Get the legend (function taken from stackoverflow)
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

legend <- g_legend(lines_plot) # save one of the plots with a name
# and insert it here. Make sure the option "show.legend" is T.
# Then you can visualize just the legend by itself as an unique object.
# In this case I used the "lines_plot", in which you can see some
# legend edition arguments.

grid.arrange(lines_plot, words_plot, linelength_plot, legend,
             ncol = 2)

# * To visualize the same variables but without differentiating between 
# movies and getting an overall view just run the same code above except
# the "facet_wrap()" line


## Top characters with most words by movies
pal_2 <- c(Dwarf= "firebrick2", Elve = "chartreuse3",
         Hobbit = "darkorange", Men = "darkcyan",
         Wizard = "mediumorchid4", Ent = "forestgreen")

ggplot(full_info[words>=200, ], 
       aes(x=reorder(character, words), y = words, fill= race)) +
  geom_bar(stat="identity", show.legend = T) +
  facet_wrap(~movie) + 
  labs(title= "Top characters with more than 200 words by movie") +
  ylim(0, 2500) +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, size = 18, vjust = 1, face = "bold"), 
        axis.title = element_blank(),
        axis.text = element_text(size = 10, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10, face = "bold")) +
  scale_fill_manual(values = pal_2)

# *the plot only visualize a bar if the character has more than 200 words


### C) Most frequent words

# For each movie wi'll create the full dialogue text
attach(trilogy)
text_fellow <- paste(trilogy[movie=="The Fellowship of the Ring", 
                             "clean_dialogue"], collapse = " ")
text_towers <- paste(trilogy[movie=="The Two Towers", 
                             "clean_dialogue"], collapse = " ")
text_king <- paste(trilogy[movie=="The Return of the King", 
                             "clean_dialogue"], collapse = " ")
text_trilogy <- paste(text_fellow, text_towers, text_king, collapse = " ")


fellow_corpus <- Corpus(VectorSource(text_fellow))
fellow_corpus <- tm_map(fellow_corpus, removeWords, stopwords("en"))
final_fellow_text <- fellow_corpus$content

towers_corpus <- Corpus(VectorSource(text_towers))
towers_corpus <- tm_map(towers_corpus, removeWords, stopwords("en"))
final_towers_text <- towers_corpus$content

king_corpus <- Corpus(VectorSource(text_king))
king_corpus <- tm_map(king_corpus, removeWords, stopwords("en"))
final_king_text <- king_corpus$content

trilogy_corpus <- Corpus(VectorSource(text_trilogy))
trilogy_corpus <- tm_map(trilogy_corpus, removeWords, stopwords("en"))
final_trilogy_text <- trilogy_corpus$content

par(mfrow=c(1,3))
wordcloud(final_fellow_text, max.words = 50, random.order = FALSE, 
          random.color = FALSE,
          colors=c("springgreen3","springgreen4", "forestgreen", "darkgreen"))
wordcloud(final_towers_text, max.words = 50, random.order = FALSE, 
          random.color = FALSE,
          colors=c("skyblue2","steelblue3", "royalblue3", "royalblue4"))
wordcloud(final_king_text, max.words = 50, random.order = FALSE, 
          random.color = FALSE,
          colors=c("tomato", "firebrick2", "firebrick3","firebrick4"))

par(mfrow=c(1,1))
wordcloud(final_trilogy_text, max.words = 50, random.order = FALSE, 
          colors= brewer.pal(6, "Dark2"))
text(x=0.5, y=0.97, "Trilogy most frequent words", cex = 1.5)

### Wordcloud of a random character
# Create a function in which insert the dataframe and the name 
# of the character we want

char_wordcloud <- function(x, y) {
  text <- paste(trilogy[character==y, "clean_dialogue"],
                collapse = " ")
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  final_text <- corpus$content
  w <- wordcloud(final_text, max.words = 50, random.order = FALSE, 
            colors=kelly(22))
  text(x=0.5, y=0.99, paste(y, "'s most frequent words", sep = ""),
       cex = 1.5)
}

char_wordcloud(trilogy, "Frodo") 


## *Note: the stemming of the words hasn't been done because packages
#  like Snowball dont really work correctly.


### D) Most named by other characters
# For this analisys we need to add 3 names to the main character list
cha <- c(main, c("Isildur","Rosie", "Déagol", "Denethor"))

# For the searching we need them to be in lowercase since the whole 
# text is in lowercase
charac_lc <- tolower(cha)

# Search and count how many times do they appear in the dialogues
count_fellow <- str_count(text_fellow, pattern = charac_lc)
count_towers <- str_count(text_towers, pattern = charac_lc)
count_king <- str_count(text_king, pattern = charac_lc)

mention_counter <- data.frame(cha, count_fellow, count_towers,
                              count_king)

mention_counter <- as.data.frame(t(mention_counter))
colnames(mention_counter) <- cha
mention_counter <- mention_counter[-1,]
Movie <- c("The Fellowship of the Ring","The Two Towers",
          "The Return of the King")
mention_counter <- cbind(Movie, mention_counter)
rownames(mention_counter) <- NULL

mention_counter <- melt(mention_counter, "Movie", variable.name = "character")
mention_counter$value <- as.integer(mention_counter_2$value)

m_race <- get_race(mention_counter, races)
mention_counter$race <- m_race

# We will only take the top 10 to visualize it

top_10 <- mention_counter %>% group_by(Movie) %>% 
  arrange(Movie, desc(value)) %>%
  filter(row_number() <= 10L)

top_10 <- top_10[top_10$Movie!="Trilogy",]

gradient <-  colorRampPalette(c("slateblue4", "firebrick"))
top_10$color <- rep(gradient(10),3)

attach(top_10)

ggplot(data=top_10,
       aes(x=Movie, y= value, fill=color)) +
  geom_bar(stat="identity", show.legend = F,
           position = "fill", colour="black",
           width=1, size=1.2) +
  ggtitle("Top 10 character mentioned") +
  geom_text(aes(label= character), position = "fill",
            vjust=1.2, hjust=0.9, size=4.5, color="white",
            fontface = "bold") +
  geom_text(aes(label= value), position = "fill",
            vjust=1.2, hjust=-0.8, size=4.5, color="white",
            fontface = "bold") +
  scale_fill_manual(values = color) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=12, face="bold"),
        plot.title = element_text(size=16, face="bold",
                                  vjust=0, hjust = 0.5)) +
  scale_x_discrete(label=Movie)
  
#############################################################
####                      THE END                        ####
#############################################################
