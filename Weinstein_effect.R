# Created by: Constance
# Created on: 10.05.18

library(GuardianR)
library(dplyr)
library(stringr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(scales)
library(gridExtra)
library(nnet)
theme_set(theme_bw())

# Analysing the Weinstein effect: how much the language changed after the Weinstein affair in articles about
# sexual harassment, looks at what words are used after he and she.

# The first parts are taken from https://www.gokhan.io/post/weinstein-effect/#fnref5
# If you're interested in this code please go check the original for the first part,
# it's the original, and it's very neatly presented there :)
# I made a few minor changes in the codes for the first parts, but my contribution is only the last part
# that adds a statistical analysis using logistic regression


#----------------------------------------------------------#
#                     GETTING THE DATA                     #
#----------------------------------------------------------#

# As Gokhan Ciflikli did on his blog, I used the Guardian API, as it is fairly easy to use
Guardian.key = 'Your guardian key'

articles <- get_guardian(keywords = "sexual+harassment",
                         section = "world",
                         from.date = "2012-05-09",
                         to.date = "2018-05-09",
                         api.key = Guardian.key)


write.csv(articles, file = "articles.csv")
articles <- read.csv("articles.csv", stringsAsFactors = FALSE)

## Keeping only the relevant columns
articles <- articles[, c("webPublicationDate", "body")]

## Cleaning up the html
# In the original post, Gokhan Ciflikli changes the encoding to parse the HTML,
# but it messes up some of the text, so I used regex insted
clean_html <- function(text){
  text <- gsub('<.*?>', '', text)
  text <- gsub('â\u0080\u009d', '', text)
  text <- gsub('â\u0080\u009c', '', text)
  text <- gsub('â\u0080\u0098', '', text)
  text <- gsub('â\u0080\u0093', '', text)
  text <- gsub('â\u0080¦', '', text)
  text <- gsub('â\u0080¢', '', text)
  text <- gsub('&#xa0;', '', text)
  text <- gsub('Â', '', text)
  text <- gsub('\n', '', text)
  text <- gsub('\\[', '', text)
  text <- gsub('\\]', '', text)
  text <- gsub('â\u0080\u0099', "'", text)
}

articles$body <- apply(matrix(articles[,2], ncol=1), MARGIN = 2, FUN = clean_html)

# Separating the articles for before and after the Weinstein affair
articles.before <- articles[articles$webPublicationDate < "2017-10-05", ]
articles.after <- articles[articles$webPublicationDate >= "2017-10-05", ]

# Creating data frames will all the texts
full.text.before <- as.vector(articles.before[, 2])
full.text.before <- data_frame(full.text.before)
full.text.after <- as.vector(articles.after[, 2])
full.text.after <- data_frame(full.text.after)

#----------------------------------------------------------#
#                GETTING THE SHE/HE BIGRAMS                #
#----------------------------------------------------------#

#Create bigrams (groups of 2 words) for the before data
bigrams.before <- full.text.before %>%
  unnest_tokens(bigram,
                full.text.before,
                token = "ngrams",
                n = 2)

# Splitting them in two words
bigrams.separated.before <- bigrams.before %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Keeping only the ones starting in he/she
he.she.words.before <- bigrams.separated.before %>%
  filter(word1 %in% c("he", "she"))

#Transform words into counts, add +1 for log transformation
he.she.counts.before <- he.she.words.before %>%
  count(word1, word2) %>%
  spread(word1, n, fill = 0) %>%
  mutate(total = he + she,
         he = (he + 1) / sum(he + 1),
         she = (she + 1) / sum(she + 1),
         log.ratio = log2(she / he),
         abs.ratio = abs(log.ratio)) %>%
  arrange(desc(log.ratio))


# Same for after
bigrams.after <- full.text.after %>%
  unnest_tokens(bigram,
                full.text.after,
                token = "ngrams",
                n = 2)

bigrams.separated.after <- bigrams.after %>%
  separate(bigram, c("word1", "word2"), sep = " ")

he.she.words.after <- bigrams.separated.after %>%
  filter(word1 %in% c("he", "she"))

he.she.counts.after <- he.she.words.after %>%
  count(word1, word2) %>%
  spread(word1, n, fill = 0) %>%
  mutate(total = he + she,
         he = (he + 1) / sum(he + 1),
         she = (she + 1) / sum(she + 1),
         log.ratio = log2(she / he),
         abs.ratio = abs(log.ratio)) %>%
  arrange(desc(log.ratio))


#----------------------------------------------------------#
#                    PLOTTING THE RESULTS                  #
#----------------------------------------------------------#


# Graphing the results
before = he.she.counts.before %>%
  filter(!word2 %in% c("himself", "herself", "ever", "quickly", "have"),
         total >= 5) %>%
  group_by(direction = ifelse(log.ratio > 0, 'More "she"', "More 'he'")) %>%
  top_n(15, abs.ratio) %>%
  ungroup() %>%
  mutate(word2 = reorder(word2, log.ratio)) %>%
  ggplot(aes(word2, log.ratio, fill = direction)) +
  geom_col() +
  coord_flip() +
  labs(x = "",
       y = 'Relative appearance after "she" compared to "he"',
       fill = "",
       title = "Pre Weinstein: 2012-17 The Guardian Articles on Sexual Harassment",
       subtitle = "Top 15 Most Gendered (Skewed) Verbs after he/she; at least 5 occurrences.") +
  theme(plot.title = element_text(size = 10),plot.subtitle = element_text(size = 8), axis.text = element_text(size = 6)) +
  scale_y_continuous(labels = c("8X", "6X", "4X", "2X", "Same", "2X", "4X", "6X", "8X"),
                     breaks = seq(-4, 4)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  expand_limits(y = c(-4, 4))

after = he.she.counts.after %>%
  filter(!word2 %in% c("himself", "herself", "ever", "quickly", "have"),
         total >= 5) %>%
  group_by(direction = ifelse(log.ratio > 0, 'More "she"', "More 'he'")) %>%
  top_n(15, abs.ratio) %>%
  ungroup() %>%
  mutate(word2 = reorder(word2, log.ratio)) %>%
  ggplot(aes(word2, log.ratio, fill = direction)) +
  geom_col() +
  coord_flip() +
  labs(x = "",
       y = 'Relative appearance after "she" compared to "he"',
       fill = "",
       title = "Post Weinstein: 2017-18 The Guardian Articles on Sexual Harassment",
       subtitle = "Top 15 Most Gendered (Skewed) Verbs after he/she; at least 5 occurrences.") +
  theme(plot.title = element_text(size = 10),plot.subtitle = element_text(size = 8), axis.text = element_text(size = 6)) +
  scale_y_continuous(labels = c("8X", "6X", "4X", "2X", "Same", "2X", "4X", "6X", "8X"),
                     breaks = seq(-4, 4)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  expand_limits(y = c(-4, 4))


# Plotting the results for both before and after together
grid.arrange(before, after, ncol = 2)



#----------------------------------------------------------#
#                    STATISTICAL ANALYSIS                  #
#----------------------------------------------------------#

## This is the part that I added:
# It is a multinomial logistic regression looking, taking the time (before or after Weinstein)
# and the pronom (he or she) as independent variables, and the words used after he or she as
# dependent variable. The coefficients are coefficients for the log odds ratios comparing each
# category with a baseline category. To limit the number of categories, I have kept only the
# most gendered words, and used the word 'has' (most frequent one, and ungendered) as a baseline.

## Putting the words in a data frame
names(he.she.words.after) <- c("Pronom", "Word")
he.she.words.after$time <- rep('after', nrow(he.she.words.after)) 
names(he.she.words.before) <- c("Pronom", "Word")
he.she.words.before$time <- rep('before', nrow(he.she.words.before)) 
he.she.words <- rbind(he.she.words.before, he.she.words.after)

# Removing the words that are not much gendered
gendered.after <- he.she.counts.after %>%
  filter(abs.ratio > 1.5, total >= 10) 
gendered.before <- he.she.counts.before %>%
  filter(abs.ratio > 1.5, total >= 10) 
gendered <- rbind(gendered.after, gendered.before)
gendered <- gendered[, 'word2']
# adds the word 'has' as a baseline category (ungendered)
gendered <- rbind(gendered, 'has')

he.she.words <- subset(he.she.words, he.she.words$Word %in% gendered$word2)
he.she.words$Word <- as.factor(he.she.words$Word)


# Multinomial logistic regression
# from https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
# Compares the odds to the word "has", as it frequent and ungendered
he.she.words$Word2 <- relevel(he.she.words$Word, ref = "has")
test <- multinom(Word2 ~ Pronom + time, data = he.she.words)

# Getting the p-values
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p <- as.data.frame(p)

# Keeping only the words with p-values below 0.05
p <- as.data.frame(p)
names(p) <- c("p-value intercept", "p-value pronom", "p-value time")
coeff <- as.data.frame(summary(test)$coefficients)
names(coeff) <- c("Coefficient intercept", "Coefficient pronom she", "Coefficient time before")
results <- cbind(coeff, p)

significant_results <- subset(results, 
                              results['p-value pronom'] < 0.1 ||  
                                results['p-value time'] < 0.1)
significant_results <- as.data.frame(significant_results)

# Getting the coefficients back to a multiplication of the odd ratios
odds_pron <- function(row){
  odd = exp(- row["Coefficient pronom she"])
  return(odd)
}
significant_results$odds_multiplication_pronom <- 
  apply(significant_results, 1, odds_pron)

odds_time <- function(row){
  odd = exp(- row["Coefficient time before"])
  return(odd)
}
significant_results$odds_multiplication_time <- 
  apply(significant_results, 1, odds_time)
View(significant_results)

# Turning the odds ratio to percentages
perc_pron <- function(row){
  percentage = (1 - row["odds_multiplication_pronom"]) * 100
  return(percentage)
}
significant_results['Percentage increase she'] <- 
  apply(significant_results, 1, perc_pron)

perc_time <- function(row){
  percentage = (row["odds_multiplication_time"] - 1) * 100
  return(percentage)
}
significant_results['Percentage increase after'] <- 
  apply(significant_results, 1, perc_time)

# Formatting so it's more readable and keeping only significant results
results_clean <- 
  significant_results[, c('Percentage increase she', 
                          'Percentage increase after', 
                          'p-value pronom', 'p-value time')]

results_clean$`Percentage increase she` <- 
  ifelse(results_clean$`p-value pronom` < 0.05, results_clean$`Percentage increase she`, NaN)

results_clean$`Percentage increase after` <- 
  ifelse(results_clean$`p-value time` < 0.05, results_clean$`Percentage increase after`, NaN)

results_clean <- 
  significant_results[, c('Percentage increase she', 
                          'Percentage increase after')]

## Each coefficient can be now understood as:
# for 'Percentage increase she': it is xx % more likely for this word to be used after she than after he,
# compared to the word 'has'
# It sounds like a mouthfull, but you can get an intuitive sense of what each percentage means
