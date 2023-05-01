
install.packages("tm")
install.packages("ldatuning")
install.packages("topicmodels")
install.packages("stm")
install.packages("wordcloud")
install.packages("ggplot2")
install.packages("reshape")
install.packages("pals")
install.packages("igraph")
library(tm)
library(ggplot2)
data_LDA <- read.csv(file = 'lda_docs.csv', sep = ',', encoding = "UTF-8")

source <- DataframeSource(data_LDA)
corp <-  Corpus(source)


DTM <- DocumentTermMatrix(corp)

library(ldatuning)


models = FindTopicsNumber(DTM, topics = 2:30, method = "Gibbs", metrics = c("Arun2010","CaoJuan2009", "Griffiths2004", "Deveaud2014"))
FindTopicsNumber_plot(models)

library(topicmodels)

set.seed(2000)
lda_tm <- LDA(DTM, 29, method = "Gibbs", encoding = "UTF-8") 

print(terms(lda_tm, 10), encoding = "UTF-8")
tm_result <- posterior(lda_tm)

#wordclouds
sel_topic <- 9
top25 <- sort(tm_result$terms[sel_topic,], decreasing=TRUE)[1:25]
terms <- names(top25)

probs <- sort(tm_result$terms[sel_topic,], decreasing=TRUE)[1:25]

library(wordcloud)

wordcloud(terms, probs, random.order = FALSE, scale=c(3.25,0.25))

#ranking

proportion <- colSums(tm_result$topics) / nDocs(DTM)
sort(proportion, decreasing = TRUE)
write.csv(proportion,"proportion_LDA.csv")

#examples
sel_topic <- 29
threshold <- 0.4
indexes <- which(tm_result$topics[, sel_topic] >= threshold)
filtered <- corp[indexes]
filtered$content
 

#distribution
#by side
STM <- read.csv(file = 'STM.csv', sep = ',', encoding = "UTF-8")
STM


byside_LDA <- aggregate(tm_result$topics, by = list(STM$side), mean)
byside_LDA <- t(byside_LDA)
write.csv(byside_LDA,"byside_LDA.csv")


#by month
STM$Created <- as.Date(STM$Created, "%Y-%m-%d")
STM$Created_month <- format(STM$Created, "%Y-%m")

bymonth_LDA <- aggregate(tm_result$topics, by = list(STM$Created_month), mean)
bymonth_LDA <- t(bymonth_LDA)
write.csv(bymonth_LDA,"bymonth_LDA.csv")


######
#STM
library(stm)

start <- "2022-02-23"
start <- as.Date(start, "%Y-%m-%d")
STM$days <- as.Date(as.character(STM$Created), format="%Y-%m-%d")-
  as.Date(as.character(start), format="%Y-%m-%d")

temp<-textProcessor(documents=STM$tokenizd, metadata=STM,  lowercase = FALSE, removestopwords = FALSE, 
                    removenumbers = FALSE,
                    removepunctuation = FALSE,
                    ucp = FALSE,
                    stem = FALSE, language = 'hungarian')
meta<-temp$meta
vocab<-temp$vocab
docs<-temp$documents
out <- prepDocuments(docs, vocab, meta)

out$meta$days <- as.numeric(out$meta$days)


ST_model <- stm(out$documents, out$vocab, K = 29, prevalence =~ side + s(days),content = ~side, max.em.its = 75, data = out$meta, seed = 2000)

effect_side <- estimateEffect(1:29 ~ side, ST_model, meta = out$meta)
summary(effect, 1:29)

effect_time <- estimateEffect(1:29 ~ s(days), ST_model, meta = out$meta)

                       


###
plot(ST_model,type = "summary", text.cex = 0.53)

#examples

labelTopics(ST_model, 6)

thoughts21 <- findThoughts(ST_model, texts = STM$Content_y, n=10, topics =21)
thoughts21

plot(effect_side, covariate = "side", topics = c(1:29),
      model = ST_model, method = "difference",
     cov.value1 = "Kormánypárti", cov.value2 = "Nem kormánypárti",
     xlab = " Nem kormánypárti oldalak                                 Kormánypárti oldalak",
     main = "A sajtótermékek politkai oldalhoz való tartozásának hatása",
     xlim = c(-.1, .1), label = "custom",
     custom.labels = c(1:29))

plot(effect_time, "days", method = "continuous", topics = c(29),
     model = ST_model, printlegend = FALSE, xaxt = "n", xlab = "Idõ")
monthseq <- seq(from = as.Date("2022-02-24"),
                 to = as.Date("2023-02-24"), by = "month")
monthnames <- months(monthseq)
axis(1,at = as.numeric(monthseq) - min(as.numeric(monthseq)),
         labels = monthnames)



plot(ST_model, type = "perspectives", topics = 12, plabels = c("Kormánypárti oldalak", "Nem kormánypárti oldalak"))



library(igraph)
STM_corr <- topicCorr(ST_model)
plot(STM_corr)


