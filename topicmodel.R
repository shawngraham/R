#Open R, click file, open script, and select this script.
#in the script window, you can either select edit > run all
#to run this entire script,
#or you can step through it by highlight each bit of code between comments and hitting ctrl+R
#this whole script is based on Ben Marwick's Day of Archaeology work https://github.com/benmarwick/dayofarchaeology
Sys.setenv(NOAWT=TRUE)

#setup the workspace
# Set working directory
dir <- "C:\\" # adjust to suit
setwd(dir)
require(mallet)

#import the documents from the folder
#each document is here its own text file
#insert the path to your documents between the quotation marks
#and be sure to use \\ instead of a single \
#note that we have put both our inputs and our outputs into a folder called C:\Mallet;
#you can change this to whatever folder name you are working from. Just make sure to change it in lines 20, 57, and 64.

documents <- mallet.read.dir("C:\\Mallet\\dcb-txt\\DCB-txt\\")
mallet.instances <- mallet.import(documents$id, documents$text, "C:/mallet/stoplists/en.txt",
token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

#create topic trainer object
n.topics <- 30
topic.model <- MalletLDA(n.topics)

#load documents
topic.model$loadDocuments(mallet.instances)

## Get the vocabulary, and some statistics about word frequencies.
## These may be useful in further curating the stopword list.
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
## Optimize hyperparameters every 20 iterations,
## after 50 burn-in iterations.
topic.model$setAlphaOptimization(20, 50)

## Now train a model. Note that hyperparameter optimization is on, by default.
## We can specify the number of iterations. Here we'll use a large-ish round number.
topic.model$train(200)

## NEW: run through a few iterations where we pick the best topic for each token,
## rather than sampling from the posterior distribution.
topic.model$maximize(10)

## Get the probability of topics in documents and the probability of words in topics.
## By default, these functions return raw word counts. Here we want probabilities,
## so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)

# from http://www.cs.princeton.edu/~mimno/R/clustertrees.R
## transpose and normalize the doc topics
topic.docs <- t(doc.topics)
topic.docs <- topic.docs / rowSums(topic.docs)
write.csv(topic.docs, "C:\\Mallet\\dcb-topic-docs.csv")

## Get a vector containing short names for the topics
topics.labels <- rep("", n.topics)
for (topic in 1:n.topics) topics.labels[topic] <- paste(mallet.top.words(topic.model, topic.words[topic,], num.top.words=5)$words, collapse=" ")
# have a look at keywords for each topic
topics.labels
write.csv(topics.labels, "C:\\Mallet\\dcb-topics-labels.csv")

# create data.frame with columns as people and rows as topics
topic_docs <- data.frame(topic.docs)
names(topic_docs) <- documents$id

## cluster based on shared words
plot(hclust(dist(topic.words)), labels=topics.labels)

#' Calculate similarity matrix
#' Shows which documents are similar to each other
#' by their proportions of topics. Based on Matt Jockers' method

library(cluster)
topic_df_dist <- as.matrix(daisy(t(topic_docs), metric = "euclidean", stand = TRUE))
# Change row values to zero if less than row minimum plus row standard deviation
# keep only closely related documents and avoid a dense spagetti diagram
# that's difficult to interpret (hat-tip: http://stackoverflow.com/a/16047196/1036500)
topic_df_dist[ sweep(topic_df_dist, 1, (apply(topic_df_dist,1,min) + apply(topic_df_dist,1,sd) )) > 0 ] <- 0

#' Use kmeans to identify groups of similar authors

km <- kmeans(topic_df_dist, n.topics)
# get names for each cluster
allnames <- vector("list", length = n.topics)
for(i in 1:n.topics){
  allnames[[i]] <- names(km$cluster[km$cluster == i])
}

# Here's the list of people by group
allnames

#' Visualize people similarity using force-directed network graphs

#### network diagram using Fruchterman & Reingold algorithm
# static
# if you don't have igraph, install it by removing the hash below:
# install.packages("igraph")
library(igraph)
g <- as.undirected(graph.adjacency(topic_df_dist))
layout1 <- layout.fruchterman.reingold(g, niter=500)
plot(g, layout=layout1, edge.curved = TRUE, vertex.size = 1, vertex.color= "grey", edge.arrow.size = 0, vertex.label.dist=0.5, vertex.label = NA)


# interactive in a web browser
# if you have a particularly large dataset, you might want to skip this section, and just run the Gephi part.
# if you don't have devtools, install it by removing the hash below:
# install.packages("devtools")

devtools::install_github("d3Network", "christophergandrud")
require(d3Network)
d3SimpleNetwork(get.data.frame(g),width = 1500, height = 800,
                textColour = "orange", linkColour = "red",
                fontsize = 10,
                nodeClickColour = "#E34A33",
                charge = -100, opacity = 0.9, file = "d3net.html")
# find the html file in working directory and open in a web browser

# for Gephi
# this line will export from R and make the file 'g.graphml'
# in the working directory, ready to open with Gephi
write.graph(g, file="g.graphml", format="graphml")




