###Lab1

1 **Why should we remove stop words**

One reason is to save the space and speed up processing. Another reason is that those words do not contain useful information.

For example words like "is" "am" "are", those words themself do not have specific meaning. And in some specific context like education, "students" are also very common used words, so it can also be put into stop words list.

2 **What is lemmatisation and stemming and why should we do**

lemmatisation : return all words to it's base form. For example, took, taken-> take
stemming      :the end or beginning of a word is cut off, keeping common prefixes and suffixes

While stemming is a rule-based approach, lemmatization is a dictionary-based approach.

In computer "playing" and "play" they are different, but in our case they are same. So, we need technology to let computers know they are same.

lemmatisation and stemming can do such work. It brings convinience for further analysis and processing.

**3 TF-IDF**

$tf = \frac{the\ number\ of word\ \textbf{t}\ showing\ up\ in\  doc\  \textbf{d}}{total\ number\ of\ words\ in\ doc\ \textbf{d}}$

$idf = log(\frac{total\ number\ of\ documents}{the\ number\ of\ documents\ that\ contain\ word\ \textbf{t}})$

high idf means the term is rare to see.
high tf means this word is inportant to this document

high tf-idf means this word is important to identify document.

only idf is not enough, consider in a document, it has two words **A** and **B**, both of them has large idf value but **A** also has high tf value. If we use IDF only, then **A** and **B** are equaly important but in fact **A** is more like representation of this text.

**4 TF-IDF vectorizing**

1614*21396

1614 number of data

21396 number of unique words. 

for each documents,we use a sparse vector to record each words and their tf-idf value.

### Lab2

**1 Countvectorizer**
Convert a collection of text documents to a matrix of token counts

**2 How to summarise the results**
When we say a result is good or bad, we need to compare it with some base lines, and they should have the same evaluation.

**3 When oversampling when under sampling**

They are used when data is not balanced. 

Oversampling is used when amount of data is insufficient to increase the data belongs to small label groups.

Undersampling is used when data is sufficient.

**4 why hyperparameter search and why we never tune hyperparameter on test data.**

1  because hyperparameter can not be learned during training. We have to manualy tune them and see which combination has better performance. And hyperparameter can largely change model's behavior and performance.

2 Because we want our model can be generalized to unseen data. And for modelling, test data plays as unseen data. If we tune hyperparameter on test data, we can not say this model can be generalized to unseen data because the model might be overfitting to test data.

### Lab 3

**K-means and LDA**

K-means : Iteratively get centroids, and assign every data point to one of the centroids. The number of centroids is the same as number of clusters. This model is hard classifier, which means the results are discriminative.

LDA : LDA treats  each document as a multinomial distribution of topics, and each topic is modeled as multi distribution of words. So, when we use LDA to do prediction on documents or words, the results we have is a probability vector.

**Why log-likelihood for LDA not accuracy**

Because LDA is a soft model, it only says the probability of a termed can be assigned to a document or a document can be assigned to a topic. So likelihood can better tell if a LDA model is better than another LDA model, high log-likelihood

**When LDA when K-means**

K-means should be used when tasks require hard clusters.
E.G. sentiment analysis would want to actually sort all data points into certain sentiments

LDA should be used when it is beneficial to know the degree up to which a data point belongs to a certain cluster 

E.G. If you do sentiment analysis and want to know for each data point up to what degree this data point is positive

**number of clusters and quality of clusters**

If the number of clusters is too low, one cluster may contain key words from different topics, so it's hard to tell the theme of the text.  

If the number of clusters is too high, some clusters may have too specific words so it's hard to generalize.  

Only when number of clusters match the ground truth (or some specific requirements), will the quality of clusters become good.


### Lab 4

**1 TSNE**

TSNE can help reduce the data dimension for better visualization.

**2 n-grams**

unigram is just one word
bigram is w1_w2 
n_gram is w1_w2_..._wn

cross-unigram is (w1 or w2)+' '+w1_w2

**3 continuous word-bag**

a vectorizer that represents each sentence as the sum of its word vectors

**4 manually enginering features vs word embedding**

Manually enginering is time-cosuming

word embedding needs more computational resources

**pre-trained word-vector v.s embedding layer**

embedding layer is more flexible because the data in hand might be in different distribution than the data where pre-trained word-vector was trained. But is also takes much more time to train and also needs large amount of data to learn comprehensive linguistic knowledge.

A good solution is using the pre-trained word-vector and fine-tuning them through the embedding layer and data in hand. Or get a pre-trained embedding layer and fine-tune them using the data in hand.

### Lab 5

**1 Mention and entity [very important]**

Entity is an object which can be named entity, nominal entity and pronominal entity.

Mention is just a textual references to objects(entity).

For example 'rock' is the mention of rock, we can also call rock as 'rick', then 'rick' become another mention. But whatever the mention we call rock, it doesn't change the fact that rock is rock.

In real case, mention are normally in two ways.

1 named mention, this is just another name of the entity, like in our example, we set another name 'rick' to rock.

2 nominal mention, this is by using a important feature of the entity. For example, American president is a important feature of Biden (at least now). So, when we mention American president, we will think about Biden. 

**2 precision recall**

precision = $\frac{tp}{tp+fp}$
recall = $\frac{tp}{tp+fn}$

f1 = $\frac{2pr}{p+r}$

