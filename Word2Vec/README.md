# Word2Vec

Collect resources for understanding word2vec here. Below are links to materials for discussion on 6/9:

## Resources for understanding the model


Blog post: https://jalammar.github.io/illustrated-word2vec/

For visual learners, there is this Youtube video (17 minutes): https://www.youtube.com/watch?v=QyrUentbkvw&ab_channel=JordanBoyd-Graber

Chapter 6 of Jurafsy and Martin's NLP textbook is a helpful detailed guide to some of the concepts related to Word2Vec and embeddings in general. Specifically pages 18-22 are great primers on the SGD-based training process for word2vec - https://web.stanford.edu/~jurafsky/slp3/6.pdf

A couple of more detailed blog posts that disassemble the model and some of the reasoning behind various elements/steps:
https://aegis4048.github.io/demystifying_neural_network_in_skip_gram_language_modeling#eq-18

https://medium.com/district-data-labs/forward-propagation-building-a-skip-gram-net-from-the-ground-up-9578814b221#:~:text=The%20Skip%2Dgram%20neural%20network,layer%2C%20and%20an%20output%20layer.


**Reminder:** The goal for Tuesday will be to just make sure we all understand what word2vec is meant to do and why it is interesting, as well as getting an initial sense of the steps involved in fitting such a model to some corpus of data. We will then spend a week playing around with getting it to work on something of interest. 



## Resources for training and downloading word embeddings (feel free to add):

**Implementations**  
Skip Gram w/ negative sampling: [Folder with scripts, corpus (simple wikipedia), and pretrained embeddings](https://uwprod-my.sharepoint.com/:f:/g/personal/borman_wisc_edu/EvzWE5VMOYtEs6-kUk7lhnoBUOQXrNZuWdiGcnuKl3V2vg?e=QYaMKF)

[PyTorch implementation of the continuous-bag-of-words (CBOW) approach to fitting models](https://rguigoures.github.io/word2vec_pytorch/) (not the version of w2v we discussed)

**Corpora**  
[Historic American cookbook corpus (needs cleaning)](https://archive.lib.msu.edu/dinfo/feedingamerica/)  
(If interested in R implementation, [this repo/package](https://github.com/bmschmidt/wordVectors) and [its introductory vignette](https://github.com/bmschmidt/wordVectors/blob/master/vignettes/introduction.Rmd) provides a simple walkthrough for training on the cookbook corpus)

**Repositories of Pretrained embeddings**  
http://vectors.nlpl.eu/repository/
https://github.com/jvparidon/subs2vec (Jeroen's repo - also includes python package for cleaning/training word embeddings using FastText)
