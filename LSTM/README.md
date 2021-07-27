# LSTMs
Files in this directory develop LSTMs and standard recurrent networks (ie simple recurrent or Elman nets) using a simple implementation of a model that carries out a 2-back matching task. Notebooks and datasets were prepared by Quan Wan.

Steve Schwering found the following resources useful for understanding LSTMs:


Follow this link to Christopher Olah's blog post about LSTM networks. This is a good introduction to the high-level interpretation of the model's behavior, walking you through the forget gate, the input gate, and the hidden state/output gate.
https://colah.github.io/posts/2015-08-Understanding-LSTMs/


If you prefer to hear someone describing the model to you, the following video may be helpful. It uses many of the same figures from the blog post linked above. The high-level description of the short-term memory and the long-term memory components occurs roughly from 4:30 - 10:00. The description of the gates and walking through some simple examples follows from 10:00 - 25:30:
https://www.youtube.com/watch?v=eCvz-kB4yko

If you would like to look at some example implementations, I found this implementation of a model doing sentiment analysis to be useful:
https://blog.floydhub.com/long-short-term-memory-from-zero-to-hero-with-pytorch/

 Furthermore, if you are interested in an example for how the LSTM cell could be implemented from scratch -- without using torch.nn.LSTM() -- then you could check out the following blog post. It's a pretty straightforward implementation of the basic LSTM cell with a short discussion of peephole variants:
 https://towardsdatascience.com/building-a-lstm-by-hand-on-pytorch-59c02a4ec091
