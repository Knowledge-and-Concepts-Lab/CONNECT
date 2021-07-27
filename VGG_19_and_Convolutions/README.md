# Deep convolutional image classifiers

This directory collects files for experimenting with deep image classifiers such as VGG19. The enclosed notebook was written by Pablo Calceres Maldonado and provides a Colab-ready example of how to use the pretrained VGG-19 model to generate activation vectors for all images in a target directory, as well as showing some Python-based tools for looking at the structure of these representations.


As a starting place, the following blog post provides A Review of Popular Deep Learning Architectures: AlexNet, VGG16, and GoogleNet. https://blog.paperspace.com/popular-deep-learning-architectures-alexnet-vgg-googlenet/

Beyond describing the evolution of design architectures, that link and blog post is useful in that it also has links (most of which still work) to implementations in both PyTorch and Tensorflow for versions of each of the models.

We will use this article (https://cs231n.github.io/convolutional-networks/#overview) to talk about Convolutional Neural Networks (CNNs/ConvNets), and this video (https://www.youtube.com/watch?v=-I0lry5ceDs) can serve as a primer for understanding that process.



Although we will not be outlining the original 2015 conference paper, Very Deep Convolutional Networks for Large-Scale Image Recognition, by Simonyan and Zisserman (https://arxiv.org/pdf/1409.1556.pdf) the first 3:40 of the following video may serve as a useful resource to unpack the ConvNet configurations graph; the video then continues by reviewing how to construct such an architecture in PyTorch: 
https://www.youtube.com/watch?v=ACmuBbuXn20 

The first 13:30 of this video also offers a good breakdown of the VGGNet architecture:
https://www.youtube.com/watch?v=mRVTKrbRYi0&t=1s 

This link provides a VGG-16 implementation in Keras: https://github.com/krishnaik06/Advanced-CNN-Architectures

