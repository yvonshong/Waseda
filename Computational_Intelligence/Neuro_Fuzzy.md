# Neuro-fuzzy Network

Authored by **SONG DAIWEI** 44161588-3

Written by Markdown and Latex.

You can see its source code in [GitHub of SONG DAIWEI](https://github.com/Yvon-Shong/Waseda/blob/master/Computational_Intelligence/Neuro_Fuzzy.md)

# Intrioduction

In the field of artificial intelligence, neuro-fuzzy refers to combinations of artificial neural networks and fuzzy logic. Neuro-fuzzy was proposed by J. S. R. Jang. Neuro-fuzzy hybridization results in a hybrid intelligent system that synergizes these two techniques by combining the human-like reasoning style of fuzzy systems with the learning and connectionist structure of neural networks. Neuro-fuzzy hybridization is widely termed as Fuzzy Neural Network (FNN) or Neuro-Fuzzy System (NFS) in the literature. Neuro-fuzzy system (the more popular term is used henceforth) incorporates the human-like reasoning style of fuzzy systems through the use of fuzzy sets and a linguistic model consisting of a set of IF-THEN fuzzy rules. The main strength of neuro-fuzzy systems is that they are universal approximators with the ability to solicit interpretable IF-THEN rules.

The strength of neuro-fuzzy systems involves two contradictory requirements in fuzzy modeling: interpretability versus accuracy. In practice, one of the two properties prevails. The neuro-fuzzy in fuzzy modeling research field is divided into two areas: linguistic fuzzy modeling that is focused on interpretability, mainly the Mamdani model; and precise fuzzy modeling that is focused on accuracy, mainly the Takagi-Sugeno-Kang (TSK) model.

Although generally assumed to be the realization of a fuzzy system through connectionist networks, this term is also used to describe some other configurations including:

- Deriving fuzzy rules from trained RBF networks.

- Fuzzy logic based tuning of neural network training parameters.

- Fuzzy logic criteria for increasing a network size.

- Realising fuzzy membership function through clustering algorithms in unsupervised learning in SOMs and neural networks.

- Representing fuzzification, fuzzy inference and defuzzification through multi-layers feed-forward connectionist networks.

It must be pointed out that interpretability of the Mamdani-type neuro-fuzzy systems can be lost. To improve the interpretability of neuro-fuzzy systems, certain measures must be taken, wherein important aspects of interpretability of neuro-fuzzy systems are also discussed.

A recent research line addresses the data stream mining case, where neuro-fuzzy systems are sequentially updated with new incoming samples on demand and on-the-fly. Thereby, system updates do not only include a recursive adaptation of model parameters, but also a dynamic evolution and pruning of model components (neurons, rules), in order to handle concept drift and dynamically changing system behavior adequately and to keep the systems/models "up-to-date" anytime. 

# fuzzy neural network 
A fuzzy neural network or neuro-fuzzy system is a learning machine that finds the parameters of a fuzzy system (i.e., fuzzy sets, fuzzy rules) by exploiting approximation techniques from neural networks.

Compared to a common neural network, connection weights and propagation and activation functions of fuzzy neural networks differ a lot. Although there are many different approaches to model a fuzzy neural network (Buckley and Hayashi, 1994, 1995; Nauck and Kruse, 1996), most of them agree on certain characteristics such as the following:

![The architecture of a neuro-fuzzy system](http://www.scholarpedia.org/w/images/thumb/3/3b/Nfc_architecture.gif/200px-Nfc_architecture.gif)

1. A neuro-fuzzy system based on an underlying fuzzy system is trained by means of a data-driven learning method derived from neural network theory. This heuristic only takes into account local information to cause local changes in the fundamental fuzzy system.

1. It can be represented as a set of fuzzy rules at any time of the learning process, i.e., before, during and after.
    - Thus the system might be initialized with or without prior knowledge in terms of fuzzy rules.
1. The learning procedure is constrained to ensure the semantic properties of the underlying fuzzy system.

1. A neuro-fuzzy system approximates a n-dimensional unknown function which is partly represented by training examples.
Fuzzy rules can thus be interpreted as vague prototypes of the training data.
1. A neuro-fuzzy system is represented as special three-layer feedforward neural network as it is shown in figure.
    
    - The first layer corresponds to the input variables.

    - The second layer symbolizes the fuzzy rules.

    - The third layer represents the output variables.

    - The fuzzy sets are converted as (fuzzy) connection weights.

    - Some approaches also use five layers where the fuzzy sets are encoded in the units of the second and fourth layer, respectively. However, these models can be transformed into a three-layer architecture.

One can basically distinguish between three different kinds of fuzzy neural networks, i.e., cooperative, concurrent and hybrid FNNs (Nauck et al., 1997).

# Comparison of neural control and fuzzy control
Both neural networks and fuzzy systems have some things in common. They can be used for solving a problem (e.g. pattern recognition, regression or density estimation) if there does not exist any mathematical model of the given problem. They solely do have certain disadvantages and advantages which almost completely disappear by combining both concepts.

Neural networks can only come into play if the problem is expressed by a sufficient amount of observed examples. These observations are used to train the black box. On the one hand no prior knowledge about the problem needs to be given. On the other hand, however, it is not straightforward to extract comprehensible rules from the neural network's structure.

On the contrary, a fuzzy system demands linguistic rules instead of learning examples as prior knowledge. Furthermore the input and output variables have to be described linguistically. If the knowledge is incomplete, wrong or contradictory, then the fuzzy system must be tuned. Since there is not any formal approach for it, the tuning is performed in a heuristic way. This is usually very time consuming and error-prone.

 Comparison of neural control and fuzzy control
|    |    |
|:---:|:---:|
|Neural Networks	|Fuzzy Systems|
|no mathematical model necessary|	no mathematical model necessary|
|learning from scratch|	apriori knowledge essential|
|several learning algorithms|	not capable to learn|
|black-box behavior|	simple interpretation and implementation|

It is desirable for fuzzy systems to have an automatic adaption procedure which is comparable to neural networks. As it can be seen in table, combining both approaches should unite advantages and exclude disadvantages.

# Reference

http://www.scholarpedia.org/article/Fuzzy_neural_network

https://en.wikipedia.org/wiki/Neuro-fuzzy