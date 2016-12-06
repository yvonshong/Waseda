__author__ = 'm.bashari'
import numpy as np
from numpy  import *
from sklearn import datasets, linear_model
import matplotlib.pyplot as plt

# generate the data
def generate_data():
    
    # import the data set of TSP
    data = loadtxt('two_spiral.txt') 
    for datalabel in data:
        if(datalabel[2]==0.1):
            datalabel[2]=0
        else:
            datalabel[2]=1

    X = data[:,:2]
    y = data[:,2]
    
    return X,y
    

# to plot the diagram
def visualize(X, y, clf):
    # plt.scatter(X[:, 0], X[:, 1], s=40, c=y, cmap=plt.cm.Spectral)
    # plt.show()
    plot_decision_boundary(lambda x: clf.predict(x), X, y)
    plt.title("Logistic Regression")

# plot the direct line to classify
def plot_decision_boundary(pred_func, X, y):
    # Set min and max values and give it some padding
    x_min, x_max = X[:, 0].min() - .5, X[:, 0].max() + .5
    y_min, y_max = X[:, 1].min() - .5, X[:, 1].max() + .5
    h = 0.01
    # Generate a grid of points with distance h between them
    xx, yy = np.meshgrid(np.arange(x_min, x_max, h), np.arange(y_min, y_max, h)) # generate a object of array
    # Predict the function value for the whole gid
    Z = pred_func(np.c_[xx.ravel(), yy.ravel()]) # ravel - flattening the multidimensional array
    Z = Z.reshape(xx.shape) # reshape to a multidimensional array
    # Plot the contour and training examples
    plt.contourf(xx, yy, Z, cmap=plt.cm.Spectral) # surface rendering
    plt.scatter(X[:, 0], X[:, 1], c=y, cmap=plt.cm.Spectral) #  Scatter plots - draw the data
    plt.show()

# classification 
def classify(X, y):
    clf = linear_model.LogisticRegressionCV()
    clf.fit(X, y)
    return clf


def main():
    X, y = generate_data()
    # visualize(X, y)
    clf = classify(X, y)
    visualize(X, y, clf)


if __name__ == "__main__":
    main()
