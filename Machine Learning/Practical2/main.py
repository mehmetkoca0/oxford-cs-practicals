import numpy as np
import math
from sklearn.linear_model import LogisticRegression
from sklearn.datasets import load_iris
import matplotlib.pyplot as plt

class NBC():
    def __init__(self,feature_types,num_classes):
        self.feature_types= feature_types
        self.num_classes=num_classes
        self.classes_ = None
        self.class_prior_ = None
        self.mean = None
        self.std = None

    def fit(self,X,y):
        self.classes_ = np.unique(y)
        X_classified =  [ X[y == c] for c in self.classes_]
        self.class_prior_ = self.calc_priors(X_classified, y)
        self.mean = self.calc_means(X_classified)
        self.std = self.calc_stds(X_classified)

    def calc_priors(self, X_classified,y):
        return np.array([X_class.size/y.size for X_class in X_classified])

    def calc_means(self,X_classified):
        return np.array([X_class.mean(axis=0) for X_class in X_classified])

    def calc_stds(self,X_classified):
        return np.array( [np.maximum(X_class.std(axis=0), 10**(-6)) for X_class in X_classified ] )


    def predict(self,X):
        jll = self._joint_log_likelihood(X)
        return [self.classes_[i] for i in np.argmax(jll, axis=1)]

    def _joint_log_likelihood(self, X):
        N, D = X.shape
        jll = np.zeros(shape=(N, self.num_classes))
        log_class_prior_ = np.log(self.class_prior_)
        for i in range(0, N):
            jll[i] = log_class_prior_
            for c in range(0, self.num_classes):

                jll[i][c] += np.sum(self.log_pdfs (X[i, :], self.mean[c], self.std[c] ) )
        return jll

    def log_pdfs(self, x, mean, std):
        D = len(x)-1
        diff = [(x[i] - mean[i]) for i in range(D)]
        top = [diff[i]**2/(2*std[i]**2) for i in range(D)]
        bottom = std * math.sqrt(2 * math.pi)
        bottom = [math.log(bottom[i]) for i in range(D)]
        logPdf = [-(bottom[i] + top[i]) for i in range(D)]
        return logPdf

def shuffle(X, y):
        N, D = X.shape
        Ntrain = int(0.8 * N)
        shuffler = np.random.permutation(N)
        Xtrain = X[shuffler[:Ntrain]]
        ytrain = y[shuffler[:Ntrain]]
        Xtest = X[shuffler[Ntrain:]]
        ytest = y[shuffler[Ntrain:]]
        return Ntrain, Xtrain, ytrain, Xtest, ytest
    
def test_accuracy(prediction, y):

    return np.mean(prediction == y)

iris = load_iris()
X, y = iris['data'], iris['target']

nbc = NBC(feature_types=['r', 'r', 'r', 'r'], num_classes=3)
Ntrain, Xtrain, ytrain, Xtest, ytest = shuffle(X, y)

nbc.fit(Xtrain, ytrain)
prediction = nbc.predict(Xtest)
data_test_accuracy = test_accuracy(prediction, ytest)
print("Data test accuracy = " + str(data_test_accuracy))

# Q1: C is the inverse of lambda so if lambda=0.1 then C=10

def compare_error():
    nbc_err = []
    log_reg_err = []
    log_reg = LogisticRegression(C=10, solver='liblinear')
    for i in np.arange(10):
        cum_nbc_err = 0
        cum_lr_err = 0
        percent = (i + 1) / 10
        for repeat in range(1000):
            Ntrain, Xtrain, ytrain, Xtest, ytest = shuffle(X, y)
            n = int(percent * Ntrain)

            num_c = len(np.unique(ytrain[:n]))
            nbc = NBC(feature_types=['r', 'r', 'r', 'r'], num_classes=num_c)
            nbc.fit(Xtrain[:n], ytrain[:n])
            cum_nbc_err += 1 - test_accuracy(nbc.predict(Xtest), ytest)

            log_reg.fit(Xtrain[:n], ytrain[:n])
            cum_lr_err += 1 - test_accuracy(log_reg.predict(Xtest), ytest)
        nbc_err.append(cum_nbc_err / 1000)
        log_reg_err.append(cum_lr_err / 1000)

    return nbc_err, log_reg_err

nbc_err, lr_err = compare_error()

plt.xlabel('Percentage of training data used')
plt.ylabel('Error')
plt.title('Comparison of mean NBC and LR classification error')
plt.xticks(list(range(10)), list(range(10, 110, 10)))
plt.plot(nbc_err, c='red', label='NBC Error')
plt.plot(lr_err, c='blue', label='LR Error')
plt.legend()

plt.show()
