
import _pickle as cp
import numpy as np
import matplotlib.pyplot as plt
from sklearn.linear_model import Ridge
from sklearn.linear_model import Lasso
from sklearn.preprocessing import PolynomialFeatures
from sklearn.pipeline import make_pipeline
from sklearn.metrics import mean_squared_error

X, y = cp.load(open('winequality-white.pickle', 'rb'))

N, D = X.shape
N_train = int(0.8 * N)
N_test = N - N_train
X_train = X[:N_train]
y_train = y[:N_train]
X_test = X[N_train:]
y_test = y[N_train:]


unique_elements, counts = np.unique(y_train, return_counts=True)
element_counts = dict(zip(unique_elements, counts))

elements=[]
counts1=[]
for element, count1 in element_counts.items():
    elements.append(element)
    counts1.append(count1)
#plt.bar(elements, counts1)
#plt.xlabel('Quality')
#plt.ylabel('Number of wines')
#plt.title(' Bar chart of Wine Quality')
#plt.show()


y_mean_prediction =y_train- np.mean(y_train)
error_mean_prediction= np.mean(np.square(y_mean_prediction))

print("Mean estimate error:",error_mean_prediction)

def normalise(X_train,y_train,X_test,y_test):
    Xmean_axis0 = np.mean(X_train, axis=0)
    Xstd_axis0 = np.std(X_train, axis=0)

    Ymean = np.mean(y_train)
    Ystd = np.std(y_train)

    Xnormalised= (X_train-Xmean_axis0)/Xstd_axis0
    Ynormalised= (y_train-Ymean)/Ystd

    X_test_normalised=(X_test-Xmean_axis0)/Xstd_axis0
    y_test_normalised=(y_test-Ymean)/Ystd

    return (Xnormalised,Ynormalised,X_test_normalised,y_test_normalised)

def trainLR(X_train,y_train,X_test,y_test):
    Xnormalised, Ynormalised, X_test_normalised, y_test_normalised= normalise(X_train,y_train,X_test,y_test)
    N, D = X_train.shape
    X0 = np.ones((N,1))
    Xnew = np.hstack((Xnormalised,X0))
    N1, D1= X_test.shape
    X01= np.ones((N1,1))
    Xnewtest = np.hstack((X_test_normalised,X01))

    """
    print(np.mean(Xnormalised, axis=0))
    print(np.std(Xnormalised, axis=0))
    
    print(np.mean(Ynormalised))
    print(np.std(Ynormalised))
    
    print(np.mean(X_test_normalised, axis=0))
    print(np.std(X_test_normalised, axis=0))
    
    print(np.mean(y_test_normalised))
    print(np.std(y_test_normalised))
    """

    prediction_weights=  np.linalg.inv(np.transpose(Xnew) @ Xnew) @ np.transpose(Xnew) @ y_train

    predicted_test_y=  Xnewtest @ prediction_weights

    predicted_train_y= Xnew @ prediction_weights

    Test_error_LR_prediction= mean_squared_error( y_test, predicted_test_y)
    Train_error_LR_prediction= mean_squared_error( y_train , predicted_train_y)

    return (Test_error_LR_prediction,Train_error_LR_prediction)

    #print("Linear Riggession test error: ",Test_error_LR_prediction)

a=trainLR(X_train,y_train,X_test,y_test)
print("Linear Regression test error: ",a[0])
print("Linear Regression training error: ",a[1])

"""
Mean estimate error: 0.7767772386501117
Linear Regression test error:  0.5607292042283468
Linear Regression training error:  0.5639996173941925
"""

numberOfDatas=[]
testErrors=[]
trainErrors=[]
for n in range(20,601,20):
    X_partial_train = X[:n]
    y_partial_train = y[:n]
    a=trainLR(X_partial_train, y_partial_train,X_test,y_test)
    testErrors.append(a[0])
    trainErrors.append(a[1])
    numberOfDatas.append(n)

plt.plot(numberOfDatas, testErrors, label='Test Error', color='blue')
plt.plot(numberOfDatas, trainErrors, label='Training Error', color='red')
plt.xlabel('Number of Data')
plt.ylabel('Error')
plt.legend()
plt.show()

# We underfitting

def normaliseAnd1s(X_train, y_train, X_test, y_test):
    Xnormalised, Ynormalised, X_test_normalised, y_test_normalised = normalise(X_train, y_train, X_test, y_test)

    N, D = X_train.shape
    X0 = np.ones((N,1))
    Xnew = np.hstack((Xnormalised,X0))

    N1, D1= X_test.shape
    X01= np.ones((N1,1))
    Xnewtest = np.hstack((X_test_normalised,X01))

    return Xnew,Ynormalised,Xnewtest, y_test_normalised


def train_Ridge(X_train,y_train,X_test,y_test,delta):
    Xnew, Ynormalised, Xnewtest, y_test_normalised = normaliseAnd1s(X_train, y_train, X_test, y_test)

    poly = PolynomialFeatures(degree=2)
    ridge_model = Ridge(alpha=delta)
    model = make_pipeline(poly, ridge_model)
    model.fit(Xnew, Ynormalised)
    return model

K= int(len(X_train)*0.8)
X_training_64= X_train[:K]
y_training_64=y_train[:K]
X_validation=X_train[K:]
y_validation=y_train[K:]

error_Ridge=[]
for i in range(-2,3):
    model= train_Ridge(X_training_64,y_training_64,X_validation,y_validation,10**(i))
    Xnew, Ynormalised, Xnewvalid, y_valid_normalised = normaliseAnd1s(X_training_64, y_training_64, X_validation, y_validation)

    y_pred=model.predict(Xnewvalid)
    mse=mean_squared_error(y_valid_normalised,y_pred)
    error_Ridge.append(mse)
print(error_Ridge)

# Delta=10 was the best

def train_Lasso(X_train,y_train,X_test,y_test,delta):
    Xnew, Ynormalised, Xnewtest, y_test_normalised = normaliseAnd1s(X_train, y_train, X_test, y_test)
    poly = PolynomialFeatures(degree=2)
    lasso_model = Lasso(alpha=delta)
    model = make_pipeline(poly, lasso_model)
    model.fit(Xnew, Ynormalised)
    return model


error_Lasso=[]
for i in range(-2,3):
    model= train_Lasso(X_training_64,y_training_64,X_test,y_test,10**(i))
    Xnormalised, Ynormalised, X_valid_normalised, y_valid_normalised = normaliseAnd1s(X_training_64, y_training_64, X_validation, y_validation)


    y_pred=model.predict(X_valid_normalised)
    mse=mean_squared_error(y_valid_normalised,y_pred)
    error_Lasso.append(mse)
print(error_Lasso)

# Delta 0.01 was the best

model= train_Ridge(X_training_64,y_training_64,X_test,y_test,1)
Xnormalised, Ynormalised, X_test_normalised, y_test_normalised= normaliseAnd1s(X_training_64, y_training_64, X_validation, y_validation)


y_pred=model.predict(X_test_normalised)
mse=mean_squared_error(y_test_normalised,y_pred)
print("Ridge regression test error with delta=10 :",mse)

y_pred=model.predict(Xnormalised)
mse=mean_squared_error(Ynormalised,y_pred)
print("Ridge regression training error with delta=10 :",mse)




model= train_Lasso(X_train,y_train,X_test,y_test,0.01)
Xnormalised, Ynormalised, X_test_normalised, y_test_normalised= normaliseAnd1s(X_train,y_train,X_test,y_test)

y_pred=model.predict(X_test_normalised)
mse=mean_squared_error(y_test_normalised,y_pred)
print("Lasso regression test error with delta=0.01 :",mse)

y_pred=model.predict(Xnormalised)
mse=mean_squared_error(Ynormalised,y_pred)
print("Lasso regression training error with delta=0.01 :",mse)

"""
Ridge regression test error with delta=10 : 0.6829689981750994
Ridge regression training error with delta=10 : 0.6347385539714845
Lasso regression test error with delta=0.01 : 0.6653414060212886
Lasso regression training error with delta=0.01 : 0.65909511948902
"""

"""
def train_Ridge(X_train,y_train,X_test,y_test,delta):
    Xnormalised, Ynormalised, X_test_normalised, y_test_normalised= normalise(X_train, y_train, X_test, y_test)

    prediction_weights =  np.linalg.inv(np.transpose(Xnormalised) @ Xnormalised + delta* np.identity(D)) @ np.transpose(Xnormalised) @ Ynormalised
    predicted_test_y=  X_test_normalised @ prediction_weights

    predicted_train_y= Xnormalised @ prediction_weights

    Test_error_LR_prediction= np.mean(np.square(( y_test_normalised - predicted_test_y)))
    Train_error_LR_prediction=np.mean(np.square(( Ynormalised - predicted_train_y)))

    return (Test_error_LR_prediction,Train_error_LR_prediction)

def train_Lasso(X_train,y_train,X_test,y_test,delta):
    Xnormalised, Ynormalised, X_test_normalised, y_test_normalised= normalise(X_train, y_train, X_test, y_test)

    prediction_weights =  np.linalg.inv(np.transpose(Xnormalised) @ Xnormalised + delta* np.identity(D)) @ np.transpose(Xnormalised) @ Ynormalised
    predicted_test_y=  X_test_normalised @ prediction_weights

    predicted_train_y= Xnormalised @ prediction_weights

    Test_error_LR_prediction= np.mean(np.square(( y_test_normalised - predicted_test_y)))
    Train_error_LR_prediction=np.mean(np.square(( Ynormalised - predicted_train_y)))

    return (Test_error_LR_prediction,Train_error_LR_prediction)
"""
