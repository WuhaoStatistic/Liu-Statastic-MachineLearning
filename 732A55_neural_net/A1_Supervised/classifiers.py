import numpy as np
from scipy import stats


def kNN(X, k, XTrain, LTrain):
    LPred = np.zeros((X.shape[0]))
    for j in range(X.shape[0]):
        distance = list(((XTrain - X[j,:])**2).sum(axis=1))
        # information about k-nerest points
        k_nerest_index = sorted(range(len(distance)), key=lambda ind: distance[ind])[0:k]
        k_nerest_distance = sorted(distance)[0:k]
        k_nerest_type = [list(LTrain)[p] for p in k_nerest_index]
        
        classes = np.unique(LTrain)
        NClasses = classes.shape[0]
        # count each label, this list has the same order as classes
        label_count = [k_nerest_type.count(i) for i in classes]
        
        # if there is only one top value
        if label_count.count(max(label_count)) == 1:
            LPred[j] = classes[sorted(range(len(label_count)), key=lambda ind: label_count[ind],reverse=True)[0]] 
        else:
            max_number = max(label_count)
            # the code below gets the labels which have the maximum number
            label_with_max_number = [classes[x] for x in range(len(label_count)) if label_count[x] == max_number]
            # strategy is calculate the sum of distance among these labels and pick the smallest one
            # if they are still same, then we pick the label which has the point with comparatively smaller distance.
            res = []
            min_label = []
            for i in label_with_max_number:
                sum_number = sum([k_nerest_distance[x] for x in range(len(k_nerest_distance)) if k_nerest_type[x] == i])
                res.append(sum_number)
                min_label.append(i)
            if res.count(min(res)) == 1:
                LPred[j] = classes[sorted(range(len(res)), key=lambda ind: res[ind])[0]] 
            else:
                label_with_min_distance = [min_label[x] for x in range(len(min_label)) if res[x] == min(res)]
                # by divide 0-1 interval into len(min_label) th blocks, we can use random.rand to randomly pick one
                xi =int(np.floor(np.random.rand(1)/(1/len(label_with_min_distance))))
                LPred[j] = label_with_min_distance[xi]
    return LPred

# each row is a sample with bias (x1,x2...,bias)
def runSingleLayer(X, W):
    """ RUNSINGLELAYER
    Performs one forward pass of the single layer network, i.e
    it takes the input data and calculates the output for each sample.

    Inputs:
            X - Samples to be classified (matrix)
            W - Weights of the neurons (matrix)

    Output:
            Y - Output for each sample and class (matrix)
            L - The resulting label of each sample (vector)
    """

    # Add your own code here
    matX = np.mat(X)
    matW = np.mat(W)
    Y = matX*matW
    Y2 = np.tanh(matX*matW)
    # Calculate labels
    L = np.argmax(Y2, axis=1) + 1
    return Y, L


def trainSingleLayer(XTrain, DTrain, XTest, DTest, W0, numIterations, learningRate):
    """ TRAINSINGLELAYER
    Trains the single-layer network (Learning)
    
    Inputs:
            X* - Training/test samples (matrix)
            D* - Training/test desired output of net (matrix)  probability (0.99,-0.99,-0.99) means class 1
            W0 - Initial weights of the neurons (matrix)
            numIterations - Number of learning steps (scalar)
            learningRate  - The learning rate (scalar)
    Output:
            Wout - Weights after training (matrix)
            ErrTrain - The training error for each iteration (vector)
            ErrTest  - The test error for each iteration (vector)
    """

    # Initialize variables
    ErrTrain = np.zeros(numIterations+1)
    ErrTest  = np.zeros(numIterations+1)
    NTrain = XTrain.shape[0]
    NTest  = XTest.shape[0]
    Wout = W0
    DTrain = np.mat(DTrain)
    DTest = np.mat(DTest)
    # Calculate initial error
    YTrain, _ = runSingleLayer(XTrain, Wout)
    YTest, _  = runSingleLayer(XTest , Wout)
    ErrTrain[0] = np.multiply(np.tanh(YTrain) - DTrain,np.tanh(YTrain) - DTrain).sum() / NTrain
    ErrTest[0]  = np.multiply(np.tanh(YTest)  - DTest,np.tanh(YTest) - DTest).sum() / NTest

    for n in range(numIterations):
        YTrain, _ = runSingleLayer(XTrain, Wout)
        grad_w = 2/NTrain*XTrain.T*np.multiply(np.tanh(YTrain)-DTrain,tanhprim(YTrain))
        
     
        # Take a learning step
        Wout = Wout - learningRate * grad_w
        # Evaluate errors
        YTrain2, _ = runSingleLayer(XTrain, Wout)
        YTest, _  = runSingleLayer(XTest , Wout)
        ErrTrain[n+1] = np.multiply(np.tanh(YTrain2) - DTrain,np.tanh(YTrain2) - DTrain).sum() / NTrain
        ErrTest[n+1]  = np.multiply(np.tanh(YTest)  - DTest,np.tanh(YTest) - DTest).sum() / NTest

    return Wout, ErrTrain, ErrTest


def runMultiLayer(X, W, V):
    """ RUNMULTILAYER
    Calculates output and labels of the net
    
    Inputs:
            X - Data samples to be classified (matrix)
            W - Weights of the hidden neurons (matrix)
            V - Weights of the output neurons (matrix)

    Output:
            Y - Output for each sample and class (matrix)
            L - The resulting label of each sample (vector)
            H - Activation of hidden neurons (vector)
    """

    # Add your own code here
    S = np.mat(X)*np.mat(W)  # Calculate the weighted sum of input signals (hidden neuron)
    H = np.tanh(S)  # Calculate the activation of the hidden neurons (use hyperbolic tangent)
    bias = np.ones(shape=(1, H.shape[0]))
    H = np.concatenate((H, bias.T), axis=1)
    Y = np.dot(H,np.mat(V))
    Y2 = np.tanh(np.dot(H,np.mat(V))) # Calculate the weighted sum of the hidden neurons

    # Calculate labels
    L = Y2.argmax(axis=1) + 1

    return np.mat(Y), L, np.mat(H)

def tanhprim(x):
    """ TANHPRIM
    Derivative of tanh
    """

    return 1 - np.multiply(np.tanh(x),np.tanh(x))

def trainMultiLayer(XTrain, DTrain, XTest, DTest, W0, V0, numIterations, learningRate):
    """ TRAINMULTILAYER
    Trains the multi-layer network (Learning)
    
    Inputs:
            X* - Training/test samples (matrix)
            D* - Training/test desired output of net (matrix)
            V0 - Initial weights of the output neurons (matrix)
            W0 - Initial weights of the hidden neurons (matrix)
            numIterations - Number of learning steps (scalar)
            learningRate  - The learning rate (scalar)

    Output:
            Wout - Weights after training (matrix)
            Vout - Weights after training (matrix)
            ErrTrain - The training error for each iteration (vector)
            ErrTest  - The test error for each iteration (vector)
    """

    # Initialize variables
    ErrTrain = np.zeros(numIterations+1)
    ErrTest  = np.zeros(numIterations+1)
    NTrain = XTrain.shape[0]
    NTest  = XTest.shape[0]
    NClasses = DTrain.shape[1]
    Wout = W0
    Vout = V0

    # Calculate initial error
    # YTrain = runMultiLayer(XTrain, W0, V0)
    YTrain, _, HTrain = runMultiLayer(XTrain, Wout, Vout)
    YTest, _, _  = runMultiLayer(XTest , W0, V0)
    ErrTrain[0] = np.multiply(np.tanh(YTrain) - DTrain,np.tanh(YTrain) - DTrain).sum() / (NTrain * NClasses)
    ErrTest[0]  = np.multiply(np.tanh(YTest) - DTest,np.tanh(YTest) - DTest).sum() / (NTest * NClasses)
    
    for n in range(numIterations):

        if not n % 1000:
            print(f'n : {n:d}')

        # Add your own code here
        YTrain, _, HTrain = runMultiLayer(XTrain, Wout, Vout)
        
        grad_v = 2*np.dot(HTrain.T,np.multiply(np.tanh(YTrain)-DTrain,tanhprim(YTrain))) /(NTrain)
        # the last column of H is from bias(hiden layer),it shouldn't have any impact on weights 'W'
        
        grad_w = 2*XTrain.T*np.multiply(np.multiply(np.tanh(YTrain)-DTrain,tanhprim(YTrain))*Vout.T[:,:-1],tanhprim(np.dot(XTrain,Wout))) /(NTrain)
        
        #grad_w = 2*XTrain.T*np.multiply(np.multiply(np.tanh(YTrain)-DTrain,tanhprim(HTrain[:,:-1]*Vout[:-1,:]))*Vout.T[:,:-1],tanhprim(np.dot(XTrain,Wout)))/(NTrain)
        
        # Take a learning step
        Vout = Vout - learningRate * grad_v
        Wout = Wout - learningRate * grad_w

        # Evaluate errors
        #YTrain = runMultiLayer(XTrain, Wout, Vout);
        YTrain2, _, HTrain = runMultiLayer(XTrain, Wout, Vout)
        YTest, _, _  = runMultiLayer(XTest , Wout, Vout)
        ErrTrain[1+n] = np.multiply(np.tanh(YTrain2) - DTrain,np.tanh(YTrain2) - DTrain).sum() / (NTrain * NClasses)
        ErrTest[1+n]  = np.multiply(np.tanh(YTest) - DTest,np.tanh(YTest) - DTest).sum() / (NTest * NClasses)

    return Wout, Vout, ErrTrain, ErrTest

