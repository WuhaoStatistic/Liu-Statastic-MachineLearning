import os

import numpy as np
from matplotlib import pyplot as plt
from scipy import io as sio

from classifiers import kNN, runSingleLayer, runMultiLayer


def calcConfusionMatrix(LPred, LTrue):
    classes = np.unique(LTrue)
    NClasses = classes.shape[0]
    classes = list(classes)
    cM = np.zeros((NClasses, NClasses))
    for i in range(len(LTrue)):
        real_class = classes.index(LTrue[i])
        pre_class = classes.index(LPred[i])
        cM[real_class, pre_class] += 1
    return cM


def calcAccuracy(cM):
    n = cM.shape[0]
    correct = 0
    for i in range(n):
        correct += cM[i, i]
    acc = correct / sum(sum(cM))
    return acc


def tanhprim(x):
    """ TANHPRIM
    Derivative of tanh
    """

    return 1 - np.tanh(x)**2


def loadDataSet(dataSetNr):
    """ LOADDATASET
    Loads data samples (X) , training targets (D) and labels (L)
    Samples are in the 1st dimension (rows), and features in the
    2nd dimension. This convention must be consistent throuhout the
    assignment, otherwise the plot code will break.
    """

    if dataSetNr == 1:
        data = sio.loadmat('lab_data.mat')

        X = np.concatenate((data['X1'].T, data['Xt1'].T),axis=0)
        D = np.concatenate((data['D1'].T, data['Dt1'].T),axis=0)
        L = np.concatenate((data['L1'], data['Lt1']),axis=0).squeeze()

    elif dataSetNr == 2:
        data = sio.loadmat('lab_data.mat')

        X = np.concatenate((data['X2'].T, data['Xt2'].T),axis=0)
        D = np.concatenate((data['D2'].T, data['Dt2'].T),axis=0)
        L = np.concatenate((data['L2'], data['Lt2']),axis=0).squeeze()

    elif dataSetNr == 3:
        data = sio.loadmat('lab_data.mat')

        X = np.concatenate((data['X3'].T, data['Xt3'].T),axis=0)
        D = np.concatenate((data['D3'].T, data['Dt3'].T),axis=0)
        L = np.concatenate((data['L3'], data['Lt3']),axis=0).squeeze()

    elif dataSetNr == 4:
        data = sio.loadmat('lab_data_digits.mat')

        X = np.concatenate((data['X'].T, data['Xt'].T),axis=0)
        D = np.concatenate((data['D'].T, data['Dt'].T),axis=0)
        L = np.concatenate((data['L'], data['Lt']),axis=0).squeeze() + 1

    else:
        raise ValueError('Invalid dataset number')

    return X.astype(float), D.astype(float), L.astype(int)


def plotCase(X,L):
    """ PLOTCASE
    Simple plot of the dataset. Can only be used with dataset 1, 2, and 3.
    """

    # TODO: match the style of original code
    plt.figure()

    for k in range(1,4):
        ind = (L == k).squeeze()
        if k == 1:
            plt.plot(X[ind,0], X[ind,1], 'r.')
        elif k == 2:
            plt.plot(X[ind,0], X[ind,1], 'g.')
        elif k == 3:
            plt.plot(X[ind,0], X[ind,1], 'b.')
    plt.gca().invert_yaxis()
    plt.show()


def selectTrainingSamples(X, D, L, NSamplesPerLabelPerBin, NBins, selectAtRandom):
    """ SELECTTRAININGSAMPLES
    Split data into separate equal-sized bins.
    Input:
            X - Data samples
            D - Training targets for all samples
            L - Data labels for all samples
            NSamplesPerLabelPerBin - Number of samples from each label in
                                     each bin, can be set to "inf" to use
                                     as much of the data as possible
            NBins          - Number of bins
            selectAtRandom - True/False to randomize the selections

    Output:
            XBins - Output bins with data from X (cell array)
            DBins - Output bins with targets from D (cell array)
            LBins - Output bins with labels from L (cell array)
    """

    labels, counts = np.unique(L, return_counts=True)
    NLabels = labels.shape[0]

    if NSamplesPerLabelPerBin == np.inf:
        NSamplesPerLabelPerBin = counts.min() // NBins

    # Get class labels
    labelInds = {}
    for n in range(NLabels):
        labelInds[labels[n]] = np.flatnonzero(L == labels[n])
        if selectAtRandom:
            np.random.shuffle(labelInds[labels[n]])

    XBins = []
    DBins = []
    LBins = []
    for m in range(NBins):
        sampleInds = np.concatenate([a[m*NSamplesPerLabelPerBin:(m+1)*NSamplesPerLabelPerBin] for a in labelInds.values()], axis=0)

        XBins.append(X[sampleInds])
        DBins.append(D[sampleInds])
        LBins.append(L[sampleInds])

    return XBins, DBins, LBins


def plotData(X, L, LPred):
    """ PLOTDATA
    Plot dataset 1, 2, or 3. Indicates correct and incorrect label
    predictions as green and red respectively.
    """

    c='xo+*sd'

    for k in range(1,7):
        ind = (L == k) & (L == LPred)
        ind_err = (L == k) & (L != LPred)
        plt.plot(X[ind,0], X[ind,1], 'g'+c[k-1])
        plt.plot(X[ind_err,0], X[ind_err,1], 'r'+c[k-1])


def plotResultDots(XTrain, LTrain, LPredTrain, XTest, LTest, LPredTest, type, Ws, k):
    """ PLOTRESULTDOTS
    Used to plot training and test data for dataset 1, 2, or 3.
    Indicates corect and incorrect label predictions, and plots the
    prediction fields as the background color.
    If called from kNN script, k should be provided and Ws is ignored.
    If called from single-layer script, Ws should be a {W} cell array, and k is ignored.
    If called from multi-layer script, Ws should be a {W,V} cell array, and k is ignored.
    """

    # Create background meshgrid for plotting label fields
    # Change nx and ny to set the resolution of the fields
    nx = 150
    ny = 150

    xMin = np.min((XTrain[:,0].min(), XTest[:,0].min())) - 1
    xMax = np.max((XTrain[:,0].max(), XTest[:,0].max())) + 1
    yMin = np.min((XTrain[:,1].min(), XTest[:,1].min())) - 1
    yMax = np.max((XTrain[:,1].max(), XTest[:,1].max())) + 1

    xi = np.linspace(xMin, xMax, nx)
    yi = np.linspace(yMin, yMax, ny)

    XI, YI = np.meshgrid(xi, yi)

    # Setup data depending on classifier type
    if type == 'single':
        XGrid = np.column_stack((XI.flatten(), YI.flatten(), np.ones(XI.size)))
        _, LGrid = runSingleLayer(XGrid, Ws)
        LGrid = LGrid.reshape((nx,ny))
        XTrain = XTrain[:,:-1]
        XTest  = XTest[:,:-1]
    elif type == 'multi':
        XGrid = np.column_stack((XI.flatten(), YI.flatten(), np.ones(XI.size)))
        _, LGrid, _ = runMultiLayer(XGrid, Ws[0], Ws[1])
        LGrid = LGrid.reshape((nx,ny))
        XTrain = XTrain[:,:-1]
        XTest  = XTest[:,:-1]
    elif type == 'kNN':
        XGrid = np.column_stack((XI.flatten(), YI.flatten()))
        LGrid = kNN(XGrid, k, XTrain, LTrain).reshape((nx,ny))

    # Plot training data
    plt.figure()
    plt.imshow(LGrid, extent=(xMin, xMax, yMin, yMax), cmap='gray', aspect='auto', origin='lower')
    plotData(XTrain, LTrain, LPredTrain)
    plt.gca().invert_yaxis()
    plt.title('Training data results (green ok, red error)')

    # Plot test data
    plt.figure()
    plt.imshow(LGrid, extent=(xMin, xMax, yMin, yMax), cmap='gray', aspect='auto', origin='lower')
    plotData(XTest, LTest, LPredTest)
    plt.gca().invert_yaxis()
    plt.title('Test data results (green ok, red error)')

    plt.show()


def plotResultsOCR(X, L, LPred):
    """ PLOTRESULTSOCR
    Plots the results using the 4th dataset (OCR). Selects a
    random set of 16 samples each time.
    """

    # Remove bias (last feature)
    if X.shape[1] == 65:
        X = X[:,:-1]

    # Create random sort vector
    ord = np.random.permutation(X.shape[0])

    plt.figure()

    # Plot 16 samples
    for n in range(16):
        idx = ord[n]
        plt.subplot(4,4,n+1)
        plt.imshow(X[idx].reshape((8,8)), cmap='gray')
        plt.title(r'$L_{true}=$' + '{:d}'.format(list(L)[idx]-1) + ', $L_{pred}=$' + '{:d}'.format(list(LPred)[idx]-1))
        plt.axis('off')
    plt.show()
