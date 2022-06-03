# -*- coding: utf-8 -*-

import numpy as np
import matplotlib.pyplot as plt
import itertools
from sklearn.metrics import confusion_matrix

def PrintPrediction(P, prec=3):
    print('[{}]'.format(' '.join([ '{:{prec}}'.format('{:.{prec}f}'.format(j,prec=prec).rstrip('0'),prec=prec+2) for j in P ])) + ' --> ' + str(P.argmax()))

# ============================================================================

def PlotModelEval(Model, History, X, Y, Labels):
    
    # Scores for each class (can be interpreted as probabilities since we use softmax output)
    S = Model.predict(X)
    # Prediction (class number) for each test image
    P = np.expand_dims(np.argmax(S,axis=1), axis=-1)
    # Calculate confusion matrix
    CM = confusion_matrix(Y,P)
    
    # Plot training history
    plt.figure(figsize=(16,6))
    plt.subplot(2,2,1)
    plt.semilogy(History.history['loss'], label="Training")
    if 'val_loss' in History.history:
        plt.semilogy(History.history['val_loss'], label="Validation")
    plt.title('Model loss')
    plt.ylabel('Loss')
    plt.xlabel('Epoch')
    plt.legend(loc='upper right')
    plt.grid(True, which="both")
    plt.subplot(2,2,3)
    plt.plot(100 * np.array(History.history['accuracy']), label="Training")
    if 'val_accuracy' in History.history:
        plt.plot(100 * np.array(History.history['val_accuracy']), label="Validation")
    plt.title('Model accuracy')
    plt.ylabel('Acc [%]')
    plt.xlabel('Epoch')
    plt.legend(loc='lower right')
    plt.grid(True, which="both")
    
    # Plot confusion matrix
    plt.subplot(2,2,(2,4))
    PlotConfusionMatrix(CM, classes=Labels, title="Confusion matrix (test)")
    plt.show()

# ============================================================================

def PlotConfusionMatrix(cm, classes,
                        normalize=False,
                        title='Confusion matrix',
                        cmap=plt.cm.Blues,
                        showAcc=True):
    """
    This function prints and plots the confusion matrix.
    Normalization can be applied by setting `normalize=True`.
    """
    plt.imshow(cm, interpolation='nearest', cmap=cmap)
    plt.colorbar()
    tick_marks = np.arange(len(classes))
    plt.xticks(tick_marks, classes, rotation=45)
    plt.yticks(tick_marks, classes)

    if normalize:
        cm = cm.astype('float') / cm.sum(axis=1)[:, np.newaxis]
    
    #print(cm)

    thresh = cm.max() / 2.
    for i, j in itertools.product(range(cm.shape[0]), range(cm.shape[1])):
        plt.text(j, i, cm[i, j],
            horizontalalignment="center",
            verticalalignment="center",
            color="white" if cm[i, j] > thresh else "black")

    plt.tight_layout()
    plt.ylabel('True label')
    plt.xlabel('Predicted label')
    
    if showAcc:
        acc = 100*(np.trace(cm) / np.sum(cm))
        title = title + " | Acc=%.2f%%" % acc
        
    plt.title(title)

# ============================================================================

def PlotRandomFromEachClass(X,Y,N,labels):    
    C = np.unique(Y)
    M = len(C)
    plt.figure(figsize=(16, N*1.5))
    for i in range(M):
        mask = np.squeeze(Y == C[i])
        indexes = np.random.choice(X.shape[0], N, replace=False, p=mask/sum(mask))
        for j in range(N):
            plt.subplot(N,M,j*M+i+1)
            plt.imshow(X[indexes[j]], aspect="equal")
            plt.axis("off")
            if j == 0:
                plt.title(labels[i])
    #plt.tight_layout()
    plt.show()
    
# ============================================================================
    
def PlotImageConfusionMatrix(Model,X,Y,Labels):
    S = Model.predict(X)
    P = np.argmax(S,axis=1)
    C = np.unique(Y)
    N = len(C)
    fig = plt.figure(figsize=(15,15))
    fig.patch.set_facecolor('white')
    for i in range(N):
        for j in range(N):
            plt.subplot(N,N,i*N+j+1)
            mask = np.squeeze(np.logical_and(Y == C[i], P == C[j]))
            if (mask.sum() > 0):
                idx = np.random.choice(X.shape[0], 1, replace=False, p=mask/mask.sum())
                plt.imshow(X[idx[0]], aspect="equal")
                #plt.title("Index %i" % idx)
            plt.xticks([])
            plt.yticks([])
            if i == N-1:
                plt.xlabel(Labels[j])
            if j == 0:
                plt.ylabel(Labels[i])
    plt.show()