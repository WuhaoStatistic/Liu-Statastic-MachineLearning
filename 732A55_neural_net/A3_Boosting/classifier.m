import numpy as np
def WeakClassifier(T, P, X):
    """ WEAKCLASSIFIER
    Classify images using a decision stump.
    Takes a vector X of scalars obtained by applying one Haar feature to all
    training images. Classifies the examples using a decision stump with
    cut-off T and polarity P. Returns a vector C of classifications for all
    examples in X.

    You are not allowed to use a loop in this function.
    This is for your own benefit, since a loop will be too slow to use
    with a reasonable amount of Haar features and training images.
    """
    one = np.ones(X.shape) * P
    minus = np.ones(X.shape) * -P
    X = np.where(X>T,one,minus)
    C = X
    
    return C


def WeakClassifierError(C, D, Y):
    """ WEAKCLASSIFIERERROR
    Calculate the error of a single decision stump.
    Takes a vector C of classifications from a weak classifier, a vector D
    with weights for each example, and a vector Y with desired
    classifications. Calculates the weighted error of C, using the 0-1 cost
    function.

    You are not allowed to use a loop in this function.
    This is for your own benefit, since a loop will be too slow to use
    with a reasonable amount of Haar features and training images.
    """
    Loss = (C != Y)+1-1
    Loss = sum(Loss*D)
    E = Loss

    return E

def ErrorRate(C,Y):
    '''
    C is the result from WeakClassifier
    Y is the desire output
    
    This function is used to calculate error rate and also return the result based on -1(mismatched) and 1(matched)
    '''
    C = np.array(C)
    Y = np.array(Y)
    error_rate = sum((C!=Y)+1-1)/len(C)
    C[C!=Y] = -2
    C[C==Y] = 2
    res = C/2
    return error_rate, np.array(res) 