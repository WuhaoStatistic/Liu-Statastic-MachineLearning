
import numpy as np
from matplotlib import pyplot as plt

# Q shape : 4 * 10 * 15 
# return best action in each point
def getpolicy(Q):
    """ GWGETPOLICY
    Get best policy matrix from the Q-matrix.
    You have to implement this function yourself. It is not necessary to loop
    in order to do this, and looping will be much slower than using matrix
    operations. It's possible to implement this in one line of code.
    """
    # return P
    return np.argmax(Q,axis = 0)+1


# return best value in each point
def getvalue(Q):
    """ GWGETVALUE
    Get best value matrix from the Q-matrix.
    You have to implement this function yourself. It is not necessary to loop
    in order to do this, and looping will be much slower than using matrix
    operations. It's possible to implement this in one line of code.
    """
    #return V
    return np.max(Q,axis = 0)

def plotarrows(P):
    """ PLOTARROWS
    Displays a policy matrix as an arrow in each state.
    """

    x,y = np.meshgrid(np.arange(P.shape[1]), np.arange(P.shape[0]))

    u = np.zeros(x.shape)
    v = np.zeros(y.shape)

    v[P==3] = 1
    v[P==4] = -1
    u[P==1] = -1
    u[P==2] = 1

    plt.quiver(v,u,color='r')

def heatmap_function(data1,title='title here'):
    '''
    data1 is the V-funtion we would like to plot.
    title is the title of this picture.
    '''
    fig, ax = plt.subplots()
    im = ax.imshow(data1)
    # Rotate the tick labels and set their alignment.
    plt.setp(ax.get_xticklabels(), rotation=45, ha="right",
             rotation_mode="anchor")
    ax.set_title(title)
    cbar = ax.figure.colorbar(im, ax=ax, )
    cbar.ax.set_ylabel('', rotation=-90, va="bottom")
    fig.tight_layout()
    plt.show()



    
    
    