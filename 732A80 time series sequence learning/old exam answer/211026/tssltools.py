import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm
from sklearn import linear_model as lm  # Used for solving linear regression problems


   
###### HELPER FUNCTIONS FOR PROBLEM 3 ##################################################################


def logwgtfun_wrong(particles, y, population):
	"""Function for propagating the particles

	:param particles: (N,) array of particles.
	:param y: the current observation

	:return logweights: (N,) array of log-weights.
	"""
	return -1/2 * (y/population - particles)**2


def propagate_wrong(particles, a, sigma):
	"""Function for propagating the particles

	:param particles: (N,) array of particles.
	:parma sigma_eps: value of the parameter sigma_eps

	:return new_particles: (N,) array of particles.
	"""
	
	return np.random.uniform(size = particles.size) 



###### FROM LAB 1 ##################################################################

def acf(x, lags=None):
    """ Computes the empirical autocorralation function.

    :param x: array (n,), sequence of data points
    :param lags: int, maximum lag to compute the ACF for. If None, this is set to n-1. Default is None.
    :return gamma: array (lags,), values of the ACF at lags 0 to lags
    """

    gamma = np.correlate(x, x, mode='full')  # Size here is always 2*len(x)-1
    gamma = gamma[int((gamma.size - 1) / 2):]  # Keep only second half
    if lags is not None and lags < len(gamma):
        gamma = gamma[0:lags + 1]
    return gamma / gamma[0]

    
def acfplot(x, lags=None, conf=0.95):
    """Plots the empirical autocorralation function.

    :param x: array (n,), sequence of data points
    :param lags: int, maximum lag to compute the ACF for. If None, this is set to n-1. Default is None.
    :param conf: float, number in the interval [0,1] which specifies the confidence level (based on a central limit
                 theorem under a white noise assumption) for two dashed lines drawn in the plot. Default is 0.95.
    :return:
    """

    n = len(x)
    y = acf(x, lags)
    lags = len(y)
    
    lag_vec = np.arange(lags)
    
    c = norm.isf((1-conf)/2,loc=0,scale=1/np.sqrt(n)) # Use inverse survival function (=1-cdf) at half the confidence interval
    plt.plot(lag_vec,c*np.ones(lags),'k--',linewidth=1, label=f"{100*conf}% confidence")
    plt.plot(lag_vec,-c*np.ones(lags),'k--',linewidth=1)
    
    plt.stem(lag_vec, y, linefmt='-', markerfmt=' ', basefmt="k ", use_line_collection=True, label="Empirical ACF")
    plt.plot(lag_vec, 0*lag_vec, 'k-')
    plt.title(f"Empirical ACF")
    plt.legend() 


def fit_ar(y, p):
    """Fits an AR(p) model. The loss function is the sum of squared errors from t=p+1 to t=n.

    :param y: array (n,), training data points
    :param p: int, AR model order
    :return theta: array (p,), learnt AR coefficients
    """

    # Number of training data points
    n = len(y)
    
    # Construct the regression matrix
    Phi = np.zeros((n-p,p))
    for j in range(p):
        Phi[:,j] = y[p-j-1:n-j-1]
    
    # Drop the first p values from the target vector y
    yy = y[p:]

    # Here we use fit_intercept=False since we do not want to include an intercept term in the AR model
    regr = lm.LinearRegression(fit_intercept=False)
    regr.fit(Phi,yy)
    
    return regr.coef_
    
    
def predict_ar_1step(theta, y_target):
    """Predicts the value y_t for t = p+1, ..., n, for an AR(p) model, based on the data in y_target using
    one-step-ahead prediction.

    :param theta: array (p,), AR coefficients, theta=(a1,a2,...,ap).
    :param y_target: array (n,), the data points used to compute the predictions.
    :return y_pred: array (n-p,), the one-step predictions (\hat y_{p+1}, ...., \hat y_n) 
    """

    n = len(y_target)
    p = len(theta)
    
    # Number of steps in prediction
    m = n-p
    y_pred = np.zeros(m)
    
    for i in range(m):
        t = i+p
        phi = np.flip(y_target[t-p:t]) # (y_{t-1}, ..., y_{t-p})^T
        y_pred[i] = np.sum( phi * theta )
        
    return y_pred

###### FROM LAB 2 ##################################################################

class LGSS:
    """Linear Gaussian State Space model. The observation is assumed to be one-dimensional."""

    def __init__(self, T, R, Q, Z, H, a1, P1):
        self.d = T.shape[0]  # State dimension
        self.deta = R.shape[1]  # Second dimension is process noise dim
        self.T = T  # Process model
        self.R = R  # Process noise prefactor
        self.Q = Q  # Process noise covariance
        self.Z = Z  # Measurement model
        self.H = H  # Measurement noise variance
        self.a1 = a1  # Initial state mean
        self.P1 = P1  # Initial state covariance

    def get_params(self):
        """Return all model parameters.

        T, R, Q, Z, H, a1, P1 = model.get_params()
        """
        return self.T, self.R, self.Q, self.Z, self.H, self.a1, self.P1


class kfs_res:
    """Container class to store result of Kalman filter and smoother."""

    def __init__(self, alpha_pred, P_pred, alpha_filt, P_filt, y_pred, F_pred):
        """Initialize with KF results"""
        self.alpha_pred = alpha_pred
        self.P_pred = P_pred
        self.alpha_filt = alpha_filt
        self.P_filt = P_filt
        self.y_pred = y_pred
        self.F_pred = F_pred

    def set_ks_res(self, alpha_sm, V, eps_hat, eps_var, eta_hat, eta_cov):
        """Update to contain also KS results"""
        self.alpha_sm = alpha_sm
        self.V = V
        self.eps_hat = eps_hat
        self.eps_var = eps_var
        self.eta_hat = eta_hat
        self.eta_cov = eta_cov


def kalman_smoother(y, model: LGSS, kf: kfs_res):
    """Kalman (state and disturbance) smoother for LGSS model with one-dimensional observation.

    :param y: (n,) array of observations. May contain nan, which encodes missing observations.
    :param model: LGSS object with the model specification.
    :parma kf: kfs_res object with result from a Kalman filter foward pass.

    :return kfs_res: Container class. The original Kalman filter result is augmented with the following member variables,
        alpha_sm: (d,1,n) array of smoothed state means.
        V: (d,d,n) array of smoothed state covariances.
        eps_hat: (n,) array of smoothed means of observation disturbances.
        eps_var: (n,) array of smoothed variances of observation disturbances.
        eta_hat: (deta,1,n) array of smoothed means of state disturbances.
        eta_cov: (deta,deta,n) array of smoothed covariances of state disturbances.
    """
    d = model.d  # State dimension
    deta = model.deta  # Number of state noise components
    n = len(y)

    # Allocate memory, see DK (4.44)
    r = np.zeros((d, 1, n))
    N = np.zeros((d, d, n))
    alpha_sm = np.zeros((d, 1, n))
    V = np.zeros((d, d, n))

    # Disturbances
    eps_hat = np.zeros(n)  # Observation noise
    eps_var = np.zeros(n)
    eta_hat = np.zeros((deta, 1, n))  # State noise
    eta_cov = np.zeros((deta, deta, n))  # State noise covariance

    # Get all model parameters (for brevity)
    T, R, Q, Z, H, a1, P1 = model.get_params()

    # Get the innovations and their variances from forward pass
    v = y.flatten() - kf.y_pred
    F = kf.F_pred.flatten().copy()

    # Simple way of handling missing observations; treat them as present but with infinite variance!
    ind = np.isnan(v)
    v[ind] = 0.
    F[ind] = np.inf

    # Compute the "L-matrices", DKp87
    L = np.zeros((d, d, n))
    K = np.zeros((d, 1, n))
    for t in range(n):
        K[:, :, t] = kf.P_pred[:, :, t] @ Z.T / F[t]  # Kalman gain (without the leading T that DK use)
        L[:, :, t] = T @ (np.identity(d) - K[:, :, t] @ Z)

    # Initialize. r and N are defined for t=0,...,n-1 in DK,  whereas other quantities are defined for t=1,...,n.
    # Hence, alpha_sm[:,t-1] = \hat alpha_{t} but r[t-1] = r_{t-1}.
    r[:, :, -1] = (Z.T / F[-1]) * v[-1]
    N[:, :, -1] = (Z.T / F[-1]) @ Z
    # This is actually an unnecessary computation, since we simply compute the filter estimates again
    # (= to smoother at time step t=n), but we keep them to agree with the algorithm in DK
    alpha_sm[:, :, -1] = kf.alpha_pred[:, :, -1] + kf.P_pred[:, :, -1] @ r[:, :, -1]
    V[:, :, -1] = kf.P_pred[:, :, -1] - kf.P_pred[:, :, -1] @ N[:, :, -1] @ kf.P_pred[:, :, -1]

    # Disturbances
    eps_hat[-1] = (H / F[-1]) * v[-1]
    eps_var[-1] = H - (H / F[-1]) * H
    eta_cov[:, :, -1] = Q

    for t in np.flip(range(n - 1)):
        # State smoothing
        r[:, :, t] = (Z.T / F[t]) * v[t] + L[:, :, t].T @ r[:, :, t + 1]
        N[:, :, t] = (Z.T / F[t]) @ Z + L[:, :, t].T @ N[:, :, t + 1] @ L[:, :, t]
        alpha_sm[:, :, t] = kf.alpha_pred[:, :, t] + kf.P_pred[:, :, t] @ r[:, :, t]
        V[:, :, t] = kf.P_pred[:, :, t] - kf.P_pred[:, :, t] @ N[:, :, t] @ kf.P_pred[:, :, t]

        # Disturbance smoothing
        eps_hat[t] = H * (v[t] / F[t] - K[:, :, t].T @ T.T @ r[:, :, t + 1])
        eps_var[t] = H - (H / F[t]) * H - H * K[:, :, t].T @ T.T @ N[:, :, t + 1] @ T @ K[:, :, t] * H
        eta_hat[:, :, t] = Q @ R.T @ r[:, :, t + 1]
        eta_cov[:, :, t] = Q - Q @ R.T @ N[:, :, t + 1] @ R @ Q

    kf.set_ks_res(alpha_sm, V, eps_hat, eps_var, eta_hat, eta_cov)

    return kf



###### FROM LAB 3 ##################################################################

from scipy.stats import binom

class smc_res:
    """Container class to store result of Sequential Monte Carlo filter."""

    def __init__(self, alpha_filt, particles=None, W=None, ancestor_indices=None, logW=None, N_eff = None, logZ = None):
        self.alpha_filt = alpha_filt  # Filtered state estimates
        self.particles = particles  # All particles
        self.ancestor_indices = ancestor_indices  # All ancestor indices
        self.logW = logW  # logarithm of unnormalized importance weights
        self.W = W  # Normalized importance weights
        self.N_eff = N_eff  # Effective number of samples
        self.logZ = logZ  # logarithm of marginal likelihood estimate
        

def binomial_rng(n, p, size=None):
    """
    Draw samples from a binomial distribution using np.random.binomial, but
    with an explicit type cast from float64 to int32 (unsafe).

    Parameters
    ----------
    n : int or array_like of ints
        Parameter of the distribution, >= 0. Floats are also accepted,
        but they will be truncated to integers.
    p : float or array_like of floats
        Parameter of the distribution, >= 0 and <=1.
    size : int or tuple of ints, optional
        Output shape.  If the given shape is, e.g., ``(m, n, k)``, then
        ``m * n * k`` samples are drawn.  If size is ``None`` (default),
        a single value is returned if ``n`` and ``p`` are both scalars.
        Otherwise, ``np.broadcast(n, p).size`` samples are drawn.
    
    Returns
    -------
    samples : ndarray or scalar
        Drawn samples from the parameterized binomial distribution, where
        each sample is equal to the number of successes over the n trials.

    """
    return np.random.binomial(np.int32(n), p, size)

class Param:
    def __init__(self, pse, pei, pir, pic, rho, sigma_epsilon, init_mean, population_size):        
        self.pse = pse
        self.pei = pei
        self.pir = pir
        self.pic = pic
        self.rho = rho
        self.sigma_epsilon = sigma_epsilon
        self.init_mean = init_mean
        self.population_size = population_size


class SEIR:
    def __init__(self, param: Param):
        self.d  = 4  # Number of states. S+E+I+R, but R is deterministc so we only represent S+E+I in state vector. Extra state for z
        self.dy = 1  # ICU measurements
        self.param = param
        
    def set_param(self, rho):
        """
        Sets the "rho parameter" of the model to the provided value.

        :param rho: float, update the model to use this value for rho.
        """
        self.param.rho = rho

    def log_lik(self, y, alpha):
        """
        Computes the observation log-likelihood, log p(y_t|alpha_t), for all values in array alpha_t

        Parameters
        ----------
        y : int or float
            Observation at time t (number of ICU cases)
        alpha : ndarray
            Array of size (d,N) where each column is a state vector.

        Returns
        -------
        ndarray
            Array of size (1,N) with log-likelihood values.

        """
        
        if y.item() is not None:
            # Binomial likelihood
            return binom.logpmf(y.item(), alpha[2, :], self.param.pic)
        else:
            # Missing observation, return all zeros.
            return np.zeros((1, alpha.shape[1]))


    def sample_state(self, alpha0=None, N=1):
        """
        Samples N state vectors from the model dynamics, p(alpha_t | alpha_{t-1})

        Parameters
        ----------
        alpha0 : ndarray or None, optional
            If array of size (d,N), the i:th column contains the state vector
            that we condition the i:th sample on (i.e., alpha_{t-1}^i).
            
            If array of size (d,1) we use the same state vector for all N samples.
            
            If None, the samples are generated from the initial distribution p(alpha_1)-
            
            The default is None.
            
        N : int, optional
            The number of samples to generate. If alpha0 is of size (d,N) with
            N > 1, then the value of N must match the size of alpha0. The default is 1.

        Returns
        -------
        alpha1 : ndarray
            Array of size (d,N) where the i:th column contains the i:th sample (i.e., alpha_t^i).
            
        """
        
        
        if alpha0 is None:  # Initialize
            alpha1 = np.zeros((self.d, N), dtype=float)
            # The S,E,I states are sampled from binomial distributions with the specified means
            s0 = self.param.init_mean[0]
            e0 = self.param.init_mean[1]
            i0 = self.param.init_mean[2]            
            alpha1[0, :] = binomial_rng(self.param.population_size, s0 / self.param.population_size, size=N)
            alpha1[1, :] = binomial_rng(self.param.population_size, e0 / self.param.population_size, size=N)
            alpha1[2, :] = binomial_rng(self.param.population_size, i0 / self.param.population_size, size=N)

            # Initial state for z
            alpha1[3, :] = np.random.normal(loc=self.param.init_mean[3], scale=1, size=N)

        else:  # Propagate
            b = np.exp(alpha0[3,:])  # Compute b[t]
            rate = 1 - (1-self.param.pse)*np.exp(-self.param.rho * b * alpha0[2,:] / self.param.population_size)          
            de = binomial_rng(alpha0[0, :], rate, size=N)
            di = binomial_rng(alpha0[1, :], self.param.pei, size=N)
            dr = binomial_rng(alpha0[2, :], self.param.pir, size=N)
            dz = np.random.normal(loc = 0., scale=self.param.sigma_epsilon, size=N)
            alpha1 = alpha0 + [-de, de - di, di - dr, dz]
        return alpha1
    

    def sample_obs(self, alpha):
        """
        Samples observation from p(y_t | alpha_t)

        Parameters
        ----------
        alpha : ndarray
            Array of size (d,N) where each column is a state vector.

        Returns
        -------
        y : int or ndarray
            If N = 1, a single sample from the observation model is generated
            If N > 1, array of size (N,) where the i:th sample is sampled conditionally
            on the i:th column of alpha.

        """
        
        y = binomial_rng(alpha[2, :], self.param.pic)
        return y
    

    def simulate(self, T, N=1):
        """
        Simulates the SEIR model for a given number of time steps. Multiple trajectories
        can be simulated simulataneously.

        Parameters
        ----------
        T : int
            Number of time steps to simulate the model for.
        N : int, optional
            Number of independent trajectories to simulate. The default is 1.

        Returns
        -------
        alpha : ndarray
            Array of size (d,N,T) with state trajectories. alpha[:,i,:] is the i:th trajectory.
        y : ndarray
            Array of size (1,N,T) with observations.

        """
                
        # We work with 3d arrays for the states/obs (d,N,T)
        alpha = np.zeros((self.d, N, T), dtype=np.float64)  # We use floats to store the state vector because the last state (z) is a real number
        y = np.zeros((self.dy, N, T), dtype=np.int32)

        for t in range(T):
            if t == 0:
                # Initialize by calling the sample_state function without conditioning
                alpha[:, :, 0] = self.sample_state(N=N)
            else:
                # Sample the next time step (propagate)
                alpha[:, :, t] = self.sample_state(alpha[:, :, t - 1], N=N)

            # Sample the observation
            y[0, :, t] = self.sample_obs(alpha[:, :, t])

        return alpha, y

def exp_norm(logW):
    """
    Exponentiates and normalizes the log-weights.
    
    Parameters
    ----------
    logwgt : ndarray
        Array of size (N,) with log-weights.
    
    Returns
    -------
    wgt : ndarray
        Array of size (N,) with normalized weights, wgt[i] = exp(logwgt[i])/sum(exp(logwgt)),
        but computed in a /numerically robust way/!
    logZ : float
        log of the normalizing constant, logZ = log(sum(exp(logwgt))),
        but computed in a /numerically robust way/!
    """

    const = np.max(logW)
    W = np.exp(logW - const)
    sumofweights = np.sum(W)
    logZ = np.log(sumofweights) + const
    W = W / sumofweights
    return W, logZ

###### FROM LAB 4 ##################################################################

    
def plot_history(history, start_at):    
    plt.plot(history.epoch[start_at:], history.history['val_loss'][start_at:])
    plt.plot(history.epoch[start_at:], history.history['loss'][start_at:])
    plt.legend(['Test error','Training error'])    
    plt.xlabel('Epoch')

# 