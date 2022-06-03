
import numpy as np
from matplotlib import pyplot as plt
import time
import IPython.display as display
import pylab as pl
from utils import plotarrows
import math

class World:

    def __init__(self, world_number):

        # Set world parameters
        self.world = world_number
        self.x_size = 15
        self.y_size = 10

        if world_number == 1:
            # Set reward map
            feed = -0.1 * np.ones((self.y_size, self.x_size))
            feed[:8, 4:8] = -(0.1+0.1+0.1+0.1+0.2+11) / 5
            # Set endpoint
            self.term = (3, 12)
        elif world_number == 2:
            # Set reward map
            feed = -0.1 * np.ones((self.y_size, self.x_size))
            if np.random.rand() < 0.2:
                feed[:8, 4:8] = -11
            # Set endpoint
            self.term = (3, 14)
        elif world_number == 3:
            feed = -0.5 * np.ones((self.y_size, self.x_size))
            feed[:3, :] = -0.01
            feed[7, :] = -0.01
            feed[:, :3] = -0.01
            feed[:, 12:15] = -0.01
            self.term = (9,13)
        elif world_number == 4:
            feed = -0.5 * np.ones((self.y_size, self.x_size))
            feed[:3, :] = -0.01
            feed[7, :] = -0.01
            feed[:, :3] = -0.01
            feed[:, 12:15] = -0.01
            self.term = (7,1)
        else:
            raise ValueError('Invalid world number')

        self.feed = feed

        # Initialize agent
        if world_number == 3:
            self.pos = (7, 1)
        elif world_number == 4:
            self.pos = (9, 13)
        else:
            self.pos = (np.random.choice(self.y_size), np.random.choice(self.x_size))
            while self.pos == self.term:
                self.pos = (np.random.choice(self.y_size), np.random.choice(self.x_size))


    def action(self, act):

        if self.world == 4 and np.random.rand() < 0.3:
            act = np.random.choice([1,2,3,4])

        pos = list(self.pos)
        pos[0] += int(act==1) - int(act==2)
        pos[1] += int(act==3) - int(act==4)

        feedback = []

        # return minus infinity value and invalid flag when robots is out of boundary
        if pos[0] >= self.y_size or pos[0] < 0 or pos[1] >= self.x_size or pos[1] < 0:
            valid = 0
            feedback = 0
        else:
            valid = 1
            self.pos = tuple(pos)
            feedback = self.feed[pos[0], pos[1]]

        return valid, feedback

    def draw(self, episode=None, policy=None, sleepTime=0.1):
        pl.clf()
        plt.imshow(self.feed)
        plt.plot(self.pos[1] , self.pos[0] , color='black', linewidth=2, marker='s', markerfacecolor='gray' , markersize=20)
        plt.plot(self.term[1], self.term[0], color='black', linewidth=2, marker='o', markerfacecolor='green', markersize=20)
        
        if episode is None:
            plt.title(f'Feedback map, World {self.world}')
        else:
            plt.title(f'Feedback map, World {self.world}, Episode {episode}')
            
        if policy is not None:
            plotarrows(policy)
            
        display.display(pl.gcf())
        display.clear_output(wait=True)
        time.sleep(sleepTime)