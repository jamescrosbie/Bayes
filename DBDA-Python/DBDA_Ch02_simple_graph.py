"""
A simple graph drawn by Python :-)
"""
import matplotlib.pyplot as plt #load the plot function
import numpy as np #load the sceientific package

x = np.linspace(-2*np.pi, 2*np.pi, 40)
y = np.sin(x)

plt.plot(x, y)
#plt.savefig('Figure_2.2.png')
plt.show()