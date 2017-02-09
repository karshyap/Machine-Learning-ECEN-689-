# -*- coding: utf-8 -*-
"""
Created on Fri Sep 23 02:56:36 2016

@author: kashyap
"""
###########Assignment-1#############
##########Question (b)

def clrscr():
    for i in range(100):
        print("\n")
        
from scipy.stats import  norm
import numpy as np
import math
import matplotlib
import matplotlib.pyplot as plt
import pylab
import ggplot
from ggplot import *
from pylab import figure, axes, pie, title, show
import pandas as pd

sigma=np.linspace(0.000001,10,1000) ###Declaring sigma
rho=[0,0.4,0.6,0.8,1] ####Declaring rho
x=[[]for i in range(len(rho))]
for i in range(0,len(x)):
    x[i]=norm.cdf(-1/(math.sqrt(2)*sigma*(math.sqrt(1+rho[i]))))
    
df = pd.DataFrame({'sigma':sigma, 'x0':x[0] , 'x4':x[1], 'x6':x[2], 'x8':x[3],
                   'x10':x[4]})
                   
#p5=ggplot(aes(x='sigma',y='x4'),data=df)+geom_line()#+ggtitle("Variation of $\epsilon^*$ as a function of $\rho$ and $\sigma$',fontsize=30")
    #x[i]=i
#x=norm.cdf(10)

f, ((ax1)) = plt.subplots(1, 1, sharex='col', sharey='row')
#figure(num=None, figsize=(8, 6), dpi=80, facecolor='w', edgecolor='k')

plt.style.use('ggplot')


plt.xlabel("$\sigma$", fontsize=35,fontweight='bold')
plt.ylabel("$\epsilon^*$", fontsize=35,fontweight='bold')


ax1.tick_params(axis='x', labelsize=20)
#ax3.tick_params(axis='x', labelsize=15)
#ax3.tick_params(axis='y', labelsize=15)
ax1.tick_params(axis='y', labelsize=20  )

plt.grid()

ax1.grid()
#ax2.grid()
#ax3.grid()
#ax4.grid()

#ax1.set_title(r'$\rho$ =0.4 ', fontsize=25)
#ax2.set_title(r'$\rho$ =0.6 ', fontsize=25)
#ax3.set_title(r'$\rho$ =0.8 ', fontsize=25)
#ax4.set_title(r'$\rho$ =1.0 ', fontsize=25)

t1,=ax1.plot(sigma,x[0],linewidth=4,label=r'$\rho=0.0$')
t2,=ax1.plot(sigma,x[1],linewidth=4, label=r'$\rho=0.4$')
t3,=ax1.plot(sigma,x[2],linewidth=4,label=r'$\rho=0.6$')
t4,=ax1.plot(sigma,x[3],linewidth=4,label=r'$\rho=0.8$')
t5,=ax1.plot(sigma,x[4],linewidth=4,label=r'$\rho=1.0$')
ax1.set_xlim([0,10])
plt.legend(handles=[t1, t2, t3,t4,t5],loc=4,fontsize=25)


#pylab.xlim[0,1.6]

fig = matplotlib.pyplot.gcf()
fig.set_size_inches(18.5, 10.5)
fig.suptitle(r'Variation of $\epsilon^*$ as a function of $\rho$ and $\sigma$',fontsize=30)

fig.savefig('test2png.png', dpi=100)
#fig.savefig('test_temp.png', dpi=100)

#######################Question-1c#####



#pylab.savefig("temp1.jpg")

