
# coding: utf-8

# Let's consider $$x^2 + 2px - q = 0$$  We know the roots to be $$x = -p \pm \sqrt{p^2 +q}$$  So let's take a look at $$x = -p + \sqrt{p^2 + q}$$

# Let's take $p$ very large and $q$ to be small:

# In[8]:

from math import sqrt
p = 1e6
q = 0.1

x = -p + sqrt(p**2 + q)
print(repr(x))
print(repr(x**2 + 2*p*x - q))


# Is this accurate?  Not quite.  Let's try rearranging:
# $$
# \frac{q}{p + \sqrt{p^2 + q}}
# $$

# In[9]:

x = q / (p + sqrt(p**2 + q))
print(repr(x))
print(repr(x**2 + 2*p*x - q))


# In[ ]:



