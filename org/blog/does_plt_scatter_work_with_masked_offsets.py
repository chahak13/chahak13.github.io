import numpy as np
import matplotlib.pyplot as plt
import matplotlib

matplotlib.use('tkagg')

# plt.ion()
x = np.ma.array([1, 2, 3, 4, 5], mask=[0, 0, 1, 1, 0])
y = np.ma.array([1, 2, 3, 4, 5])

plt.figure()
plt.scatter(x, y)

x = np.ma.array([1, 2, 3, 4, 5], mask=[0, 0, 1, 1, 0])
y = np.arange(1, 6)

fig, ax = plt.subplots()
scat = ax.scatter(x, y)

x = x/ 2
print(f"Updated x: {x}")
scat.set_offsets(np.ma.column_stack([x, y]))
ax.set_xlim(0, 6)
print(scat.get_offsets())
plt.show()
