date: 2014-09-29 10:50:00
tags: python, matplotlib


代码如下：

```python
#!/usr/bin/python

import matplotlib as mpl
import matplotlib.pyplot as plt

myfont = mpl.font_manager.FontProperties(fname='/usr/share/fonts/wenquanyi/wqy-microhei/wqy-microhei.ttc')

fig = plt.figure()
fig.suptitle('Bold figure suptitle', fontsize=14, fontweight='bold')

ax = fig.add_subplot(111)
fig.subplots_adjust(top=0.85)
ax.set_title('axes title')
ax.set_xlabel('xlabel')
ax.set_ylabel('ylabel')

ax.text(3, 8, 'boxed italics text in data coords', style='italic',
        bbox={'facecolor':'red', 'alpha':0.5, 'pad':10})

ax.text(2, 6, r'an equation: $E=mc^2$', fontsize=15)

ax.text(3, 2, 'unicode:　中文', fontproperties=myfont)

ax.text(0.95, 0.01, 'colored text in axes coords',
        verticalalignment='bottom', horizontalalignment='right',
        transform=ax.transAxes,
        color='green', fontsize=15)

ax.plot([2], [1], 'o')

ax.annotate('annotate', xy=(2, 1), xytext=(3, 4),
            arrowprops={'facecolor':'black', 'shrink':0.05})

ax.axis([0, 10, 0, 10])

plt.show()
```

以上代码生成图片如下：

![matplotlib-zh](/media/python/matplotlib-zh.png)
