created: 2015-04-15T12:04:00+08:00
tags: [python, web, 自动化]


最近在做一些自动化Web测试方面的任务，用的是python selenium，很是方便。


## 介绍

Selenium是一个用于Web应用程序测试的工具，Selenium的测试用例直接运行在浏览器中，就像真正的用户在操作一样。
使用Selenium测试Web页面，节省了大量人力，同时也避免了测试中不必要的错误。


## 安装

```
pip install selenium
```


## 简单示例

```python
from selenium import webdriver
from selenium.webdriver.common.keys import Keys

driver = webdriver.Firefox()
driver.get("http://www.python.org")
assert "Python" in driver.title
elem = driver.find_element_by_name("q")
elem.send_keys("pycon")
elem.send_keys(Keys.RETURN)
assert "No results found." not in driver.page_source
driver.close()
```

上面示例简单的模拟了一个这样的测试操作：

* 打开firefox浏览器
* 进入`http://www.python.org`网址
* 测试打开网页标题是否还有`Python`字符串
* 在这个网页上找到`name`为`q`的元素
* 向这个元素上输入`pycon`字符串，并回车
* 测试网页返回是否成功
* 关闭浏览器


## 应用于unittest

上面示例可以与python测试框架unittest（当然也可以用pytest，这里只以unittest举例）结合在一起测试Web页面，其对应代码如下：

```python
import unittest
from selenium import webdriver
from selenium.webdriver.common.keys import Keys

class PythonOrgSearch(unittest.TestCase):

    def setUp(self):
        self.driver = webdriver.Firefox()

    def tearDown(self):
        self.driver.close()

    def test_search_in_python_org(self):
        driver = self.driver
        driver.get("http://www.python.org")
        self.assertIn("Python", driver.title)
        elem = driver.find_element_by_name("q")
        elem.send_keys("pycon")
        elem.send_keys(Keys.RETURN)
        assert "No results found." not in driver.page_source

if __name__ == "__main__":
    unittest.main()
```


## 选择元素

在测试中，操作流程基本就是选择测试DOM元素，而后测试这个元素是否符合要求或对这个元素执行一些命令。
其中Selenium为我们提供了很多选择元素的方法。

选择单个元素的方法：

* driver.find\_element\_by\_id
* driver.find\_element\_by\_name
* driver.find\_element\_by\_xpath
* driver.find\_element\_by\_link\_text
* driver.find\_element\_by\_partial\_link\_text
* driver.find\_element\_by\_tag\_name
* driver.find\_element\_by\_class\_name
* driver.find\_element\_by\_css\_selector

选择多个元素的方法：

* driver.find\_elements\_by\_name
* driver.find\_elements\_by\_xpath
* driver.find\_elements\_by\_link\_text
* driver.find\_elements\_by\_partial\_link\_text
* driver.find\_elements\_by\_tag\_name
* driver.find\_elements\_by\_class\_name
* driver.find\_elements\_by\_css\_selector

上面是直接调用方法，也可以向下面一样：

```python
from selenium.webdriver.common.by import By

driver.find_element(By.XPATH, '//button[text()="Some text"]')
driver.find_elements(By.XPATH, '//button')
```

其中XPath语法请参考[W3C Tutorial](http://www.w3schools.com/xpath/)或者[W3C教程](http://www.w3school.com.cn/xpath/xpath_intro.asp)。


## 页面交互

选择元素后，就可以与页面交互了。

可以对一些输入框键入字符串：

```python
element.send_keys("some text")
```

也可以模拟键入回车键：

```python
element.send_keys(" and some", Keys.ARROW_DOWN)
```

当然也可以清除已经键入的字符串：

```python
element.clear()
```

可以选择form中的选项：

```python
option_element.click()
```

也可以这样：

```python
from selenium.webdriver.support.ui import Select
select = Select(driver.find_element_by_name('name'))
select.select_by_index(index)
select.select_by_visible_text("text")
select.select_by_value(value)
```

form选项选择完后，可以进行提交：

```python
driver.find_element_by_id("submit").click()
```

或

```python
# 这里的element可以是form中任何一个元素
element.submit()
```

也可以模拟拖拽：

```python
element = driver.find_element_by_name("source")
target = driver.find_element_by_name("target")

from selenium.webdriver import ActionChains
action_chains = ActionChains(driver)
action_chains.drag_and_drop(element, target)
```

可以在切换浏览器窗口：

```python
driver.switch_to_window("windowName")

for handle in driver.window_handles:
    driver.switch_to_window(handle)

driver.switch_to_frame("frameName")
```

也可以切换到弹出的对话框：

```python
alert = driver.switch_to_alert()
```

可以模拟浏览器历史回访：

```python
driver.forward()
driver.back()
```

最后可以对浏览器添加cookies:

```python
# Go to the correct domain
driver.get("http://www.example.com")

# Now set the cookie. This one's valid for the entire domain
cookie = {‘name’ : ‘foo’, ‘value’ : ‘bar’}
driver.add_cookie(cookie)

# And now output all the available cookies for the current URL
driver.get_cookies()
```
