date: 2015-04-14 11:52:00
tags: python, network


最近工作应用了自动发送邮件功能，使用到了 python smtplib 模块


## 发送文本格式

```python
import smtplib
from email.mime.text import MIMEText

mailto_list=['username@163.com']
mail_host="smtp.163.com"  # 设置服务器
mail_user="username"      # 用户名
mail_postfix="163.com"    # 发件箱的后缀
mail_pass="password"      # 口令

def send_mail(to_list,sub,content):
    me="hello"+"<"+mail_user+"@"+mail_postfix+">"
    msg = MIMEText(content,_subtype='plain',_charset='utf-8')
    msg['Subject'] = sub
    msg['From'] = me
    msg['To'] = ";".join(mailto_list)
    try:
        server = smtplib.SMTP()
        server.connect(mail_host)
        server.login(mail_user,mail_pass)
        server.sendmail(msg['from'], msg['to'], msg.as_string())
        server.close()
        return True
    except:
        return False

if __name__ == '__main__':
    if send_mail(mailto_list,"hello","hello world！"):
        print("发送成功")
    else:
        print("发送失败")
```


## 发送 html 格式

```python
import smtplib
from email.mime.text import MIMEText

mailto_list=['username@163.com']
mail_host="smtp.163.com"  # 设置服务器
mail_user="username"      # 用户名
mail_postfix="163.com"    # 发件箱的后缀
mail_pass="password"      # 口令

def send_mail(to_list,sub,content):
    me="hello"+"<"+mail_user+"@"+mail_postfix+">"
    msg = MIMEText(content,_subtype='html',_charset='utf-8')
    msg['Subject'] = sub
    msg['From'] = me
    msg['To'] = ";".join(mailto_list)
    try:
        server = smtplib.SMTP()
        server.connect(mail_host)
        server.login(mail_user,mail_pass)
        server.sendmail(msg['from'], msg['to'], msg.as_string())
        server.close()
        return True
    except:
        return False

if __name__ == '__main__':
    if send_mail(mailto_list,"你好","你好，<a href='http://www.baidu.com'>点击</a>"):
        print("发送成功")
    else:
        print("发送失败")
```


## 发送附件

```python
import smtplib
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart

mailto_list=['username@163.com']
mail_host="smtp.163.com"  # 设置服务器
mail_user="username"      # 用户名
mail_postfix="163.com"    # 发件箱的后缀
mail_pass="password"      # 口令

def send_mail(to_list,sub):
    msg = MIMEMultipart()

    me="hello"+"<"+mail_user+"@"+mail_postfix+">"
    msg['Subject'] = sub
    msg['From'] = me
    msg['To'] = ";".join(mailto_list)

    #构造附件1
    att1 = MIMEText(open('att1.txt', 'rb').read(), 'base64', 'utf-8')
    att1["Content-Type"] = 'application/octet-stream'
    att1["Content-Disposition"] = 'attachment; filename="att1.txt"'
    msg.attach(att1)

    #构造附件2
    att2 = MIMEText(open('att2.txt', 'rb').read(), 'base64', 'utf-8')
    att2["Content-Type"] = 'application/octet-stream'
    att2["Content-Disposition"] = 'attachment; filename="att2.txt"'
    msg.attach(att2)

    try:
        server = smtplib.SMTP()
        server.connect(mail_host)
        server.login(mail_user,mail_pass)
        server.sendmail(msg['from'], msg['to'], msg.as_string())
        server.close()
        return True
    except:
        return False

if __name__ == '__main__':
    if send_mail(mailto_list,"hello",):
        print("发送成功")
    else:
        print("发送失败")
```
