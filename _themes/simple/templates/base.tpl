<!doctype html>
<html>
<head>
  <meta charset="utf-8">
  <link rel="icon" href="/static/img/favicon.png">
  <link rel="stylesheet" href="/static/css/main.css">
  {% block css %}{% endblock css %}
  <title>{{ title }}</title>
</head>
<body>
<header class="clearfix">
  <section id="imglogo">
    <a href="/index.html" title="Fu"><img src="/static/img/logo.png"></a>
  </section>

  <section id="textlogo">
    <h1 id="site-name"><a href="/index.html" title="Fu">Fu</a></h1>
    <h2 id="site-motto">Simple is Beautiful!</h2>
  </section>

  <nav>
    <ul>
      <li><a href="/index.html">博文</a></li>
      <li><a href="https://github.com/FuGangqiang/myblog/issues/new" target="_blank">留言</a></li>
    </ul>
  </nav>
</header>
<div id="container" class="clearfix">
  <main>
    {% block main %}{% endblock main %}
  </main>

  <aside>
    <section class="tags clearfix">
      <h1>标签</h1>
      <ul>
      {% for tag in all_tags %}
        <li><a href="{{ tag.url }}">{{ tag.name }}<sup>{{ tag.num }}</sup></a></li>
      {% endfor %}
      </ul>
    </section>

    <section class="links clearfix">
      <h1>链接</h1>
      <ul>
	<li><a href="/blog/posts/notes.html">笔记</a></li>
	<li><a href="http://github.com/FuGangqiang" target="_blank">Github</a></li>
      </ul>
    </section>
  </aside>
</div>

<footer>
  <p>
    博学之　审问之　慎思之　明辨之　笃行之
  </p>
</footer>
{% block js %}{% endblock js %}
</body>
</html>
