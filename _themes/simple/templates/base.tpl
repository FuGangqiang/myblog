<!doctype html>
<html>
<head>
  <meta charset="utf-8">
  <link rel="icon" href="/static/favicon.png">
  <link rel="stylesheet" href="/static/main.css">
  {% block css %}{% endblock css %}
  <title>{{ title }}</title>
</head>
<body>
<header class="clearfix">
  <section id="imglogo">
    <a href="/index.html" title="{{ site_name }}"><img src="{{ site_logo }}"></a>
  </section>

  <section id="textlogo">
    <h1 id="site-name"><a href="/index.html" title="{{ site_name }}">{{ site_name }}</a></h1>
    <h2 id="site-motto">{{ site_motto }}</h2>
  </section>

  <nav>
    <ul>
      <li><a href="/index.html">博文</a></li>
      <li><a href="/blog/posts/notes.html">笔记</a></li>
      <li><a href="/blog/posts/docs.html">文档</a></li>
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
        <li><a href="https://github.com/FuGangqiang" target="_blank">Github</a></li>
      </ul>
    </section>
  </aside>
</div>

<footer>
  <p>
    {{ footer_note }}
  </p>
</footer>
{% block js %}{% endblock js %}
</body>
</html>
