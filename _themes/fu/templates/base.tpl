<!doctype html>
<html>
<head>
  <meta charset="utf-8">
  <meta name="generator" content="mdblog.rs">
  <meta name="viewport" content="width=device-width,initial-scale=1.0,maximum-scale=1.0,shrink-to-fit=no,user-scalable=0">
  {%- block title %}{% endblock title -%}
  <link rel="icon" href="{{ config.site_url }}/media/favicon.png">
  <link rel="stylesheet" href="{{ config.site_url }}/static/main.css">
  {%- block css %}{% endblock css -%}
</head>
<body>
<header>
  <div class="container">
    <div id="site">
      <div id="site-name">
        <a href="{{ config.site_url }}/index.html" title="{{ config.site_name }}">{{ config.site_name }}</a>
      </div>
      <div id="site-motto">{{ config.site_motto }}</div>
    </div>
    <nav id="header-nav">
      <a href="{{ config.site_url }}/index.html">博客</a>
      <a href="{{ config.site_url }}/tags.html">标签</a>
      <a href="{{ config.site_url }}/atom.xml">订阅</a>
    </nav>
    <svg id="menu" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg">
      <path d="M128 298.666667h768a42.666667 42.666667 0 0 0 0-85.333334H128a42.666667 42.666667 0 0 0 0 85.333334z m768 170.666666H128a42.666667 42.666667 0 0 0 0 85.333334h768a42.666667 42.666667 0 0 0 0-85.333334z m0 256H128a42.666667 42.666667 0 0 0 0 85.333334h768a42.666667 42.666667 0 0 0 0-85.333334z" fill="#fff"></path>
    </svg>
  </div>
</header>

<main class="container">
  {%- block main %}{% endblock main %}
</main>

<footer>
  <div class="container">{{ config.footer_note }}</div>
</footer>

<script>
  function setMenu() {
    var menu = document.getElementById('menu');
    if (!menu) {
      return;
    }

    var headerNav = document.getElementById('header-nav');
    menu.addEventListener('click', function () {
      if (headerNav.style.display === 'flex') {
        headerNav.style.display = 'none';
      } else {
        headerNav.style.display = 'flex';
      }
    });

    document.addEventListener('click', function (evt) {
      if (!window.matchMedia('(max-width: 767px)').matches) {
        return;
      }
      if (headerNav.style.display !== 'flex') {
        return;
      }

      let targetElement = evt.target;
      do {
        if (targetElement == menu) {
          return;
        }
        targetElement = targetElement.parentNode;
      } while (targetElement)

      headerNav.style.display = 'none';
    })
  }

  window.addEventListener('load', setMenu);
</script>
{%- block js %}{% endblock js -%}
</body>
</html>
