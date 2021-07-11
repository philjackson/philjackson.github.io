---
layout: default
---

<main>
  <h2>Categories</h2>
  <ul>
    {% assign categories_list = site.categories %}
    {% if categories_list.first[0] == null %}
      {% for category in categories_list %}
        <li><a href="#{{ category }}">{{ category | capitalize }} ({{ site.tags[category].size }})</a></li>
      {% endfor %}
    {% else %}
      {% for category in categories_list %}
        <li><a href="#{{ category[0] }}">{{ category[0] | capitalize }} ({{ category[1].size }})</a></li>
      {% endfor %}
    {% endif %}
    {% assign categories_list = nil %}
  </ul>

  {% for tag in site.categories %}
  <h3 id="{{ tag[0] }}">{{ tag[0] | capitalize }}</h3>
  <ul>
    {% assign pages_list = tag[1] %}
    {% for post in pages_list %}
      {% if post.title != null %}
        {% if group == null or group == post.group %}
          <li>
            <a href="{{ site.url }}{{ post.url }}">{{ post.title }}</a> - <i>{{ post.date | date: "%b %-d, %Y" }}</i>
          </li>
        {% endif %}
      {% endif %}
    {% endfor %}
    {% assign pages_list = nil %}
    {% assign group = nil %}
  </ul>
  {% endfor %}
</main>
