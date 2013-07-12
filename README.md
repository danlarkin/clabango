# clabango

It's a templating library for clojure, I guess?

This library (its functionality, not its code) is modeled after Django's templating library.

It isn't at feature parity yet, but I've used it successfully on a few projects.

I'll try to document it here, but you may get more out of Django's template docs (https://docs.djangoproject.com/en/1.4/topics/templates/) than anything I can write.

## Usage

```clojure
[clabango "0.5"]
```

## Major Concepts

### Template basics
Templates are just a normal text files with embedded tags and filters and they can produce any text format, not just HTML. Templates are rendered with a `context` -- a hashmap -- which is used for variable interpolation and in most template tags.

### Variables
Variables are one way to introduce dynamic content into what would otherwise be a plain text file. They're looked up as keywords in the `context`, and if missing then rendered as "", an empty string. Here's an example of a template snippet including a variable: `Hello, my name is {{first-name}}!`. When rendered with a `context` of `{:first-name "Dan"}` it will produce `Hello, my name is Dan!`.

### Filters
Sometimes just including a variable as-is isn't enough, it may need to be modified. For this, we have filters. Filters are functions that operate on variables and return a transformation. For instance: `{{first-name|upper}}` rendered with a `{:first-name "Dan"}` would produce `DAN`.

Some filters take parameters. `{{domain|hash:"md5"}}` rendered with `{:domain "example.org"}` would produce `1bdf72e04d6b50c82a48c7e4dd38cc69`.

It's possible to define custom filters for your project.

```clojure
(ns example.core
  (:require [clabango.filters :refer [deftemplatefilter]]
            [clabango.parser :refer [render]]))

(deftemplatefilter "prepend-hi" [node body arg]
  {:body (str "Hi, " body)})

(defn render-hi [name]
  (render "{{name|prepend-hi}}" {:name name}))
```

Calling `(render-hi "Dan")` would result in `"Hi, Dan"`.

By default all content from variables is html-escaped. If you have some content you know is safe, you can exempt it from getting escaped by using the `safe` filter, like this: `{{foo|safe}}`.

Your custom template filter can return safe content by setting `:safe?` to `true` in the map it returns.

Here's a list of all the builtin template filters:

**upper**

Calls .toUpperCase on the variable.

`{{name|upper}}` w/ `{:name "Dan"}` => `DAN`

**date**

Format a variable as a date string using joda time. The rules for date formats are here: http://joda-time.sourceforge.net/apidocs/org/joda/time/format/DateTimeFormat.html

`{{created-at|date:"yyyy-MM-dd_HH:mm:ss"}}` w/ `{:created-at (java.util.Date. 0)}` => `1970-01-01_00:00:00`

**hash**

Compute a hash of the variable using one of a handful of algorithms (md5 | sha | |sha256 | sha384 | sha512).

`{{domain|hash:"md5"}}` w/ `{:domain "example.org"}` => `1bdf72e04d6b50c82a48c7e4dd38cc69`

**count**

Call clojure.core/count on the variable.

`{{name|count}}` w/ `{:name "Dan"}` => `3`

`{{items|count}}` w/ `{:items [1 2 3 4 5]}` => `5`

**pluralize**

Returns the correct (English) pluralization based on the variable. This works with many words, but certainly not all (eg. foot/feet, mouse/mice, etc.)

`{{items|count}} item{{items|pluralize}}` w/ `{:items []}` => `0 items`

`{{items|count}} item{{items|pluralize}}` w/ `{:items [1]}` => `1 item`

`{{items|count}} item{{items|pluralize}}` w/ `{:items [1 2]}` => `2 items`

`{{fruit|count}} tomato{{fruit|pluralize:"es"}}` w/ `{:fruit []}` => `0 tomatoes`

`{{fruit|count}} tomato{{fruit|pluralize:"es"}}` w/ `{:fruit [1]}` => `1 tomato`

`{{fruit|count}} tomato{{fruit|pluralize:"es"}}` w/ `{:fruit [1 2]}` => `2 tomatoes`

`{{people|count}} lad{{people|pluralize:"y,ies"}}` w/ `{:people []}` => `0 ladies`

`{{people|count}} lad{{people|pluralize:"y,ies"}}` w/ `{:people [1]}` => `1 lady`

`{{people|count}} lad{{people|pluralize:"y,ies"}}` w/ `{:people [1 2]}` => `2 ladies`

**to-json**

Render a clojure datastrucure into JSON.

`{{data|to-json}}` w/ `{:data [1 2 3 {:a "b"}]}` => `[1,2,3,{&quot;a&quot;:&quot;b&quot;}]`

Remember, all content from variables is automatically html-escaped. If you want to get non-escaped JSON, use the `safe` filter.

`{{data|to-json|safe}}` w/ `{:data [1 2 3 {:a "b"}]}` => `[1,2,3,{"a":"b"}]`

**safe**

Exempts the variable from being html-escaped.

`{{data}}` w/ `{:data "<foo>"}` => `&lt;foo&gt;`

`{{data|safe}}` w/ `{:data "<foo>"}` => `<foo>`


### Tags
There are two types of tags: `inline` and `block`. `block` tags have a start and an end -- they operate on a block of text, but `inline` tags don't close, they're just statements.

It's possible to define custom tags, just like with filters.

```clojure
(ns example.core
  (:require [clabango.tags :refer [deftemplatetag]]
            [clabango.parser :refer [render]]))

(deftemplatetag "foo" [nodes context]
  {:string (str "foo " (first (:args (first nodes))))
   :context context})

(deftemplatetag "repeat" "endrepeat" [nodes context]
  (let [repeat-times (first (:args (first nodes)))
        body-nodes (rest (butlast nodes))]
    {:nodes (repeat repeat-times body-nodes)}))

(defn render-foo []
  (render "{% foo quux %} {% foo baz %}" {}))

(defn render-repeat [name]
  (render "{% repeat 2 %}{{name}}{% endrepeat %}" {:name name}))
```

Calling `(render-foo)` would result in `"foo quux foo baz"`.

Calling `(render-repeat "Dan")` would result in `"Dan Dan"`.


Here's a list of the builtin tags:

**include** *inline*

Include another template into this one.

`{% include "path/to/comments.html" %}`

**block/endblock** *block*

This tag is used in template inheritance -- It defines a block that can be overridden in templates that extend this one. Or, if you're in a template that's extending a base template with blocks, create an override.

`{% block foo %}This text can be overridden later{% endblock %}`

**extends** *inline*

This tag is also used in template inheritance -- It defines the current template as inheriting from another.

`{% extends "path/to/foo.html" %}`

**if/endif** *block*

It's an `if` -- only render the body if the conditional is true.

`{% if condition %}yes!{% endif %}`

`{% if condition %}yes!{% else %}no!{% endif %}`

**ifequal/endifequal** *block*

Only render the body if the two args are equal (according to clojure.core/=).

`{% ifequal foo bar %}yes!{% endifequal %}`

`{% ifequal foo bar %}yes!{% else %}no!{% endifequal %}`

`{% ifequal foo "this also works" %}yes!{% endifequal %}`

**for/endfor** *block*

Render the body one time for each element in the list. Each "render" will introduce two extra variables into the context -- `forloop.first` and `forloop.last`.

`{% for x in some-list %}element: {{x}} first? {{forloop.first}} last? {{forloop.last}}{% endfor %}`

### Template Inheritance
Clabango's template inheritance is modeled on Django's -- docs available here: https://docs.djangoproject.com/en/1.4/topics/templates/#template-inheritance

### How to Call from Clojure

`src/example/core.clj`
```clojure
(ns example.core
  (:require [clabango.parser :refer [render-file]]))

(defn index [request]
  (render-file "example/templates/index.html" {:greeting "Hey!"}))
```

`src/example/templates/index.html`
```html
<html>
<body>
{{greeting}} Welcome to example.com!
</body>
</html>
```

## YourKit

YourKit is kindly supporting open source projects with its full-featured Java Profiler. YourKit, LLC is the creator of innovative and intelligent tools for profiling Java and .NET applications. Take a look at YourKit's leading software products:

* <a href="http://www.yourkit.com/java/profiler/index.jsp">YourKit Java Profiler</a> and
* <a href="http://www.yourkit.com/.net/profiler/index.jsp">YourKit .NET Profiler</a>.
