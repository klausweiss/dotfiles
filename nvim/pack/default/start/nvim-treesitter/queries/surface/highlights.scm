; Surface text is not highlighted
(text) @none

; Surface has two types of comments, both are highlighted as such
(comment) @comment @spell

; Surface attributes are highlighted as HTML attributes
(attribute_name) @tag.attribute

; Attributes are highlighted as strings
(quoted_attribute_value) @string

; Surface blocks are highlighted as keywords
[
  (start_block)
  (end_block)
  (subblock)
] @keyword

; Surface supports HTML tags and are highlighted as such
[
  "<"
  ">"
  "</"
  "/>"
  "{"
  "}"
  "<!--"
  "-->"
  "{!--"
  "--}"
] @tag.delimiter

; Surface tags are highlighted as HTML
(tag_name) @tag

; Surface components are highlighted as types (Elixir modules)
(component_name) @type

; Surface directives are highlighted as keywords
(directive_name) @keyword

; Surface operators
"=" @operator
