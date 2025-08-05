# Used by "mix format"
# Configuration patterns: Use application configuration instead of hardcoded values
# Type safety: Configure formatter to handle type-safe modules
[
  import_deps: [:ecto, :phoenix],
  locals_without_parens: [
    # Add common DSL-like macros without parens to keep readability
    resource: 1,
    tag: 1,
    json_api: 1,
    graphql: 1,
    admin: 1
  ],
  inputs: ["{mix,.formatter}.exs", "{config,lib,test}/**/*.{ex,exs}", "storybook/**/*.exs"],
  export: [
    locals_without_parens: [
      resource: 1,
      tag: 1,
      json_api: 1,
      graphql: 1,
      admin: 1
    ]
  ],
  # Functional programming patterns: Use consistent formatting for pipelines
  line_length: 120,
  # Single level of abstraction: Keep formatting consistent with functional style
  remove_parens?: true,
  # Code readability: Format maps and keyword lists consistently
  format_macro_arguments: true
]
