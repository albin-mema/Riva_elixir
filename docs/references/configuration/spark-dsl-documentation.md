# Spark DSL Documentation

## Introduction

Spark is a framework for building Domain-Specific Languages (DSLs) in Elixir. This documentation provides comprehensive guidance on creating and using DSLs with Spark.

## Core Concepts

### DSL Structure

A Spark DSL consists of:

1. **Entities** - The basic building blocks of your DSL
2. **Sections** - Groupings of related entities
3. **Extensions** - Modules that extend or modify the DSL behavior
4. **Transformers** - Functions that transform the DSL state
5. **Verifiers** - Functions that validate the DSL state

### Basic DSL Example

```elixir
defmodule MyDsl do
  use Spark.Dsl, 
    schema: [
      my_field: [
        type: :string,
        required: true,
        doc: "A required string field"
      ],
      optional_field: [
        type: :integer,
        default: 42,
        doc: "An optional integer field with default"
      ]
    ]
end
```

## DSL Schema Definition

### Field Types

Spark supports various field types:

```elixir
# Basic types
field: [
  type: :string,
  required: true
]

# Lists
tags: [
  type: {:list, :string},
  default: []
]

# Maps
config: [
  type: :map,
  default: %{}
]

# Custom types
custom_field: [
  type: {:custom, MyCustomType},
  required: false
]

# Union types
union_field: [
  type: {:or, [:string, :integer, {:struct, MyStruct}]},
  required: false
]
```

### Field Options

| Option | Type | Description |
|--------|------|-------------|
| `type` | atom | The type of the field |
| `required` | boolean | Whether the field is required |
| `default` | any | Default value for optional fields |
| `doc` | string | Documentation for the field |
| `keys` | map | For map types, define expected keys |
| `items` | map | For list types, define item structure |

### Complex Schema Example

```elixir
defmodule ComplexDsl do
  use Spark.Dsl,
    schema: [
      name: [
        type: :string,
        required: true,
        doc: "The name of the resource"
      ],
      settings: [
        type: :map,
        default: %{},
        doc: "Configuration settings",
        keys: [
          timeout: [
            type: :integer,
            default: 30,
            doc: "Request timeout in seconds"
          ],
          retries: [
            type: :integer,
            default: 3,
            doc: "Number of retry attempts"
          ]
        ]
      ],
      tags: [
        type: {:list, :string},
        default: [],
        doc: "Resource tags for categorization"
      ],
      complex_field: [
        type: {:or, [:atom, :string, {:struct, MyStruct}]},
        required: false,
        doc: "A field that can be multiple types"
      ]
    ]
end
```

## Implementation Details

### 1. Compile-Time vs Runtime

- **DSL processing happens at compile time**
- Transformers and verifiers run during compilation
- Generated code is optimized for runtime performance
- Use `persist/3` to cache expensive computations

### 2. Error Handling

Always provide context in errors:
```elixir
{:error,
 Spark.Error.DslError.exception(
   message: "Clear error message",
   path: [:section, :subsection],  # DSL path
   module: module                   # Module being compiled
 )}
```

## Common Gotchas

### 1. Compilation Deadlocks
```elixir
# WRONG - Causes deadlock
def transform(dsl_state) do
  module = get_persisted(dsl_state, :module)
  module.some_function()  # Module isn't compiled yet!
end

# RIGHT - Use DSL state
def transform(dsl_state) do
  entities = get_entities(dsl_state, [:section])
  # Work with DSL state, not module functions
end
```

### 2. Extension Order
- Extensions are processed in order
- Later extensions can modify earlier ones
- Use transformer ordering for dependencies

## Performance Considerations

1. **Compilation Performance**
   - Heavy transformers slow compilation
   - Cache expensive computations with `persist/3`
   - Use verifiers instead of transformers when possible

2. **Runtime Performance**
   - DSL processing has zero runtime overhead
   - Generated code is optimized
   - Info modules cache DSL data efficiently

3. **Memory Usage**
   - DSL state is cleaned up after compilation
   - Runtime footprint is minimal
   - Use structs efficiently in entities

## Summary

Spark enables:
- Clean, declarative DSL syntax
- Compile-time validation and transformation
- Extensible architecture
- Excellent developer experience

When coding with Spark:
1. Think in terms of entities, sections, and extensions
2. Leverage compile-time processing for validation
3. Use transformers for complex logic
4. Test DSLs thoroughly
5. Provide clear error messages
6. Document your DSLs well