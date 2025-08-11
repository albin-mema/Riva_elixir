# Ash Usage Rules (Condensed)

Essential guidelines for working with Ash in this codebase. Prefer this compact doc; see `ash_usage_rules.md` for deep dives.

## Core principles
- Model around Domains and Resources; keep resources focused and named by the domain language
- Expose functionality via Domain code interfaces; avoid calling Ash directly in web modules
- Use DB-level filters via `Ash.Query.filter(expr(...))`; never in-memory filter when DB can do it
- Favor pipelines (|>) and pattern matching; one level of abstraction per function

## Code interfaces (preferred entry point)
- Define code interfaces on the Domain:
```elixir
resource Post do
  define :list_published, action: :read
end
```
- Pass options to interfaces instead of manual query building:
```elixir
MyDomain.list_published!(
  query: [filter: [status: :published], sort: [inserted_at: :desc], limit: 10],
  load: [:author]
)
```

## Querying
- `Ash.Query.filter/2` is a macro; ensure `require Ash.Query` or use `import Ash.Expr` for `expr/1`
- Use `expr(...)` for safe DB-pushed expressions and pins (`^var`)
- Common ops: `filter`, `sort`, `limit`, `offset`, `load`

Minimal example:
```elixir
import Ash.Expr
User
|> Ash.Query.filter(expr(is_nil(archived_at)))
|> Ash.read!(domain: MyDomain)
```

## Actions and hooks
- Prefer specific, well-named actions; put business logic in actions
- Use hooks inside actions for behavior:
  - before/after_action, before/after_transaction
- Use raising versions (`create!`, `update!`, ...) when appropriate

## Validations, preparations, changes
- Validations: ensure data meets requirements; use built-ins (`compare`, `present`, `match`) or custom modules
- Preparations: modify queries (filters/sorts/limits) before execution
- Changes: mutate attributes/relationships; use `manage_relationship/…` for related data

## Relationships (quick ref)
- Manage via action changes or changeset functions
- Types: `:append`, `:append_and_remove`, `:remove`, `:direct_control`, `:create`
- Loading: use `load:` in code interfaces or `Ash.Query.load/2` and prefer `strict?: true` when applicable

## Authorization (policies)
- Add `Ash.Policy.Authorizer` to resources; set actor on queries/changesets (not on the final call)
- Policy logic: first check that yields a decision determines outcome
- Compose required conditions with `forbid_unless`; use `authorize_if` for final allow
- Use bypass policies sparingly (e.g., admin)
- Field policies limit access to attributes/calculations/aggregates

## Calculations & Aggregates
- Calculations: expression-based or module-based; declare loads/args as needed
- Aggregates: `count`, `sum`, `exists`, `max/min/avg`, etc.; can include join filters

## Data layers (brief)
- Postgres (AshPostgres), ETS/Mnesia, Embedded, or Simple (no persistence)

## Migrations/codegen
- After changes: `mix ash.codegen <name>` (or `--dev` iteratively) to generate migrations and glue

## Testing
- Test via Domain code interfaces; use `Ash.Test` utilities; `Ash.can?` for policies
- In concurrent tests, use globally unique values for identity fields (avoid deadlocks)

## Examples (sketch)
```elixir
# Policy snippet
policies do
  policy action_type(:read) do
    authorize_if expr(public == true)
    authorize_if relates_to_actor_via(:owner)
  end
end

# Relationship manage
actions do
  update :update do
    argument :tags, {:array, :map}
    change manage_relationship(:tags, type: :append_and_remove)
  end
end
```

