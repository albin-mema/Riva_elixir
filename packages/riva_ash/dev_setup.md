# Riva Ash Development Setup & Code Quality Workflow

[![Elixir Version](https://img.shields.io/badge/Elixir-1.18+-blue.svg)](https://elixir-lang.org)
[![Phoenix Framework](https://img.shields.io/badge/Phoenix-1.7+-orange.svg)](https://www.phoenixframework.org)
[![Ash Framework](https://img.shields.io/badge/Ash-3.5+-green.svg)](https://ash-hq.org)

## 🚀 Quick Start

### Prerequisites

- **Elixir**: 1.18+ with OTP 26+
- **PostgreSQL**: 14+
- **Node.js**: 18+ (for asset compilation)
- **Git**: For version control
- **Pre-commit**: For automated quality checks (optional but recommended)

### 1. Clone and Setup

```bash
# Clone the repository
git clone https://github.com/your-org/riva_ash.git
cd riva_ash/packages/riva_ash

# Install Elixir dependencies
mix deps.get

# Install Node.js dependencies and setup assets
mix assets.setup

# Create and migrate database
mix ecto.setup
```

### 2. Verify Setup

```bash
# Run quick quality checks
mix quality.quick

# Start the development server
mix phx.server
```

Visit `http://localhost:4000` to verify the application is running.

## 🛠️ Development Environment

### VS Code Setup (Recommended)

Install these VS Code extensions for optimal development experience:

```bash
# Install VS Code ElixirLS extension
# Search for "ElixirLS" in VS Code extensions

# Install VS Code Elixir Formatter
# Search for "Elixir Formatter" in VS Code extensions

# Install VS Code Tailwind CSS IntelliSense
# Search for "Tailwind CSS IntelliSense" in VS Code extensions
```

### VS Code Configuration

Create `.vscode/settings.json`:

```json
{
  "elixirLS.dialyzerEnabled": true,
  "elixirLS.enableTestLenses": true,
  "elixirLS.fetchDependencies": true,
  "elixirLS.projectDir": ".",
  "editor.formatOnSave": true,
  "editor.defaultFormatter": "jakebecker.elixir-ls",
  "[elixir]": {
    "editor.defaultFormatter": "jakebecker.elixir-ls"
  },
  "files.associations": {
    "*.exs": "elixir",
    "*.heex": "html"
  }
}
```

### IDE Configuration

#### Emacs with Elixir Mode

```elisp
;; Install elixir-mode and lsp-mode
(use-package elixir-mode
  :ensure t)

(use-package lsp-mode
  :ensure t
  :hook (elixir-mode . lsp))

(use-package lsp-ui
  :ensure t)
```

#### Vim/Neovim with Coc

```vim
" Install coc-elixir and coc-json
:CocInstall coc-elixir coc-json

" Add to .vimrc
autocmd BufEnter *.ex setlocal filetype=elixir
autocmd BufEnter *.exs setlocal filetype=elixir
autocmd BufEnter *.heex setlocal filetype=heex
```

## 🔧 Code Quality Tools

### Available Mix Aliases

#### Quick Quality Checks
- `mix quality.quick` - Run credo and dialyzer checks (fast)
- `mix quality.check` - Run formatting + credo + dialyzer
- `mix quality.full` - Complete quality check including tests and security

#### Individual Tools
- `mix credo.check` - Run strict credo analysis
- `mix credo.suggest` - Get credo suggestions
- `mix credo.fix` - Auto-format and run credo
- `mix dialyzer.check` - Run dialyzer type checking
- `mix sobelow.check` - Run security analysis
- `mix format` - Format code according to project standards
- `mix test` - Run the test suite
- `mix cover` - Generate test coverage report

### Credo Configuration

The project uses strict credo analysis with comprehensive checks:

```elixir
# .credo.exs
%{
  configs: [
    %{
      name: "default",
      files: %{
        included: ["lib/", "src/", "web/", "test/"],
        excluded: [~r"/_build/", ~r"/deps/", ~r"/node_modules/", ~r"/priv/static/"]
      },
      checks: [
        {Credo.Check.Design.DuplicatedCode, exit_status: 0},
        {Credo.Check.Readability.MaxLineLength, max_length: 120},
        {Credo.Check.Design.AliasUsage, if_nested_deeper_than: 3},
        {Credo.Check.Design.TagTODO, exit_status: 0},
        {Credo.Check.Design.TagFIXME, exit_status: 0},
        {Credo.Check.Design.TagNOTE, exit_status: 0},
        {Credo.Check.Refactor.FunctionArity, max_arity: 5},
        {Credo.Check.Refactor.CyclomaticComplexity, max_complexity: 10},
        {Credo.Check.Refactor.Nesting, max_nesting: 3},
        {Credo.Check.Readability.Specs, exit_status: 0},
        {Credo.Check.Readability.ParenthesesOnZeroArityDefs, exit_status: 0},
        {Credo.Check.Readability.ParenthesesAroundCondition, exit_status: 0},
        {Credo.Check.Readability.ParenthesesAroundArguments, exit_status: 0},
        {Credo.Check.Readability.TrailingBlankLine, exit_status: 0},
        {Credo.Check.Readability.TrailingWhiteSpace, exit_status: 0},
        {Credo.Check.Readability.SpaceAroundOperators, exit_status: 0},
        {Credo.Check.Readability.SpaceAfterComma, exit_status: 0},
        {Credo.Check.Readability.SinglePipe, exit_status: 0},
        {Credo.Check.Readability.StringSigils, exit_status: 0},
        {Credo.Check.Readability.UnnecessaryAliasExpansion, exit_status: 0},
        {Credo.Check.Readability.VariableNaming, prefix: ["_", "__"], exit_status: 0},
        {Credo.Check.Readability.FunctionNames, exit_status: 0},
        {Credo.Check.Readability.ModuleDoc, exit_status: 0},
        {Credo.Check.Readability.MultiAlias, exit_status: 0},
        {Credo.Check.Readability.ModuleAttribute, exit_status: 0},
        {Credo.Check.Readability.ImplTrue, exit_status: 0},
        {Credo.Check.Readability.ImplSpecs, exit_status: 0},
        {Credo.Check.Readability.ImplPipe, exit_status: 0},
        {Credo.Check.Readability.ImplAnonymousFunctions, exit_status: 0},
        {Credo.Check.Readability.ImplArity, exit_status: 0},
        {Credo.Check.Readability.ImplNoAlias, exit_status: 0},
        {Credo.Check.Readability.Impl, exit_status: 0},
        {Credo.Check.Readability.ImplTrue, exit_status: 0},
        {Credo.Check.Readability.ImplSpecs, exit_status: 0},
        {Credo.Check.Readability.ImplPipe, exit_status: 0},
        {Credo.Check.Readability.ImplAnonymousFunctions, exit_status: 0},
        {Credo.Check.Readability.ImplArity, exit_status: 0},
        {Credo.Check.Readability.ImplNoAlias, exit_status: 0},
        {Credo.Check.Readability.Impl, exit_status: 0}
      ]
    }
  ]
}
```

### Dialyzer Configuration

```elixir
# mix.exs
dialyzer: [
  plt_file: {:no_warn, "priv/plts/dialyzer.plt"},
  flags: [:error_handling, :underspecs, :unknown],
  ignore_warnings: ".dialyzer_ignore.exs",
  plt_add_apps: [:mix, :eex, :iex, :ex_unit],
  plt_core_path: "priv/plts",
  check_plt: true
]
```

## 🔄 Pre-commit Hooks

### Setup Pre-commit Hooks

1. **Install pre-commit** (if not already installed):
```bash
pip install pre-commit
```

2. **Create pre-commit configuration** (`.pre-commit-config.yaml`):
```yaml
repos:
  - repo: https://github.com/pre-commit/pre-commit-hooks
    rev: v4.4.0
    hooks:
      - id: trailing-whitespace
      - id: end-of-file-fixer
      - id: check-yaml
      - id: check-added-large-files
        args: ['--maxkb=1000']

  - repo: https://github.com/juancarlospaco/elixir-pre-commit-hooks
    rev: v1.0.0
    hooks:
      - id: mix-format
      - id: mix-credo
        args: ['--strict']

  - repo: https://github.com/renatoGarcia/elixir-pre-commit
    rev: v0.1.0
    hooks:
      - id: mix-dialyzer
```

3. **Install the git hooks**:
```bash
pre-commit install
```

### Pre-commit Configuration

The pre-commit hooks are configured to run:
- **Elixir formatting** (`mix format`)
- **Credo checks** (`mix credo.check`)
- **Type checking** (`mix dialyzer.check`)
- **File cleanup** (trailing whitespace, end of file)

### Skipping Pre-commit Hooks

To skip pre-commit hooks (use sparingly):
```bash
git commit --no-verify
```

## 🚀 Development Workflow

### 1. Feature Development

```bash
# Create a feature branch from main
git checkout -b feature/your-feature-name

# Make your changes
# ...

# Run quality checks before committing
mix quality.quick

# If formatting is needed
mix format
mix credo.fix

# Test your changes
mix test

# Commit your changes
git add .
git commit -m "feat: add your feature description"
```

### 2. Code Quality Process

#### Before Committing
1. **Format code**: `mix format`
2. **Run credo**: `mix credo.check`
3. **Type check**: `mix dialyzer.check`
4. **Test changes**: `mix test`
5. **Check security**: `mix sobelow.check` (optional for features)

#### Fixing Issues
```bash
# Auto-format and fix simple issues
mix credo.fix

# Get suggestions for improvements
mix credo.suggest

# Run specific checks
mix credo.check --only Readability
mix credo.check --only Warnings

# Check specific issue types
mix credo explain <filename>

# Build PLT if needed
mix dialyzer plt

# Check with specific focus
mix dialyzer.check --warnings
```

### 3. Pull Request Process

#### PR Checklist
- [ ] All quality checks pass: `mix quality.full`
- [ ] Tests are updated and passing
- [ ] Documentation is updated for new public APIs
- [ ] Code follows project standards
- [ ] Changes are tested in development environment
- [ ] PR title follows conventional commits format
- [ ] PR description includes context and reasoning

#### PR Quality Gates
The CI/CD pipeline will automatically run:
- Code formatting checks
- Credo strict analysis
- Dialyzer type checking
- Security analysis (Sobelow)
- Full test suite
- Test coverage analysis

## 🧪 Testing

### Test Structure

```
test/
├── riva_ash/              # Core application tests
│   ├── accounts/          # Account management tests
│   ├── resources/         # Resource tests
│   ├── policies/          # Policy tests
│   └── reactors/          # Reactor tests
├── riva_ash_web/          # Web layer tests
│   ├── components/        # Component tests
│   ├── controllers/       # Controller tests
│   └── live/              # LiveView tests
└── support/               # Test support files
    ├── mocks/             # Mock modules
    └── fixtures.ex        # Test fixtures
```

### Running Tests

```bash
# Run all tests
mix test

# Run tests with coverage
mix test --cover

# Run specific test file
mix test test/riva_ash/resource_test.exs

# Run specific test
mix test test/riva_ash/resource_test.exs:42

# Run tests in watch mode
mix test --stale

# Run tests without database setup
SKIP_DB=true mix test

# Run tests with specific seed
mix test --seed 12345
```

### Property-Based Testing

```bash
# Run property-based tests
mix test test/riva_ash/property/

# Run specific property test
mix test test/riva_ash/property/resource_property_test.exs

# Run with verbose output
mix test --include property --trace
```

### Test Coverage

```bash
# Generate coverage report
mix cover

# Open coverage report in browser
mix cover.html

# Run tests with coverage threshold
mix test --cover --minimum-coverages 80
```

## 🔧 Database Development

### Database Operations

```bash
# Create database
mix ecto.create

# Run migrations
mix ecto.migrate

# Reset database
mix ecto.reset

# Rollback migration
mix ecto.rollback

# Generate migration
mix ecto.gen.migration migration_name

# Generate Ash migration
mix ash.gen.migration Resource

# Create and seed database
mix ecto.setup

# Dump database schema
mix ecto.dump

# Load database schema
mix ecto.load
```

### Database Seeding

```bash
# Run seeds
mix run priv/repo/seeds.exs

# Generate seeder
mix phx.gen.seed Resource attributes

# Test database setup
mix test --include integration
```

## 🎨 Frontend Development

### Asset Management

```bash
# Install asset dependencies
mix assets.setup

# Build assets for development
mix assets.build

# Build assets for production
mix assets.deploy

# Watch assets during development
mix phx.server

# Run Tailwind CSS
mix tailwind default

# Run Esbuild
mix esbuild default
```

### Storybook Development

```bash
# Start Storybook
mix phx.storybook

# Build Storybook
mix phx.storybook.build

# Run Storybook tests
mix phx.storybook.test
```

### LiveView Development

```bash
# Start LiveView development server
mix phx.server

# Access LiveView debugging
# Visit http://localhost:4000/live/debug
```

## 📚 Documentation

### Generating Documentation

```bash
# Generate documentation
mix docs

# Open documentation in browser
mix docs.open

# Generate documentation with custom options
mix docs --output docs --source-ref main
```

### API Documentation

```bash
# Access Swagger UI
# Visit http://localhost:4000/docs

# Access AshAdmin
# Visit http://localhost:4000/admin
```

## 🛠️ Troubleshooting

### Common Issues

#### Credo Failures
```bash
# Check specific issue types
mix credo.check --only Readability
mix credo.check --only Warnings

# Get detailed explanations
mix credo explain <filename>

# Auto-fix common issues
mix credo.fix
```

#### Dialyzer Issues
```bash
# Build PLT if needed
mix dialyzer plt

# Check with specific focus
mix dialyzer.check --warnings

# Ignore specific warnings
echo '-spec my_function/1 :: any().' >> .dialyzer_ignore.exs
```

#### Formatting Issues
```bash
# Format all files
mix format

# Format specific file
mix format lib/your_file.ex

# Check formatting without applying
mix format --check-formatted
```

#### Database Issues
```bash
# Reset database
mix ecto.reset

# Check database connection
mix ecto.migrate

# View database logs
mix phx.server
```

#### Asset Compilation Issues
```bash
# Clean and rebuild assets
mix assets.clean
mix assets.setup
mix assets.build

# Install Node.js dependencies
cd assets && npm install
```

### Performance Tips

#### Fast Quality Checks
- Use `mix quality.quick` for quick feedback
- Run `mix credo.check` without dialyzer for faster feedback
- Use `mix credo --only [category]` for specific checks
- Use `mix test --stale` for incremental testing

#### Development Performance
- Use `mix phx.server` with code reloading
- Enable LiveView debugging for development
- Use `mix test --trace` for debugging test failures
- Configure proper database pool sizes

## 📋 Code Standards

### Naming Conventions
- **Modules**: PascalCase (e.g., `RivaAsh.UserService`)
- **Functions**: snake_case (e.g., `create_user/1`)
- **Variables**: snake_case (e.g., `user_name`)
- **Atoms**: snake_case (e.g., `:active_status`)
- **Unused variables**: Prefix with underscore (e.g., `_unused_var`)

### Documentation Standards
- **Public modules**: Include `@moduledoc` with purpose
- **Public functions**: Include `@doc` with purpose, parameters, and return values
- **Complex functions**: Include examples and edge cases
- **Type specs**: Use `@spec` for all public functions

### Function Complexity
- **Target complexity**: ≤ 10
- **Maximum complexity**: ≤ 15 (with justification)
- **Parameter count**: ≤ 5 (use structs for complex data)
- **Line length**: ≤ 120 characters

### Error Handling
- Use `{:ok, result}` and `{:error, reason}` tuples
- Include context in error messages
- Use `with` statements for sequential operations
- Handle Ash errors with proper error mapping

### Ash Best Practices
```elixir
# Good: Use Ash resources with proper attributes
defmodule RivaAsh.Resources.Resource do
  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer

  attributes do
    uuid_primary_key :id
    attribute :name, :string, allow_nil?: false
    attribute :status, :atom, default: :active, constraints: [one_of: [:active, :inactive]]
    timestamps()
  end

  # Good: Use proper relationships
  relationships do
    belongs_to :business, RivaAsh.Resources.Business
    has_many :reservations, RivaAsh.Resources.Reservation
  end

  # Good: Use proper actions
  actions do
    defaults [:read, :create, :update, :destroy]

    # Good: Use custom actions with proper validation
    action :activate, :boolean do
      argument :resource_id, :uuid, allow_nil?: false
      run fn input, context ->
        # Implementation
      end
    end
  end

  # Good: Use proper policies
  policies do
    # Good: Use policy breakdowns for debugging
    policy action(:read) do
      authorize_if always()
    end

    policy action(:create) do
      authorize_if relates_to_actor(:business, :owner)
    end
  end
end
```

### Phoenix Best Practices
```elixir
# Good: Use proper controller structure
defmodule RivaAshWeb.ResourceController do
  use RivaAshWeb, :controller

  # Good: Use proper action naming
  def index(conn, _params) do
    resources = RivaAsh.Resource.read!()
    render(conn, "index.json", resources: resources)
  end

  # Good: Use proper error handling
  def create(conn, %{"data" => resource_params}) do
    case RivaAsh.Resource.create(resource_params) do
      {:ok, resource} ->
        conn
        |> put_status(:created)
        |> render("show.json", resource: resource)

      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> render("error.json", changeset: changeset)
    end
  end
end
```

## 📚 Resources

### Documentation
- [Elixir Documentation](https://hexdocs.pm/elixir/)
- [Phoenix Framework Documentation](https://hexdocs.pm/phoenix/)
- [Ash Framework Documentation](https://ash-hq.org/docs/guides/ash/3.5.0)
- [Ecto Documentation](https://hexdocs.pm/ecto/)
- [LiveView Documentation](https://hexdocs.phoenixframework.org/phoenix_live_view/)

### Tools
- [Credo Documentation](https://hexdocs.pm/credo/)
- [Dialyzer Documentation](https://hexdocs.pm/dialyzer/)
- [Sobelow Documentation](https://github.com/nccgroup/sobelow)
- [ExCoveralls Documentation](https://hexdocs.pm/excoveralls/)
- [Tailwind CSS Documentation](https://tailwindcss.com/docs)

### Community
- [Elixir Forum](https://elixirforum.com/)
- [Elixir Slack](https://elixir-slackin.herokuapp.com/)
- [Phoenix Framework Discord](https://discord.gg/phoenixframework)
- [Ash Framework Discord](https://discord.gg/ashframework)

## 🤝 Getting Help

### Team Communication
- Join the #development channel for questions
- Create issues for bugs or feature requests
- Review existing PRs to understand standards
- Participate in code reviews

### Code Review Process
1. Submit PR with clear description
2. Ensure all quality checks pass
3. Address review comments promptly
4. Get approval from at least one team member
5. Merge after CI/CD passes

### Debugging Techniques
```bash
# Enable debug logging
export RIVA_ASH_DEBUG=1

# Run with verbose output
mix phx.server --verbose

# Use LiveView debugging
# Visit http://localhost:4000/live/debug

# Use IEx in production
# Attach to running process with :observer
```

## 📝 Contributing Guidelines

### Development Setup
1. Fork the repository
2. Create a feature branch
3. Follow the development workflow
4. Ensure all quality checks pass
5. Submit a pull request

### Code Style
- Follow the Elixir Style Guide
- Use consistent formatting with `mix format`
- Write comprehensive tests
- Document public APIs
- Use meaningful commit messages

### Pull Request Guidelines
- Use conventional commits format
- Include detailed description
- Link related issues
- Ensure CI/CD passes
- Address review comments promptly

---

*This document should be kept up to date with the project's development practices and tooling. For the most current information, please visit the [GitHub repository](https://github.com/your-org/riva-ash).*