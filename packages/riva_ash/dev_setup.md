# Development Setup & Code Quality Workflow

## Quick Start

### 1. Install Dependencies
```bash
mix deps.get
mix assets.setup
```

### 2. Database Setup
```bash
mix ecto.setup
```

### 3. Run Quality Checks
```bash
# Quick checks (fast)
mix quality.quick

# Full quality check (includes formatting)
mix quality.check

# Complete quality check with tests
mix quality.full
```

## Code Quality Tools

### Available Mix Aliases

#### Quick Quality Checks
- `mix quality.quick` - Run credo and dialyzer checks
- `mix quality.check` - Run formatting + credo + dialyzer
- `mix quality.full` - Complete quality check including tests and security

#### Individual Tools
- `mix credo.check` - Run strict credo analysis
- `mix credo.suggest` - Get credo suggestions
- `mix credo.fix` - Auto-format and run credo
- `mix dialyzer.check` - Run dialyzer type checking
- `mix sobelow.check` - Run security analysis
- `mix format` - Format code according to project standards

### Credo Configuration

The project uses strict credo analysis with comprehensive checks:

- **Line length**: Maximum 120 characters
- **Function complexity**: Maximum cyclomatic complexity of 10
- **Parameter count**: Maximum 5 parameters per function
- **Nesting depth**: Maximum 3 levels deep
- **Documentation**: Public functions require documentation

## Pre-commit Hooks

### Setup Pre-commit Hooks

1. **Install pre-commit** (if not already installed):
```bash
pip install pre-commit
```

2. **Install the git hooks**:
```bash
pre-commit install
```

### Pre-commit Configuration

The `.pre-commit-config.yaml` file is configured to run:
- **Elixir formatting** (`mix format`)
- **Credo checks** (`mix credo.check`)
- **Type checking** (`mix dialyzer.check`)

### Skipping Pre-commit Hooks

To skip pre-commit hooks (use sparingly):
```bash
git commit --no-verify
```

## Development Workflow

### 1. Feature Development

```bash
# Create a feature branch
git checkout -b feature/your-feature-name

# Make your changes
# ...

# Run quality checks before committing
mix quality.quick

# If formatting is needed
mix format
mix credo.fix

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

#### Fixing Issues
```bash
# Auto-format and fix simple issues
mix credo.fix

# Get suggestions for improvements
mix credo.suggest

# Run specific checks
mix credo.check --only Readability
mix credo.check --only Warnings
```

### 3. Pull Request Process

#### PR Checklist
- [ ] All quality checks pass: `mix quality.full`
- [ ] Tests are updated and passing
- [ ] Documentation is updated for new public APIs
- [ ] Code follows project standards
- [ ] Changes are tested in development environment

#### PR Quality Gates
The CI/CD pipeline will automatically run:
- Code formatting checks
- Credo strict analysis
- Dialyzer type checking
- Security analysis (Sobelow)
- Full test suite

## Troubleshooting

### Common Issues

#### Credo Failures
```bash
# Check specific issue types
mix credo.check --only Readability
mix credo.check --only Warnings

# Get detailed explanations
mix credo explain <filename>
```

#### Dialyzer Issues
```bash
# Build PLT if needed
mix dialyzer plt

# Check with specific focus
mix dialyzer.check --warnings
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

### Performance Tips

#### Fast Quality Checks
- Use `mix quality.quick` for quick feedback
- Run `mix credo.check` without dialyzer for faster feedback
- Use `mix credo --only [category]` for specific checks

#### Incremental Testing
- Run specific tests: `mix test test/path/to/file_test.exs`
- Watch mode: `mix test --stale`
- Skip DB tests: `SKIP_DB=true mix test`

## Code Standards

### Naming Conventions
- **Modules**: PascalCase (e.g., `RivaAsh.UserService`)
- **Functions**: snake_case (e.g., `create_user/1`)
- **Variables**: snake_case (e.g., `user_name`)
- **Unused variables**: Prefix with underscore (e.g., `_unused_var`)

### Documentation Standards
- **Public modules**: Include `@moduledoc`
- **Public functions**: Include `@doc` with purpose and parameters
- **Complex functions**: Include examples and edge cases

### Function Complexity
- **Target complexity**: ≤ 10
- **Maximum complexity**: ≤ 15 (with justification)
- **Parameter count**: ≤ 5 (use structs for complex data)

### Error Handling
- Use `{:ok, result}` and `{:error, reason}` tuples
- Include context in error messages
- Use `with` statements for sequential operations

## Resources

### Documentation
- [Credo Documentation](https://hexdocs.pm/credo/)
- [Elixir Style Guide](https://hexdocs.pm/elixir/style-usage.html)
- [Ash Framework Documentation](https://ash-hq.org/docs/guides/ash/2.0.0)

### Tools
- [Elixir Formatter](https://hexdocs.pm/mix/Mix.Tasks.Format.html)
- [Dialyzer](https://hexdocs.pm/dialyzer/)
- [Sobelow](https://github.com/nccgroup/sobelow)

## Getting Help

### Team Communication
- Join the #development channel for questions
- Create issues for bugs or feature requests
- Review existing PRs to understand standards

### Code Review Process
1. Submit PR with clear description
2. Ensure all quality checks pass
3. Address review comments promptly
4. Get approval from at least one team member
5. Merge after CI/CD passes

---

*This document should be kept up to date with the project's development practices and tooling.*
content>
<line_count>167</line_count>
</write_to_file>