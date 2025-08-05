# Contributing to Riva Ash

Thank you for your interest in contributing to Riva Ash! This guide will help you get started with contributing to our business management system built with Elixir/Phoenix and Ash Framework.

## Quick Start for Contributors

1. **Fork and Clone**: Fork the repository and clone your fork
2. **Setup Environment**: Follow our [SETUP_GUIDE.md](SETUP_GUIDE.md) for complete setup
3. **Create Branch**: Create a feature branch for your changes
4. **Make Changes**: Implement your changes following our guidelines
5. **Test**: Run tests and ensure quality checks pass
6. **Submit**: Create a pull request with a clear description

## Development Environment Setup

### Prerequisites

- **Elixir** >= 1.19
- **Erlang** >= 27
- **Node.js** >= 18
- **PNPM** >= 8
- **PostgreSQL** >= 14 (or Docker Desktop)
- **Git**

### Quick Setup

```bash
# Clone your fork
git clone https://github.com/YOUR_USERNAME/Riva_elixir.git
cd Riva_elixir

# Start PostgreSQL
./docker-dev.sh start

# Setup dependencies and database
pnpm setup

# Start development server
pnpm dev
```

For detailed setup instructions, see [SETUP_GUIDE.md](SETUP_GUIDE.md).

## Development Workflow

### 1. Creating a Branch

```bash
# Create and switch to a new branch
git checkout -b feature/your-feature-name
# or
git checkout -b fix/issue-description
# or
git checkout -b docs/documentation-update
```

### 2. Making Changes

Follow our established patterns and conventions:

- **Code Style**: Use `mix format` for consistent formatting
- **Architecture**: Follow patterns documented in [packages/riva_ash/patterns.md](packages/riva_ash/patterns.md)
- **Testing**: Write comprehensive tests (aim for 90% coverage)
- **Documentation**: Update relevant documentation

### 3. Database Changes

When modifying Ash resources:

```bash
cd packages/riva_ash

# Generate migrations after resource changes
mix ash_postgres.generate_migrations

# Review generated migration files
# Run migrations
mix ecto.migrate

# Update seeds if schema changed
mix run priv/repo/seeds.exs
```

### 4. Code Quality Checks

Before committing, ensure your code passes all quality checks:

```bash
cd packages/riva_ash

# Format code
mix format

# Check formatting
mix format --check-formatted

# Run static analysis
mix credo --strict

# Run type checking
mix dialyzer

# Compile and check for warnings
mix compile --warnings-as-errors
```

### 5. Testing

Run the comprehensive test suite:

```bash
cd packages/riva_ash

# Run all tests
mix test

# Run with coverage
mix test --cover

# Run integration tests
mix test --include integration

# Run property-based tests
mix test --include property

# Run specific test file
mix test test/path/to/test_file.exs
```

### 6. Committing Changes

Use conventional commit messages:

```bash
# Examples
git commit -m "feat: add reservation cancellation feature"
git commit -m "fix: resolve time zone issue in booking"
git commit -m "docs: update API documentation"
git commit -m "test: add property tests for validation"
```

## Types of Contributions

### Bug Fixes

1. **Identify the Issue**: Clearly describe the bug and steps to reproduce
2. **Write a Test**: Create a failing test that demonstrates the bug
3. **Fix the Issue**: Implement the minimal fix required
4. **Verify**: Ensure the test passes and no regressions occur

### New Features

1. **Discuss First**: Open an issue to discuss the feature before implementation
2. **Design**: Consider how the feature fits with existing architecture
3. **Implement**: Follow established patterns and conventions
4. **Test Thoroughly**: Include unit, integration, and property-based tests
5. **Document**: Update relevant documentation and examples

### Documentation Updates

1. **Accuracy**: Ensure information is current and correct
2. **Clarity**: Write for developers of all experience levels
3. **Examples**: Include practical, working examples
4. **Consistency**: Follow existing documentation style and structure

### Architectural Patterns

For contributing to architectural patterns, see the detailed guidelines in [../packages/riva_ash/patterns.md](../packages/riva_ash/patterns.md#how-to-contribute-to-architectural-patterns).

## Code Standards

### Elixir/Phoenix Conventions

- Follow standard Elixir naming conventions
- Use `mix format` for consistent code formatting
- Write clear, descriptive function and variable names
- Include comprehensive documentation with `@doc` and `@spec`

### Ash Framework Conventions

- Define resources with clear policies and validations
- Use resource helpers from `RivaAsh.ResourceHelpers`
- Implement proper authorization with Ash policies
- Follow domain-driven design principles

### Testing Standards

- **Unit Tests**: Test individual functions and modules
- **Integration Tests**: Test resource interactions and workflows
- **Property-Based Tests**: Use StreamData for randomized testing
- **Coverage**: Aim for 90% test coverage
- **Performance**: Include benchmarks for performance-critical code

### Database Standards

- Use UUID primary keys for all resources
- Implement soft deletion with `AshArchival`
- Add audit trails with `AshPaperTrail`
- Use proper foreign key constraints
- Write reversible migrations

## Pull Request Process

### Before Submitting

1. **Rebase**: Rebase your branch on the latest main branch
2. **Test**: Ensure all tests pass locally
3. **Quality**: Run all code quality checks
4. **Documentation**: Update relevant documentation

### Pull Request Description

Include in your PR description:

- **Summary**: Brief description of changes
- **Motivation**: Why this change is needed
- **Changes**: Detailed list of modifications
- **Testing**: How you tested the changes
- **Screenshots**: For UI changes (if applicable)
- **Breaking Changes**: Any breaking changes and migration notes

### Review Process

1. **Automated Checks**: CI/CD pipeline must pass
2. **Code Review**: At least one maintainer review required
3. **Testing**: Reviewers may request additional tests
4. **Documentation**: Ensure documentation is updated
5. **Approval**: Maintainer approval required for merge

## Common Development Tasks

### Adding a New Resource

1. **Create the Resource**: Define in `lib/riva_ash/resources/`
2. **Add to Domain**: Include in `lib/riva_ash/domain.ex`
3. **Generate Migration**: Run `mix ash_postgres.generate_migrations`
4. **Add Tests**: Create comprehensive test coverage
5. **Update Documentation**: Add to relevant documentation

### Modifying Existing Resources

1. **Update Resource Definition**: Modify attributes, relationships, or actions
2. **Generate Migration**: Run `mix ash_postgres.generate_migrations`
3. **Update Tests**: Modify existing tests and add new ones
4. **Check Dependencies**: Ensure changes don't break dependent resources
5. **Update Seeds**: Modify seed data if necessary

### Adding UI Components

1. **Follow Atomic Design**: Place in appropriate component level (atoms/molecules/organisms)
2. **Use Storybook**: Add stories for component documentation
3. **Include Tests**: Use phoenix_test for UI testing
4. **Follow Patterns**: Use established component patterns
5. **Update Documentation**: Add component to relevant documentation

### Performance Optimization

1. **Profile First**: Use benchmarking tools to identify bottlenecks
2. **Database Optimization**: Add indexes, optimize queries
3. **Caching**: Implement appropriate caching strategies
4. **Load Testing**: Test under realistic load conditions
5. **Monitor**: Set up monitoring for production performance

## Troubleshooting

### Common Issues

**Database Connection Issues**:
```bash
# Check if PostgreSQL is running
./docker-dev.sh status

# Restart PostgreSQL
./docker-dev.sh restart

# Check database connectivity
mix ecto.migrate
```

**Test Failures**:
```bash
# Run tests with detailed output
mix test --trace

# Run specific failing test
mix test test/path/to/failing_test.exs:line_number

# Reset test database
MIX_ENV=test mix ecto.reset
```

**Compilation Issues**:
```bash
# Clean and recompile
mix clean
mix deps.clean --all
mix deps.get
mix compile
```

**Migration Issues**:
```bash
# Check migration status
mix ecto.migrations

# Rollback last migration
mix ecto.rollback

# Reset database (development only)
mix ecto.reset
```

### Getting Help

- **Issues**: Open an issue for bugs or feature requests
- **Discussions**: Use GitHub Discussions for questions
- **Documentation**: Check existing documentation first
- **Code Examples**: Look at existing code for patterns and conventions
- **Community**: Join our development community discussions

## Project Structure

```
Riva_elixir/
├── packages/riva_ash/          # Main Elixir/Phoenix application
│   ├── lib/riva_ash/          # Business logic and resources
│   ├── lib/riva_ash_web/      # Web layer (controllers, views, components)
│   ├── test/                  # Test files
│   ├── docs/                  # Project-specific documentation
│   └── patterns.md            # Architectural patterns
├── documentation/             # General project documentation
│   ├── CONTRIBUTING.md        # Contribution guide
│   ├── SETUP_GUIDE.md        # Environment setup guide
│   ├── DEVELOPMENT_WORKFLOW.md # Development workflow
│   └── ...                   # Other documentation files
└── README.md                 # Project overview
```

## License

By contributing to Riva Ash, you agree that your contributions will be licensed under the same license as the project.

---

Thank you for contributing to Riva Ash! Your contributions help make this project better for everyone.
