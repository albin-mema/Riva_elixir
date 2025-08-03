# UI Guidelines

## UI Component Architecture

**Atomic Design Pattern**: All UI components follow atomic design:
- **Atoms**: `lib/riva_ash_web/components/atoms/` (Button, Input, Text, etc.)
- **Molecules**: `lib/riva_ash_web/components/molecules/` (Card, FormField, etc.)
- **Organisms**: `lib/riva_ash_web/components/organisms/` (DataTable, CalendarView, etc.)

**Custom Components**: Always create reusable custom components instead of inline HTML.

**Flop Integration**: Use Flop library for ALL table functionality and pagination.

**Form Handling**: Use AshPhoenix.Form for all form operations with proper validation.

## UI Development

**LiveView First**: Prefer LiveView over React components unless specific interactivity is needed.

**Storybook**: Use `phoenix_storybook` for component documentation and development.

**Styling**: Use Tailwind CSS exclusively - no custom CSS.

**Tables**: Always use Flop library for table functionality.

## Component Patterns
- **Stateful Combinations**: Use LiveView to manage state across multiple atomic components
- **Validation Flows**: Implement real-time validation using Phoenix form handling
- **Paginated Data**: Combine tables with pagination controls for large datasets

## Common Patterns

### 3. UI Component Creation

1. Follow atomic design hierarchy
2. Create in appropriate component directory
3. Add to Storybook
4. **MANDATORY**: Comprehensive test suite:
   - Property-based tests for component props with random valid values
   - Interaction tests using `phoenix_test`
   - Accessibility tests for proper ARIA attributes
   - Responsive design tests across different screen sizes
   - Form validation tests with random invalid inputs
   - LiveView event handling tests
5. Document props and usage in Storybook

## Debugging and Development Tools

### 1. Available Tools
- **AshAdmin**: Web-based admin interface at `/admin`
- **GraphQL Playground**: Available for API exploration
- **LiveView Debugger**: Use for LiveView debugging
- **Ash Console**: `iex -S mix` for interactive development

## Troubleshooting Common Issues

### 2. LiveView Issues
- Check socket connections
- Verify proper assigns usage
- Debug with LiveView debugger
- Test with different browsers