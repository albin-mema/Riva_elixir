# Atomic Components Migration Plan

## Overview

This document outlines the plan for removing hardcoded styles throughout the project and replacing them with atomic component usage based on the new design system.

## Current State Analysis

The project currently has:
1. Hardcoded Tailwind classes scattered throughout templates and components
2. Inconsistent styling approaches
3. Direct usage of HTML elements instead of atomic components
4. Custom CSS in various files

## Migration Goals

1. Replace hardcoded styles with atomic component usage
2. Maintain consistent design system application
3. Improve code maintainability
4. Ensure accessibility compliance
5. Preserve existing functionality

## Migration Strategy

### 1. Component-by-Component Replacement

#### Buttons
Replace hardcoded button styles with the new `UI.Button` component:

##### Before
```heex
<button class="px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600">
  Click me
</button>
```

##### After
```heex
<UI.Button>
  Click me
</UI.Button>
```

#### Inputs
Replace hardcoded input styles with the new `UI.Input` component:

##### Before
```heex
<input type="text" class="border border-gray-300 rounded px-3 py-2 w-full" />
```

##### After
```heex
<UI.Input />
```

#### Forms
Replace form elements with appropriate UI components:

##### Before
```heex
<form>
  <div class="mb-4">
    <label class="block text-gray-700 text-sm font-bold mb-2">Name</label>
    <input type="text" class="border border-gray-300 rounded px-3 py-2 w-full" />
  </div>
  <div class="mb-4">
    <label class="flex items-center">
      <input type="checkbox" class="rounded border-gray-300" />
      <span class="ml-2 text-gray-700">Accept terms</span>
    </label>
  </div>
  <button type="submit" class="px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600">
    Submit
  </button>
</form>
```

##### After
```heex
<form>
  <div class="mb-4">
    <UI.Label>Name</UI.Label>
    <UI.Input />
  </div>
  <div class="mb-4">
    <UI.Checkbox label="Accept terms" />
  </div>
  <UI.Button type="submit">
    Submit
  </UI.Button>
</form>
```

### 2. Directory-by-Directory Migration

#### Components Directory
- `packages/riva_ash/lib/riva_ash_web/components/atoms/`
- `packages/riva_ash/lib/riva_ash_web/components/molecules/`
- `packages/riva_ash/lib/riva_ash_web/components/organisms/`
- `packages/riva_ash/lib/riva_ash_web/components/templates/`

#### Templates Directory
- `packages/riva_ash/lib/riva_ash_web/components/core/layouts/`
- `packages/riva_ash/lib/riva_ash_web/components/layouts/`

#### Live Views
- `packages/riva_ash/lib/riva_ash_web/live/`

#### Storybook
- `packages/riva_ash/storybook/`

### 3. File-by-File Migration Process

1. Identify files with hardcoded styles
2. Replace with atomic components
3. Test functionality
4. Update stories if needed
5. Document changes

## Migration Priorities

### High Priority (Core Components)
1. Authentication components (`core/auth/`)
2. Layout components (`core/layouts/`)
3. Form components (`forms/`)
4. Navigation components (`navigation/`)

### Medium Priority (Business Components)
1. Business components (`business/`)
2. Interactive components (`interactive/`)
3. Organism components (`organisms/`)

### Low Priority (Supporting Components)
1. Atoms (`atoms/`)
2. Molecules (`molecules/`)
3. Templates (`templates/`)

## Implementation Steps

### Phase 1: Foundation
1. Complete implementation of new UI components
2. Create component stories
3. Document component usage
4. Set up testing framework

### Phase 2: Core Components Migration
1. Migrate authentication components
2. Migrate layout components
3. Migrate form components
4. Migrate navigation components

### Phase 3: Business Components Migration
1. Migrate business components
2. Migrate interactive components
3. Migrate organism components

### Phase 4: Supporting Components Migration
1. Migrate atoms
2. Migrate molecules
3. Migrate templates

### Phase 5: Cleanup
1. Remove old component files
2. Update documentation
3. Final testing

## Risk Mitigation

### Backward Compatibility
- Maintain old components during transition
- Provide migration guides
- Ensure gradual migration is possible

### Testing Strategy
- Visual regression testing
- Functional testing
- Accessibility testing
- Performance testing

### Rollback Plan
- Git version control
- Feature flags where possible
- Incremental deployment

## Tools and Techniques

### Code Analysis
- Use regex to find hardcoded styles
- Identify common patterns
- Create migration scripts where possible

### Automated Replacement
- Create codemods for common patterns
- Use search and replace for simple cases
- Manual review for complex cases

### Testing
- Visual diff tools
- Automated testing suites
- Manual QA process

## Timeline

### Week 1-2: Foundation
- Complete UI component implementation
- Create stories and documentation

### Week 3-4: Core Components
- Migrate authentication components
- Migrate layout components

### Week 5-6: Form and Navigation
- Migrate form components
- Migrate navigation components

### Week 7-8: Business Components
- Migrate business components
- Migrate interactive components

### Week 9-10: Remaining Components
- Migrate remaining components
- Cleanup and testing

## Success Metrics

1. 100% of components use atomic component approach
2. Consistent design system application
3. Improved code maintainability
4. No regression in functionality
5. Positive accessibility audit results