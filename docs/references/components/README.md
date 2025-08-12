# Component Specifications

This directory contains detailed specifications for UI components used in the Riva Ash application. Each specification provides comprehensive information about component APIs, styling, accessibility considerations, and usage examples.

## Component Categories

### Form Components
- [Input Component](../input_spec.md) - Versatile text input with validation states
- [Textarea Component](../textarea_spec.md) - Multi-line text input with character limits
- [Checkbox Component](../checkbox_spec.md) - Boolean selection with labels and validation
- [Select Component](../select_spec.md) - Dropdown selection with options and prompts
- [Radio Component](../radio_spec.md) - Single selection from multiple options

### Action Components
- [Button Component](../button_spec.md) - Interactive buttons with variants and states
- [Toggle Component](../toggle_spec.md) - Switch/checkbox toggle for boolean values
- [Action Menu Component](../action_menu_spec.md) - Dropdown contextual menus

### Layout Components
- [Card Components](../card_spec.md) - Content containers with headers, titles, descriptions, content, and footers
- [Card Header](../card_header_spec.md) - Header section for card components
- [Card Title](../card_title_spec.md) - Title component for card headers
- [Card Description](../card_description_spec.md) - Description component for card headers
- [Card Content](../card_content_spec.md) - Main content area for cards
- [Card Footer](../card_footer_spec.md) - Footer section for cards with actions

### Status Components
- [Alert Component](../alert_spec.md) - Important messages with different severity levels
- [Badge Component](../badge_spec.md) - Status and labeling indicators
- [Spinner Component](../spinner_spec.md) - Loading indicators with various sizes
- [Status Indicator Component](../status_indicator_spec.md) - Visual status representation
- [Notification Toast Component](../notification_toast_spec.md) - Temporary notification messages

### Navigation Components
- [Breadcrumb Navigation Component](../breadcrumb_nav_spec.md) - Hierarchical navigation display
- [Tab Navigation Component](../tab_navigation_spec.md) - Tabbed interface navigation

### Data Display Components
- [Data Table Component](../data_table_spec.md) - Tabular data display with sorting and pagination
- [Avatar Component](../avatar_spec.md) - User or business image display with fallbacks

### Interactive Components
- [Tooltip Component](../tooltip_spec.md) - Contextual help text on hover or focus
- [Date Picker Component](../date_picker_spec.md) - Date selection with calendar popup
- [Time Picker Component](../time_picker_spec.md) - Time selection interface
- [Icon Component](../icon_spec.md) - SVG icon rendering with consistent sizing

### Business Components
- [Business Card Component](../business_card_spec.md) - Organization information display
- [Business Form Component](../business_form_spec.md) - Organization creation and editing forms
- [Client Form Component](../client_form_spec.md) - Client management forms
- [Employee Form Component](../employee_form_spec.md) - Employee management forms
- [Item Form Component](../item_form_spec.md) - Item/asset management forms
- [Pricing Form Component](../pricing_form_spec.md) - Pricing configuration forms
- [Reservation Form Component](../reservation_form_spec.md) - Booking management forms

### Layout & UI Components
- [Layout Designer Component](../layout_designer_spec.md) - Visual layout creation interface
- [Page Header Component](../page_header_spec.md) - Page title and header sections
- [Empty State Component](../empty_state_spec.md) - No data placeholder displays
- [Filter Panel Component](../filter_panel_spec.md) - Data filtering interface
- [Pagination Component](../pagination_spec.md) - Data navigation controls
- [Progress Bar Component](../progress_bar_spec.md) - Progress indication
- [Dashboard Stats Component](../dashboard_stats_spec.md) - Statistical displays

### Dialog Components
- [Confirm Dialog Component](../confirm_dialog_spec.md) - Confirmation dialogs for destructive actions

## Component Implementation Status

### ✅ Fully Implemented
- Button, Input, Checkbox, Select components (following UI library patterns)
- Card components and sub-components
- Alert and Badge components
- Avatar, Icon, Spinner components
- Textarea component

### 🔄 In Progress
- Date Picker, Time Picker components
- Radio, Toggle components
- Tooltip component
- Data Table component

### 📋 Planned
- Modal component
- Popover component
- Dropdown Menu component
- Progress component
- Skeleton component
- Label component
- Form component
- Tabs component
- Accordion component

## Design System Integration

All components follow the Riva Ash design system with:
- Consistent CSS variable usage for theming
- Proper accessibility support with ARIA attributes
- Responsive design patterns
- Consistent spacing and typography
- Standardized color variants

## Component Development Guidelines

When creating new components:
1. Follow the established specification format
2. Include comprehensive accessibility considerations
3. Use CSS variables for theming
4. Provide multiple size variants where appropriate
5. Include proper error states and validation support
6. Add comprehensive usage examples
7. Ensure proper form field integration

## Related Documentation

- [Component Library Plan](../component_library_plan.md) - Overall component library strategy
- [Additional Components Plan](../planning/additional_components_plan.md) - Implementation roadmap
- [UI Guidelines](../ui-guidelines.md) - Design system and styling guidelines
- [Development Guide](../DEVELOPMENT_GUIDE.md) - General development practices