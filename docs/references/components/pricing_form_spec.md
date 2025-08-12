# Pricing Form Component Specification

## Component Name
Pricing Form

## Description
A form component for creating and editing pricing information for items. Provides a structured interface for users to input and update pricing details including item selection, price amount, currency, effective dates, and notes.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `form` | map | %{source: %{}, errors: []} | Form data with source fields and errors |
| `items` | list | [] | List of items for selection dropdown |
| `editing` | boolean | false | Whether the form is in edit mode |
| `loading` | boolean | false | Whether the form is in a loading state |
| `on_submit` | string | "" | Event to send when form is submitted |
| `on_change` | string | "" | Event to send when form values change |
| `on_cancel` | string | "" | Event to send when cancel action is triggered |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## Form Fields
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `item_id` | string | Yes | ID of the item being priced |
| `price_per_day` | string | Yes | Price per day for the item |
| `currency` | string | Yes | Currency code (e.g., USD, EUR) |
| `effective_from` | string | No | Start date for pricing (YYYY-MM-DD format) |
| `effective_until` | string | No | End date for pricing (YYYY-MM-DD format) |
| `notes` | string | No | Additional notes about the pricing |

## CSS Classes and Styling Approach
### Base Classes
The component uses a form container with structured sections for different field groups, applying consistent styling to all form elements.

### CSS Variables Used
- `--background` / `--foreground` - Background and text colors
- `--border` - Border color
- `--primary` / `--primary-foreground` - Colors for primary actions
- `--secondary` / `--secondary-foreground` - Colors for secondary actions
- `--destructive` / `--destructive-foreground` - Colors for destructive actions
- `--radius` - Border radius

## Accessibility Considerations
- Semantic HTML structure with form and fieldset elements
- Proper labeling of all form fields
- Focus management between form elements
- Error messages associated with fields using aria-describedby
- Sufficient color contrast for all text elements
- Form buttons have visible focus states
- Loading state indicated with aria-busy
- Field groups are organized with appropriate headings
- Screen reader-friendly error notifications
- Date fields have appropriate input types for assistive technology
- Currency fields use appropriate formatting

## Usage Examples
```heex
<!-- Create pricing form -->
<.pricing_form
  form={%{
    id: "pricing-form",
    source: %{
      item_id: "",
      price_per_day: "",
      currency: "USD",
      effective_from: "",
      effective_until: "",
      notes: ""
    },
    errors: []
  }}
  items={[
    %{id: "1", name: "Beach Umbrella"},
    %{id: "2", name: "Sunbed"},
    %{id: "3", name: "Cabin"}
  ]}
  editing={false}
  on_submit="save_pricing"
  on_change="validate_pricing"
  on_cancel="cancel_form"
  loading={false}
/>

<!-- Edit pricing form -->
<.pricing_form
  form={%{
    id: "pricing-form",
    source: %{
      item_id: "1",
      price_per_day: "25.00",
      currency: "USD",
      effective_from: "2024-06-01",
      effective_until: "2024-08-31",
      notes: "Summer pricing"
    },
    errors: []
  }}
  items={[
    %{id: "1", name: "Beach Umbrella"},
    %{id: "2", name: "Sunbed"},
    %{id: "3", name: "Cabin"}
  ]}
  editing={true}
  on_submit="save_pricing"
  on_change="validate_pricing"
  on_cancel="cancel_form"
  loading={false}
/>

<!-- Loading pricing form -->
<.pricing_form
  form={%{
    id: "pricing-form",
    source: %{
      item_id: "1",
      price_per_day: "25.00",
      currency: "USD",
      effective_from: "2024-06-01",
      effective_until: "2024-08-31",
      notes: "Summer pricing"
    },
    errors: []
  }}
  items={[
    %{id: "1", name: "Beach Umbrella"},
    %{id: "2", name: "Sunbed"},
    %{id: "3", name: "Cabin"}
  ]}
  editing={true}
  on_submit="save_pricing"
  on_change="validate_pricing"
  on_cancel="cancel_form"
  loading={true}
/>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- Consistent with the design system using CSS variables
- Follows accessibility best practices with semantic HTML
- Integrates with AshPhoenix forms for validation and error handling
- Form fields are organized into logical sections
- Item selection uses dropdown with provided item list
- Loading state provides visual feedback during form submission
- Edit mode shows existing pricing data for updates
- Create mode shows empty fields for new pricing entry
- Form validation is handled through on_change events
- Submit and cancel actions provide clear user interactions
- Error messages are displayed next to relevant fields
- Component adapts to different screen sizes with responsive design
- Price fields use numeric input with decimal support
- Currency field defaults to USD but can be changed
- Effective date fields allow for seasonal pricing
- Notes field provides space for additional information
- Date fields use appropriate input types for better UX