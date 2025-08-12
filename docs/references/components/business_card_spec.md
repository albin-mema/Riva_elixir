# Organization Card Component Specification

## Component Name
Organization Card

## Description
An organization card component for displaying organization information in a structured card format. Shows key details about an organization including name, description, contact information, and status.

## Props/Attributes
| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `organization` | map | %{} | Organization data with id, name, description, address, phone, email, website, organization_type, is_active, inserted_at, updated_at, and owner_id |
| `current_user` | map | %{} | Current user data with id and email |
| `is_admin` | boolean | false | Whether the current user is an admin |
| `on_edit` | string | "" | Event to send when edit action is triggered |
| `on_delete` | string | "" | Event to send when delete action is triggered |
| `class` | string | "" | Additional CSS classes to apply |
| `rest` | global attributes | - | Any additional HTML attributes |

## Slots
This component does not use slots.

## CSS Classes and Styling Approach
### Base Classes
The component uses a card container with structured sections for business information, applying consistent styling to all elements.

### CSS Variables Used
- `--card` / `--card-foreground` - Background and text colors for card
- `--border` - Border color
- `--primary` / `--primary-foreground` - Colors for primary actions
- `--destructive` / `--destructive-foreground` - Colors for destructive actions
- `--muted` / `--muted-foreground` - Colors for secondary text
- `--radius` - Border radius

## Accessibility Considerations
- Semantic HTML structure with appropriate elements
- Sufficient color contrast for all text elements
- Proper heading hierarchy (h3 for business name)
- Contact information uses appropriate semantic elements
- Action buttons have visible focus states
- Inactive businesses are clearly indicated
- Screen reader-friendly status indicators
- Links are properly labeled for assistive technology

## Usage Examples
```heex
<!-- Basic business card -->
<.business_card
  business={%{
    id: "123e4567-e89b-12d3-a456-426614174000",
    name: "Sunny Beach Resort",
    description: "A beautiful beachfront resort with stunning ocean views and world-class amenities.",
    address: "123 Ocean Drive, Miami Beach, FL 33139",
    phone: "+1 (305) 555-0123",
    email: "info@sunnybeachresort.com",
    website: "https://sunnybeachresort.com",
    business_type: "resort",
    is_active: true,
    inserted_at: ~N[2024-01-15 10:30:00],
    updated_at: ~N[2024-01-15 10:30:00],
    owner_id: "123e4567-e89b-12d3-a456-426614174000"
  }}
  current_user={%{
    id: "123e4567-e89b-12d3-a456-426614174000",
    email: "owner@sunnybeachresort.com"
  }}
  is_admin={true}
  on_edit="edit_business"
  on_delete="delete_business"
/>

<!-- Inactive business card -->
<.business_card
  business={%{
    id: "789e0123-e89b-12d3-a456-426614174002",
    name: "Closed Venue",
    description: "This business is currently inactive.",
    address: "789 Closed Street, Nowhere, NY 10001",
    phone: "+1 (555) 999-0000",
    email: "contact@closedvenue.com",
    website: nil,
    business_type: "venue",
    is_active: false,
    inserted_at: ~N[2023-12-01 09:00:00],
    updated_at: ~N[2024-01-01 12:00:00],
    owner_id: "different-user-id"
  }}
  current_user={%{
    id: "different-user-id",
    email: "user@example.com"
  }}
  is_admin={false}
  on_edit="edit_business"
  on_delete="delete_business"
/>

<!-- Business card with custom styling -->
<.business_card
  business={%{
    id: "abc123-def456-ghi789",
    name: "Custom Styled Business",
    description: "This business card has custom styling applied.",
    address: "123 Custom Ave, Style City, SC 12345",
    phone: "+1 (555) CUSTOM",
    email: "hello@customstyle.com",
    website: "https://customstyle.com",
    business_type: "other",
    is_active: true,
    inserted_at: ~N[2024-03-01 16:45:00],
    updated_at: ~N[2024-03-01 16:45:00],
    owner_id: "abc123-def456-ghi789"
  }}
  current_user={%{
    id: "abc123-def456-ghi789",
    email: "hello@customstyle.com"
  }}
  is_admin={true}
  on_edit="edit_business"
  on_delete="delete_business"
  class="border-2 border-purple-300 bg-purple-50"
/>
```

## Implementation Notes
- Uses Phoenix Component for rendering
- Supports global attributes for Phoenix LiveView events
- Consistent with the design system using CSS variables
- Follows accessibility best practices with semantic HTML
- Displays business information in a structured card format
- Shows active/inactive status with visual indicators
- Provides edit and delete actions based on user permissions
- Current user and admin status determine available actions
- Contact information is displayed with appropriate icons
- Business type is displayed for additional context
- Creation and update timestamps provide audit information
- Component adapts to different screen sizes with responsive design
- Action menu is provided for business management actions
- Inactive businesses are visually distinct from active ones