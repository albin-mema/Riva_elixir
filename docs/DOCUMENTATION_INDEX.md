# Riva Ash Documentation Index

This index provides a comprehensive overview of all documentation files in the Riva Ash project, organized by category and purpose. Use this index to quickly locate the information you need.

## 📚 Documentation Structure

### 🏗️ Architecture & Technical Overview

| Document | Path | Description | Audience |
|----------|------|-------------|----------|
| **Architecture Overview** | [`ARCHITECTURE.md`](./ARCHITECTURE.md) | Comprehensive system architecture with domain-driven design, stack details, and data flow | Developers, Architects |
| **Architecture Guidelines** | [`architecture-guidelines.md`](./architecture-guidelines.md) | Best practices and architectural principles for the Riva Ash system | Developers, Architects |
| **System Architecture** | [`ARCHITECTURE_OVERVIEW.md`](./ARCHITECTURE_OVERVIEW.md) | High-level architectural overview and system design | Developers, Architects |
| **Technical Documentation** | [`technical/spark_dsl_documentation.md`](./technical/spark_dsl_documentation.md) | Complete guide to the Spark Domain Specific Language | Developers |

### 🛠️ Development & Setup

| Document | Path | Description | Audience |
|----------|------|-------------|----------|
| **Development Guide** | [`DEVELOPMENT_GUIDE.md`](./DEVELOPMENT_GUIDE.md) | Comprehensive development setup, coding standards, and workflow | Developers |
| **Development Guide (Compact)** | [`DEVELOPMENT_GUIDE_COMPACT.md`](./DEVELOPMENT_GUIDE_COMPACT.md) | Quick reference for development practices | Developers |
| **Getting Started** | [`GETTING_STARTED.md`](./GETTING_STARTED.md) | Step-by-step installation and first-time setup | New Developers |
| **Development Setup** | [`DEVELOPMENT_SETUP.md`](./DEVELOPMENT_SETUP.md) | Detailed development environment configuration | Developers |
| **Development Workflow** | [`DEVELOPMENT_WORKFLOW.md`](./DEVELOPMENT_WORKFLOW.md) | Standard development processes and workflows | Developers |
| **Database Setup** | [`DATABASE_SETUP.md`](./DATABASE_SETUP.md) | Database configuration and setup instructions | Developers, DevOps |
| **Environment Variables** | [`ENVIRONMENT_VARIABLES.md`](./ENVIRONMENT_VARIABLES.md) | Configuration and environment setup | Developers, DevOps |
| **Configuration Guide** | [`CONFIGURATION.md`](./CONFIGURATION.md) | System configuration and customization | Developers, DevOps |
| **Security Configuration** | [`SECURITY_CONFIGURATION.md`](./SECURITY_CONFIGURATION.md) | Security settings and configuration | Security Team, DevOps |

### 🔌 API Documentation

| Document | Path | Description | Audience |
|----------|------|-------------|----------|
| **API Reference** | [`API_REFERENCE.md`](./API_REFERENCE.md) | Complete API documentation with endpoints, models, and examples | Developers, Integrators |
| **API Quick Reference** | [`API_REFERENCE_COMPACT.md`](./API_REFERENCE_COMPACT.md) | Essential API information for quick reference | Developers, Integrators |
| **GraphQL API Guide** | [`GRAPHQL_API_GUIDE.md`](./GRAPHQL_API_GUIDE.md) | GraphQL-specific API documentation and examples | Developers, Integrators |
| **JSON:API Guide** | [`JSON_API_GUIDE.md`](./JSON_API_GUIDE.md) | JSON:API specification implementation details | Developers, Integrators |
| **Authentication API** | [`AUTHENTICATION_API.md`](./AUTHENTICATION_API.md) | Authentication endpoints and security implementation | Developers, Security Team |

### 📋 Component Specifications

| Document | Path | Description | Audience |
|----------|------|-------------|----------|
| **Component Specifications** | [`components/README.md`](./components/README.md) | Overview of all UI components and their specifications | Developers, Designers |
| **Form Components** | | | |
| Input Component | [`input_spec.md`](./input_spec.md) | Versatile text input with validation states | Developers |
| Textarea Component | [`textarea_spec.md`](./textarea_spec.md) | Multi-line text input with character limits | Developers |
| Checkbox Component | [`checkbox_spec.md`](./checkbox_spec.md) | Boolean selection with labels and validation | Developers |
| Select Component | [`select_spec.md`](./select_spec.md) | Dropdown selection with options and prompts | Developers |
| Radio Component | [`radio_spec.md`](./radio_spec.md) | Single selection from multiple options | Developers |
| **Action Components** | | | |
| Button Component | [`button_spec.md`](./button_spec.md) | Interactive buttons with variants and states | Developers |
| Toggle Component | [`toggle_spec.md`](./toggle_spec.md) | Switch/checkbox toggle for boolean values | Developers |
| Action Menu Component | [`action_menu_spec.md`](./action_menu_spec.md) | Dropdown contextual menus | Developers |
| **Layout Components** | | | |
| Card Components | [`card_spec.md`](./card_spec.md) | Content containers with headers, titles, descriptions, content, and footers | Developers |
| Card Header | [`card_header_spec.md`](./card_header_spec.md) | Header section for card components | Developers |
| Card Title | [`card_title_spec.md`](./card_title_spec.md) | Title component for card headers | Developers |
| Card Description | [`card_description_spec.md`](./card_description_spec.md) | Description component for card headers | Developers |
| Card Content | [`card_content_spec.md`](./card_content_spec.md) | Main content area for cards | Developers |
| Card Footer | [`card_footer_spec.md`](./card_footer_spec.md) | Footer section for cards with actions | Developers |
| **Status Components** | | | |
| Alert Component | [`alert_spec.md`](./alert_spec.md) | Important messages with different severity levels | Developers |
| Badge Component | [`badge_spec.md`](./badge_spec.md) | Status and labeling indicators | Developers |
| Spinner Component | [`spinner_spec.md`](./spinner_spec.md) | Loading indicators with various sizes | Developers |
| Status Indicator Component | [`status_indicator_spec.md`](./status_indicator_spec.md) | Visual status representation | Developers |
| Notification Toast Component | [`notification_toast_spec.md`](./notification_toast_spec.md) | Temporary notification messages | Developers |
| **Navigation Components** | | | |
| Breadcrumb Navigation Component | [`breadcrumb_nav_spec.md`](./breadcrumb_nav_spec.md) | Hierarchical navigation display | Developers |
| Tab Navigation Component | [`tab_navigation_spec.md`](./tab_navigation_spec.md) | Tabbed interface navigation | Developers |
| **Data Display Components** | | | |
| Data Table Component | [`data_table_spec.md`](./data_table_spec.md) | Tabular data display with sorting and pagination | Developers |
| Avatar Component | [`avatar_spec.md`](./avatar_spec.md) | User or business image display with fallbacks | Developers |
| **Interactive Components** | | | |
| Tooltip Component | [`tooltip_spec.md`](./tooltip_spec.md) | Contextual help text on hover or focus | Developers |
| Date Picker Component | [`date_picker_spec.md`](./date_picker_spec.md) | Date selection with calendar popup | Developers |
| Time Picker Component | [`time_picker_spec.md`](./time_picker_spec.md) | Time selection interface | Developers |
| Icon Component | [`icon_spec.md`](./icon_spec.md) | SVG icon rendering with consistent sizing | Developers |
| **Business Components** | | | |
| Business Card Component | [`business_card_spec.md`](./business_card_spec.md) | Organization information display | Developers |
| Business Form Component | [`business_form_spec.md`](./business_form_spec.md) | Organization creation and editing forms | Developers |
| Client Form Component | [`client_form_spec.md`](./client_form_spec.md) | Client management forms | Developers |
| Employee Form Component | [`employee_form_spec.md`](./employee_form_spec.md) | Employee management forms | Developers |
| Item Form Component | [`item_form_spec.md`](./item_form_spec.md) | Item/asset management forms | Developers |
| Pricing Form Component | [`pricing_form_spec.md`](./pricing_form_spec.md) | Pricing configuration forms | Developers |
| Reservation Form Component | [`reservation_form_spec.md`](./reservation_form_spec.md) | Booking management forms | Developers |
| **Layout & UI Components** | | | |
| Layout Designer Component | [`layout_designer_spec.md`](./layout_designer_spec.md) | Visual layout creation interface | Developers |
| Page Header Component | [`page_header_spec.md`](./page_header_spec.md) | Page title and header sections | Developers |
| Empty State Component | [`empty_state_spec.md`](./empty_state_spec.md) | No data placeholder displays | Developers |
| Filter Panel Component | [`filter_panel_spec.md`](./filter_panel_spec.md) | Data filtering interface | Developers |
| Pagination Component | [`pagination_spec.md`](./pagination_spec.md) | Data navigation controls | Developers |
| Progress Bar Component | [`progress_bar_spec.md`](./progress_bar_spec.md) | Progress indication | Developers |
| Dashboard Stats Component | [`dashboard_stats_spec.md`](./dashboard_stats_spec.md) | Statistical displays | Developers |
| **Dialog Components** | | | |
| Confirm Dialog Component | [`confirm_dialog_spec.md`](./confirm_dialog_spec.md) | Confirmation dialogs for destructive actions | Developers |

### 📖 User Guides & Business Logic

| Document | Path | Description | Audience |
|----------|------|-------------|----------|
| **User Guide** | [`USER_GUIDE.md`](./USER_GUIDE.md) | Comprehensive end-user documentation and workflows | End Users |
| **Getting Started** | [`GETTING_STARTED.md`](./GETTING_STARTED.md) | First-time user setup and basic workflows | New Users |
| **Business Logic** | [`business/user_actions_business_logic.md`](./business/user_actions_business_logic.md) | Detailed business rules and data flow | Business Analysts, Developers |
| **Usage Rules** | [`usage_rules/`](./usage_rules/) | Various usage rules and guidelines for system components | All Users |

### 🔄 Workflows & Processes

| Document | Path | Description | Audience |
|----------|------|-------------|----------|
| **AI Agent Playbook** | [`workflows/ai_agent_playbook.md`](./workflows/ai_agent_playbook.md) | Comprehensive guide for AI agent operations and integration | AI Team, Developers |
| **Development Workflow** | [`DEVELOPMENT_WORKFLOW.md`](./DEVELOPMENT_WORKFLOW.md) | Standard development processes and workflows | Developers |

### 📊 Planning & Strategy

| Document | Path | Description | Audience |
|----------|------|-------------|----------|
| **Component Library Plan** | [`component_library_plan.md`](./component_library_plan.md) | Overall component library strategy and roadmap | Product Managers, Developers |
| **Additional Components Plan** | [`additional_components_plan.md`](./additional_components_plan.md) | Implementation roadmap for additional UI components | Product Managers, Developers |
| **Additional Components Implementation Plan** | [`additional_components_implementation_plan.md`](./additional_components_implementation_plan.md) | Detailed implementation plan for additional UI components | Developers, Project Managers |
| **Mission and Scope** | [`MISSION_AND_SCOPE.md`](./MISSION_AND_SCOPE.md) | Project mission, scope, and objectives | Stakeholders, Product Team |
| **Reservation System Assessment** | [`RESERVATION_SYSTEM_ASSESSMENT.md`](./RESERVATION_SYSTEM_ASSESSMENT.md) | Assessment of reservation system capabilities | Product Team, Stakeholders |
| **Reservation Center Design** | [`reservation-center-design.md`](./reservation-center-design.md) | Design specifications for reservation center | Designers, Developers |

### 🚀 Deployment & Operations

| Document | Path | Description | Audience |
|----------|------|-------------|----------|
| **Deployment Guide** | [`DEPLOYMENT_GUIDE.md`](./DEPLOYMENT_GUIDE.md) | Comprehensive deployment instructions | DevOps, Operations |
| **Deployment Guide (Compact)** | [`DEPLOYMENT_GUIDE_COMPACT.md`](./DEPLOYMENT_GUIDE_COMPACT.md) | Quick reference for deployment procedures | DevOps, Operations |
| **Production Deployment** | [`production-deployment.md`](./production-deployment.md) | Production-specific deployment considerations | DevOps, Operations |
| **Setup Guide** | [`SETUP_GUIDE.md`](./SETUP_GUIDE.md) | General setup and installation guide | All Users |

### 🔐 Security & Compliance

| Document | Path | Description | Audience |
|----------|------|-------------|----------|
| **Security** | [`SECURITY.md`](./SECURITY.md) | Overall security documentation and best practices | Security Team, Developers |
| **GDPR Compliance** | [`GDPR_COMPLIANCE.md`](./GDPR_COMPLIANCE.md) | GDPR compliance documentation and requirements | Legal, Compliance Team |
| **GDPR Implementation Checklist** | [`GDPR_IMPLEMENTATION_CHECKLIST.md`](./GDPR_IMPLEMENTATION_CHECKLIST.md) | Implementation checklist for GDPR requirements | Legal, Compliance Team, Developers |
| **Permissions and Policies** | [`PERMISSIONS_AND_POLICIES.md`](./PERMISSIONS_AND_POLICIES.md) | Access control policies and permissions | Security Team, Administrators |
| **Permissions Matrix** | [`PERMISSIONS_MATRIX.md`](./PERMISSIONS_MATRIX.md) | Detailed permission matrix for user roles | Security Team, Administrators |
| **Superadmin Implementation** | [`SUPERADMIN_IMPLEMENTATION.md`](./SUPERADMIN_IMPLEMENTATION.md) | Superadmin role implementation details | Security Team, Developers |

### 📋 Testing & Quality

| Document | Path | Description | Audience |
|----------|------|-------------|----------|
| **Testing Guidelines** | [`testing-guidelines.md`](./testing-guidelines.md) | Testing standards and best practices | Developers, QA Team |
| **Contributing Guide** | [`CONTRIBUTING.md`](./CONTRIBUTING.md) | Guidelines for contributing to the project | Developers, Contributors |

### 🎨 Design & UI

| Document | Path | Description | Audience |
|----------|------|-------------|----------|
| **UI Guidelines** | [`ui-guidelines.md`](./ui-guidelines.md) | Design system and styling guidelines | Designers, Developers |
| **UI Redesign Proposal** | [`ui-redesign-proposal.md`](./ui-redesign-proposal.md) | Proposal for UI redesign improvements | Design Team, Stakeholders |
| **UI Redesign Implementation Summary** | [`ui-redesign-implementation-summary.md`](./ui-redesign-implementation-summary.md) | Summary of UI redesign implementation | Design Team, Developers |
| **UI Components Migration Guide** | [`ui_components_migration_guide.md`](./ui_components_migration_guide.md) | Guide for migrating UI components | Developers |
| **UI Routes and Navigation** | [`ui_routes_and_navigation.md`](./ui_routes_and_navigation.md) | UI routing and navigation documentation | Developers, UX Team |
| **Design Tokens Proposal** | [`design_tokens_proposal.md`](./design_tokens_proposal.md) | Proposal for design system tokens | Design Team, Developers |
| **CSS Variables Consistency Plan** | [`css_variables_consistency_plan.md`](./css_variables_consistency_plan.md) | Plan for CSS variable consistency | Developers, Designers |

### 📦 Integration & External Systems

| Document | Path | Description | Audience |
|----------|------|-------------|----------|
| **Timex Integration** | [`TIMEX_INTEGRATION.md`](./TIMEX_INTEGRATION.md) | Integration documentation for Timex library | Developers |
| **Minimal Booking Core** | [`minimal_booking_core.md`](./minimal_booking_core.md) | Core booking functionality documentation | Developers, Product Team |

### 📋 Administration & Management

| Document | Path | Description | Audience |
|----------|------|-------------|----------|
| **User Actions** | [`USER_ACTIONS.md`](./USER_ACTIONS.md) | User action management and workflows | Administrators |
| **Rules** | [`RULES.md`](./RULES.md) | System rules and regulations | Administrators, Users |
| **Registration Improvements** | [`REGISTRATION_IMPROVEMENTS.md`](./REGISTRATION_IMPROVEMENTS.md) | Registration system improvements | Product Team, Developers |

### 🗄️ Archive & Historical Documentation

| Document | Path | Description | Audience |
|----------|------|-------------|----------|
| **Component Specifications Archive** | [`archive/component_specs/`](./archive/component_specs/) | Archived component specifications for reference | Developers, Historians |
| **Migration Documents Archive** | [`archive/migration_docs/`](./archive/migration_docs/) | Archived migration documentation | Developers, Historians |

### 🔧 Package Technical Documentation

| Document | Path | Description | Audience |
|----------|------|-------------|----------|
| **Riva Ash Technical Documentation** | [`packages/riva_ash/docs/README.md`](../packages/riva_ash/docs/README.md) | Navigation hub for package-specific technical documentation | Developers, Architects |
| **Ash Architecture TODOs** | [`packages/riva_ash/docs/ash_architecture_todo.md`](../packages/riva_ash/docs/ash_architecture_todo.md) | Tasks for aligning with Ash Framework best practices | Developers, Architects |
| **Testing Guide** | [`packages/riva_ash/docs/testing_guide.md`](../packages/riva_ash/docs/testing_guide.md) | Comprehensive testing guidelines and best practices | Developers, QA Team |
| **Glossary** | [`packages/riva_ash/docs/glossary.md`](../packages/riva_ash/docs/glossary.md) | Technical terms and definitions | All Users |
| **Property-Based Browser Testing** | [`packages/riva_ash/docs/property_based_browser_testing.md`](../packages/riva_ash/docs/property_based_browser_testing.md) | Testing approach using property-based browser testing | QA Team, Developers |

### 📊 Visual Documentation (Mermaid Diagrams)

| Document | Path | Description | Audience |
|----------|------|-------------|----------|
| **Basic Reactor Flow** | [`packages/riva_ash/docs/basic_reactor_flow.mmd`](../packages/riva_ash/docs/basic_reactor_flow.mmd) | Flow diagram for basic reactor workflows | Developers, Architects |
| **Recurring Reservation Reactor Flow** | [`packages/riva_ash/docs/recurring_reservation_reactor_flow.mmd`](../packages/riva_ash/docs/recurring_reservation_reactor_flow.mmd) | Complex reactor flow for recurring reservations | Developers, Architects |
| **Recurring Reservation Workflow** | [`packages/riva_ash/docs/recurring_reservation_workflow.mmd`](../packages/riva_ash/docs/recurring_reservation_workflow.mmd) | End-to-end workflow for recurring reservation processing | Developers, Product Team |

### 📄 Compact Documentation

| Document | Path | Description | Audience |
|----------|------|-------------|----------|
| **Compact Documentation** | [`COMPACT_DOCS.md`](./COMPACT_DOCS.md) | Compact version of key documentation | Quick Reference |
| **Component Library Documentation Plan** | [`component_library_documentation_plan.md`](./component_library_documentation_plan.md) | Plan for component library documentation | Documentation Team |
| **Component Library Specifications** | [`component_library_specifications.md`](./component_library_specifications.md) | Specifications for component library | Developers, Designers |
| **Component Stories Update Plan** | [`component_stories_update_plan.md`](./component_stories_update_plan.md) | Plan for updating component stories | Developers, Documentation Team |
| **Button Component Update Plan** | [`button_component_update_plan.md`](./button_component_update_plan.md) | Plan for button component updates | Developers |
| **Input Component Update Plan** | [`input_component_update_plan.md`](./input_component_update_plan.md) | Plan for input component updates | Developers |
| **Shadcn Component Library Implementation Summary** | [`shadcn_component_library_implementation_summary.md`](./shadcn_component_library_implementation_summary.md) | Summary of shadcn component library implementation | Developers, Product Team |
| **Documentation Alignment Checklist** | [`DOCUMENTATION_ALIGNMENT_CHECKLIST.md`](./DOCUMENTATION_ALIGNMENT_CHECKLIST.md) | Checklist for documentation alignment | Documentation Team |
| **Review 2025-08-11 Compile Fix Recommendations** | [`review_2025-08-11_compile_fix_recommendations.md`](./review_2025-08-11_compile_fix_recommendations.md) | Review of compile fix recommendations | Developers |
| **Routes and Navigation** | [`routes-and-navigation.md`](./routes-and-navigation.md) | Routes and navigation documentation | Developers |
| **Routes Checklist** | [`routes-checklist.md`](./routes-checklist.md) | Checklist for routes implementation | Developers |
| **Devtools** | [`devtools.md`](./devtools.md) | Development tools documentation | Developers |
| **Calendar View Specification** | [`calendar_view_spec.md`](./calendar_view_spec.md) | Calendar view component specification | Developers |
| **Text Component Specification** | [`text_spec.md`](./text_spec.md) | Text component specification | Developers |

## 🎯 Quick Start Guides

### For New Developers
1. Start with [`GETTING_STARTED.md`](./GETTING_STARTED.md) for installation
2. Read [`DEVELOPMENT_GUIDE.md`](./DEVELOPMENT_GUIDE.md) for development practices
3. Review [`ARCHITECTURE.md`](./ARCHITECTURE.md) to understand the system
4. Check [`API_REFERENCE.md`](./API_REFERENCE.md) for complete API documentation
5. Use [`API_REFERENCE_COMPACT.md`](./API_REFERENCE_COMPACT.md) for quick API reference

### For End Users
1. Begin with [`USER_GUIDE.md`](./USER_GUIDE.md) for comprehensive instructions
2. Use [`GETTING_STARTED.md`](./GETTING_STARTED.md) for first-time setup
3. Refer to component specifications for specific feature details

### For Architects & Designers
1. Study [`ARCHITECTURE.md`](./ARCHITECTURE.md) for system design
2. Review [`architecture-guidelines.md`](./architecture-guidelines.md) for best practices
3. Examine component specifications for UI/UX details

## 🔍 Navigation Tips

- **Use the table above** to quickly locate documentation by category
- **Check both root and subdirectory** files for comprehensive coverage
- **Look for compact versions** of documents marked with "(Compact)" for quick reference
- **Review archived documentation** for historical context and deprecated features
- **Follow cross-references** within documents for related topics

## 📝 Documentation Maintenance

### Adding New Documentation
1. Place new files in the appropriate category directory
2. Update this index with the new entry
3. Ensure cross-references are properly maintained
4. Follow the established naming conventions

### Updating Existing Documentation
1. Review the index to ensure consistency
2. Update cross-references if file paths change
3. Mark deprecated/archived files appropriately
4. Maintain version control for documentation changes

### File Naming Conventions
- Use descriptive, lowercase names with hyphens
- Group related files with consistent prefixes
- Use `_spec.md` suffix for component specifications
- Use `_guide.md` suffix for user guides
- Use `_plan.md` suffix for planning documents

---

*This documentation index is automatically generated and should be maintained as the project evolves. For questions or suggestions about documentation structure, please contact the documentation team.*