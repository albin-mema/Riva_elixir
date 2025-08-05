# Elixir Files Checklist

This is a comprehensive list of all Elixir files (.ex and .exs) in the Riva_Ash codebase.

## Configuration Files
- [x] packages/riva_ash/.formatter.exs
- [x] packages/riva_ash/config/config.exs
- [x] packages/riva_ash/config/dev.exs
- [x] packages/riva_ash/config/prod.exs
- [x] packages/riva_ash/config/runtime.exs
- [x] packages/riva_ash/config/test.exs
- [x] packages/riva_ash/config/test_no_sandbox.exs

## Mix Project Files
- [x] packages/riva_ash/mix.exs

## Main Application Files
- [x] packages/riva_ash/lib/riva_ash.ex
- [x] packages/riva_ash/lib/riva_ash/application.ex
- [x] packages/riva_ash/lib/riva_ash/domain.ex
- [x] packages/riva_ash/lib/riva_ash/repo.ex
- [x] packages/riva_ash/lib/riva_ash/release.ex

## Core Business Logic
- [x] packages/riva_ash/lib/riva_ash/authorization.ex
- [x] packages/riva_ash/lib/riva_ash/availability.ex
- [x] packages/riva_ash/lib/riva_ash/booking.ex
- [x] packages/riva_ash/lib/riva_ash/changes.ex
- [x] packages/riva_ash/lib/riva_ash/database_health.ex
- [x] packages/riva_ash/lib/riva_ash/date_time_helpers.ex
- [x] packages/riva_ash/lib/riva_ash/error_helpers.ex
- [x] packages/riva_ash/lib/riva_ash/mermaid.ex
- [x] packages/riva_ash/lib/riva_ash/permissions.ex
- [x] packages/riva_ash/lib/riva_ash/queries.ex
- [x] packages/riva_ash/lib/riva_ash/recurring_reservations.ex
- [x] packages/riva_ash/lib/riva_ash/resource_helpers.ex
- [x] packages/riva_ash/lib/riva_ash/validations.ex

## Accounts Module
- [x] packages/riva_ash/lib/riva_ash/accounts/accounts.ex
- [x] packages/riva_ash/lib/riva_ash/accounts/rate_limiter.ex

## GDPR Compliance
- [x] packages/riva_ash/lib/riva_ash/gdpr/consent_record.ex
- [x] packages/riva_ash/lib/riva_ash/gdpr/data_subject_rights.ex
- [x] packages/riva_ash/lib/riva_ash/gdpr/retention_policy.ex

## Background Jobs
- [x] packages/riva_ash/lib/riva_ash/jobs/gdpr_retention_job.ex
- [x] packages/riva_ash/lib/riva_ash/jobs/hold_cleanup_job.ex

## Permissions System
- [x] packages/riva_ash/lib/riva_ash/permissions/constants.ex

## Policies
- [x] packages/riva_ash/lib/riva_ash/policies/ownership_check.ex
- [x] packages/riva_ash/lib/riva_ash/policies/permission_check.ex

## Reactors (Business Workflows)
- [x] packages/riva_ash/lib/riva_ash/reactors/business_setup_flow.ex
- [x] packages/riva_ash/lib/riva_ash/reactors/example_reactor.ex
- [x] packages/riva_ash/lib/riva_ash/reactors/reservation_reactor.ex

## Resources (Data Models)
- [x] packages/riva_ash/lib/riva_ash/resources/availability_exception.ex
- [x] packages/riva_ash/lib/riva_ash/resources/business.ex
- [x] packages/riva_ash/lib/riva_ash/resources/client.ex
- [x] packages/riva_ash/lib/riva_ash/resources/employee.ex
- [x] packages/riva_ash/lib/riva_ash/resources/employee_permission.ex
- [x] packages/riva_ash/lib/riva_ash/resources/item.ex
- [x] packages/riva_ash/lib/riva_ash/resources/item_hold.ex
- [x] packages/riva_ash/lib/riva_ash/resources/item_position.ex
- [x] packages/riva_ash/lib/riva_ash/resources/item_schedule.ex
- [x] packages/riva_ash/lib/riva_ash/resources/item_type.ex
- [x] packages/riva_ash/lib/riva_ash/resources/layout.ex
- [x] packages/riva_ash/lib/riva_ash/resources/payment.ex
- [x] packages/riva_ash/lib/riva_ash/resources/permission.ex
- [x] packages/riva_ash/lib/riva_ash/resources/plot.ex
- [ ] packages/riva_ash/lib/riva_ash/resources/pricing.ex
- [ ] packages/riva_ash/lib/riva_ash/resources/recurring_reservation.ex
- [ ] packages/riva_ash/lib/riva_ash/resources/recurring_reservation_instance.ex
- [x] packages/riva_ash/lib/riva_ash/resources/reservation.ex
- [x] packages/riva_ash/lib/riva_ash/resources/section.ex
- [ ] packages/riva_ash/lib/riva_ash/resources/token.ex
- [ ] packages/riva_ash/lib/riva_ash/resources/user.ex

## Storybook Testing
- [ ] packages/riva_ash/lib/riva_ash/storybook_testing/property_generators.ex

## Validations
- [x] packages/riva_ash/lib/riva_ash/validations/consecutive_days.ex
- [x] packages/riva_ash/lib/riva_ash/validations/full_day_reservation.ex
- [x] packages/riva_ash/lib/riva_ash/validations/reservation_time_slot.ex

## Web Layer - Main Files
- [x] packages/riva_ash/lib/riva_ash_web.ex
- [x] packages/riva_ash/lib/riva_ash_web/ash_admin_config.ex
- [x] packages/riva_ash/lib/riva_ash_web/endpoint.ex
- [x] packages/riva_ash/lib/riva_ash_web/gettext.ex
- [x] packages/riva_ash/lib/riva_ash_web/graphql_router.ex
- [x] packages/riva_ash/lib/riva_ash_web/json_api_router.ex
- [x] packages/riva_ash/lib/riva_ash_web/layout_hook.ex
- [x] packages/riva_ash/lib/riva_ash_web/router.ex
- [x] packages/riva_ash/lib/riva_ash_web/storybook.ex
- [x] packages/riva_ash/lib/riva_ash_web/telemetry.ex

## Components - Atomic Design System
- [x] packages/riva_ash/lib/riva_ash_web/components/atomic_components.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/core_components.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/layouts.ex

## Components - Atoms
- [x] packages/riva_ash/lib/riva_ash_web/components/atoms/all_atoms.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/atoms/avatar.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/atoms/badge.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/atoms/button.ex
- [ ] packages/riva_ash/lib/riva_ash_web/components/atoms/checkbox.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/atoms/date_picker.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/atoms/icon.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/atoms/input.ex
- [ ] packages/riva_ash/lib/riva_ash_web/components/atoms/radio.ex
- [ ] packages/riva_ash/lib/riva_ash_web/components/atoms/select.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/atoms/spinner.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/atoms/text.ex
- [ ] packages/riva_ash/lib/riva_ash_web/components/atoms/text_input.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/atoms/time_picker.ex
- [ ] packages/riva_ash/lib/riva_ash_web/components/atoms/toggle.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/atoms/tooltip.ex

## Components - Business Domain
- [x] packages/riva_ash/lib/riva_ash_web/components/business/business_card.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/business/business_form.ex

## Components - Forms
- [x] packages/riva_ash/lib/riva_ash_web/components/forms/item_type_form.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/forms/layout_form.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/forms/payment_form.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/forms/plot_form.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/forms/recurring_reservation_instance_form.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/forms/reservation_booking_form.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/forms/reservation_form.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/forms/schedule_form.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/forms/section_form.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/forms/token_form.ex

## Components - Interactive
- [x] packages/riva_ash/lib/riva_ash_web/components/interactive/availability_grid.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/interactive/daily_schedule.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/interactive/grid_position_picker.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/interactive/monthly_calendar.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/interactive/plot_layout_designer.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/interactive/recurrence_pattern.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/interactive/time_slot_picker.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/interactive/weekly_calendar.ex

## Components - Molecules
- [x] packages/riva_ash/lib/riva_ash_web/components/molecules/action_menu.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/molecules/breadcrumb_nav.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/molecules/card.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/molecules/confirm_dialog.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/molecules/empty_state.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/molecules/filter_panel.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/molecules/form_field.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/molecules/notification_toast.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/molecules/pagination.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/molecules/progress_bar.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/molecules/search_bar.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/molecules/status_indicator.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/molecules/tab_navigation.ex

## Components - Navigation
- [x] packages/riva_ash/lib/riva_ash_web/components/navigation/breadcrumb_trail.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/navigation/expanded_sidebar.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/navigation/notification_center.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/navigation/quick_actions.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/navigation/search_global.ex

## Components - Organisms
- [x] packages/riva_ash/lib/riva_ash_web/components/organisms/business_card.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/organisms/business_form.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/organisms/calendar_view.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/organisms/client_form.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/organisms/dashboard_stats.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/organisms/data_table.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/organisms/employee_form.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/organisms/item_form.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/organisms/layout_designer.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/organisms/page_header.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/organisms/permission_matrix.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/organisms/pricing_form.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/organisms/reservation_form.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/organisms/timeline_view.ex

## Components - Templates
- [x] packages/riva_ash/lib/riva_ash_web/components/templates/calendar_template.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/templates/dashboard_template.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/templates/detail_view_template.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/templates/form_view_template.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/templates/list_view_template.ex

## Components - UI (SaladUI)
- [ ] packages/riva_ash/lib/riva_ash_web/components/ui.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/ui/alert.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/ui/badge.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/ui/button.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/ui/card.ex
- [ ] packages/riva_ash/lib/riva_ash_web/components/ui/card_content.ex
- [ ] packages/riva_ash/lib/riva_ash_web/components/ui/card_description.ex
- [ ] packages/riva_ash/lib/riva_ash_web/components/ui/card_footer.ex
- [ ] packages/riva_ash/lib/riva_ash_web/components/ui/card_header.ex
- [ ] packages/riva_ash/lib/riva_ash_web/components/ui/card_title.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/ui/checkbox.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/ui/icon.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/ui/input.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/ui/select.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/ui/spinner.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/ui/text.ex
- [x] packages/riva_ash/lib/riva_ash_web/components/ui/textarea.ex

## Controllers
- [x] packages/riva_ash/lib/riva_ash_web/controllers/auth_controller.ex
- [x] packages/riva_ash/lib/riva_ash_web/controllers/auth_helpers.ex
- [x] packages/riva_ash/lib/riva_ash_web/controllers/auth_html.ex
- [x] packages/riva_ash/lib/riva_ash_web/controllers/booking_controller.ex
- [x] packages/riva_ash/lib/riva_ash_web/controllers/error_json.ex
- [x] packages/riva_ash/lib/riva_ash_web/controllers/fallback_controller.ex
- [x] packages/riva_ash/lib/riva_ash_web/controllers/health_controller.ex
- [x] packages/riva_ash/lib/riva_ash_web/controllers/mermaid_controller.ex
- [x] packages/riva_ash/lib/riva_ash_web/controllers/swagger_controller.ex
- [x] packages/riva_ash/lib/riva_ash_web/controllers/ui_demo_controller.ex
- [x] packages/riva_ash/lib/riva_ash_web/controllers/ui_demo_html.ex

## Dev Tools
- [x] packages/riva_ash/lib/riva_ash_web/dev_tools/metrics_store.ex

## LiveView Pages
- [ ] packages/riva_ash/lib/riva_ash_web/live/auth_helpers.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/auth/register_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/auth/sign_in_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/availability_exception_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/booking_calendar_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/business_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/business_setup_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/client_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/dashboard_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/employee_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/financial_operations_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/global_search_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/inventory_management_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/item_hold_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/item_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/item_position_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/item_schedule_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/item_type_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/layout_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/payment_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/people_management_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/permission_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/plot_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/pricing_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/recurring_reservation_instance_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/recurring_reservation_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/reservation_center_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/reservation_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/section_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/superadmin_dashboard_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/system_settings_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/token_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/user_live.ex

## LiveView Dev Tools
- [ ] packages/riva_ash/lib/riva_ash_web/live/dev_tools/ash_inspector_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/dev_tools/business_context_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/dev_tools/dev_tools_home_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/dev_tools/performance_dashboard_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/dev_tools/policy_visualizer_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/dev_tools/reactor_visualizer_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/dev_tools/test_data_generator_live.ex

## LiveView Error Pages
- [ ] packages/riva_ash/lib/riva_ash_web/live/error/access_denied_live.ex
- [ ] packages/riva_ash/lib/riva_ash_web/live/error/not_found_live.ex

## Mix Tasks
- [ ] packages/riva_ash/lib/mix/tasks/create_superadmin.ex
- [ ] packages/riva_ash/lib/mix/tasks/erd.ex
- [ ] packages/riva_ash/lib/mix/tasks/routes.check.ex

## Database Migrations
- [ ] packages/riva_ash/priv/repo/migrations/20240100999999_enable_citext.exs
- [ ] packages/riva_ash/priv/repo/migrations/20240101000000_create_users_and_tokens.exs
- [ ] packages/riva_ash/priv/repo/migrations/20240101000001_create_items.exs
- [ ] packages/riva_ash/priv/repo/migrations/20240101000002_create_businesses.exs
- [ ] packages/riva_ash/priv/repo/migrations/20240101000003_create_sections.exs
- [ ] packages/riva_ash/priv/repo/migrations/20240101000004_add_section_to_items.exs
- [ ] packages/riva_ash/priv/repo/migrations/20250702232140_add_clients_and_reservations_extensions_1.exs
- [ ] packages/riva_ash/priv/repo/migrations/20250702232225_create_clients_and_reservations.exs
- [ ] packages/riva_ash/priv/repo/migrations/20250702233854_add_paper_trail_version_tables.exs
- [ ] packages/riva_ash/priv/repo/migrations/20250703064819_add_availability_tables.exs
- [ ] packages/riva_ash/priv/repo/migrations/20250703070126_add_employees_table.exs
- [ ] packages/riva_ash/priv/repo/migrations/20250703134328_add_archival_fields.exs
- [ ] packages/riva_ash/priv/repo/migrations/20250703152356_add_recurring_reservations.exs
- [ ] packages/riva_ash/priv/repo/migrations/20250703175411_add_permissions.exs
- [ ] packages/riva_ash/priv/repo/migrations/20250704181801_make_employee_id_nullable_in_reservations.exs
- [ ] packages/riva_ash/priv/repo/migrations/20250705135251_add_layout_pricing_payment_system.exs
- [ ] packages/riva_ash/priv/repo/migrations/20250705142134_add_reservation_exclusion_constraint.exs
- [ ] packages/riva_ash/priv/repo/migrations/20250705143000_add_client_verification_fields.exs
- [ ] packages/riva_ash/priv/repo/migrations/20250711200613_add_owner_id_to_businesses.exs
- [ ] packages/riva_ash/priv/repo/migrations/20250711222649_check_businesses_table.exs
- [ ] packages/riva_ash/priv/repo/migrations/20250713131054_add_weekday_weekend_pricing.exs
- [ ] packages/riva_ash/priv/repo/migrations/20250713131055_add_performance_indexes.exs
- [ ] packages/riva_ash/priv/repo/migrations/20250713131056_add_denormalized_business_ids.exs
- [ ] packages/riva_ash/priv/repo/migrations/20250713133232_add_missing_archived_at_fields.exs
- [ ] packages/riva_ash/priv/repo/migrations/20250727222949_add_public_search_fields.exs
- [ ] packages/riva_ash/priv/repo/migrations/20250727224917_add_business_location_fields.exs
- [ ] packages/riva_ash/priv/repo/migrations/20250728130926_add_superadmin_role.exs

## Database Seeds
- [ ] packages/riva_ash/priv/repo/seeds.exs
- [ ] packages/riva_ash/priv/repo/seeds/permissions.exs

## Scripts
- [ ] scripts/component_migration.ex
- [ ] packages/riva_ash/debug_phoenix_test.exs
- [ ] packages/riva_ash/test_property_runner.exs
- [ ] packages/riva_ash/scripts/demo_property_testing.exs
- [ ] packages/riva_ash/scripts/generate_reactor_diagram.exs
- [ ] packages/riva_ash/scripts/run_property_tests.exs
- [ ] packages/riva_ash/scripts/run_tests.exs
- [ ] packages/riva_ash/scripts/test_generators.exs
- [ ] packages/riva_ash/scripts/test_runner.exs

## Storybook Files
- [ ] packages/riva_ash/storybook/_root.index.exs
- [ ] packages/riva_ash/storybook/atoms.index.exs
- [ ] packages/riva_ash/storybook/business.index.exs
- [ ] packages/riva_ash/storybook/molecules.index.exs
- [ ] packages/riva_ash/storybook/templates.index.exs
- [ ] packages/riva_ash/storybook/ui.index.exs
- [ ] packages/riva_ash/storybook/design_tokens.story.exs
- [ ] packages/riva_ash/storybook/property_testing.story.exs
- [ ] packages/riva_ash/storybook/welcome.story.exs

## Storybook - Atoms
- [ ] packages/riva_ash/storybook/atoms/avatar.story.exs
- [ ] packages/riva_ash/storybook/atoms/badge.story.exs
- [ ] packages/riva_ash/storybook/atoms/button.story.exs
- [ ] packages/riva_ash/storybook/atoms/checkbox.story.exs
- [ ] packages/riva_ash/storybook/atoms/date_picker.story.exs
- [ ] packages/riva_ash/storybook/atoms/icon.story.exs
- [ ] packages/riva_ash/storybook/atoms/input.story.exs
- [ ] packages/riva_ash/storybook/atoms/radio.story.exs
- [ ] packages/riva_ash/storybook/atoms/select.story.exs
- [ ] packages/riva_ash/storybook/atoms/spinner.story.exs
- [ ] packages/riva_ash/storybook/atoms/text.story.exs
- [ ] packages/riva_ash/storybook/atoms/text_input.story.exs
- [ ] packages/riva_ash/storybook/atoms/time_picker.story.exs
- [ ] packages/riva_ash/storybook/atoms/toggle.story.exs
- [ ] packages/riva_ash/storybook/atoms/tooltip.story.exs

## Storybook - Business
- [ ] packages/riva_ash/storybook/business/business_card.story.exs

## Storybook - Combinations
- [ ] packages/riva_ash/storybook/combinations/dashboard_header.story.exs
- [ ] packages/riva_ash/storybook/combinations/data_table.story.exs
- [ ] packages/riva_ash/storybook/combinations/form_with_validation.story.exs
- [ ] packages/riva_ash/storybook/combinations/reservation_card.story.exs
- [ ] packages/riva_ash/storybook/combinations/ui_component_showcase.story.exs

## Storybook - Molecules
- [ ] packages/riva_ash/storybook/molecules/action_menu.story.exs
- [ ] packages/riva_ash/storybook/molecules/breadcrumb_nav.story.exs
- [ ] packages/riva_ash/storybook/molecules/card.story.exs
- [ ] packages/riva_ash/storybook/molecules/confirm_dialog.story.exs
- [ ] packages/riva_ash/storybook/molecules/empty_state.story.exs
- [ ] packages/riva_ash/storybook/molecules/filter_panel.story.exs
- [ ] packages/riva_ash/storybook/molecules/form_field.story.exs
- [ ] packages/riva_ash/storybook/molecules/notification_toast.story.exs
- [ ] packages/riva_ash/storybook/molecules/pagination.story.exs
- [ ] packages/riva_ash/storybook/molecules/progress_bar.story.exs
- [ ] packages/riva_ash/storybook/molecules/search_bar.story.exs
- [ ] packages/riva_ash/storybook/molecules/status_indicator.story.exs
- [ ] packages/riva_ash/storybook/molecules/tab_navigation.story.exs

## Storybook - Organisms
- [ ] packages/riva_ash/storybook/organisms/business_card.story.exs
- [ ] packages/riva_ash/storybook/organisms/business_form.story.exs
- [ ] packages/riva_ash/storybook/organisms/calendar_view.story.exs
- [ ] packages/riva_ash/storybook/organisms/client_form.story.exs
- [ ] packages/riva_ash/storybook/organisms/dashboard_stats.story.exs
- [ ] packages/riva_ash/storybook/organisms/data_table.story.exs
- [ ] packages/riva_ash/storybook/organisms/employee_form.story.exs
- [ ] packages/riva_ash/storybook/organisms/header.story.exs
- [ ] packages/riva_ash/storybook/organisms/item_form.story.exs
- [ ] packages/riva_ash/storybook/organisms/layout_designer.story.exs
- [ ] packages/riva_ash/storybook/organisms/page_header.story.exs
- [ ] packages/riva_ash/storybook/organisms/permission_matrix.story.exs
- [ ] packages/riva_ash/storybook/organisms/pricing_form.story.exs
- [ ] packages/riva_ash/storybook/organisms/reservation_form.story.exs
- [ ] packages/riva_ash/storybook/organisms/timeline_view.story.exs

## Storybook - Templates
- [ ] packages/riva_ash/storybook/templates/calendar_template.story.exs
- [ ] packages/riva_ash/storybook/templates/dashboard_template.story.exs
- [ ] packages/riva_ash/storybook/templates/detail_view_template.story.exs
- [ ] packages/riva_ash/storybook/templates/form_view_template.story.exs
- [ ] packages/riva_ash/storybook/templates/list_view_template.story.exs

## Storybook - UI Components
- [ ] packages/riva_ash/storybook/ui/alert.story.exs
- [ ] packages/riva_ash/storybook/ui/badge.story.exs
- [ ] packages/riva_ash/storybook/ui/button.story.exs
- [ ] packages/riva_ash/storybook/ui/card.story.exs
- [ ] packages/riva_ash/storybook/ui/card_content.story.exs
- [ ] packages/riva_ash/storybook/ui/card_description.story.exs
- [ ] packages/riva_ash/storybook/ui/card_footer.story.exs
- [ ] packages/riva_ash/storybook/ui/card_header.story.exs
- [ ] packages/riva_ash/storybook/ui/card_title.story.exs
- [ ] packages/riva_ash/storybook/ui/checkbox.story.exs
- [ ] packages/riva_ash/storybook/ui/input.story.exs
- [ ] packages/riva_ash/storybook/ui/select.story.exs
- [ ] packages/riva_ash/storybook/ui/textarea.story.exs

## Test Files - Main
- [ ] packages/riva_ash/test/test_helper.exs
- [ ] packages/riva_ash/test/unit_test_helper.exs
- [ ] packages/riva_ash/test/riva_ash_test.exs
- [ ] packages/riva_ash/test/archival_test.exs
- [ ] packages/riva_ash/test/comprehensive_archival_test.exs
- [ ] packages/riva_ash/test/paper_trail_archival_test.exs
- [ ] packages/riva_ash/test/property_testing_simple_test.exs
- [ ] packages/riva_ash/test/property_testing_system_test.exs
- [ ] packages/riva_ash/test/simple_browser_test.exs
- [ ] packages/riva_ash/test/simple_output_test.exs
- [ ] packages/riva_ash/test/simple_property_test.exs
- [ ] packages/riva_ash/test/simple_test.exs
- [ ] packages/riva_ash/test/working_browser_test.exs

## Test Files - Core Domain
- [ ] packages/riva_ash/test/riva_ash/accounts/rate_limiter_test.exs
- [ ] packages/riva_ash/test/riva_ash/authorization_test.exs
- [ ] packages/riva_ash/test/riva_ash/availability_property_test.exs
- [ ] packages/riva_ash/test/riva_ash/availability_test.exs
- [ ] packages/riva_ash/test/riva_ash/booking_test.exs
- [ ] packages/riva_ash/test/riva_ash/changes_test.exs
- [ ] packages/riva_ash/test/riva_ash/database_health_test.exs
- [ ] packages/riva_ash/test/riva_ash/date_time_helpers_test.exs
- [ ] packages/riva_ash/test/riva_ash/error_helpers_test.exs
- [ ] packages/riva_ash/test/riva_ash/mermaid_test.exs
- [ ] packages/riva_ash/test/riva_ash/permissions_test.exs
- [ ] packages/riva_ash/test/riva_ash/queries_test.exs
- [ ] packages/riva_ash/test/riva_ash/recurring_reservations_test.exs
- [ ] packages/riva_ash/test/riva_ash/release_test.exs
- [ ] packages/riva_ash/test/riva_ash/resource_helpers_test.exs
- [ ] packages/riva_ash/test/riva_ash/security_test.exs
- [ ] packages/riva_ash/test/riva_ash/validations_test.exs

## Test Files - GDPR
- [ ] packages/riva_ash/test/riva_ash/gdpr/data_subject_rights_test.exs
- [ ] packages/riva_ash/test/riva_ash/gdpr/retention_policy_test.exs

## Test Files - Jobs
- [ ] packages/riva_ash/test/riva_ash/jobs/gdpr_retention_job_test.exs
- [ ] packages/riva_ash/test/riva_ash/jobs/hold_cleanup_job_test.exs

## Test Files - Policies
- [ ] packages/riva_ash/test/riva_ash/policies/ownership_check_test.exs
- [ ] packages/riva_ash/test/riva_ash/policies/permission_check_test.exs

## Test Files - Reactors
- [ ] packages/riva_ash/test/riva_ash/reactors/business_setup_flow_test.exs
- [ ] packages/riva_ash/test/riva_ash/reactors/example_reactor_test.exs
- [ ] packages/riva_ash/test/riva_ash/reactors/reservation_reactor_test.exs

## Test Files - Resources
- [ ] packages/riva_ash/test/riva_ash/resources/client_test.exs
- [ ] packages/riva_ash/test/riva_ash/resources/payment_resource_test.exs
- [ ] packages/riva_ash/test/riva_ash/resources/pricing_resource_test.exs
- [ ] packages/riva_ash/test/riva_ash/resources/public_search_test.exs
- [ ] packages/riva_ash/test/riva_ash/resources/reservation_resource_test.exs
- [ ] packages/riva_ash/test/riva_ash/resources/reservation_test.exs

## Test Files - Web Layer
- [ ] packages/riva_ash/test/riva_ash_web/authentication_flow_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/authentication_playwright_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/auth_flow_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/liveview_routes_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/property_based_browser_example_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/property_generators_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/route_authorization_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/route_enumeration_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/route_navigation_property_test.exs
- [x] packages/riva_ash/test/riva_ash_web/storybook_property_test.exs

## Test Files - Components
- [ ] packages/riva_ash/test/riva_ash_web/components/component_unit_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/components/ui_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/components/atoms/button_compatibility_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/components/atoms/input_compatibility_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/components/forms/recurring_reservation_instance_form_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/components/interactive/time_slot_picker_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/components/molecules/card_component_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/components/molecules/card_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/components/navigation/sidebar_nav_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/components/ui/button_component_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/components/ui/button_property_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/components/ui/button_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/components/ui/input_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/components/ui/select_component_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/components/ui/textarea_component_test.exs

## Test Files - Controllers
- [ ] packages/riva_ash/test/riva_ash_web/controllers/auth_controller_csrf_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/controllers/booking_controller_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/controllers/health_controller_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/controllers/mermaid_controller_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/controllers/swagger_controller_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/controllers/api/v1/items_controller_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/controllers/api/v1/json_api_contract_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/controllers/api/v1/json_api_test.exs

## Test Files - Integration
- [ ] packages/riva_ash/test/riva_ash_web/integration/no_db_integration_test.exs

## Test Files - LiveView
- [ ] packages/riva_ash/test/riva_ash_web/live/business_live_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/live/business_management_feature_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/live/global_search_live_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/live/payment_live_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/live/react_integration_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/live/recurring_reservation_instance_live_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/live/reservation_center_live_test.exs
- [ ] packages/riva_ash/test/riva_ash_web/live/reservation_live_test.exs

## Test Support Files
- [ ] packages/riva_ash/test/support/component_case.ex
- [ ] packages/riva_ash/test/support/conn_case.ex
- [ ] packages/riva_ash/test/support/data_case.ex
- [ ] packages/riva_ash/test/support/endpoint_case.ex
- [ ] packages/riva_ash/test/support/factory.ex
- [ ] packages/riva_ash/test/support/feature_case.ex
- [ ] packages/riva_ash/test/support/jsonapi_helpers.ex
- [ ] packages/riva_ash/test/support/liveview_helpers.ex
- [ ] packages/riva_ash/test/support/mox_helpers.ex
- [ ] packages/riva_ash/test/support/oban_helpers.ex
- [ ] packages/riva_ash/test/support/property_helpers.ex
- [ ] packages/riva_ash/test/support/property_testing_config.ex
- [ ] packages/riva_ash/test/support/test_helpers.ex
- [ ] packages/riva_ash/test/support/time_helpers.ex
- [ ] packages/riva_ash/test/support/mocks/repo_mock.ex
- [ ] packages/riva_ash/test/support/property_testing/browser_executor.ex
- [ ] packages/riva_ash/test/support/property_testing/data_manager.ex
- [ ] packages/riva_ash/test/support/property_testing/flow_generator.ex
- [ ] packages/riva_ash/test/support/property_testing/route_enumerator.ex
- [ ] packages/riva_ash/test/support/property_testing/state_machine.ex

---

**Total Files:** 500+ Elixir files (.ex and .exs) in the Riva_Ash codebase