defmodule RivaAsh.Domain do
  use Ash.Domain,
    extensions: [
      AshJsonApi.Domain,
      AshGraphql.Domain,
      AshAdmin.Domain,
      AshPaperTrail.Domain
    ]

  # Configure AshPaperTrail to include version tables in the domain
  paper_trail do
    # Auto-include all version resources
    include_versions?(true)
  end

  resources do
    resource(RivaAsh.Resources.Business)
    resource(RivaAsh.Resources.Plot)
    resource(RivaAsh.Resources.Section)
    resource(RivaAsh.Resources.ItemType)
    resource(RivaAsh.Resources.Item)
    resource(RivaAsh.Resources.Layout)
    resource(RivaAsh.Resources.ItemPosition)
    resource(RivaAsh.Resources.ItemHold)
    resource(RivaAsh.Resources.Client)
    resource(RivaAsh.Resources.Employee)
    resource(RivaAsh.Resources.Permission)
    resource(RivaAsh.Resources.EmployeePermission)
    resource(RivaAsh.Resources.Reservation)
    resource(RivaAsh.Resources.Pricing)
    resource(RivaAsh.Resources.Payment)
    resource(RivaAsh.Resources.ItemSchedule)
    resource(RivaAsh.Resources.AvailabilityException)
    resource(RivaAsh.Resources.RecurringReservation)
    resource(RivaAsh.Resources.RecurringReservationInstance)
    resource(RivaAsh.GDPR.ConsentRecord)
  end

  json_api do
    prefix("/api")

    open_api do
      tag("Businesses")
      tag("Sections")
      tag("Item Types")
      tag("Items")
      tag("Layouts")
      tag("Item Positions")
      tag("Clients")
      tag("Employees")
      tag("Permissions")
      tag("Employee Permissions")
      tag("Reservations")
      tag("Pricing")
      tag("Payments")
      tag("Item Schedules")
      tag("Availability Exceptions")
      tag("Recurring Reservations")
      tag("Recurring Reservation Instances")
      tag("GDPR Consent Records")
      group_by(:resource)
    end
  end

  graphql do
    # Generate queries and mutations for all resources
    authorize?(false)  # Disable authorization for now, can be enabled later
  end

  admin do
    show?(true)
  end
end
