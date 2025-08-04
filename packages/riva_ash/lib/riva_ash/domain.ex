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
    # Authentication resources
    resource(RivaAsh.Accounts.User)
    resource(RivaAsh.Accounts.Token)

    # Business domain resources
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

    # GDPR compliance resources
    resource(RivaAsh.GDPR.ConsentRecord)
  end

  json_api do
    prefix("/api")

    open_api do
      tag("Authentication")
      tag("Users")
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
    # Enable authorization at GraphQL layer; resource policies still apply.
    # Ensure your web layer passes the actor into Absinthe context.
    authorize?(true)
  end

  admin do
    show?(true)
  end
end
