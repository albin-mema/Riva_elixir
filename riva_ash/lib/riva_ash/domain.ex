defmodule RivaAsh.Domain do
  use Ash.Domain,
    extensions: [
      AshJsonApi.Domain, 
      AshAdmin.Domain,
      AshPaperTrail.Domain
    ]
    
  # Configure AshPaperTrail to include version tables in the domain
  paper_trail do
    # Auto-include all version resources
    include_versions? true
  end

  resources do
    resource RivaAsh.Resources.Business
    resource RivaAsh.Resources.Section
    resource RivaAsh.Resources.Item
    resource RivaAsh.Resources.Client
    resource RivaAsh.Resources.Reservation
  end

  json_api do
    prefix "/api"

    open_api do
      tag "Businesses"
      tag "Sections"
      tag "Items"
      tag "Clients"
      tag "Reservations"
      group_by :resource
    end
  end

  admin do
    show? true
  end
end
