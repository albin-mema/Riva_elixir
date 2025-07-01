defmodule RivaAsh.Domain do
  use Ash.Domain,
    extensions: [AshJsonApi.Domain, AshAdmin.Domain]

  resources do
    resource RivaAsh.Resources.Business
    resource RivaAsh.Resources.Section
    resource RivaAsh.Resources.Item
  end

  json_api do
    prefix "/api"

    open_api do
      tag "Businesses"
      tag "Sections"
      tag "Items"
      group_by :resource
    end
  end

  admin do
    show? true
  end
end
