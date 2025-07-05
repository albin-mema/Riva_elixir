defmodule RivaAshWeb.JsonApiRouter do
  use AshJsonApi.Router,
    domains: [RivaAsh.Domain],
    open_api: "/open_api"
end
